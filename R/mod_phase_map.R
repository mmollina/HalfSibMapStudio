# Stage 6: Maternal phase assembly and map fitting.

mod_phase_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(6, "Phase and maps", "Assemble the mother's linkage phase along each order, then fit maps with an explicitly chosen model."),
    shiny::uiOutput(ns("prereq")),
    bslib::card(bslib::card_header("Groups: phase and model route"), shiny::uiOutput(ns("routes"))),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
      bslib::card(
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
                                      "Assemble phase", shiny::uiOutput(ns("ph_badges"), inline = TRUE))),
        shiny::uiOutput(ns("ph_groups")),
        shiny::uiOutput(ns("ph_form")),
        shiny::actionButton(ns("run_ph"), "Assemble phase", class = "btn-primary hsms-job-btn")),
      bslib::card(
        bslib::card_header("Phase details"),
        shiny::selectInput(ns("ph_detail"), "Group", choices = NULL, width = "200px"),
        shiny::uiOutput(ns("ph_info")),
        shiny::plotOutput(ns("ph_plot"), height = "190px"),
        bslib::navset_tab(
          bslib::nav_panel("Intervals", DT::DTOutput(ns("ph_intervals"))),
          bslib::nav_panel("Markers", DT::DTOutput(ns("ph_markers")))))),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
      bslib::card(
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
                                      "Fit maps", shiny::uiOutput(ns("map_badges"), inline = TRUE))),
        shiny::radioButtons(ns("model"), "Model", c(
          "Source-aware model (fully resolved phase only)" = "source_aware",
          "Population model with phase-block handling" = "population")),
        shiny::uiOutput(ns("model_help")),
        shiny::uiOutput(ns("map_groups")),
        shiny::uiOutput(ns("map_form")),
        shiny::uiOutput(ns("map_block_msg")),
        shiny::actionButton(ns("run_map"), "Fit selected maps", class = "btn-primary hsms-job-btn")),
      bslib::card(
        bslib::card_header("Fit summaries"),
        shiny::uiOutput(ns("fit_notices")),
        DT::DTOutput(ns("fit_table")),
        shiny::p(class = "small text-muted", "Objective values from different models, penalties or marker sets are not comparable evidence; formal model selection is outside this release."))))
}

mod_phase_map_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    imp <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "import"))) NULL else ctx$load("import") })
    groups <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (!hsms_is_current(p, "grouping")) character(0) else hsms_group_ids(p) })
    jv_stage <- function(s) { jv <- ctx$job_view(); !is.null(jv) && jv$stage == s }
    orig <- function(x) { i <- imp(); if (is.null(i)) x else hsms_to_original(i$mapping, "marker", x) }
    phase_of <- function(g) { p <- ctx$proj(); if (is.null(hsms_current_id(p, "phase", g))) NULL else ctx$load("phase", g) }

    output$prereq <- shiny::renderUI({ p <- ctx$proj()
      if (is.null(p)) return(hsms_prereq_ui("Create or open a project first (stage 1)."))
      if (!hsms_is_current(p, "grouping")) return(hsms_prereq_ui("Build current linkage groups and order them in stage 5."))
      NULL })

    route_df <- shiny::reactive({
      p <- ctx$proj(); gs <- groups(); if (!length(gs)) return(NULL)
      do.call(rbind, lapply(gs, function(g) {
        ph <- if (hsms_is_current(p, "phase", g)) phase_of(g) else NULL
        rt <- if (!is.null(ph)) hsms_phase_routes(ph) else NULL
        data.frame(group = g, name = unname(hsms_group_label(p, g)),
                   order = hsms_stage_status(p, "order", g), phase = hsms_stage_status(p, "phase", g),
                   components = if (!is.null(ph)) ph$summary$n_components else NA_integer_,
                   unresolved_intervals = if (!is.null(ph)) ph$summary$n_unresolved else NA_integer_,
                   route = if (is.null(rt)) "assemble phase first" else if (rt$source_aware) "resolved: source-aware or population"
                           else if (rt$population) "unresolved: population model with phase blocks" else "single marker: no map",
                   model = p$map_spec[[g]]$model %||% "-", map = hsms_stage_status(p, "map", g), stringsAsFactors = FALSE)
      }))
    })
    output$routes <- shiny::renderUI({
      r <- route_df(); if (is.null(r)) return(hsms_empty_state("No current groups", "Build groups and order them first."))
      shiny::tags$table(class = "table table-sm", shiny::tags$thead(shiny::tags$tr(lapply(
        c("Group", "Order", "Phase", "Components", "Unresolved intervals", "Route", "Model", "Map"), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(r)), function(i) shiny::tags$tr(
          shiny::tags$td(r$name[i]), shiny::tags$td(hsms_status_badge(r$order[i])), shiny::tags$td(hsms_status_badge(r$phase[i])),
          shiny::tags$td(r$components[i] %||% "NA"), shiny::tags$td(r$unresolved_intervals[i] %||% "NA"), shiny::tags$td(r$route[i]),
          shiny::tags$td(r$model[i]), shiny::tags$td(hsms_status_badge(r$map[i]))))))
    })
    group_checks <- function(id, eligible, label) {
      p <- ctx$proj(); gs <- groups(); gs <- gs[vapply(gs, eligible, logical(1))]
      if (!length(gs)) return(hsms_notice(type = "info", sprintf("No group is ready to %s.", label)))
      shiny::checkboxGroupInput(ns(id), "Groups", inline = TRUE, choices = stats::setNames(gs, hsms_group_label(p, gs)),
                                selected = intersect(shiny::isolate(input[[id]]) %||% gs, gs))
    }
    output$ph_groups <- shiny::renderUI(group_checks("ph_sel", function(g) hsms_is_current(ctx$proj(), "order", g), "phase"))
    output$ph_form <- shiny::renderUI({ ctx$proj_id(); hsms_settings_form(ns, "phase", ctx$defaults$phase) })
    ph_draft <- shiny::reactive(hsms_read_form(input, "phase"))
    output$ph_badges <- shiny::renderUI({ p <- ctx$proj(); gs <- groups(); if (!length(gs)) return(NULL)
      d <- input$ph_detail
      shiny::tagList(if (!is.null(d) && !is.null(p$phase_spec[[d]])) hsms_draft_badge(hsms_form_differs(ph_draft(), p$phase_spec[[d]], "phase")),
                     hsms_status_badge(if (jv_stage("phase")) "running" else hsms_combine_status(lapply(gs, function(g) hsms_stage_status(p, "phase", g))))) })
    shiny::observeEvent(input$run_ph, {
      p <- ctx$proj(); gs <- input$ph_sel
      if (!length(gs)) { ctx$notify("Select at least one group with a current order.", "warning"); return() }
      s <- ph_draft(); errs <- hsms_validate_settings("phase", s)
      if (length(errs)) { ctx$notify(paste(errs, collapse = " "), "error"); return() }
      for (g in gs) p <- hsms_set_phase_spec(p, g, s)
      ctx$set_proj(p); ctx$log("phase_settings_applied", list(groups = gs, min_phase_lod = s$min_phase_lod))
      ctx$launch("phase", hsms_group_items(p, ctx$dir(), "phase", gs), label = sprintf("Phase assembly (%d group(s))", length(gs)))
    })
    shiny::observe({ gs <- groups(); p <- ctx$proj()
      hsms_update_select(session, "ph_detail", if (length(gs)) stats::setNames(gs, hsms_group_label(p, gs)) else character(0), shiny::isolate(input$ph_detail)) })
    ph <- shiny::reactive({ g <- input$ph_detail; shiny::req(g); phase_of(g) })
    output$ph_info <- shiny::renderUI({
      x <- ph(); p <- ctx$proj(); g <- input$ph_detail
      if (is.null(x)) return(hsms_empty_state("No phase for this group", "Assemble the phase for this group."))
      s <- x$summary; rt <- hsms_phase_routes(x)
      shiny::tagList(
        if (!hsms_is_current(p, "phase", g)) hsms_notice(type = "warning", shiny::strong("Outdated: "), "the order or phase settings changed."),
        hsms_kv(`Markers` = nrow(s$markers), `Phase components` = s$n_components,
                `Component sizes` = paste(s$component_sizes, collapse = ", "),
                `Unresolved adjacent intervals` = s$n_unresolved, `Assembly converged` = as.character(s$converged),
                `Minimum phase LOD used` = p$phase_spec[[g]]$min_phase_lod %||% "NA"),
        hsms_notice(type = if (rt$source_aware) "success" else "warning", rt$reason),
        shiny::p(class = "small text-muted", "Unresolved intervals are never filled with coupling, repulsion or a distance. Homolog labels are comparable only within one component."))
    })
    output$ph_plot <- shiny::renderPlot({ x <- ph(); shiny::req(x, nrow(x$summary$intervals) > 0); hsms_plot_phase(x$summary$intervals) }, res = 96)
    output$ph_intervals <- DT::renderDT({ x <- ph(); shiny::req(x)
      iv <- x$summary$intervals; iv$from <- orig(iv$from); iv$to <- orig(iv$to)
      names(iv)[names(iv) == "direct_lod"] <- "direct_adjacent_lod"
      hsms_dt(iv, page_length = 10, scroll_y = "260px") })
    output$ph_markers <- DT::renderDT({ x <- ph(); shiny::req(x)
      m <- x$summary$markers; m$marker <- orig(m$marker); names(m)[names(m) == "homolog_label"] <- "homolog_label_within_component"
      hsms_dt(m, page_length = 10, scroll_y = "260px") })

    # ---- map fitting ----
    output$model_help <- shiny::renderUI({
      if (identical(input$model, "source_aware"))
        shiny::p(class = "small text-muted", "Pairs the transmitted maternal homolog with a paternal source state, so paternal haplotype-sharing tracts are modelled separately from maternal recombination. Alpha and beta are transition probabilities per marker step (entering / leaving the sharing mode); they are not physical rates or numbers of fathers.")
      else shiny::p(class = "small text-muted", "Fits the identifiable gametic paternal model within each resolved phase block (hmm_map_blocks). Unresolved intervals split the order into blocks; no distance is estimated across them.")
    })
    eligible_map <- function(g) {
      p <- ctx$proj(); if (!hsms_is_current(p, "phase", g)) return(FALSE)
      rt <- hsms_phase_routes(phase_of(g))
      if (identical(input$model, "source_aware")) rt$source_aware else rt$population
    }
    output$map_groups <- shiny::renderUI(group_checks("map_sel", eligible_map, "fit with this model"))
    output$map_block_msg <- shiny::renderUI({
      if (!identical(input$model, "source_aware")) return(NULL)
      p <- ctx$proj(); gs <- groups()
      blocked <- gs[vapply(gs, function(g) hsms_is_current(p, "phase", g) && !hsms_phase_routes(phase_of(g))$source_aware, logical(1))]
      if (!length(blocked)) return(NULL)
      hsms_notice(type = "warning", sprintf("Source-aware fitting is unavailable for %s: the phase is not fully resolved. ", paste(hsms_group_label(p, blocked), collapse = ", ")),
                  "Choose the population model with phase-block handling for these groups. The app never switches models automatically.")
    })
    output$map_form <- shiny::renderUI({
      m <- input$model %||% "source_aware"
      hsms_settings_form(ns, m, ctx$defaults[[m]])
    })
    output$map_badges <- shiny::renderUI({ p <- ctx$proj(); gs <- groups(); if (!length(gs)) return(NULL)
      hsms_status_badge(if (jv_stage("map")) "running" else hsms_combine_status(lapply(gs, function(g) hsms_stage_status(p, "map", g)))) })
    shiny::observeEvent(input$run_map, {
      p <- ctx$proj(); gs <- input$map_sel; m <- input$model
      if (!length(gs)) { ctx$notify("Select at least one eligible group.", "warning"); return() }
      bad <- gs[!vapply(gs, eligible_map, logical(1))]
      if (length(bad)) { ctx$notify("Some selected groups are not eligible for this model.", "error"); return() }
      s <- hsms_read_form(input, m); errs <- hsms_validate_settings(m, s)
      if (length(errs)) { ctx$notify(paste(errs, collapse = " "), "error"); return() }
      for (g in gs) p <- hsms_set_map_spec(p, g, m, s)
      ctx$set_proj(p); ctx$log("map_settings_applied", list(groups = gs, model = m))
      ctx$launch("map", hsms_group_items(p, ctx$dir(), "map", gs),
                 label = sprintf("%s map fit (%d group(s))", if (m == "source_aware") "Source-aware" else "Population", length(gs)))
    })

    fits <- shiny::reactive({
      p <- ctx$proj(); gs <- groups(); i <- imp(); if (!length(gs) || is.null(i)) return(NULL)
      rows <- lapply(gs, function(g) {
        if (is.null(hsms_current_id(p, "map", g))) return(NULL)
        f <- ctx$load("map", g); if (is.null(f)) return(NULL)
        s <- hsms_map_summary(f, p$settings$display$map_function, p$settings$display$gap_r,
                              list(group_label = unname(hsms_group_label(p, g)), mapping = i$mapping))
        cbind(s$fit_summary, result = hsms_stage_status(p, "map", g))
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows)) do.call(rbind, rows) else NULL
    })
    output$fit_notices <- shiny::renderUI({
      f <- fits(); if (is.null(f)) return(hsms_empty_state("No maps yet", "Choose a model and fit the selected groups."))
      nc <- f[f$status == "fitted" & !f$converged, , drop = FALSE]
      fl <- f[f$status == "failed", , drop = FALSE]
      shiny::tagList(
        if (nrow(nc)) hsms_notice(type = "warning", shiny::strong("Not converged: "),
                                  paste(sprintf("%s block %s stopped after %s iterations", nc$linkage_group, nc$block, nc$iterations), collapse = "; "),
                                  ". These estimates are the last EM iterate, not a converged map; the warning is kept in every export."),
        if (nrow(fl)) hsms_notice(type = "danger", shiny::strong("Failed blocks: "),
                                  paste(sprintf("%s block %s (%s)", fl$linkage_group, fl$block, fl$error), collapse = "; "),
                                  ". Other blocks of these groups were kept (partial result)."),
        if (any(f$result == "outdated")) hsms_notice(type = "warning", "Some maps are outdated (their phase or model settings changed)."))
    })
    output$fit_table <- DT::renderDT({ f <- fits(); shiny::req(f)
      hsms_dt(f[, c("linkage_group", "model", "block", "n_markers", "n_offspring", "status", "converged", "iterations",
                    "objective_type", "objective_value", "alpha", "beta", "sharing_occupancy", "linked_length_cM_group",
                    "n_gaps_group", "elapsed_sec", "result", "warnings")], page_length = 10) })
  })
}
