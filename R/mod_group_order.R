# Stage 5: Linkage groups and marker order.

mod_group_order_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(5, "Groups and order", "Cluster the retained markers into linkage groups, then order markers within groups by MDS or from a supplied order."),
    shiny::uiOutput(ns("prereq")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
      bslib::card(
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
                                      "Build groups", shiny::uiOutput(ns("grp_badges"), inline = TRUE))),
        shiny::uiOutput(ns("grp_form")),
        shiny::div(class = "d-flex gap-2 flex-wrap",
                   shiny::actionButton(ns("run_grp"), "Build groups", class = "btn-primary hsms-job-btn"),
                   shiny::actionButton(ns("discard_grp"), "Discard edits", class = "btn-outline-secondary")),
        shiny::p(class = "small text-muted mt-2", "Group numbers are inferred labels, not validated chromosome identities. You can give groups display names; renaming never changes assignments or results.")),
      bslib::navset_card_tab(
        title = "Groups",
        bslib::nav_panel("Overview", shiny::uiOutput(ns("grp_summary")), DT::DTOutput(ns("grp_table")),
                         shiny::div(class = "d-flex gap-2 align-items-end flex-wrap mt-2",
                                    shiny::selectInput(ns("rename_g"), "Group", choices = NULL, width = "140px"),
                                    shiny::textInput(ns("rename_lab"), "Display name", width = "200px"),
                                    shiny::actionButton(ns("rename"), "Rename", class = "btn-outline-secondary mb-3"))),
        bslib::nav_panel("Dendrogram", shiny::plotOutput(ns("dendro"), height = "420px")),
        bslib::nav_panel("Assignments", DT::DTOutput(ns("assign_table")), hsms_download_link(ns("dl_assign"))),
        bslib::nav_panel("Supplied annotations", shiny::uiOutput(ns("chrom_comp"))))),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
      bslib::card(
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
                                      "Order markers", shiny::uiOutput(ns("ord_badges"), inline = TRUE))),
        shiny::uiOutput(ns("ord_groups")),
        shiny::uiOutput(ns("ord_form")),
        shiny::actionButton(ns("run_ord"), "Order selected groups", class = "btn-primary hsms-job-btn"),
        shiny::hr(),
        shiny::tags$h6("Reverse an order"),
        shiny::div(class = "d-flex gap-2 align-items-end",
                   shiny::selectInput(ns("rev_g"), NULL, choices = NULL, width = "160px"),
                   shiny::actionButton(ns("reverse"), "Reverse order", class = "btn-outline-secondary mb-3")),
        shiny::p(class = "small text-muted", "Reversing changes that group's phase and map results (they become outdated). The previous order is kept in history."),
        shiny::hr(),
        shiny::tags$h6("Supplied order"),
        shiny::fileInput(ns("ord_file"), "Order CSV (marker_id, linkage_group, rank)", accept = ".csv"),
        shiny::uiOutput(ns("recon")),
        shiny::downloadLink(ns("tpl_order"), "Order template")),
      bslib::card(
        bslib::card_header("Order details"),
        shiny::uiOutput(ns("ord_overview")),
        shiny::selectInput(ns("detail_g"), "Group", choices = NULL, width = "200px"),
        bslib::navset_tab(
          bslib::nav_panel("Ordered markers", shiny::uiOutput(ns("ord_info")), DT::DTOutput(ns("ord_table"))),
          bslib::nav_panel("Ordered RF matrix", shiny::plotOutput(ns("ord_matrix"), height = "520px"))))))
}

mod_group_order_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    imp <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "import"))) NULL else ctx$load("import") })
    flt <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "tpt_filtered"))) NULL else ctx$load("tpt_filtered") })
    grp <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "grouping"))) NULL else ctx$load("grouping") })
    groups <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); hsms_group_ids(p) })
    jv_stage <- function(s) { jv <- ctx$job_view(); !is.null(jv) && jv$stage == s }
    orig <- function(x) { i <- imp(); if (is.null(i)) x else hsms_to_original(i$mapping, "marker", x) }

    output$prereq <- shiny::renderUI(hsms_prereq_ui(hsms_prereq_message(ctx$proj(), "grouping")))
    output$grp_form <- shiny::renderUI({ ctx$proj_id(); p <- shiny::isolate(ctx$proj()); shiny::req(p)
      hsms_settings_form(ns, "group", p$settings$group) })
    output$ord_form <- shiny::renderUI({ ctx$proj_id(); hsms_settings_form(ns, "order", ctx$defaults$order) })
    grp_draft <- shiny::reactive(hsms_read_form(input, "group"))
    ord_draft <- shiny::reactive(hsms_read_form(input, "order"))
    output$grp_badges <- shiny::renderUI({ p <- ctx$proj(); shiny::req(p)
      shiny::tagList(hsms_draft_badge(hsms_form_differs(grp_draft(), p$settings$group, "group")),
                     hsms_status_badge(if (jv_stage("grouping")) "running" else hsms_stage_status(p, "grouping"))) })
    shiny::observeEvent(input$discard_grp, hsms_update_form(session, "group", ctx$proj()$settings$group))

    shiny::observeEvent(input$run_grp, {
      p <- ctx$proj(); shiny::req(p)
      msg <- hsms_prereq_message(p, "grouping"); if (!is.null(msg)) { ctx$notify(msg, "warning"); return() }
      s <- grp_draft(); f <- flt()
      if (!is.null(f) && !is.null(s$k) && s$k > length(f$kept)) {
        ctx$notify(sprintf("k = %d exceeds the %d retained markers.", s$k, length(f$kept)), "error"); return() }
      p2 <- tryCatch(hsms_apply_settings(p, "group", s), error = function(e) e)
      if (inherits(p2, "error")) { ctx$notify(conditionMessage(p2), "error"); return() }
      ctx$set_proj(p2); ctx$log("settings_applied", list(stage = "group", k = s$k))
      ctx$launch("grouping", label = sprintf("Build groups (k = %d)", s$k))
    })

    grp_table <- shiny::reactive({
      p <- ctx$proj(); g <- grp(); shiny::req(p, g)
      sizes <- unlist(g$summary$sizes)
      ids <- names(sizes) %||% as.character(seq_along(sizes))
      data.frame(group = ids, display_name = unname(hsms_group_label(p, ids)), markers = as.integer(sizes),
                 note = ifelse(sizes == 1, "singleton: no ordering or map estimate", ifelse(sizes == 2, "two markers: no MDS estimate", "")),
                 order_status = vapply(ids, function(x) hsms_stage_status(p, "order", x), ""),
                 stringsAsFactors = FALSE)
    })
    output$grp_summary <- shiny::renderUI({
      p <- ctx$proj(); g <- grp()
      if (is.null(g)) return(hsms_empty_state("No groups yet", "Choose k and build groups."))
      s <- g$summary
      shiny::tagList(
        if (!hsms_is_current(p, "grouping")) hsms_notice(type = "warning", shiny::strong("Outdated: "), "shown for inspection; rebuild groups."),
        if (isTRUE(s$method_switched)) hsms_notice(type = "info", sprintf("You requested '%s' linkage; with LOD weighting the package used '%s'. The method actually used is recorded.", s$method_requested, s$method_used)),
        hsms_kv(`Groups (k)` = s$k, `Clustering method used` = s$method_used, `Markers` = s$n_markers, `Singleton groups` = s$n_singletons))
    })
    output$grp_table <- DT::renderDT(hsms_dt(grp_table(), page_length = 10, scroll_y = "260px"))
    shiny::observe({ gs <- groups(); p <- ctx$proj()
      ch <- if (length(gs)) stats::setNames(gs, paste0(gs, ": ", hsms_group_label(p, gs))) else character(0)
      hsms_update_select(session, "rename_g", ch, shiny::isolate(input$rename_g))
      hsms_update_select(session, "rev_g", ch, shiny::isolate(input$rev_g))
      hsms_update_select(session, "detail_g", ch, shiny::isolate(input$detail_g)) })
    shiny::observeEvent(input$rename, {
      p <- ctx$proj(); lab <- trimws(input$rename_lab)
      if (!nzchar(lab)) return()
      if (lab %in% hsms_group_label(p, setdiff(groups(), input$rename_g))) { ctx$notify("Another group already uses that name.", "warning"); return() }
      p$group_labels[[input$rename_g]] <- lab
      ctx$set_proj(p); ctx$log("group_renamed", list(group = input$rename_g, label = lab))
    })
    output$dendro <- shiny::renderPlot({ g <- grp(); shiny::req(g); hsms_draw_dendrogram(g$raw) }, res = 96)
    assign_df <- shiny::reactive({
      g <- grp(); i <- imp(); p <- ctx$proj(); shiny::req(g, i)
      gv <- g$raw$groups; ann <- i$marker_annotation
      data.frame(marker_id = orig(names(gv)), group = unname(gv), display_name = unname(hsms_group_label(p, unname(gv))),
                 supplied_chrom = ann$chrom[match(names(gv), ann$marker_internal)],
                 supplied_position = ann$position[match(names(gv), ann$marker_internal)], stringsAsFactors = FALSE)
    })
    output$assign_table <- DT::renderDT(hsms_dt(assign_df(), page_length = 12))
    output$dl_assign <- shiny::downloadHandler("group_assignments.csv", function(f) hsms_write_csv(assign_df(), f))
    output$chrom_comp <- shiny::renderUI({
      a <- assign_df()
      if (all(is.na(a$supplied_chrom) | !nzchar(a$supplied_chrom)))
        return(hsms_empty_state("No chromosome annotation", "The genotype file has no chrom values. Annotations are optional and are never needed for de novo grouping."))
      shiny::tagList(shiny::p(class = "small text-muted", "Comparison with SUPPLIED physical annotations. These are labels from your file; they do not define the genetic order or a chromosome assembly."),
                     shiny::renderTable(as.data.frame.matrix(table(group = a$display_name, chrom = a$supplied_chrom)), rownames = TRUE))
    })

    # ---- ordering ----
    output$ord_groups <- shiny::renderUI({
      gs <- groups(); p <- ctx$proj(); if (!length(gs) || !hsms_is_current(p, "grouping")) return(hsms_notice("Build current groups first."))
      t <- grp_table()
      shiny::checkboxGroupInput(ns("ord_sel"), "Groups to order", inline = TRUE,
                                choices = stats::setNames(t$group, sprintf("%s (%d)", t$display_name, t$markers)),
                                selected = shiny::isolate(input$ord_sel) %||% t$group)
    })
    output$ord_badges <- shiny::renderUI({ p <- ctx$proj(); shiny::req(p)
      gs <- groups(); if (!length(gs)) return(NULL)
      hsms_status_badge(if (jv_stage("order")) "running" else hsms_combine_status(lapply(gs, function(g) hsms_stage_status(p, "order", g)))) })
    shiny::observeEvent(input$run_ord, {
      p <- ctx$proj(); shiny::req(p)
      if (!hsms_is_current(p, "grouping")) { ctx$notify("Build current groups first.", "warning"); return() }
      gs <- input$ord_sel
      if (!length(gs)) { ctx$notify("Select at least one group.", "warning"); return() }
      s <- ord_draft(); errs <- hsms_validate_settings("order", s)
      if (length(errs)) { ctx$notify(paste(errs, collapse = " "), "error"); return() }
      for (g in gs) p <- hsms_set_order_spec_mds(p, g, s)
      ctx$set_proj(p); ctx$log("order_settings_applied", list(groups = gs))
      ctx$launch("order", hsms_group_items(p, ctx$dir(), "order", gs, ctx$cache),
                 list(order_settings = hsms_clean_settings("order", s)), label = sprintf("MDS order (%d group(s))", length(gs)))
    })
    shiny::observeEvent(input$reverse, {
      p <- ctx$proj(); g <- input$rev_g; shiny::req(p, g)
      p2 <- tryCatch(hsms_action_reverse_order(p, ctx$dir(), g, ctx$cache), error = function(e) e)
      if (inherits(p2, "error")) { ctx$notify(conditionMessage(p2), "error"); return() }
      ctx$set_proj(p2); ctx$events_tick(ctx$events_tick() + 1)
      ctx$notify(sprintf("Order of %s reversed. Its phase and map results are now outdated.", hsms_group_label(p, g)))
    })
    output$tpl_order <- shiny::downloadHandler("order_template.csv", function(f) {
      a <- tryCatch(assign_df(), error = function(e) NULL)
      df <- if (is.null(a)) data.frame(marker_id = c("snp_0001", "snp_0002"), linkage_group = "LG1", rank = 1:2)
            else data.frame(marker_id = a$marker_id, linkage_group = a$display_name, rank = stats::ave(seq_len(nrow(a)), a$group, FUN = seq_along))
      utils::write.csv(df, f, row.names = FALSE) })
    recon <- shiny::reactive({
      shiny::req(input$ord_file); g <- grp(); i <- imp(); p <- ctx$proj(); shiny::req(g, i)
      df <- tryCatch(utils::read.csv(input$ord_file$datapath, colClasses = "character", check.names = FALSE,
                                     na.strings = character(0), encoding = "UTF-8"), error = function(e) NULL)
      if (is.null(df)) return(list(ok = FALSE, issues = "The file could not be read as CSV.", groups = list()))
      gs <- groups(); labs <- stats::setNames(unname(hsms_group_label(p, gs)), gs)
      hsms_validate_supplied_order(df, g$raw, i$mapping, labs)
    })
    output$recon <- shiny::renderUI({
      r <- recon(); p <- ctx$proj()
      shiny::tagList(
        if (length(r$issues)) hsms_notice(type = "danger", shiny::strong("Cannot apply: "), shiny::tags$ul(lapply(r$issues, shiny::tags$li))),
        if (length(r$groups)) shiny::tags$table(class = "table table-sm small",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Group"), shiny::tags$th("Listed"), shiny::tags$th("Group members omitted"))),
          shiny::tags$tbody(lapply(r$groups, function(x) shiny::tags$tr(shiny::tags$td(hsms_group_label(p, x$group)), shiny::tags$td(x$n_listed),
                                                                        shiny::tags$td(length(x$omitted), if (length(x$omitted)) paste0(" (", paste(utils::head(x$omitted_original, 5), collapse = ", "), if (length(x$omitted) > 5) ", ..." else "", ")")))))),
        if (r$ok) shiny::tagList(
          shiny::checkboxInput(ns("ord_accept"), "Use only the listed markers; omitted group members are excluded from these groups' maps (recorded).", FALSE),
          shiny::actionButton(ns("apply_supplied"), "Apply supplied order", class = "btn-primary btn-sm")))
    })
    shiny::observeEvent(input$apply_supplied, {
      if (!isTRUE(input$ord_accept)) { ctx$notify("Confirm the inclusion rule first.", "warning"); return() }
      p <- ctx$proj(); r <- recon()
      if (!hsms_is_current(p, "grouping")) { ctx$notify("Groups are outdated; rebuild them first.", "warning"); return() }
      p2 <- tryCatch(hsms_action_apply_supplied_orders(p, ctx$dir(), r), error = function(e) e)
      if (inherits(p2, "error")) { ctx$notify(conditionMessage(p2), "error"); return() }
      ctx$set_proj(p2); ctx$notify(sprintf("Supplied order applied to %d group(s).", length(r$groups)))
    })

    output$ord_overview <- shiny::renderUI({
      p <- ctx$proj(); gs <- groups(); if (!length(gs)) return(NULL)
      rows <- lapply(gs, function(g) {
        id <- hsms_current_id(p, "order", g); m <- if (!is.null(id)) p$artifacts[[id]] else NULL
        shiny::tags$tr(shiny::tags$td(hsms_group_label(p, g)), shiny::tags$td(m$summary$n_markers %||% "-"),
                       shiny::tags$td(m$summary$method %||% "-"), shiny::tags$td(m$summary$n_removed %||% "-"),
                       shiny::tags$td(hsms_status_badge(hsms_stage_status(p, "order", g))))
      })
      shiny::tags$table(class = "table table-sm", shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("Group"), shiny::tags$th("Ordered markers"), shiny::tags$th("Order source"),
        shiny::tags$th("Removed"), shiny::tags$th("Status"))), shiny::tags$tbody(rows))
    })
    ord <- shiny::reactive({ p <- ctx$proj(); g <- input$detail_g; shiny::req(p, g)
      if (is.null(hsms_current_id(p, "order", g))) NULL else ctx$load("order", g) })
    output$ord_info <- shiny::renderUI({
      o <- ord(); p <- ctx$proj()
      if (is.null(o)) return(hsms_empty_state("No order for this group", "Select the group and run ordering, or apply a supplied order."))
      shiny::tagList(
        if (!hsms_is_current(p, "order", input$detail_g)) hsms_notice(type = "warning", shiny::strong("Outdated: "), "groups or the order specification changed."),
        hsms_kv(`Order source` = o$method, `Markers` = length(o$order),
                `Removed by envelope` = if (length(o$removed)) paste(orig(o$removed), collapse = ", ") else "none",
                `Omitted (supplied order)` = if (length(o$omitted)) paste(orig(unlist(o$omitted)), collapse = ", ") else "none",
                Note = if (!isTRUE(o$mds_estimated) && o$method != "supplied") "No MDS estimate for this group." else NULL))
    })
    output$ord_table <- DT::renderDT({
      o <- ord(); i <- imp(); shiny::req(o, i)
      ann <- i$marker_annotation
      hsms_dt(data.frame(rank = seq_along(o$order), marker_id = orig(o$order),
                         supplied_chrom = ann$chrom[match(o$order, ann$marker_internal)],
                         supplied_position = ann$position[match(o$order, ann$marker_internal)]), page_length = 12)
    })
    output$ord_matrix <- shiny::renderPlot({
      o <- ord(); f <- flt(); shiny::req(o, f, length(o$order) >= 2)
      M <- hsms_matrix_slice(f$raw, o$order, "r")
      ag <- hsms_aggregate_square(M, ctx$defaults$app$heatmap_max_cells)
      labs <- if (ag$aggregated) rownames(ag$matrix) else orig(rownames(M))
      hsms_plot_matrix(ag$matrix, "r", labs, ag$aggregated, ag$block,
                       title = sprintf("Recombination fractions in map order (%s)", hsms_group_label(ctx$proj(), input$detail_g)))
    }, res = 96)
  })
}
