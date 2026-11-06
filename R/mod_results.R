# Stage 7: Maps, intervals and inheritance probabilities.

mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(7, "Inheritance and results", "Gap-aware maps, interval tables and per-offspring maternal inheritance probabilities for fitted groups."),
    shiny::uiOutput(ns("prereq")),
    bslib::card(
      bslib::card_header("Display"),
      shiny::div(class = "d-flex gap-4 flex-wrap align-items-end",
                 shiny::selectInput(ns("group"), "Linkage group", choices = NULL, width = "220px"),
                 shiny::selectInput(ns("mapfun"), hsms_label("Map function (display)", "Converts fitted recombination fractions into distances for display and export. This never refits a model. For source-aware fits, Kosambi or Morgan values are display conversions of the fitted r."),
                                    c(Haldane = "haldane", Kosambi = "kosambi", Morgan = "morgan"), width = "180px"),
                 hsms_num(ns("gap_r"), "No-linkage threshold (display)", 0.499,
                          "Fitted r at or above this value is shown as a no-linkage gap (distance NA; positions restart). Changing it does not refit any model.",
                          min = 0.3, max = 0.5, step = 0.001, width = "220px"),
                 shiny::actionButton(ns("apply_display"), "Apply display settings", class = "btn-outline-secondary mb-3"))),
    bslib::navset_card_tab(
      id = ns("tabs"),
      bslib::nav_panel("Map overview", shiny::plotOutput(ns("overview"), height = "460px")),
      bslib::nav_panel("Group map",
                       shiny::uiOutput(ns("map_info")),
                       shiny::plotOutput(ns("map_plot"), height = "420px"),
                       shiny::plotOutput(ns("iv_plot"), height = "280px")),
      bslib::nav_panel("Intervals and positions",
                       shiny::tags$h6("Adjacent intervals"), DT::DTOutput(ns("iv_table")), hsms_download_link(ns("dl_iv")),
                       shiny::tags$h6("Marker positions"), DT::DTOutput(ns("pos_table")), hsms_download_link(ns("dl_pos"))),
      bslib::nav_panel("Maternal inheritance",
                       shiny::div(class = "d-flex gap-3 flex-wrap align-items-end",
                                  shiny::selectInput(ns("block"), "Phase block", choices = NULL, width = "160px"),
                                  shiny::sliderInput(ns("rank"), "Marker ranks", min = 1, max = 2, value = c(1, 2), step = 1, width = "300px"),
                                  shiny::selectizeInput(ns("offspring"), "Offspring (empty = first 60)", choices = NULL, multiple = TRUE, width = "320px")),
                       shiny::plotOutput(ns("inh_plot"), height = "520px"),
                       shiny::selectizeInput(ns("profile_off"), "Offspring profile", choices = NULL, multiple = TRUE, options = list(maxItems = 6)),
                       shiny::plotOutput(ns("profile_plot"), height = "260px"),
                       shiny::tags$h6("Values in the current view"), DT::DTOutput(ns("inh_table")), hsms_download_link(ns("dl_inh"), "Download current view (CSV)"),
                       shiny::p(class = "small text-muted", "Homolog 1 is a statistical label: within a resolved phase block it is the maternal homolog carrying allele '1' at the block's first marker. It does not identify a maternal grandparent, and colours are not comparable across blocks.")),
      bslib::nav_panel("Paternal sharing (source-aware)",
                       shiny::uiOutput(ns("share_info")),
                       shiny::plotOutput(ns("share_plot"), height = "520px"),
                       hsms_download_link(ns("dl_share"), "Download current view (CSV)"))))
}

mod_results_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    imp <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "import"))) NULL else ctx$load("import") })
    mapped <- shiny::reactive({ p <- ctx$proj(); shiny::req(p)
      gs <- hsms_group_ids(p); gs[vapply(gs, function(g) !is.null(hsms_current_id(p, "map", g)), logical(1))] })
    output$prereq <- shiny::renderUI({ if (!length(mapped())) hsms_prereq_ui("Fit at least one map in stage 6 (Phase and maps).") })
    shiny::observe({ p <- ctx$proj(); shiny::req(p)
      shiny::updateSelectInput(session, "mapfun", selected = p$settings$display$map_function)
      shiny::updateNumericInput(session, "gap_r", value = p$settings$display$gap_r) }) |> shiny::bindEvent(ctx$proj_id())
    shiny::observe({ gs <- mapped(); p <- ctx$proj()
      hsms_update_select(session, "group", if (length(gs)) stats::setNames(gs, hsms_group_label(p, gs)) else character(0), shiny::isolate(input$group)) })
    shiny::observeEvent(input$apply_display, {
      p <- ctx$proj(); s <- list(map_function = input$mapfun, gap_r = input$gap_r)
      errs <- hsms_validate_settings("display", s)
      if (length(errs)) { ctx$notify(paste(errs, collapse = " "), "error"); return() }
      p$settings$display <- s; ctx$set_proj(p); ctx$log("display_settings_applied", s)
      ctx$notify("Display settings applied. No model was refitted.")
    })
    disp <- shiny::reactive(ctx$proj()$settings$display)
    summary_of <- function(g) {
      p <- ctx$proj(); i <- imp(); f <- ctx$load("map", g); if (is.null(f) || is.null(i)) return(NULL)
      sel <- ctx$load("selection")
      hsms_map_summary(f, disp()$map_function, disp()$gap_r,
                       list(family = p$family, mother = sel$mother, group_label = unname(hsms_group_label(p, g)),
                            run_id = p$artifacts[[hsms_current_id(p, "map", g)]]$run_id, mapping = i$mapping))
    }
    fit <- shiny::reactive({ g <- input$group; shiny::req(g, g %in% mapped()); ctx$load("map", g) })
    smry <- shiny::reactive({ g <- input$group; shiny::req(g, g %in% mapped()); summary_of(g) })

    output$overview <- shiny::renderPlot({
      gs <- mapped(); shiny::req(length(gs))
      ss <- stats::setNames(lapply(gs, summary_of), gs)
      hsms_plot_map_overview(Filter(Negate(is.null), ss))
    }, res = 96)
    output$map_info <- shiny::renderUI({
      s <- smry(); f <- fit(); p <- ctx$proj(); shiny::req(s, f)
      st <- hsms_stage_status(p, "map", input$group)
      shiny::tagList(
        if (st == "outdated") hsms_notice(type = "warning", shiny::strong("Outdated result: "), "an upstream input or setting changed. It is shown as a historical result and is not exported as current."),
        if (any(f$fit_table$status == "fitted" & !f$fit_table$converged)) hsms_notice(type = "warning", shiny::strong("Not converged: "), "the EM reached its iteration cap; this is not a final map."),
        if (!is.null(s$note)) hsms_notice(type = "info", s$note),
        hsms_kv(Model = if (f$model == "source_aware") "Source-aware HMM" else paste("Population model,", f$mode),
                `Linked length` = sprintf("%.2f cM (%s, finite linked intervals only)", s$linked_length, s$map_function),
                `Gaps` = s$n_gaps, `Segments` = s$n_segments, `Markers` = nrow(s$positions),
                `alpha / beta` = if (f$model == "source_aware") sprintf("%.5g / %.5g (per marker step)", f$raw$alpha, f$raw$beta) else NULL,
                `Observed log-likelihood` = if (f$model == "source_aware") sprintf("%.3f", f$raw$loglik) else NULL,
                `Iterations` = paste(f$fit_table$iters, collapse = ", "),
                Warnings = if (length(f$warnings)) paste(f$warnings, collapse = "; ") else "none"))
    })
    output$map_plot <- shiny::renderPlot({ s <- smry(); shiny::req(s); hsms_plot_map_group(s, title = NULL) }, res = 96)
    output$iv_plot <- shiny::renderPlot({ s <- smry(); shiny::req(s, nrow(s$intervals) > 0); hsms_plot_intervals(s) }, res = 96)
    output$iv_table <- DT::renderDT({ s <- smry(); shiny::req(s); hsms_dt(s$intervals, page_length = 10, scroll_y = "300px") })
    output$pos_table <- DT::renderDT({ s <- smry(); shiny::req(s); hsms_dt(s$positions, page_length = 10, scroll_y = "300px") })
    output$dl_iv <- shiny::downloadHandler(function() paste0("intervals_", hsms_safe_filename(hsms_group_label(ctx$proj(), input$group)), ".csv"),
                                           function(f) hsms_write_csv(smry()$intervals, f))
    output$dl_pos <- shiny::downloadHandler(function() paste0("positions_", hsms_safe_filename(hsms_group_label(ctx$proj(), input$group)), ".csv"),
                                            function(f) hsms_write_csv(smry()$positions, f))

    # ---- inheritance views ----
    post <- shiny::reactive({ f <- fit(); shiny::req(f); tryCatch(hsms_posterior_maternal(f), error = function(e) { ctx$notify(conditionMessage(e), "error"); NULL }) })
    shiny::observe({
      f <- fit(); pm <- post(); i <- imp(); shiny::req(f, pm, i)
      blocks <- sort(unique(pm$block))
      hsms_update_select(session, "block", c("All blocks" = "all", stats::setNames(blocks, paste("Block", blocks))),
                         shiny::isolate(input$block))
      offs <- hsms_to_original(i$mapping, "sample", f$offspring)
      shiny::updateSelectizeInput(session, "offspring", choices = offs, server = TRUE)
      shiny::updateSelectizeInput(session, "profile_off", choices = offs, selected = offs[1], server = TRUE)
    })
    shiny::observe({ f <- fit(); pm <- post(); shiny::req(f, pm)
      idx <- if (identical(input$block, "all") || is.null(input$block)) seq_along(f$markers) else which(pm$block == as.integer(input$block))
      if (!length(idx)) return()
      shiny::updateSliderInput(session, "rank", min = min(idx), max = max(idx), value = c(min(idx), max(idx))) })
    view <- function(what) {
      f <- fit(); pm <- post(); i <- imp(); shiny::req(f, pm, i)
      r <- input$rank; idx <- seq.int(max(1, r[1]), min(length(f$markers), r[2]))
      if (!identical(input$block, "all") && !is.null(input$block)) idx <- intersect(idx, which(pm$block == as.integer(input$block)))
      offs <- if (length(input$offspring)) hsms_to_internal(i$mapping, "sample", input$offspring) else utils::head(f$offspring, 60)
      hsms_posterior_slice(f, i$mapping, offspring = offs, markers = f$markers[idx], what = what)
    }
    inh <- shiny::reactive(view("maternal"))
    output$inh_plot <- shiny::renderPlot({
      d <- inh(); shiny::req(d)
      hsms_plot_posterior_heatmap(d, sprintf("Maternal inheritance: P(homolog 1), %s", hsms_group_label(ctx$proj(), input$group)),
                                  "P(homolog 1)", sprintf("%s model. Each panel is a resolved phase block with its own homolog orientation. Grey = not estimated.",
                                                          if (fit()$model == "source_aware") "Source-aware posterior (gammaM)" else "Population-model posterior (calc_haploprob)"))
    }, res = 96)
    output$profile_plot <- shiny::renderPlot({
      i <- imp(); f <- fit(); shiny::req(i, f, length(input$profile_off))
      d <- hsms_posterior_slice(f, i$mapping, offspring = hsms_to_internal(i$mapping, "sample", input$profile_off), what = "maternal")
      hsms_plot_profile(d, input$profile_off)
    }, res = 96)
    output$inh_table <- DT::renderDT({ d <- inh(); shiny::req(d); hsms_dt(d, page_length = 10, scroll_y = "260px") })
    output$dl_inh <- shiny::downloadHandler(function() paste0("maternal_inheritance_view_", hsms_safe_filename(hsms_group_label(ctx$proj(), input$group)), ".csv"),
                                            function(f) hsms_write_csv(inh(), f))
    share <- shiny::reactive({ f <- fit(); shiny::req(f); if (f$model != "source_aware") return(NULL); view("sharing") })
    output$share_info <- shiny::renderUI({
      f <- fit(); shiny::req(f)
      if (f$model != "source_aware") return(hsms_notice(type = "info", "Sharing-mode probabilities exist only for source-aware fits. This group was fitted with the population model."))
      hsms_notice(type = "info", sprintf("Posterior probability that the paternal allele came from the haplotype-sharing mode (gammaH). Mean occupancy %.4f. ", mean(f$raw$gammaH)),
                  "Sharing states are a statistical description of haplotype-sharing patterns: they do not identify fathers, reconstruct a pedigree, prove identity by descent, or detect selfed offspring.")
    })
    output$share_plot <- shiny::renderPlot({ d <- share(); shiny::req(d)
      hsms_plot_posterior_heatmap(d, sprintf("Paternal haplotype-sharing mode occupancy, %s", hsms_group_label(ctx$proj(), input$group)),
                                  "P(sharing mode)", "Source-aware posterior (gammaH). Uses the same offspring and marker selection as the inheritance view.")
    }, res = 96)
    output$dl_share <- shiny::downloadHandler("paternal_sharing_view.csv", function(f) hsms_write_csv(share(), f))
  })
}
