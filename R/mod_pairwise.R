# Stage 4: Two-point analysis and filtering.

mod_pairwise_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(4, "Two-point analysis", "Estimate maternal recombination fractions and LOD scores for every pair of informative markers, then filter the two-point table."),
    shiny::uiOutput(ns("prereq")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
      bslib::card(
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
                                      "Run two-point analysis", shiny::uiOutput(ns("tpt_badges"), inline = TRUE))),
        shiny::uiOutput(ns("prerun")),
        shiny::uiOutput(ns("tpt_form")),
        shiny::uiOutput(ns("resource_confirm")),
        shiny::div(class = "d-flex gap-2 flex-wrap",
                   shiny::actionButton(ns("run_tpt"), "Run two-point analysis", class = "btn-primary hsms-job-btn"),
                   shiny::actionButton(ns("discard_tpt"), "Discard edits", class = "btn-outline-secondary")),
        shiny::p(class = "small text-muted mt-2", "Two-point estimation assumes no genotyping error (the package does not accept an error rate here), so there is no epsilon setting at this stage.")),
      bslib::navset_card_tab(
        title = "Two-point results",
        bslib::nav_panel("Summary", shiny::uiOutput(ns("tpt_summary")), shiny::plotOutput(ns("rf_hist"), height = "280px"),
                         shiny::plotOutput(ns("lod_hist"), height = "260px")),
        bslib::nav_panel("Matrix",
                         shiny::div(class = "d-flex gap-3 flex-wrap",
                                    shiny::radioButtons(ns("mat_what"), "Show", c("Recombination fraction" = "r", "Linkage LOD" = "lod_r"), inline = TRUE),
                                    shiny::radioButtons(ns("mat_src"), "Markers", c("All analysed" = "raw", "Retained by filter" = "filtered"), inline = TRUE)),
                         shiny::plotOutput(ns("matrix"), height = "560px")),
        bslib::nav_panel("Pair inspection",
                         shiny::selectizeInput(ns("pair_markers"), "Markers to inspect (2 to 40)", choices = NULL, multiple = TRUE,
                                               options = list(maxItems = 40, placeholder = "Type marker IDs")),
                         DT::DTOutput(ns("pair_table")),
                         shiny::p(class = "small text-muted", "Pairs without an estimate show NA; they are not evidence of linkage.")))),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
      bslib::card(
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
                                      "Filter", shiny::uiOutput(ns("flt_badges"), inline = TRUE))),
        shiny::uiOutput(ns("flt_form")),
        shiny::div(class = "d-flex gap-2 flex-wrap",
                   shiny::actionButton(ns("preview_flt"), "Preview filter", class = "btn-outline-secondary"),
                   shiny::actionButton(ns("run_flt"), "Apply filter", class = "btn-primary hsms-job-btn"),
                   shiny::actionButton(ns("discard_flt"), "Discard edits", class = "btn-outline-secondary")),
        shiny::uiOutput(ns("preview_out"))),
      bslib::card(
        bslib::card_header("Filter result"),
        shiny::uiOutput(ns("flt_summary")),
        shiny::plotOutput(ns("flt_plot"), height = "260px"),
        DT::DTOutput(ns("flt_table")))))
}

mod_pairwise_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sel <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (!hsms_is_current(p, "selection")) return(NULL); ctx$load("selection") })
    tpt <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "tpt_raw"))) return(NULL); ctx$load("tpt_raw") })
    flt <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "tpt_filtered"))) return(NULL); ctx$load("tpt_filtered") })
    imp <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "import"))) return(NULL); ctx$load("import") })
    jv_stage <- function(s) { jv <- ctx$job_view(); !is.null(jv) && jv$stage == s }

    output$prereq <- shiny::renderUI(hsms_prereq_ui(hsms_prereq_message(ctx$proj(), "tpt_raw")))
    output$tpt_form <- shiny::renderUI({ ctx$proj_id(); p <- shiny::isolate(ctx$proj()); shiny::req(p)
      hsms_settings_form(ns, "pairwise", p$settings$pairwise) })
    output$flt_form <- shiny::renderUI({ ctx$proj_id(); p <- shiny::isolate(ctx$proj()); shiny::req(p)
      hsms_settings_form(ns, "filter", p$settings$filter) })
    tpt_draft <- shiny::reactive(hsms_read_form(input, "pairwise"))
    flt_draft <- shiny::reactive(hsms_read_form(input, "filter"))
    output$tpt_badges <- shiny::renderUI({ p <- ctx$proj(); shiny::req(p)
      shiny::tagList(hsms_draft_badge(hsms_form_differs(tpt_draft(), p$settings$pairwise, "pairwise")),
                     hsms_status_badge(if (jv_stage("tpt_raw")) "running" else hsms_stage_status(p, "tpt_raw"))) })
    output$flt_badges <- shiny::renderUI({ p <- ctx$proj(); shiny::req(p)
      shiny::tagList(hsms_draft_badge(hsms_form_differs(flt_draft(), p$settings$filter, "filter")),
                     hsms_status_badge(if (jv_stage("tpt_filtered")) "running" else hsms_stage_status(p, "tpt_filtered"))) })
    shiny::observeEvent(input$discard_tpt, hsms_update_form(session, "pairwise", ctx$proj()$settings$pairwise))
    shiny::observeEvent(input$discard_flt, hsms_update_form(session, "filter", ctx$proj()$settings$filter))

    est <- shiny::reactive({ s <- sel(); if (is.null(s)) return(NULL)
      hsms_pairwise_memory_estimate(length(s$markers), isTRUE(input$pairwise__return_diagnostics)) })
    too_big <- shiny::reactive({ e <- est(); if (is.null(e)) return(FALSE)
      e$n_markers > ctx$defaults$app$pairwise_marker_warning || e$peak_bytes_estimate > ctx$defaults$app$memory_warning_gb * 1024^3 })
    output$prerun <- shiny::renderUI({
      s <- sel(); e <- est(); if (is.null(s)) return(NULL)
      p <- ctx$proj()
      shiny::tagList(hsms_kv(`Family` = p$family, `Offspring` = length(s$offspring), `Informative markers` = e$n_markers,
                             `Unique pairs` = format(e$n_pairs, big.mark = ","),
                             `Output size (estimate)` = hsms_fmt_bytes(e$output_bytes),
                             `Peak memory (estimate)` = hsms_fmt_bytes(e$peak_bytes_estimate),
                             `Worker threads` = p$settings$app$threads),
                     shiny::p(class = "small text-muted", "Memory figures are estimates from the documented output structure, not measurements. Threads are set in Settings."))
    })
    output$resource_confirm <- shiny::renderUI({
      if (!too_big()) return(NULL)
      e <- est()
      shiny::tagList(hsms_notice(type = "warning", sprintf("Large analysis: %s markers, estimated peak memory %s. Consider excluding markers in Quality review; markers are never subsampled automatically.",
                                                           format(e$n_markers, big.mark = ","), hsms_fmt_bytes(e$peak_bytes_estimate))),
                     shiny::checkboxInput(ns("confirm_big"), "I understand the resource cost; run anyway", FALSE))
    })

    shiny::observeEvent(input$run_tpt, {
      p <- ctx$proj(); shiny::req(p)
      msg <- hsms_prereq_message(p, "tpt_raw"); if (!is.null(msg)) { ctx$notify(msg, "warning"); return() }
      if (too_big() && !isTRUE(input$confirm_big)) { ctx$notify("Confirm the resource warning first.", "warning"); return() }
      p2 <- tryCatch(hsms_apply_settings(p, "pairwise", tpt_draft()), error = function(e) e)
      if (inherits(p2, "error")) { ctx$notify(conditionMessage(p2), "error"); return() }
      ctx$set_proj(p2); ctx$log("settings_applied", list(stage = "pairwise"))
      ctx$launch("tpt_raw", label = "Two-point analysis")
    })

    output$tpt_summary <- shiny::renderUI({
      p <- ctx$proj(); x <- tpt()
      if (is.null(x)) return(hsms_empty_state("No two-point result yet", "Run the two-point analysis to see recombination fractions and LOD scores."))
      meta <- p$artifacts[[hsms_current_id(p, "tpt_raw")]]
      shiny::tagList(
        if (!hsms_is_current(p, "tpt_raw")) hsms_notice(type = "warning", shiny::strong("Outdated: "), "inputs, exclusions or two-point settings changed after this result was computed. It is shown for inspection only."),
        hsms_kv(`Markers analysed` = x$summary$n_markers, `Offspring` = if (!is.null(sel())) length(sel()$offspring) else "NA",
                `Pairs` = format(x$summary$n_pairs, big.mark = ","), `Pairs without estimate` = x$summary$n_pairs_na,
                `Pairs at r = 0.5` = x$summary$n_pairs_r_half,
                `Elapsed (HalfSibMap)` = sprintf("%.2f s", x$summary$time_sec), `Threads` = x$summary$threads,
                `Applied lambda / q0` = paste(meta$settings$lambda, "/", meta$settings$q0),
                Warnings = if (length(x$warnings)) paste(x$warnings, collapse = "; ") else "none"))
    })
    dist <- shiny::reactive({ x <- tpt(); shiny::req(x); hsms_pairwise_distribution(x$raw) })
    output$rf_hist <- shiny::renderPlot(hsms_plot_rf_hist(dist()), res = 96)
    output$lod_hist <- shiny::renderPlot(hsms_plot_lod_hist(dist()), res = 96)
    output$matrix <- shiny::renderPlot({
      x <- tpt(); i <- imp(); shiny::req(x, i)
      mk <- x$raw$markers
      if (identical(input$mat_src, "filtered")) { f <- flt(); shiny::req(f); mk <- f$kept }
      M <- hsms_matrix_slice(x$raw, mk, input$mat_what %||% "r")
      ag <- hsms_aggregate_square(M, ctx$defaults$app$heatmap_max_cells)
      labs <- if (ag$aggregated) rownames(ag$matrix) else hsms_to_original(i$mapping, "marker", rownames(M))
      if (ag$aggregated) labs <- vapply(strsplit(labs, "..", fixed = TRUE), function(z) paste(hsms_to_original(i$mapping, "marker", z), collapse = ".."), "")
      hsms_plot_matrix(ag$matrix, input$mat_what %||% "r", labs, ag$aggregated, ag$block,
                       title = sprintf("%s (%d markers, file order)", if (identical(input$mat_what, "lod_r")) "Linkage LOD" else "Recombination fraction", nrow(M)))
    }, res = 96)
    shiny::observe({ x <- tpt(); i <- imp(); shiny::req(x, i)
      shiny::updateSelectizeInput(session, "pair_markers", choices = hsms_to_original(i$mapping, "marker", x$raw$markers), server = TRUE) })
    output$pair_table <- DT::renderDT({
      x <- tpt(); i <- imp(); shiny::req(x, i, length(input$pair_markers) >= 2)
      hsms_dt(hsms_pair_table(x$raw, hsms_to_internal(i$mapping, "marker", input$pair_markers), i$mapping), page_length = 10)
    })

    # ---- filter ----
    preview <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$preview_flt, {
      x <- tpt(); p <- ctx$proj()
      if (is.null(x) || !hsms_is_current(p, "tpt_raw")) { ctx$notify("Run (or rerun) the two-point analysis first.", "warning"); return() }
      s <- flt_draft(); errs <- hsms_validate_settings("filter", s)
      if (length(errs)) { ctx$notify(paste(errs, collapse = " "), "error"); return() }
      if (length(x$raw$markers) > ctx$defaults$app$preview_filter_max_markers) {
        preview(list(ok = FALSE, message = "Too many markers for an in-app preview; Apply filter runs it in the background.")); return() }
      preview(c(hsms_filter_preview(x$raw, hsms_clean_settings("filter", s)), list(settings = s)))
    })
    output$preview_out <- shiny::renderUI({
      pv <- preview(); if (is.null(pv)) return(NULL)
      if (!isTRUE(pv$ok)) return(hsms_notice(type = "warning", pv$message))
      stale <- hsms_form_differs(flt_draft(), pv$settings, "filter")
      hsms_notice(type = if (stale) "warning" else "info",
                  sprintf("Preview with these exact settings: %d of %d markers retained (%d removed).", pv$n_after, pv$n_before, pv$n_before - pv$n_after),
                  if (stale) " Settings changed since this preview; preview again." else "",
                  if (pv$n_after < 2) shiny::tags$div(shiny::strong("Fewer than 2 markers would remain; grouping needs at least 2.")))
    })
    shiny::observeEvent(input$run_flt, {
      p <- ctx$proj(); shiny::req(p)
      msg <- hsms_prereq_message(p, "tpt_filtered"); if (!is.null(msg)) { ctx$notify(msg, "warning"); return() }
      p2 <- tryCatch(hsms_apply_settings(p, "filter", flt_draft()), error = function(e) e)
      if (inherits(p2, "error")) { ctx$notify(conditionMessage(p2), "error"); return() }
      ctx$set_proj(p2); ctx$log("settings_applied", list(stage = "filter"))
      ctx$launch("tpt_filtered", label = "Two-point filter")
    })
    output$flt_summary <- shiny::renderUI({
      p <- ctx$proj(); f <- flt()
      if (is.null(f)) {
        oc <- p$last_outcome[["tpt_filtered"]]
        if (!is.null(oc) && identical(oc$status, "failed")) return(hsms_notice(type = "danger", shiny::strong("Filter failed: "), oc$message))
        return(hsms_empty_state("No filter applied yet", "Set the cutoffs, preview, and apply the filter."))
      }
      shiny::tagList(
        if (!hsms_is_current(p, "tpt_filtered")) hsms_notice(type = "warning", shiny::strong("Outdated: "), "shown for inspection; reapply the filter."),
        hsms_kv(`Markers before` = f$summary$n_before, `Markers retained` = f$summary$n_after, `Removed` = f$summary$n_excluded,
                `Effective linkage-LOD cutoff` = f$summary$lod_used, `Maximum r` = f$summary$rf,
                `Row-information quantiles` = paste(f$summary$probs, collapse = " to "),
                `Diagonal band` = f$summary$diag_markers %||% "not used"),
        if (f$summary$n_after < 2) hsms_notice(type = "danger", "Fewer than 2 markers retained: grouping cannot run."))
    })
    output$flt_plot <- shiny::renderPlot({ f <- flt(); shiny::req(f); hsms_plot_filter_counts(f$row_counts, f$kept) }, res = 96)
    output$flt_table <- DT::renderDT({ f <- flt(); i <- imp(); shiny::req(f, i)
      hsms_dt(data.frame(marker_id = hsms_to_original(i$mapping, "marker", c(f$kept, f$excluded)),
                         status = c(rep("retained", length(f$kept)), rep("removed", length(f$excluded))),
                         surviving_pairs = unname(f$row_counts[c(f$kept, f$excluded)])), page_length = 8, scroll_y = "220px") })
  })
}
