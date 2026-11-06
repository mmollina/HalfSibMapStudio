# Stage 8: Export (results package, replay bundle, project bundle, tables).

mod_exports_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(8, "Export", "Download results with their provenance, a replayable R analysis, or a portable project bundle."),
    shiny::uiOutput(ns("prereq")),
    bslib::card(bslib::card_header("What would be exported"), shiny::uiOutput(ns("status_table"))),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(
        bslib::card_header("Results package (ZIP)"),
        shiny::p(class = "small", "Tables (original IDs, full precision, explicit NA), unmodified HalfSibMap objects (RDS), figures (PNG 300 dpi and PDF), applied settings, versions, input checksums and job logs. Built in the background."),
        shiny::checkboxInput(ns("inc_fig"), "Include figures", TRUE),
        shiny::checkboxInput(ns("inc_post"), "Include per-offspring posterior tables", TRUE),
        shiny::checkboxInput(ns("inc_mat"), "Include full pairwise r / LOD matrices as CSV", FALSE),
        shiny::uiOutput(ns("mat_size")),
        shiny::checkboxInput(ns("inc_hist"), "Include outdated results, labelled 'historical'", FALSE),
        shiny::actionButton(ns("build_results"), "Build results package", class = "btn-primary hsms-job-btn")),
      bslib::card(
        bslib::card_header("Replay bundle (ZIP)"),
        shiny::p(class = "small", "replay.R recomputes every current stage from the included raw inputs with the recorded settings and seeds, using the real HalfSibMap API, and compare.R checks the outputs against this analysis. See its README for the commands."),
        shiny::actionButton(ns("build_replay"), "Build replay bundle", class = "btn-primary hsms-job-btn")),
      bslib::card(
        bslib::card_header("Project bundle (.hsmap.zip)"),
        shiny::p(class = "small", "A portable copy of the project that can be imported on another machine (stage 1)."),
        shiny::checkboxInput(ns("b_inputs"), "Include raw input files (needed to recompute)", TRUE),
        shiny::checkboxInput(ns("b_artifacts"), "Include analysis results (otherwise they must be recomputed)", TRUE),
        shiny::downloadButton(ns("dl_bundle"), "Download project bundle", class = "btn-outline-primary"))),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Built packages"), shiny::uiOutput(ns("built"))),
      bslib::card(bslib::card_header("Individual tables"),
                  shiny::tags$ul(
                    shiny::tags$li(shiny::downloadLink(ns("dl_val"), "Validation report")),
                    shiny::tags$li(shiny::downloadLink(ns("dl_fam"), "Family summary")),
                    shiny::tags$li(shiny::downloadLink(ns("dl_ex"), "Applied exclusions")),
                    shiny::tags$li(shiny::downloadLink(ns("dl_set"), "Applied settings (JSON)")),
                    shiny::tags$li(shiny::downloadLink(ns("dl_pos"), "Marker positions, all current groups")),
                    shiny::tags$li(shiny::downloadLink(ns("dl_iv"), "Adjacent intervals, all current groups")),
                    shiny::tags$li(shiny::downloadLink(ns("dl_fit"), "Fit summaries, all current groups")),
                    shiny::tags$li(shiny::downloadLink(ns("dl_diag"), "Diagnostic bundle (versions, settings, logs, errors)"))))))
}

mod_exports_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$prereq <- shiny::renderUI({ p <- ctx$proj()
      if (is.null(p)) hsms_prereq_ui("Create or open a project first (stage 1).")
      else if (!hsms_is_current(p, "import")) hsms_prereq_ui("Import data first (stage 2).") })
    output$status_table <- shiny::renderUI({
      p <- ctx$proj(); shiny::req(p)
      rows <- list()
      for (s in HSMS_GLOBAL_STAGES) rows[[length(rows) + 1]] <- list(HSMS_STAGE_LABELS[[s]], hsms_stage_status(p, s))
      for (g in hsms_group_ids(p)) for (s in HSMS_GROUP_STAGES)
        rows[[length(rows) + 1]] <- list(paste0(HSMS_STAGE_LABELS[[s]], " - ", hsms_group_label(p, g)), hsms_stage_status(p, s, g))
      shiny::tagList(
        shiny::p(class = "small text-muted", "Current results are exported by default. Outdated results are left out unless you include them; they are then labelled 'historical' in file names, manifests and result_label columns."),
        shiny::div(class = "hsms-status-grid", lapply(rows, function(r) shiny::div(class = "hsms-status-cell", shiny::tags$span(r[[1]]), hsms_status_badge(r[[2]])))))
    })
    output$mat_size <- shiny::renderUI({
      p <- ctx$proj(); shiny::req(p); id <- hsms_current_id(p, "tpt_raw"); if (is.null(id)) return(NULL)
      n <- p$artifacts[[id]]$summary$n_markers %||% 0
      shiny::p(class = "small text-muted", sprintf("Estimated size of the three matrices: %s.", hsms_fmt_bytes(3 * as.numeric(n)^2 * 20)))
    })
    shiny::observeEvent(input$build_results, {
      p <- ctx$proj(); if (is.null(p) || !hsms_is_current(p, "import")) { ctx$notify("Import data first.", "warning"); return() }
      ctx$launch("export", extra = list(export_options = list(kind = "results", include_figures = input$inc_fig,
                                                              include_posteriors = input$inc_post, include_pairwise_matrices = input$inc_mat,
                                                              include_historical = input$inc_hist)), label = "Build results package")
    })
    shiny::observeEvent(input$build_replay, {
      p <- ctx$proj(); if (is.null(p) || !hsms_is_current(p, "import")) { ctx$notify("Import data first.", "warning"); return() }
      ctx$launch("export", extra = list(export_options = list(kind = "replay")), label = "Build replay bundle")
    })
    output$built <- shiny::renderUI({
      p <- ctx$proj(); shiny::req(p)
      ex <- rev(p$exports %||% list())
      ex <- Filter(function(e) file.exists(file.path(ctx$dir(), e$file)), ex)
      if (!length(ex)) return(hsms_empty_state("Nothing built yet", "Build a results package or replay bundle; it appears here for download."))
      shiny::tagList(
        shiny::selectInput(ns("built_sel"), NULL, width = "100%", choices = stats::setNames(
          vapply(ex, `[[`, "", "id"), vapply(ex, function(e) sprintf("%s  [%s]  %s", basename(e$file), e$status, substr(e$created, 1, 19)), ""))),
        shiny::downloadButton(ns("dl_built"), "Download selected", class = "btn-primary"),
        shiny::p(class = "small text-muted mt-2", "Status in the file name: complete, complete-with-warnings, partial, or includes-historical."))
    })
    output$dl_built <- shiny::downloadHandler(
      filename = function() basename(ctx$proj()$exports[[input$built_sel]]$file),
      content = function(f) file.copy(file.path(ctx$dir(), ctx$proj()$exports[[input$built_sel]]$file), f))
    output$dl_bundle <- shiny::downloadHandler(
      filename = function() paste0(hsms_safe_filename(ctx$proj()$name), "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".hsmap.zip"),
      content = function(f) {
        hsms_export_bundle(ctx$proj(), ctx$dir(), f, isTRUE(input$b_inputs), isTRUE(input$b_artifacts))
        ctx$log("bundle_exported", list(inputs = isTRUE(input$b_inputs), artifacts = isTRUE(input$b_artifacts)))
      })
    output$dl_val <- shiny::downloadHandler("validation_report.csv", function(f) hsms_write_csv(hsms_rows_to_df(ctx$proj()$validation$issues), f))
    output$dl_fam <- shiny::downloadHandler("family_summary.csv", function(f) hsms_write_csv(hsms_rows_to_df(ctx$proj()$validation$families), f))
    output$dl_ex <- shiny::downloadHandler("exclusions.csv", function(f) hsms_write_csv(ctx$proj()$exclusions, f))
    output$dl_set <- shiny::downloadHandler("applied_settings.json", function(f) hsms_write_json(hsms_settings_export(ctx$proj()), f))
    all_summaries <- function() {
      p <- ctx$proj(); i <- ctx$load("import"); d <- p$settings$display
      lapply(Filter(function(g) hsms_is_current(p, "map", g), hsms_group_ids(p)), function(g)
        hsms_map_summary(ctx$load("map", g), d$map_function, d$gap_r,
                         list(family = p$family, group_label = unname(hsms_group_label(p, g)), mapping = i$mapping,
                              run_id = p$artifacts[[hsms_current_id(p, "map", g)]]$run_id)))
    }
    bind <- function(l, k) { x <- lapply(l, `[[`, k); if (length(x)) do.call(rbind, x) else data.frame(message = "No current maps") }
    output$dl_pos <- shiny::downloadHandler("marker_positions.csv", function(f) hsms_write_csv(bind(all_summaries(), "positions"), f))
    output$dl_iv <- shiny::downloadHandler("adjacent_intervals.csv", function(f) hsms_write_csv(bind(all_summaries(), "intervals"), f))
    output$dl_fit <- shiny::downloadHandler("fit_summary.csv", function(f) hsms_write_csv(bind(all_summaries(), "fit_summary"), f))
    output$dl_diag <- shiny::downloadHandler("hsms_diagnostics.zip", function(f) {
      p <- ctx$proj(); d <- ctx$dir(); tmp <- tempfile("diag_"); dir.create(tmp)
      hsms_write_json(hsms_versions(), file.path(tmp, "versions.json"))
      writeLines(utils::capture.output(utils::sessionInfo()), file.path(tmp, "sessionInfo.txt"))
      hsms_write_json(hsms_settings_export(p), file.path(tmp, "settings.json"))
      hsms_write_json(list(jobs = p$jobs, last_outcome = p$last_outcome), file.path(tmp, "jobs_and_errors.json"))
      ev <- file.path(d, "history", "events.jsonl"); if (file.exists(ev)) file.copy(ev, tmp)
      lg <- list.files(file.path(d, "history", "logs"), full.names = TRUE)
      if (length(lg)) { dir.create(file.path(tmp, "logs")); file.copy(lg, file.path(tmp, "logs")) }
      zip::zip(f, files = list.files(tmp, recursive = TRUE), root = tmp)
    })
  })
}
