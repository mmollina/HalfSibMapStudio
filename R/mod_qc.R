# Stage 3: Quality review and exclusions.

mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(3, "Quality review", "Inspect the selected family before computing all marker pairs. Exclusions are explicit, reversible, and recorded with a reason."),
    shiny::uiOutput(ns("prereq")),
    shiny::uiOutput(ns("overview")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Offspring missingness"), shiny::plotOutput(ns("off_plot"), height = "260px")),
      bslib::card(bslib::card_header("Marker missingness"), shiny::plotOutput(ns("mk_plot"), height = "260px"))),
    bslib::navset_card_tab(
      title = "Tables",
      bslib::nav_panel("Markers", shiny::p(class = "small text-muted", "Call frequencies use the observed offspring calls at each marker as the denominator. This is descriptive only: it is not a Hardy-Weinberg test."),
                       DT::DTOutput(ns("mk_table")), hsms_download_link(ns("dl_mk"))),
      bslib::nav_panel("Offspring", DT::DTOutput(ns("off_table")), hsms_download_link(ns("dl_off")))),
    bslib::card(
      bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center", "Exclusions",
                                    shiny::uiOutput(ns("draft_badge"), inline = TRUE))),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::div(shiny::tags$h6("Missingness filter"),
                   hsms_num(ns("max_off"), "Exclude offspring with missing proportion above", 1, min = 0, max = 1, step = 0.05),
                   hsms_num(ns("max_mk"), "Exclude markers with missing proportion above", 1, min = 0, max = 1, step = 0.05),
                   shiny::actionButton(ns("preview_miss"), "Preview", class = "btn-outline-secondary btn-sm"),
                   shiny::uiOutput(ns("miss_preview"))),
        shiny::div(shiny::tags$h6("Manual exclusion"),
                   shiny::radioButtons(ns("man_kind"), NULL, c("Offspring" = "sample", "Markers" = "marker"), inline = TRUE),
                   shiny::selectizeInput(ns("man_ids"), "IDs", choices = NULL, multiple = TRUE,
                                         options = list(placeholder = "Type to search")),
                   shiny::textInput(ns("man_reason"), "Reason (required)"),
                   shiny::actionButton(ns("add_manual"), "Add to draft", class = "btn-outline-secondary btn-sm")),
        shiny::div(shiny::tags$h6("Apply"),
                   shiny::p(class = "small", "Applying exclusions rebuilds the analysis set and marks the two-point analysis and everything after it as outdated."),
                   shiny::actionButton(ns("apply"), "Apply exclusions", class = "btn-primary"),
                   shiny::actionButton(ns("discard"), "Discard edits", class = "btn-outline-secondary"),
                   shiny::actionButton(ns("remove_sel"), "Remove selected rows from draft", class = "btn-outline-danger btn-sm mt-2"))),
      shiny::tags$h6("Draft exclusion list"),
      DT::DTOutput(ns("draft_table")),
      shiny::tags$h6("Markers not used because they are uninformative for maternal mapping"),
      shiny::p(class = "small text-muted", "Markers where the mother is homozygous or missing cannot carry maternal heterozygous phase information in this single-family workflow. They stay in the raw data and are listed here; they are not sent to the two-point analysis."),
      DT::DTOutput(ns("auto_table"))),
    hsms_notice(type = "info", shiny::strong("Selfed offspring: "), "the app does not detect or remove suspected selfs. If some offspring are confirmed selfs, remove them here: maternal-specific transmission is not identifiable for selfed offspring under this model."))
}

mod_qc_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sel <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); if (is.null(hsms_current_id(p, "selection"))) return(NULL); ctx$load("selection") })
    imp <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); ctx$load("import") })
    draft <- shiny::reactiveVal(hsms_empty_exclusions())
    shiny::observe({ p <- ctx$proj(); if (!is.null(p)) draft(p$exclusions) })

    output$prereq <- shiny::renderUI({
      p <- ctx$proj()
      if (is.null(p)) return(hsms_prereq_ui("Create or open a project first (stage 1)."))
      if (is.null(p$family) || !hsms_is_current(p, "import")) return(hsms_prereq_ui("Import the data and choose a family in stage 2 (Data)."))
      if (!hsms_is_current(p, "selection")) return(hsms_notice(type = "warning", "The family selection is outdated (inputs changed). Choose the family again in stage 2."))
      NULL
    })
    output$overview <- shiny::renderUI({
      s <- sel(); if (is.null(s)) return(NULL)
      o <- s$qc$overall
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box("Family / mother", paste(o$family, "/", o$mother), theme = "light"),
        bslib::value_box("Offspring retained", sprintf("%d of %d", o$retained_offspring, o$raw_offspring), theme = "light"),
        bslib::value_box("Informative markers", sprintf("%d of %d", o$retained_markers, o$raw_markers),
                         shiny::p(class = "small", sprintf("Mother: %d heterozygous, %d homozygous, %d missing",
                                                           o$maternal_heterozygous, o$maternal_homozygous, o$maternal_missing)), theme = "light"),
        bslib::value_box("Offspring missing calls", sprintf("%.1f%%", 100 * o$offspring_missing_rate),
                         shiny::p(class = "small", "all markers, all offspring"), theme = "light"))
    })
    output$off_plot <- shiny::renderPlot({ s <- sel(); shiny::req(s); hsms_plot_missingness(s$qc$offspring$missing_rate, "offspring") }, res = 96)
    output$mk_plot <- shiny::renderPlot({ s <- sel(); shiny::req(s); hsms_plot_missingness(s$qc$markers$missing_rate, "marker") }, res = 96)
    mk_df <- shiny::reactive({ s <- sel(); shiny::req(s); s$qc$markers[, setdiff(names(s$qc$markers), "internal")] })
    off_df <- shiny::reactive({ s <- sel(); shiny::req(s); s$qc$offspring[, setdiff(names(s$qc$offspring), "internal")] })
    output$mk_table <- DT::renderDT(hsms_dt(mk_df(), page_length = 10), server = TRUE)
    output$off_table <- DT::renderDT(hsms_dt(off_df(), page_length = 10), server = TRUE)
    output$dl_mk <- shiny::downloadHandler("qc_markers.csv", function(f) hsms_write_csv(mk_df(), f))
    output$dl_off <- shiny::downloadHandler("qc_offspring.csv", function(f) hsms_write_csv(off_df(), f))

    shiny::observe({
      s <- sel(); i <- imp(); shiny::req(s, i)
      fi <- s$family_internal
      ids <- if (identical(input$man_kind, "marker")) colnames(i$data$G_list[[fi]]) else rownames(i$data$G_list[[fi]])
      kind <- if (identical(input$man_kind, "marker")) "marker" else "sample"
      shiny::updateSelectizeInput(session, "man_ids", choices = hsms_to_original(i$mapping, kind, ids), server = TRUE)
    })
    miss_cand <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$preview_miss, {
      p <- ctx$proj(); i <- imp(); shiny::req(p, i)
      c1 <- tryCatch(hsms_missingness_candidates(i, p$family, draft(), input$max_off, input$max_mk), error = function(e) e)
      if (inherits(c1, "error")) { ctx$notify(conditionMessage(c1), "error"); return() }
      miss_cand(c1)
    })
    output$miss_preview <- shiny::renderUI({
      c1 <- miss_cand(); if (is.null(c1)) return(NULL)
      shiny::div(class = "mt-2 small",
                 sprintf("Would add %d offspring and %d marker(s) to the draft.", sum(c1$kind == "sample"), sum(c1$kind == "marker")),
                 if (nrow(c1)) shiny::actionButton(ns("add_miss"), "Add these to draft", class = "btn-outline-secondary btn-sm ms-2"))
    })
    shiny::observeEvent(input$add_miss, { d <- rbind(draft(), miss_cand()); draft(d[!duplicated(d[, c("kind", "internal")]), ]); miss_cand(NULL) })
    shiny::observeEvent(input$add_manual, {
      i <- imp(); shiny::req(i)
      if (!length(input$man_ids)) { ctx$notify("Choose at least one ID.", "warning"); return() }
      if (!nzchar(trimws(input$man_reason))) { ctx$notify("A reason is required for every exclusion.", "warning"); return() }
      kind <- input$man_kind
      new <- data.frame(kind = kind, internal = hsms_to_internal(i$mapping, kind, input$man_ids),
                        original = input$man_ids, reason = trimws(input$man_reason), source = "manual",
                        applied_at = hsms_now(), stringsAsFactors = FALSE)
      d <- rbind(draft(), new); draft(d[!duplicated(d[, c("kind", "internal")]), ])
      shiny::updateSelectizeInput(session, "man_ids", selected = character(0))
    })
    shiny::observeEvent(input$remove_sel, {
      r <- input$draft_table_rows_selected
      if (length(r)) draft(draft()[-r, , drop = FALSE])
    })
    differs <- shiny::reactive({ p <- ctx$proj(); shiny::req(p); !identical(hsms_exclusion_signature(draft()), hsms_exclusion_signature(p$exclusions)) })
    output$draft_badge <- shiny::renderUI(hsms_draft_badge(differs()))
    output$draft_table <- DT::renderDT({
      d <- draft(); d <- d[, c("kind", "original", "reason", "source", "applied_at")]
      names(d)[2] <- "id"
      hsms_dt(d, page_length = 8, selection = "multiple", scroll_y = "220px")
    })
    output$auto_table <- DT::renderDT({ s <- sel(); shiny::req(s); hsms_dt(s$excluded_markers_auto[, c("original", "reason")], page_length = 8, scroll_y = "220px") })
    shiny::observeEvent(input$discard, { draft(ctx$proj()$exclusions); ctx$notify("Draft reset to the applied exclusions.") })
    shiny::observeEvent(input$apply, {
      p <- ctx$proj(); shiny::req(p)
      if (!differs()) { ctx$notify("No changes to apply."); return() }
      p2 <- tryCatch(hsms_action_set_exclusions(p, ctx$dir(), draft(), ctx$cache), error = function(e) e)
      if (inherits(p2, "error")) { ctx$notify(conditionMessage(p2), "error", 10); return() }
      ctx$set_proj(p2); ctx$events_tick(ctx$events_tick() + 1)
      ctx$notify(sprintf("Exclusions applied (%d in total). Two-point analysis and later stages are now outdated.", nrow(draft())))
    })
  })
}
