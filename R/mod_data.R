# Stage 2: Data (upload, validation, family selection).

hsms_template_pedigree <- function() {
  data.frame(id = c("MOM_A", "A_001", "A_002", "A_003"), mother = c("", "MOM_A", "MOM_A", "MOM_A"),
             father = "", generation = c(1, 2, 2, 2), family_id = "FAM_A", stringsAsFactors = FALSE)
}
hsms_template_genotypes <- function() {
  data.frame(marker_id = c("snp_0001", "snp_0002"), REF = c("A", "G"), ALT = c("G", "T"),
             chrom = c("Chr01", "Chr01"), position = c(10500, 52310),
             MOM_A = c(1, 1), A_001 = c(0, 1), A_002 = c(2, NA), A_003 = c(1, 0),
             check.names = FALSE, stringsAsFactors = FALSE)
}

mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(2, "Data", "Upload the pedigree and genotype CSV files, validate them, and choose one maternal family to analyse."),
    shiny::uiOutput(ns("prereq")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(
        bslib::card_header("Input files"),
        shiny::uiOutput(ns("current_inputs")),
        shiny::fileInput(ns("ped"), "Pedigree CSV", accept = c(".csv", "text/csv")),
        shiny::fileInput(ns("geno"), "Genotype CSV", accept = c(".csv", "text/csv")),
        shiny::checkboxInput(ns("accept_father"), shiny::tagList(
          "If the pedigree records fathers, proceed under the unknown-father model (sire data are not used; this choice is recorded)",
          hsms_help("This application fits only the open-pollinated, unknown-father model. Known-sire (full-sib) analysis is future work. Ticking this box records that father information was present and deliberately ignored.", "Father information")),
          value = FALSE),
        shiny::actionButton(ns("validate"), "Validate files", class = "btn-primary"),
        shiny::hr(),
        shiny::p(class = "small", "Formats: ", shiny::downloadLink(ns("tpl_ped"), "pedigree template"), " | ",
                 shiny::downloadLink(ns("tpl_geno"), "genotype template")),
        shiny::tags$details(
          shiny::tags$summary("File format summary"),
          shiny::tags$ul(class = "small",
            shiny::tags$li(shiny::strong("Pedigree: "), "columns id, mother, father, generation, family_id. Generation 1 = maternal parent, 2 = offspring. Offspring must name their mother."),
            shiny::tags$li(shiny::strong("Genotypes: "), "columns marker_id, REF, ALT, optional chrom and position, then one column per sample named exactly as in the pedigree."),
            shiny::tags$li("Calls are hard-call allele counts 0, 1, 2 (0 = aa, 1 = Aa, 2 = AA, counting the same allele across a marker, as read by HalfSibMap). Missing: empty, NA or '.'. Decimal dosages are rejected, never rounded. REF/ALT are kept as annotation and are not used to recode calls."),
            shiny::tags$li("Identifiers are preserved exactly (leading zeros, numeric-looking IDs, punctuation, Unicode)."))))
      ,
      bslib::card(
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
                                      "Validation report", shiny::uiOutput(ns("import_badge"), inline = TRUE))),
        shiny::uiOutput(ns("validation")))
    ),
    bslib::card(
      bslib::card_header("Families in the file"),
      shiny::p(class = "small text-muted", "One family is analysed at a time; families are never pooled. Families that cannot be analysed list the reason."),
      DT::DTOutput(ns("families")),
      shiny::uiOutput(ns("family_select"))
    ))
}

mod_data_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$prereq <- shiny::renderUI(if (is.null(ctx$proj())) hsms_prereq_ui("Create or open a project first (stage 1)."))
    output$tpl_ped <- shiny::downloadHandler("pedigree_template.csv", function(f) utils::write.csv(hsms_template_pedigree(), f, row.names = FALSE, na = ""))
    output$tpl_geno <- shiny::downloadHandler("genotypes_template.csv", function(f) utils::write.csv(hsms_template_genotypes(), f, row.names = FALSE, na = ""))

    output$current_inputs <- shiny::renderUI({
      p <- ctx$proj(); if (is.null(p) || is.null(p$inputs)) return(NULL)
      hsms_notice(type = "info",
                  shiny::strong("Stored inputs: "), p$inputs$pedigree$original_name, " and ", p$inputs$genotypes$original_name,
                  if (isTRUE(p$demo)) shiny::tags$span(" (simulated demo data shipped with HalfSibMap)"),
                  shiny::br(), shiny::tags$small("SHA-256: ", substr(p$inputs$pedigree$sha256, 1, 12), "... / ",
                                                 substr(p$inputs$genotypes$sha256, 1, 12), "... Uploading new files replaces them and marks all results outdated."))
    })
    output$import_badge <- shiny::renderUI({
      p <- ctx$proj(); if (is.null(p)) return(NULL)
      jv <- ctx$job_view()
      hsms_status_badge(if (!is.null(jv) && jv$stage == "import") "running" else hsms_stage_status(p, "import"))
    })

    shiny::observeEvent(input$validate, {
      p <- ctx$proj()
      if (is.null(p)) { ctx$notify("Create or open a project first.", "warning"); return() }
      if (is.null(input$ped) || is.null(input$geno)) {
        if (!is.null(p$inputs)) {
          p$accept_unknown_father <- isTRUE(input$accept_father)
          ctx$set_proj(p); ctx$launch("import", label = "Import and validate"); return()
        }
        ctx$notify("Choose both a pedigree and a genotype file.", "warning"); return()
      }
      p <- tryCatch(hsms_action_set_inputs(p, ctx$dir(), input$ped$datapath, input$geno$datapath,
                                           input$ped$name, input$geno$name, isTRUE(input$accept_father)),
                    error = function(e) e)
      if (inherits(p, "error")) { ctx$notify(conditionMessage(p), "error"); return() }
      p$demo <- FALSE
      ctx$set_proj(p)
      ctx$launch("import", label = "Import and validate")
    })

    output$validation <- shiny::renderUI({
      p <- ctx$proj()
      if (is.null(p)) return(hsms_empty_state("No project", "Create or open a project first."))
      v <- p$validation
      if (is.null(v)) return(hsms_empty_state("Not validated yet", "Choose the two files and click Validate files. Validation runs in the background and checks headers, identifiers, calls and family structure before HalfSibMap reads anything."))
      iss <- hsms_rows_to_df(v$issues)
      n_err <- sum(iss$severity == "error"); n_warn <- sum(iss$severity == "warning")
      shiny::tagList(
        if (n_err) hsms_notice(type = "danger", shiny::strong(sprintf("%d blocking problem(s).", n_err)), " Fix the files and validate again; nothing was imported.")
        else hsms_notice(type = "success", shiny::strong("Files imported."), sprintf(" %d warning(s), shown below. Original identifiers are preserved through a reversible internal mapping.", n_warn)),
        if (isTRUE(v$needs_father_confirmation)) hsms_notice(type = "warning", "Father information is present. Tick the unknown-father box and click Validate files to proceed, or remove the father column values."),
        shiny::tags$h6("Blocking errors"), DT::DTOutput(ns("errors")),
        shiny::tags$h6("Warnings and notes"), DT::DTOutput(ns("warnings")),
        shiny::downloadLink(ns("dl_report"), "Download validation report (CSV)"))
    })
    issues <- shiny::reactive({
      v <- ctx$proj()$validation; if (is.null(v)) return(NULL)
      x <- hsms_rows_to_df(v$issues)
      if (!NROW(x) || !"severity" %in% names(x)) hsms_empty_issues() else x
    })
    output$errors <- DT::renderDT({ i <- issues(); shiny::req(i); hsms_dt(i[i$severity == "error", c("code", "message", "ids", "n_affected", "location")], page_length = 5, scroll_y = "200px") })
    output$warnings <- DT::renderDT({ i <- issues(); shiny::req(i); hsms_dt(i[i$severity != "error", c("severity", "code", "message", "ids", "n_affected", "location")], page_length = 5, scroll_y = "200px") })
    output$dl_report <- shiny::downloadHandler("validation_report.csv", function(f) hsms_write_csv(issues(), f))

    families <- shiny::reactive({
      p <- ctx$proj(); if (is.null(p) || is.null(p$validation$families)) return(NULL)
      hsms_rows_to_df(p$validation$families)
    })
    output$families <- DT::renderDT({
      f <- families(); shiny::req(f)
      cols <- intersect(c("family_id", "mother_id", "n_offspring_pedigree", "n_offspring_genotyped", "n_markers",
                          "maternal_het", "maternal_hom", "maternal_missing", "offspring_missing_rate",
                          "known_father_rows", "analyzable", "notes"), names(f))
      hsms_dt(f[, cols], page_length = 10, scroll_y = "260px")
    })
    output$family_select <- shiny::renderUI({
      p <- ctx$proj(); f <- families()
      if (is.null(f) || !hsms_is_current(p, "import")) return(NULL)
      ok <- f$family_id[as.logical(f$analyzable)]
      if (!length(ok)) return(hsms_notice(type = "danger", "No family can be analysed."))
      shiny::div(class = "d-flex gap-2 align-items-end flex-wrap mt-2",
                 shiny::selectInput(ns("family"), "Family to analyse", choices = ok, selected = p$family %||% ok[1]),
                 shiny::actionButton(ns("use_family"), "Use this family", class = "btn-primary mb-3"),
                 if (!is.null(p$family)) shiny::tags$span(class = "mb-3", "Current: ", shiny::strong(p$family), " ",
                                                          hsms_status_badge(hsms_stage_status(p, "selection"))))
    })
    shiny::observeEvent(input$use_family, {
      p <- ctx$proj()
      changing <- !is.null(p$family) && !identical(p$family, input$family)
      p2 <- tryCatch(hsms_action_select_family(p, ctx$dir(), input$family, ctx$cache), error = function(e) e)
      if (inherits(p2, "error")) { ctx$notify(conditionMessage(p2), "error"); return() }
      ctx$set_proj(p2)
      ctx$notify(sprintf("Family %s selected%s.", input$family,
                         if (changing) "; exclusions were reset and all analysis results are now outdated" else ""))
      ctx$go("quality")
    })
  })
}
