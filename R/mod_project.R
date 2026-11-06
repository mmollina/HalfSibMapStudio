# Stage 1: Project (create, open, import bundle, demo; summary and history).

mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hsms_stage_header(1, "Project", "Create or open a project. Projects keep inputs, applied settings, results and history together on this machine."),
    shiny::uiOutput(ns("body")))
}

mod_project_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    projects <- shiny::reactive({ ctx$events_tick(); ctx$proj(); hsms_list_projects(ctx$projects_dir) })

    output$body <- shiny::renderUI({
      p <- ctx$proj()
      if (is.null(p)) return(start_ui())
      project_ui(p)
    })

    start_ui <- function() {
      pl <- shiny::isolate(projects())
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6, 6, 6)),
        bslib::card(bslib::card_header("Try the simulated demo"),
                    shiny::p("The HalfSibMap package ships a small ", shiny::strong("simulated"), " dataset: 24 markers on one linkage group and three mothers (MOM1, MOM2, MOM3) with 60, 50 and 45 offspring. The demo analyses family MOM1 only."),
                    shiny::div(class = "d-flex gap-2 flex-wrap",
                               shiny::actionButton(ns("demo_load"), "Load demo data", class = "btn-outline-primary"),
                               shiny::actionButton(ns("demo_run"), "Run demo workflow", class = "btn-primary")),
                    shiny::p(class = "small text-muted mt-2", "Load demo data imports the files and selects MOM1. Run demo workflow then runs every stage with recorded preset settings (package defaults, with k = 1 as a demo setting), one background job at a time.")),
        bslib::card(bslib::card_header("New project"),
                    shiny::textInput(ns("new_name"), "Project name", placeholder = "e.g. Eucalyptus family E12"),
                    shiny::actionButton(ns("create"), "Create project", class = "btn-primary")),
        bslib::card(bslib::card_header("Open a saved project"),
                    if (!nrow(pl)) shiny::p(class = "text-muted", "No saved projects yet.") else shiny::tagList(
                      shiny::selectInput(ns("open_dir"), "Project", choices = stats::setNames(pl$dir,
                        sprintf("%s  (%s%s)", pl$name, substr(pl$modified, 1, 16), ifelse(pl$locked, ", open elsewhere?", "")))),
                      shiny::actionButton(ns("open"), "Open project", class = "btn-primary")),
                    shiny::p(class = "small text-muted mt-2", "Project store: ", shiny::tags$code(ctx$projects_dir))),
        bslib::card(bslib::card_header("Import a project bundle"),
                    shiny::fileInput(ns("bundle"), "Project bundle (.hsmap.zip)", accept = c(".zip", "application/zip")),
                    shiny::p(class = "small text-muted", "Bundles are validated (paths, schema, checksums) and extracted into a new project folder. RDS results are meant for trusted, local workflows; do not open bundles from untrusted sources.")))
    }

    project_ui <- function(p) {
      ev <- hsms_read_events(ctx$dir(), 300)
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        bslib::card(bslib::card_header("Project"),
                    hsms_kv(Name = p$name, Family = p$family %||% "not selected",
                            Created = substr(p$created, 1, 19), Folder = shiny::tags$code(ctx$dir()),
                            `HalfSibMap commit` = shiny::tags$code(substr(p$halfsibmap_commit %||% "", 1, 12)),
                            `App version` = p$app_version, `Worker threads` = p$settings$app$threads,
                            Seed = p$settings$app$seed,
                            Data = if (isTRUE(p$demo)) "Simulated demo data from the HalfSibMap package" else
                              if (is.null(p$inputs)) "No files yet" else paste(p$inputs$pedigree$original_name, "+", p$inputs$genotypes$original_name)),
                    shiny::textInput(ns("rename"), "Rename project", value = p$name),
                    shiny::div(class = "d-flex gap-2 flex-wrap",
                               shiny::actionButton(ns("do_rename"), "Rename", class = "btn-outline-secondary btn-sm"),
                               shiny::actionButton(ns("save"), "Save project", class = "btn-primary btn-sm"),
                               shiny::actionButton(ns("close"), "Close project", class = "btn-outline-secondary btn-sm")),
                    shiny::p(class = "small text-muted mt-2", "The project manifest is saved automatically after every applied change and accepted result; Save project writes it again explicitly. Download a portable bundle from Export.")),
        bslib::card(bslib::card_header("History"),
                    shiny::p(class = "small text-muted", "Applied changes and completed jobs, newest first."),
                    DT::DTOutput(ns("history"))))
    }

    output$history <- DT::renderDT({
      ctx$events_tick(); shiny::req(ctx$dir())
      hsms_dt(hsms_read_events(ctx$dir(), 500), page_length = 12)
    })

    new_project <- function(name) {
      cr <- hsms_create_project(name, ctx$projects_dir, ctx$defaults)
      ctx$open_project(cr$dir, cr$proj)
      cr
    }
    shiny::observeEvent(input$create, {
      nm <- trimws(input$new_name)
      if (!nzchar(nm)) { ctx$notify("Enter a project name.", "warning"); return() }
      new_project(nm); ctx$go("data")
    })
    shiny::observeEvent(input$open, {
      r <- tryCatch(hsms_open_project(input$open_dir), error = function(e) e)
      if (inherits(r, "error")) { ctx$notify(conditionMessage(r), "error", 12); return() }
      ok <- tryCatch({ ctx$open_project(input$open_dir, r$proj, c(r$problems, r$notices)); TRUE },
                     error = function(e) { ctx$notify(conditionMessage(e), "error", 12); FALSE })
    })
    shiny::observeEvent(input$bundle, {
      r <- tryCatch(hsms_import_bundle(input$bundle$datapath, ctx$projects_dir), error = function(e) e)
      if (inherits(r, "error")) { ctx$notify(conditionMessage(r), "error", 15); return() }
      ctx$open_project(r$dir, r$proj, c(r$problems, r$notices))
      ctx$notify("Project bundle imported.")
    })
    start_demo <- function(full) {
      if (ctx$busy()) { ctx$notify("A job is already running.", "warning"); return() }
      cr <- new_project(if (full) "Demo workflow (simulated MOM1)" else "Demo data (simulated)")
      ped <- system.file("extdata", "example_pedigree.csv", package = "HalfSibMap")
      geno <- system.file("extdata", "example_genotypes.csv", package = "HalfSibMap")
      p <- hsms_action_set_inputs(shiny::isolate(ctx$proj()), cr$dir, ped, geno,
                                  "example_pedigree.csv", "example_genotypes.csv")
      p$demo <- TRUE
      ctx$set_proj(p)
      ctx$start_demo(full)
      ctx$launch("import", label = "Demo: import and validate")
      ctx$go(if (full) "data" else "data")
    }
    shiny::observeEvent(input$demo_load, start_demo(FALSE))
    shiny::observeEvent(input$demo_run, start_demo(TRUE))
    shiny::observeEvent(input$do_rename, {
      nm <- trimws(input$rename); p <- ctx$proj()
      if (!nzchar(nm)) return()
      p$name <- nm; ctx$set_proj(p); ctx$log("project_renamed", list(name = nm))
    })
    shiny::observeEvent(input$save, {
      ok <- ctx$set_proj(ctx$proj())
      if (ok) { ctx$log("project_saved"); ctx$notify("Project saved.") }
    })
    shiny::observeEvent(input$close, ctx$close_project())
  })
}
