# Application shell UI.

HSMS_NAV <- c(project = "Project", data = "Data", quality = "Quality review", twopoint = "Two-point analysis",
              groups = "Groups and order", phasemaps = "Phase and maps", results = "Inheritance and results",
              export = "Export")

hsms_ui <- function() {
  header <- shiny::div(
    class = "hsms-header",
    shiny::div(class = "hsms-brand",
               shiny::tags$span(class = "hsms-title", "HalfSibMap Studio"),
               shiny::tags$span(class = "hsms-subtitle", "Maternal linkage mapping for open-pollinated half-sib families")),
    shiny::div(class = "hsms-header-right",
               shiny::uiOutput("header_status", inline = TRUE),
               shiny::actionButton("open_settings", "Settings", class = "btn-sm btn-outline-secondary"),
               shiny::actionButton("open_help", "Help", class = "btn-sm btn-outline-secondary"),
               shiny::actionButton("toggle_jobs", shiny::uiOutput("jobs_button_label", inline = TRUE),
                                   class = "btn-sm btn-outline-primary"))
  )
  bslib::page_sidebar(
    title = header,
    window_title = "HalfSibMap Studio",
    theme = hsms_theme(),
    fillable = FALSE,
    sidebar = bslib::sidebar(id = "nav_sidebar", width = 290, open = "desktop", gap = "0.25rem",
                             shiny::tags$nav(`aria-label` = "Workflow stages", shiny::uiOutput("nav"))),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "app.css"),
      shiny::tags$script(src = "app.js")),
    bslib::layout_sidebar(
      fillable = FALSE, border = FALSE, border_radius = FALSE, bg = "transparent",
      sidebar = bslib::sidebar(id = "jobs_sidebar", position = "right", open = FALSE, width = 400,
                               title = "Jobs and log", mod_jobs_ui("jobs")),
      bslib::navset_hidden(
        id = "stage",
        bslib::nav_panel_hidden("project", mod_project_ui("project")),
        bslib::nav_panel_hidden("data", mod_data_ui("data")),
        bslib::nav_panel_hidden("quality", mod_qc_ui("qc")),
        bslib::nav_panel_hidden("twopoint", mod_pairwise_ui("twopoint")),
        bslib::nav_panel_hidden("groups", mod_group_order_ui("groups")),
        bslib::nav_panel_hidden("phasemaps", mod_phase_map_ui("phasemaps")),
        bslib::nav_panel_hidden("results", mod_results_ui("results")),
        bslib::nav_panel_hidden("export", mod_exports_ui("export"))
      )
    )
  )
}

hsms_stage_header <- function(number, title, text, ...) {
  shiny::div(class = "hsms-stage-header",
             shiny::tags$h2(shiny::tags$span(class = "hsms-stage-num", number), title),
             shiny::p(class = "hsms-stage-intro", text), ...)
}
