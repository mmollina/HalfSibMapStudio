# Jobs drawer: active job, cancel, bounded log, history.

mod_jobs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("active")),
    shiny::tags$h6(class = "mt-3", "Worker log (latest lines)"),
    shiny::tags$pre(class = "hsms-log", `aria-live` = "polite", shiny::textOutput(ns("log"))),
    shiny::tags$h6(class = "mt-3", "Recent jobs"),
    shiny::uiOutput(ns("history")))
}

mod_jobs_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$active <- shiny::renderUI({
      jv <- ctx$job_view()
      if (is.null(jv)) return(shiny::p(class = "text-muted", "No job is running. Heavy stages run one at a time in a separate R process; the interface stays usable."))
      prog <- if (!is.null(jv$done) && !is.null(jv$total) && jv$total > 1)
        shiny::div(class = "progress mb-2", role = "progressbar", `aria-valuenow` = jv$done, `aria-valuemin` = 0, `aria-valuemax` = jv$total,
                   shiny::div(class = "progress-bar", style = sprintf("width: %d%%", round(100 * jv$done / jv$total)),
                              sprintf("%d of %d", jv$done, jv$total)))
      else shiny::div(class = "small text-muted mb-2", "Progress: indeterminate (a single HalfSibMap call does not report intermediate progress).")
      shiny::div(class = "hsms-job-card",
                 shiny::tags$strong(jv$label), " ", hsms_status_badge(jv$state),
                 shiny::div(class = "small", jv$message),
                 shiny::div(class = "small text-muted", sprintf("Elapsed: %.0f s", jv$elapsed)),
                 prog,
                 shiny::actionButton(ns("cancel"), if (jv$state == "cancelling") "Cancelling..." else "Cancel job",
                                     class = "btn-outline-danger btn-sm", disabled = if (jv$state == "cancelling") "disabled" else NULL))
    })
    output$log <- shiny::renderText({ jv <- ctx$job_view(); if (is.null(jv)) "" else paste(utils::tail(jv$log, 60), collapse = "\n") })
    shiny::observeEvent(input$cancel, { ctx$cancel_job(); ctx$notify("Cancellation requested; waiting for the worker to stop.", "warning") })
    output$history <- shiny::renderUI({
      p <- ctx$proj(); if (is.null(p) || !length(p$jobs)) return(shiny::p(class = "text-muted small", "No jobs yet."))
      js <- utils::tail(p$jobs, 12)
      shiny::tags$table(class = "table table-sm small", shiny::tags$tbody(lapply(rev(js), function(j) shiny::tags$tr(
        shiny::tags$td(substr(j$started %||% "", 12, 19)), shiny::tags$td(j$label %||% j$stage),
        shiny::tags$td(j$status), shiny::tags$td(if (!is.null(j$elapsed_sec)) sprintf("%.1f s", j$elapsed_sec) else "")))))
    })
  })
}
