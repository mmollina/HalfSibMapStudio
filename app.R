# HalfSibMap Studio -- Shiny entry point.
# Run from the repository root:  shiny::runApp(".")   or   Rscript scripts/run_app.R
# Shiny sources every file in R/ automatically before this file runs.
options(
  hsms.app_dir = normalizePath(".", winslash = "/"),
  shiny.maxRequestSize = 200 * 1024^2
)
local({
  d <- hsms_defaults()
  options(shiny.maxRequestSize = d$app$max_upload_mb * 1024^2)
})
shiny::shinyApp(ui = hsms_ui(), server = hsms_server)
