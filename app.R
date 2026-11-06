# HalfSibMap Studio -- Shiny entry point.
# Run from the repository root:  shiny::runApp(".")   or   Rscript scripts/run_app.R
# Shiny normally sources every file in R/ before this file runs. Some tools
# (e.g. shinytest2, which sees DESCRIPTION) disable that autoloading, so the
# files are sourced here when needed.
if (!exists("hsms_ui", mode = "function")) {
  for (f in sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE))) source(f, local = TRUE)
}
options(hsms.app_dir = normalizePath(".", winslash = "/"))
options(shiny.maxRequestSize = hsms_defaults()$app$max_upload_mb * 1024^2)
shiny::shinyApp(ui = hsms_ui(), server = hsms_server)
