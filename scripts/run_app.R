# Launch HalfSibMap Studio locally.
#   Rscript scripts/run_app.R [--host=127.0.0.1] [--port=3838] [--no-browser]
# Environment overrides: HSMS_HOST, HSMS_PORT, HSMS_PROJECTS_DIR.
local({
  args <- commandArgs(trailingOnly = TRUE)
  get_arg <- function(name, default) {
    hit <- grep(paste0("^--", name, "="), args, value = TRUE)
    if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
  }
  app_dir <- normalizePath(".", winslash = "/")
  if (!file.exists(file.path(app_dir, "app.R")))
    stop("Run this script from the HalfSibMap Studio folder (the one that contains app.R).", call. = FALSE)
  if (file.exists(file.path(app_dir, "renv", "activate.R")) && !nzchar(Sys.getenv("RENV_PROJECT")))
    source(file.path(app_dir, "renv", "activate.R"))
  for (p in c("shiny", "bslib", "HalfSibMap", "callr", "DT"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' is not installed. Run: Rscript scripts/bootstrap.R", call. = FALSE)
  host <- get_arg("host", Sys.getenv("HSMS_HOST", "127.0.0.1"))
  port <- as.integer(get_arg("port", Sys.getenv("HSMS_PORT", "3838")))
  browser <- !("--no-browser" %in% args) && !nzchar(Sys.getenv("CI"))
  if (!identical(host, "127.0.0.1") && !identical(host, "localhost"))
    message("Note: binding to ", host, " exposes the app beyond this computer. This release is not hardened for public hosting.")
  message("HalfSibMap Studio: http://", if (host == "0.0.0.0") "127.0.0.1" else host, ":", port)
  shiny::runApp(app_dir, host = host, port = port, launch.browser = browser)
})
