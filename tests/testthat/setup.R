# Load the application's pure-R core (no Shiny) for tests.
hsms_test_app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(hsms_test_app_dir, "R", "core_files.R"))
hsms_source_core(hsms_test_app_dir, envir = globalenv())
options(hsms.app_dir = hsms_test_app_dir)
Sys.setenv(HSMS_PROJECTS_DIR = file.path(tempdir(), "hsms_test_projects"))
RcppParallel::setThreadOptions(numThreads = 1L)
