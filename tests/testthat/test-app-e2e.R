# Browser end-to-end test (shinytest2 + headless Chrome/Chromium).
# Runs only with HSMS_BROWSER_TESTS=true (Rscript scripts/test.R --browser).
# If no browser is available the test is SKIPPED with a clear reason, never
# reported as passing.

test_that("demo workflow runs end to end in the browser, with saving and exports", {
  testthat::skip_if_not(identical(Sys.getenv("HSMS_BROWSER_TESTS"), "true"), "browser tests not requested (use --browser)")
  testthat::skip_if_not_installed("shinytest2")
  chrome <- Sys.getenv("CHROMOTE_CHROME")
  if (!nzchar(chrome) && file.exists("/opt/pw-browsers/chromium")) Sys.setenv(CHROMOTE_CHROME = "/opt/pw-browsers/chromium")
  ok <- tryCatch(nzchar(chromote::find_chrome()), error = function(e) FALSE)
  testthat::skip_if_not(isTRUE(ok), "no Chrome/Chromium found for chromote (set CHROMOTE_CHROME)")
  if (.Platform$OS.type == "unix" && identical(Sys.info()[["user"]], "root"))
    chromote::set_chrome_args(c(chromote::default_chrome_args(), "--no-sandbox"))
  Sys.setenv(HSMS_PROJECTS_DIR = tempfile("e2e_projects_"))

  app <- shinytest2::AppDriver$new(hsms_test_app_dir, name = "e2e", load_timeout = 90000, timeout = 30000,
                                   width = 1366, height = 900, seed = 1)
  on.exit(app$stop(), add = TRUE)
  nav_text <- function() app$get_text("#nav")
  go <- function(stage) { app$run_js(sprintf("Shiny.setInputValue('nav_click', '%s', {priority: 'event'})", stage)); app$wait_for_idle(2000) }

  expect_match(nav_text(), "Project")
  app$click("project-demo_run")
  # the interface keeps answering while background jobs run
  responsive <- FALSE; t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 240) {
    # the always-visible header button is updated by the server while workers run
    if (grepl("Job running", app$get_text("#toggle_jobs"))) responsive <- TRUE
    if (grepl("Inheritance and results\\s*Available", nav_text())) break
    Sys.sleep(1)
  }
  expect_true(responsive)
  expect_match(nav_text(), "Phase and maps\\s*Complete")
  expect_match(app$get_text(".hsms-header-right"), "Family:\\s*MOM1")

  go("results")
  app$run_js("document.querySelector('a[data-value=\"Group map\"]').click()")
  app$wait_for_value(output = "results-map_info", ignore = list(NULL, ""), timeout = 30000)
  info <- app$get_text("#results-map_info")
  expect_match(info, "Source-aware HMM")
  expect_match(info, "Gaps")
  app$run_js("document.querySelector('a[data-value=\"Maternal inheritance\"]').click()"); app$wait_for_idle(3000)
  inh <- app$get_value(output = "results-inh_plot")
  expect_true(!is.null(inh$src) && nzchar(inh$src))

  go("project")
  app$click("project-save"); app$wait_for_idle(1500)
  expect_match(app$get_text(".hsms-header-right"), "Saved")

  go("export")
  bundle <- app$get_download("export-dl_bundle")
  expect_true("project.json" %in% zip::zip_list(bundle)$filename)
  app$click("export-build_results")
  t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 120) {
    if (grepl("Download selected", app$get_text("#export-built"))) break
    Sys.sleep(1)
  }
  res <- app$get_download("export-dl_built")
  files <- zip::zip_list(res)$filename
  expect_true(all(c("tables/marker_positions.csv", "tables/fit_summary.csv", "manifest.json") %in% files))
  expect_match(basename(res), "complete")
})
