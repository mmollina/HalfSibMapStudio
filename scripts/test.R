# Run the automated checks.
#   Rscript scripts/test.R            # all checks except the browser end-to-end test
#   Rscript scripts/test.R --fast     # quick unit checks only (skips worker/integration tests)
#   Rscript scripts/test.R --browser  # also run the shinytest2 browser test (needs Chrome/Chromium)
local({
  args <- commandArgs(trailingOnly = TRUE)
  if (!file.exists("app.R")) stop("Run from the repository root.", call. = FALSE)
  if (file.exists("renv/activate.R") && !nzchar(Sys.getenv("RENV_PROJECT"))) source("renv/activate.R")
  Sys.setenv(NOT_CRAN = "true",
             HSMS_FAST_TESTS = if ("--fast" %in% args) "true" else "false",
             HSMS_BROWSER_TESTS = if ("--browser" %in% args) "true" else "false")
  res <- testthat::test_dir("tests/testthat", reporter = testthat::ProgressReporter$new(show_praise = FALSE),
                            stop_on_failure = FALSE, load_package = "none")
  df <- as.data.frame(res)
  cat(sprintf("\nTests: %d files, %d expectations, %d failed, %d skipped, %d errors\n",
              length(unique(df$file)), sum(df$nb), sum(df$failed), sum(df$skipped), sum(df$error)))
  if (any(df$failed > 0) || any(df$error)) quit(status = 1, save = "no")
})
