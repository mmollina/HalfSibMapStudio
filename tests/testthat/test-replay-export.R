# Results export and the replay bundle executed in a fresh R process.

test_that("the results package contains labelled tables, native objects, figures and provenance", {
  skip_if_fast()
  dp <- demo_project()
  z <- tempfile(fileext = ".zip")
  r <- hsms_export_results(dp$proj, dp$dir, z)
  expect_identical(r$status, "complete")
  files <- zip::zip_list(z)$filename
  for (f in c("manifest.json", "README.txt", "tables/marker_positions.csv", "tables/adjacent_intervals.csv",
              "tables/fit_summary.csv", "tables/inheritance_maternal_LG1.csv", "tables/paternal_sharing_LG1.csv",
              "tables/exclusions.csv", "native/map_LG1_HSMap.sourceaware.rds", "figures/map_LG1.png",
              "figures/map_LG1.pdf", "settings/applied_settings.json", "provenance/versions.json",
              "provenance/identifier_mapping.csv"))
    expect_true(f %in% files, info = f)
  ex <- tempfile(); zip::unzip(z, exdir = ex)
  pos <- utils::read.csv(file.path(ex, "tables", "marker_positions.csv"), colClasses = "character")
  expect_true(all(pos$marker_id %in% sprintf("m%d", 1:24)))
  expect_true(all(pos$result_label %in% c("complete", "complete_with_warnings")))
  sa <- readRDS(file.path(ex, "native", "map_LG1_HSMap.sourceaware.rds"))
  expect_s3_class(sa, "HSMap.sourceaware")
})

test_that("outdated results are excluded by default and labelled historical when included", {
  skip_if_fast()
  dp <- demo_project(); p <- dp$proj
  p$phase_spec[["1"]]$min_phase_lod <- 2      # makes phase and map outdated
  z <- tempfile(fileext = ".zip")
  r <- hsms_export_results(p, dp$dir, z)
  expect_false("tables/marker_positions.csv" %in% zip::zip_list(z)$filename)
  expect_true(any(grepl("outdated", r$notes)))
  z2 <- tempfile(fileext = ".zip")
  r2 <- hsms_export_results(p, dp$dir, z2, include_historical = TRUE)
  expect_identical(r2$status, "includes-historical")
  ex <- tempfile(); zip::unzip(z2, exdir = ex)
  pos <- utils::read.csv(file.path(ex, "tables", "marker_positions.csv"))
  expect_true(all(pos$result_label == "historical"))
})

test_that("a non-converged fit is exported with its warning state", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- hsms_open_project(dir)$proj
  p <- hsms_set_map_spec(p, "1", "source_aware", modifyList(dp$defaults$source_aware, list(maxit = 3)))
  p <- hsms_run_stage_sync(p, dir, "map", hsms_group_items(p, dir, "map", "1"))
  expect_identical(hsms_stage_status(p, "map", "1"), "complete_with_warnings")
  z <- tempfile(fileext = ".zip")
  r <- hsms_export_results(p, dir, z)
  expect_identical(r$status, "complete-with-warnings")
  ex <- tempfile(); zip::unzip(z, exdir = ex)
  fs <- utils::read.csv(file.path(ex, "tables", "fit_summary.csv"))
  expect_false(fs$converged[1]); expect_match(fs$warnings[1], "did not converge")
  man <- jsonlite::read_json(file.path(ex, "manifest.json"))
  expect_true(length(man$warnings) > 0)
})

test_that("the replay bundle recomputes the analysis in a fresh R session from another directory", {
  skip_if_fast()
  dp <- demo_project()
  z <- tempfile(fileext = ".zip")
  hsms_export_replay_bundle(dp$proj, dp$dir, z)
  root <- file.path(tempfile("replay "), "with space")   # paths with spaces
  dir.create(root, recursive = TRUE)
  zip::unzip(z, exdir = root)
  wd <- tempfile("elsewhere_"); dir.create(wd)
  env <- c(callr::rcmd_safe_env(), R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep), R_LIBS_USER = "")
  run <- function(script) processx::run(file.path(R.home("bin"), "Rscript"), c("--vanilla", file.path(root, "replay", script)),
                                        wd = wd, env = env, error_on_status = FALSE, timeout = 600)
  s <- run("setup.R"); expect_identical(s$status, 0L, info = s$stderr)
  r <- run("replay.R"); expect_identical(r$status, 0L, info = paste(r$stdout, r$stderr))
  expect_match(r$stdout, "Replay complete")
  c1 <- run("compare.R"); expect_identical(c1$status, 0L, info = c1$stdout)
  expect_match(c1$stdout, "All replay outputs match")
  expect_false(grepl("FAIL", c1$stdout))
})
