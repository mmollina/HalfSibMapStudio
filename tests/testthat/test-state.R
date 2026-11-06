# Dependency model: which applied changes make which results outdated.

statuses <- function(p) c(
  import = hsms_stage_status(p, "import"), selection = hsms_stage_status(p, "selection"),
  tpt_raw = hsms_stage_status(p, "tpt_raw"), tpt_filtered = hsms_stage_status(p, "tpt_filtered"),
  grouping = hsms_stage_status(p, "grouping"), order = hsms_stage_status(p, "order", "1"),
  phase = hsms_stage_status(p, "phase", "1"), map = hsms_stage_status(p, "map", "1"))
current <- function(p) vapply(c("import", "selection", "tpt_raw", "tpt_filtered", "grouping"),
                              function(s) hsms_is_current(p, s), logical(1))

test_that("the completed demo project is current at every stage", {
  skip_if_fast()
  dp <- demo_project()
  expect_true(all(statuses(dp$proj) %in% c("complete", "complete_with_warnings")))
})

test_that("invalidation rules follow the documented table", {
  skip_if_fast()
  p0 <- demo_project()$proj
  grp_cur <- function(p) c(order = hsms_is_current(p, "order", "1"), phase = hsms_is_current(p, "phase", "1"),
                           map = hsms_is_current(p, "map", "1"))
  # raw input change -> everything
  p <- p0; p$inputs$genotypes$sha256 <- "changed"
  expect_false(any(current(p))); expect_false(any(grp_cur(p)))
  # family change -> all analytical descendants, import still current
  p <- p0; p$family <- "MOM2"
  expect_identical(unname(current(p)), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  # exclusions -> selection and everything after
  p <- p0; p$exclusions <- rbind(p$exclusions, data.frame(kind = "sample", internal = "S000002", original = "P1_O1",
                                                          reason = "test", source = "manual", applied_at = "", stringsAsFactors = FALSE))
  expect_identical(unname(current(p)), c(TRUE, FALSE, FALSE, FALSE, FALSE)); expect_false(any(grp_cur(p)))
  # two-point parameters -> raw two-point and descendants
  p <- p0; p$settings$pairwise$lambda <- 10
  expect_identical(unname(current(p)), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  # filter -> filtered and descendants; raw two-point reusable
  p <- p0; p$settings$filter$lod <- 4
  expect_identical(unname(current(p)), c(TRUE, TRUE, TRUE, FALSE, FALSE)); expect_false(any(grp_cur(p)))
  # grouping parameters -> groups and all group descendants
  p <- p0; p$settings$group$na_r <- 0.4
  expect_identical(unname(current(p)), c(TRUE, TRUE, TRUE, TRUE, FALSE)); expect_false(any(grp_cur(p)))
  # one group's order -> that group's phase and map only
  p <- p0; p$order_spec[["1"]]$settings$itmax <- 5000L
  expect_true(all(current(p))); expect_identical(unname(grp_cur(p)), c(FALSE, FALSE, FALSE))
  # phase settings -> phase and map
  p <- p0; p$phase_spec[["1"]]$min_phase_lod <- 1
  expect_identical(unname(grp_cur(p)), c(TRUE, FALSE, FALSE))
  # model settings -> map only
  p <- p0; p$map_spec[["1"]]$settings$epsilon <- 0.02
  expect_identical(unname(grp_cur(p)), c(TRUE, TRUE, FALSE))
  # display-only changes -> nothing
  p <- p0; p$settings$display <- list(gap_r = 0.45, map_function = "kosambi"); p$group_labels <- c(`1` = "Chr 7")
  p$settings$app$threads <- 3L
  expect_true(all(current(p))); expect_true(all(grp_cur(p)))
  # an outdated stage reports "outdated", and its descendants too
  p <- p0; p$settings$filter$lod <- 4
  expect_identical(hsms_stage_status(p, "tpt_filtered"), "outdated")
  expect_identical(hsms_stage_status(p, "map", "1"), "outdated")
})

test_that("signatures survive a JSON save/reopen round trip", {
  skip_if_fast()
  dp <- demo_project()
  dir <- copy_project(dp$dir)
  re <- hsms_open_project(dir)
  expect_length(re$problems, 0L)
  for (s in HSMS_GLOBAL_STAGES) expect_identical(hsms_expected_signature(re$proj, s), hsms_expected_signature(dp$proj, s))
  for (s in HSMS_GROUP_STAGES) expect_identical(hsms_expected_signature(re$proj, s, "1"), hsms_expected_signature(dp$proj, s, "1"))
})

test_that("stale job results are kept as historical, never current", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- hsms_open_project(dir)$proj
  p$settings$filter$lod <- 4
  req <- hsms_build_request(p, dir, "tpt_filtered", list(list(scope = NULL)))
  job <- hsms_job_launch(req)
  p_changed <- p; p_changed$settings$filter$lod <- 4.5   # user applies new settings while the job runs
  job$proc$wait(120000); job <- hsms_job_poll(job)
  expect_identical(job$state, "finished")
  acc <- hsms_accept_job(p_changed, dir, job, hsms_job_manifest(job))
  expect_identical(acc$accepted, 0L)
  expect_identical(acc$events[[1]]$type, "stale_result_kept_as_historical")
  expect_false(hsms_is_current(acc$proj, "tpt_filtered"))
  hist <- Filter(function(m) identical(m$status, "historical"), acc$proj$artifacts)
  expect_length(hist, 1L)
})
