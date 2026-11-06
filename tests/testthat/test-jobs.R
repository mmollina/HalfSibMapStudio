# Background jobs with real computation: cancellation, failure, recovery.

test_that("cancellation stops a real running pairwise computation and earlier results stay usable", {
  skip_if_fast()
  f <- sim_family_files(3000, 300, r_const = 0.01, seed = 11)
  base <- tempfile("cancel_"); dir.create(base)
  cr <- hsms_create_project("Cancel test", base); p <- cr$proj; dir <- cr$dir
  p <- hsms_action_set_inputs(p, dir, f$ped, f$geno, "p.csv", "g.csv")
  p <- hsms_run_stage_sync(p, dir, "import")
  p <- hsms_action_select_family(p, dir, "MOM1")
  sel_id <- hsms_current_id(p, "selection")
  req <- hsms_build_request(p, dir, "tpt_raw", list(list(scope = NULL)))
  job <- hsms_job_launch(req)
  # wait for the worker-start signal: the stage is actually computing
  t0 <- Sys.time()
  repeat {
    st <- hsms_job_status_file(job)
    if (!is.null(st) && grepl("^Running", st$message %||% "")) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 60) fail("worker never started")
    Sys.sleep(0.2)
  }
  Sys.sleep(1)
  expect_true(job$proc$is_alive())
  pid <- job$proc$get_pid()
  t_cancel <- Sys.time()
  job <- hsms_job_cancel(job)
  repeat { job <- hsms_job_poll(job); if (job$state == "cancelled") break; Sys.sleep(0.1)
    if (as.numeric(difftime(Sys.time(), t_cancel, units = "secs")) > 15) fail("worker did not stop") }
  expect_false(job$proc$is_alive())
  expect_false(hsms_pid_alive(pid))
  expect_lt(as.numeric(difftime(Sys.time(), t_cancel, units = "secs")), 10)
  expect_null(hsms_job_manifest(job))      # nothing half-written is accepted
  for (it in req$items) p <- hsms_record_outcome(p, "tpt_raw", it$scope, "cancelled", it$signature, "test", job$id)
  expect_identical(hsms_stage_status(p, "tpt_raw"), "cancelled")
  expect_identical(hsms_current_id(p, "selection"), sel_id)
  expect_true(hsms_is_current(p, "selection"))
})

test_that("a worker error is reported as failed and the stage can be rerun", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- hsms_open_project(dir)$proj
  p$settings$pairwise$lambda <- 10
  req <- hsms_build_request(p, dir, "tpt_raw", list(list(scope = NULL)))
  req$inputs$selection <- file.path(dir, "artifacts", "does_not_exist.rds")
  saveRDS(req, file.path(req$job_dir, "request.rds"))
  job <- hsms_job_launch(req)
  job$proc$wait(120000); job <- hsms_job_poll(job)
  man <- hsms_job_manifest(job)
  expect_false(isTRUE(man$items[[1]]$ok))
  acc <- hsms_accept_job(p, dir, job, man)
  expect_identical(hsms_stage_status(acc$proj, "tpt_raw"), "failed")
  expect_match(acc$proj$last_outcome$tpt_raw$message, "cannot open|does_not_exist")
  # earlier valid artifacts are still there, and a correct rerun succeeds
  expect_true(hsms_is_current(acc$proj, "selection"))
  p2 <- hsms_run_stage_sync(acc$proj, dir, "tpt_raw")
  expect_identical(hsms_stage_status(p2, "tpt_raw"), "complete")
})

test_that("a multi-group job accepts finished groups and marks a failing group separately", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- hsms_open_project(dir)$proj
  p <- hsms_apply_settings(p, "group", modifyList(p$settings$group, list(k = 2)))
  p <- hsms_run_stage_sync(p, dir, "grouping")
  for (g in c("1", "2")) p <- hsms_set_order_spec_mds(p, g, dp$defaults$order)
  items <- hsms_group_items(p, dir, "order", c("1", "2"))
  items[[2]]$markers <- c("NOT_1", "NOT_2", "NOT_3")
  p <- hsms_run_stage_sync(p, dir, "order", items, list(order_settings = dp$defaults$order))
  expect_true(hsms_is_current(p, "order", "1"))
  expect_identical(hsms_stage_status(p, "order", "2"), "failed")
})

test_that("job requests are immutable files with only paths, settings and signatures", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- hsms_open_project(dir)$proj
  req <- hsms_build_request(p, dir, "phase", hsms_group_items(p, dir, "phase", "1"))
  on.exit(unlink(req$job_dir, recursive = TRUE))
  saved <- readRDS(file.path(req$job_dir, "request.rds"))
  expect_identical(saved, req)
  expect_false(any(vapply(unlist(saved, recursive = TRUE), is.environment, logical(1))))
  expect_true(all(file.exists(unlist(c(saved$inputs, lapply(saved$items, `[[`, "inputs"))))))
  expect_identical(saved$items[[1]]$signature, hsms_expected_signature(p, "phase", "1"))
})
