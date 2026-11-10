# Thread selection and the calibrated two-point memory estimate.

test_that("the memory model grows with markers squared and with diagnostics", {
  a <- hsms_pairwise_memory_estimate(1000); b <- hsms_pairwise_memory_estimate(2000)
  d <- hsms_pairwise_memory_estimate(1000, return_diagnostics = TRUE)
  expect_equal(a$object_bytes, 48 * 1000^2)
  expect_equal((b$object_bytes / a$object_bytes), 4)
  for (k in c("object_bytes", "worker_peak_bytes", "filter_peak_bytes", "app_bytes", "file_bytes"))
    expect_gt(d[[k]], a[[k]])
  expect_gt(a$filter_peak_bytes, a$worker_peak_bytes)   # measured: the filter step needs more
  expect_equal(a$n_pairs, 1000 * 999 / 2)
  expect_true(is.na(a$available_bytes) || a$available_bytes > 0)
})

test_that("the memory warning compares need with available memory and the configured cap", {
  e <- hsms_pairwise_memory_estimate(3000)
  e$available_bytes <- 100 * 1024^3
  expect_false(hsms_pairwise_memory_warning(e, cap_gb = Inf)$warn)
  e$available_bytes <- 1 * 1024^3
  w <- hsms_pairwise_memory_warning(e, cap_gb = Inf)
  expect_true(w$warn); expect_true(w$over_avail)
  e$available_bytes <- NA_real_
  w2 <- hsms_pairwise_memory_warning(e, cap_gb = 0.5)
  expect_true(w2$warn); expect_true(w2$over_cap); expect_false(w2$over_avail)
})

test_that("the estimate covers the measured peak of a real two-point worker without gross overestimation", {
  skip_if_fast()
  skip_if_not(file.exists("/proc/self/status"), "peak-memory measurement needs Linux /proc")
  # The app's real job workers are measured: the parent reads the worker's
  # peak resident memory (VmHWM) from /proc while it runs.
  f <- sim_family_files(1500, 200, r_const = 0.01, seed = 5)
  base <- tempfile("mem_"); dir.create(base)
  cr <- hsms_create_project("Memory", base); p <- cr$proj; dir <- cr$dir
  p <- hsms_action_set_inputs(p, dir, f$ped, f$geno, "p.csv", "g.csv")
  p <- hsms_run_stage_sync(p, dir, "import"); p <- hsms_action_select_family(p, dir, "MOM1")
  hwm <- function(pid) {
    l <- tryCatch(readLines(sprintf("/proc/%d/status", pid)), error = function(e) character(0), warning = function(w) character(0))
    x <- grep("^VmHWM", l, value = TRUE)
    if (!length(x)) NA_real_ else as.numeric(sub("[^0-9]+([0-9]+).*", "\\1", x)) * 1024
  }
  est <- hsms_pairwise_memory_estimate(1500, FALSE, 200)
  for (stage in c("tpt_raw", "tpt_filtered")) {
    job <- hsms_job_launch(hsms_build_request(p, dir, stage, list(list(scope = NULL))))
    pid <- job$proc$get_pid(); peak <- 0
    while (job$proc$is_alive()) { h <- hwm(pid); if (!is.na(h)) peak <- max(peak, h); Sys.sleep(0.02) }
    job <- hsms_job_poll(job)
    p <- hsms_accept_job(p, dir, job, hsms_job_manifest(job))$proj
    e <- if (stage == "tpt_raw") est$worker_peak_bytes else est$filter_peak_bytes
    expect_gt(peak, 0)
    expect_gte(e, peak)                 # never an underestimate
    expect_lte(e, 1.35 * peak)          # and not grossly pessimistic
  }
})

test_that("the chosen thread count reaches HalfSibMap and is recorded with the result", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- hsms_open_project(dir)$proj
  p$settings$app$threads <- 2L
  p$settings$pairwise$lambda <- 19        # force a new two-point run
  p <- hsms_run_stage_sync(p, dir, "tpt_raw")
  meta <- p$artifacts[[hsms_current_id(p, "tpt_raw")]]
  expect_identical(as.integer(meta$threads), 2L)
  expect_identical(hsms_load_current(p, dir, "tpt_raw")$raw$threads, 2L)
  # threads are not part of the signature: results do not depend on them
  q <- p; q$settings$app$threads <- 1L
  expect_identical(hsms_expected_signature(q, "tpt_raw"), hsms_expected_signature(p, "tpt_raw"))
})
