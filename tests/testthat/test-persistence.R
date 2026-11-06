# Save/reopen, compatibility events, locks, and portable bundles.

test_that("save and reopen keep IDs, settings, status and provenance without recomputation", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  re <- hsms_open_project(dir)
  expect_length(re$problems, 0L); expect_length(re$notices, 0L)
  p <- re$proj
  expect_identical(p$family, "MOM1")
  expect_equal(p$settings$pairwise$lambda, 20)
  expect_true(hsms_is_current(p, "map", "1"))
  meta <- p$artifacts[[hsms_current_id(p, "map", "1")]]
  expect_identical(meta$settings$model, "source_aware")
  expect_identical(meta$versions$HalfSibMap_commit, hsms_pinned_halfsibmap_commit())
  fit <- hsms_load_current(p, dir, "map", "1")
  imp <- hsms_load_current(p, dir, "import")
  expect_true(all(hsms_to_original(imp$mapping, "marker", fit$markers) %in% sprintf("m%d", 1:24)))
})

test_that("corrupt or missing artifacts are detected on reopen and set aside", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- dp$proj
  f_map <- file.path(dir, p$artifacts[[hsms_current_id(p, "map", "1")]]$file)
  writeBin(as.raw(1:10), f_map)
  unlink(file.path(dir, p$artifacts[[hsms_current_id(p, "phase", "1")]]$file))
  re <- hsms_open_project(dir)
  expect_true(any(grepl("checksum", re$problems)))
  expect_true(any(grepl("missing", re$problems)))
  expect_null(hsms_current_id(re$proj, "map", "1"))
  expect_true(hsms_is_current(re$proj, "tpt_raw"))
})

test_that("a different installed HalfSibMap commit is a compatibility event that outdates results", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  # simulate a different installed HalfSibMap commit
  real <- hsms_versions
  assign("hsms_versions", function() modifyList(real(), list(HalfSibMap_commit = strrep("0", 40))), envir = globalenv())
  on.exit(assign("hsms_versions", real, envir = globalenv()), add = TRUE)
  re <- hsms_open_project(dir)
  expect_true(any(grepl("Compatibility event", re$notices)))
  expect_identical(hsms_stage_status(re$proj, "import"), "outdated")
  expect_false(is.null(hsms_current_id(re$proj, "map", "1")))   # kept for inspection
})

test_that("interrupted jobs are marked interrupted on reopen", {
  skip_if_fast()
  dp <- demo_project(); dir <- copy_project(dp$dir)
  p <- dp$proj
  p$settings$pairwise$lambda <- 5
  sig <- hsms_expected_signature(p, "tpt_raw")
  p$jobs$job_x <- list(id = "job_x", stage = "tpt_raw", status = "running", items = list(list(scope = NULL, signature = sig)))
  hsms_save_project(p, dir)
  re <- hsms_open_project(dir)
  expect_identical(re$proj$jobs$job_x$status, "interrupted")
  expect_identical(hsms_stage_status(re$proj, "tpt_raw"), "interrupted")
})

test_that("a second session cannot take the lock of an open project", {
  d <- tempfile("lock_"); dir.create(d)
  hsms_lock_project(d, "token-A")
  expect_error(hsms_lock_project(d, "token-B"), "already open")
  hsms_unlock_project(d, "token-B")
  expect_true(file.exists(file.path(d, ".lock")))
  hsms_unlock_project(d, "token-A")
  expect_false(file.exists(file.path(d, ".lock")))
})

test_that("project bundles round-trip, and unsafe or corrupt bundles are rejected", {
  skip_if_fast()
  dp <- demo_project()
  z <- tempfile(fileext = ".hsmap.zip")
  hsms_export_bundle(dp$proj, dp$dir, z)
  base <- tempfile("store_"); dir.create(base)
  imp <- hsms_import_bundle(z, base)
  expect_length(imp$problems, 0L)
  expect_true(hsms_is_current(imp$proj, "map", "1"))
  expect_true(startsWith(imp$dir, normalizePath(base, winslash = "/")))
  # inputs-only and artifacts-only options state their limitation
  z2 <- tempfile(fileext = ".zip"); hsms_export_bundle(dp$proj, dp$dir, z2, include_artifacts = FALSE)
  i2 <- hsms_import_bundle(z2, base)
  expect_true(any(grepl("did not include analysis artifacts", i2$notices)))
  expect_null(hsms_current_id(i2$proj, "map", "1"))
  # path traversal
  evil <- tempfile("evil_"); dir.create(file.path(evil, "x"), recursive = TRUE)
  writeLines("{}", file.path(evil, "bundle.json")); writeLines("{}", file.path(evil, "project.json"))
  writeLines("boom", file.path(evil, "x", "evil.csv"))
  ze <- tempfile(fileext = ".zip")
  zip::zip(ze, c("bundle.json", "project.json", "x/evil.csv"), root = evil)
  expect_error(hsms_import_bundle(ze, base), "unsafe paths")
  expect_false(hsms_bundle_path_ok("../../etc/passwd"))
  expect_false(hsms_bundle_path_ok("/abs/project.json"))
  expect_false(hsms_bundle_path_ok("C:/x/project.json"))
  expect_false(hsms_bundle_path_ok("artifacts/run.R"))
  expect_true(hsms_bundle_path_ok("artifacts/a_1.rds"))
  # checksum mismatch
  st <- tempfile("unz_"); zip::unzip(z, exdir = st)
  cat("tampered", file = file.path(st, "metadata", "settings.json"), append = TRUE)
  zt <- tempfile(fileext = ".zip")
  zip::zip(zt, list.files(st, recursive = TRUE), root = st)
  expect_error(hsms_import_bundle(zt, base), "checksum")
  # schema version
  st2 <- tempfile("unz2_"); zip::unzip(z, exdir = st2)
  bj <- jsonlite::read_json(file.path(st2, "bundle.json")); bj$schema_version <- 99
  jsonlite::write_json(bj, file.path(st2, "bundle.json"), auto_unbox = TRUE)
  zs <- tempfile(fileext = ".zip"); zip::zip(zs, list.files(st2, recursive = TRUE), root = st2)
  expect_error(hsms_import_bundle(zs, base), "schema version")
})
