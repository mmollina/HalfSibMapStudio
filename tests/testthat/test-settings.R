# config/defaults.json must mirror the pinned package's real defaults, and
# each stage keeps its own regularization setting.

test_that("stage defaults equal the formals() of the pinned HalfSibMap functions", {
  d <- hsms_defaults(hsms_test_app_dir)
  f <- function(fn) formals(utils::getFromNamespace(fn, "HalfSibMap"))
  pw <- f("pairwise_rf")
  for (nm in c("lambda", "q0", "tol", "maxit", "tiny", "return_diagnostics")) expect_equal(d$pairwise[[nm]], eval(pw[[nm]]), info = nm)
  tf <- f("tpt_filter")
  expect_equal(d$filter$lod, eval(tf$thresh.LOD.rf)); expect_equal(d$filter$lod, eval(tf$thresh.LOD.ph))
  expect_equal(d$filter$thresh_rf, eval(tf$thresh.rf))
  expect_equal(c(d$filter$prob_low, d$filter$prob_high), eval(tf$probs))
  gm <- f("group_markers")
  expect_equal(d$group$method, eval(gm$method)); expect_equal(d$group$na_r, eval(gm$na_r))
  expect_equal(d$group$weight_by_lod, eval(gm$weight_by_lod))
  mo <- f("mds_order")
  for (nm in c("itmax", "drop_outside_envelope", "envelope_k", "envelope_span", "min_keep")) expect_equal(d$order[[nm]], eval(mo[[nm]]), info = nm)
  ph <- f("phase_from_pairwise")
  for (nm in c("min_phase_lod", "tie_tol", "max_passes", "tol")) expect_equal(d$phase[[nm]], eval(ph[[nm]]), info = nm)
  sa <- f("hmm_map_source_aware")
  for (nm in c("epsilon", "lambda", "alpha_start", "beta_start", "r_start", "tol", "maxit", "fit_mode", "nmin", "smooth"))
    expect_equal(d$source_aware[[nm]], eval(sa[[nm]]), info = nm)
  hm <- f("hmm_map")
  for (nm in c("epsilon", "lambda", "r_start", "tol", "maxit")) expect_equal(d$population[[nm]], eval(hm[[nm]]), info = nm)
  expect_identical(d$population$paternal_mode, eval(hm$paternal_mode)[1])
  bl <- f("hmm_map_blocks")
  expect_equal(d$population$min_block_markers, eval(bl$min_block_markers)); expect_equal(d$population$gap_r, eval(bl$gap_r))
})

test_that("regularization is stage-specific (no single global lambda)", {
  d <- hsms_defaults(hsms_test_app_dir)
  expect_equal(d$pairwise$lambda, 20); expect_equal(d$source_aware$lambda, 2); expect_equal(d$population$lambda, 20)
  expect_equal(hsms_args_source_aware(modifyList(d$source_aware, list(lambda = 7)))$lambda, 7)
  expect_equal(hsms_args_pairwise(d$pairwise, 1)$lambda, 20)
})

test_that("only the identifiable paternal model is offered; the source-aware API has no threads argument", {
  expect_identical(hsms_setting_specs()$population$paternal_mode$choices, "gametic")
  expect_false("threads" %in% names(formals(HalfSibMap::hmm_map_source_aware)))
  expect_false("threads" %in% names(hsms_args_source_aware(hsms_defaults(hsms_test_app_dir)$source_aware)))
  expect_error(HalfSibMap:::.hsmap_paternal_engine("two_locus"), "disabled")
})

test_that("settings validation enforces the upstream domains", {
  d <- hsms_defaults(hsms_test_app_dir)
  expect_length(hsms_validate_settings("source_aware", d$source_aware), 0L)
  expect_true(length(hsms_validate_settings("source_aware", modifyList(d$source_aware, list(epsilon = 1)))) > 0)
  expect_true(length(hsms_validate_settings("filter", modifyList(d$filter, list(thresh_rf = 0.6)))) > 0)
  expect_true(length(hsms_validate_settings("filter", modifyList(d$filter, list(prob_low = 0.9, prob_high = 0.1)))) > 0)
  expect_true(length(hsms_validate_settings("group", modifyList(d$group, list(k = 1.5)))) > 0)
})

test_that("CSV exports keep full precision, explicit NA and Unicode", {
  f <- tempfile(fileext = ".csv")
  x <- data.frame(id = c("Ébène", "007", NA), v = c(0.1, 1 / 3, NA), stringsAsFactors = FALSE)
  hsms_write_csv(x, f)
  y <- utils::read.csv(f, colClasses = c("character", "numeric"), encoding = "UTF-8")
  expect_identical(enc2utf8(y$id[1:2]), enc2utf8(c("Ébène", "007")))
  expect_true(is.na(y$id[3])); expect_true(is.na(y$v[3]))
  expect_identical(y$v[2], 1 / 3)
})
