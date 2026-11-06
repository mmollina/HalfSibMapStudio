# App-owned gap-aware summary logic. These use small HAND-BUILT inputs (not
# fitted maps) to pin down the classification rules exactly; real fits are
# cross-checked against HalfSibMap::get_block_map() at the end.

test_that("no-linkage, unresolved and unfitted intervals are gaps with NA distance and positions restart", {
  mk <- paste0("m", 1:7)
  r <- c(0.1, 0.5, 0.05, NA, 0.2, NA)          # interval 2: no linkage; 4: boundary; 6: unfitted block
  boundary <- c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  block <- c(1, 1, 1, 1, 2, 2, 3)
  s <- hsms_summarize_map(mk, r, boundary, block, "haldane", 0.499)
  expect_identical(s$intervals$status, c("linked", "no_linkage_boundary", "linked", "unresolved_phase",
                                         "linked", "unresolved_phase"))
  expect_true(all(is.na(s$intervals$distance_cM[s$intervals$status != "linked"])))
  expect_identical(s$positions$segment_id, c(1L, 1L, 2L, 2L, 3L, 3L, 4L))
  expect_equal(s$positions$position_cM[c(1, 3, 5)], c(0, 0, 0))
  expect_equal(s$linked_length, sum(HalfSibMap::inv_haldane(c(0.1, 0.05, 0.2))))
  expect_identical(s$n_gaps, 3L)
  # a marker alone in its segment is kept but gets no estimated position
  expect_true(s$positions$singleton_segment[7]); expect_true(is.na(s$positions$position_cM[7]))
})

test_that("r near 0.5 is classified as a gap before any distance transformation", {
  s <- hsms_summarize_map(c("a", "b", "c"), c(0.4995, 0.1), c(FALSE, FALSE), c(1, 1, 1), "kosambi", 0.499)
  expect_identical(s$intervals$status[1], "no_linkage_boundary")
  expect_true(is.na(s$intervals$distance_cM[1]))
  expect_true(is.finite(s$intervals$distance_cM[2]))
})

test_that("map function changes distances but never statuses or segments", {
  args <- list(paste0("m", 1:4), c(0.1, 0.5, 0.2), rep(FALSE, 3), rep(1, 4))
  h <- do.call(hsms_summarize_map, c(args, list("haldane", 0.499)))
  k <- do.call(hsms_summarize_map, c(args, list("kosambi", 0.499)))
  expect_identical(h$intervals$status, k$intervals$status)
  expect_identical(h$positions$segment_id, k$positions$segment_id)
  expect_lt(k$linked_length, h$linked_length)
})

test_that("misaligned inputs are refused", {
  expect_error(hsms_summarize_map(c("a", "b"), c(0.1, 0.2), FALSE, c(1, 1)), "misaligned")
})
