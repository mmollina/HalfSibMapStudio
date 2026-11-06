# Analysis adapters against the real HalfSibMap API.

d <- hsms_defaults(hsms_test_app_dir)

test_that("the multi-family demo is analysed as one selected family, never pooled", {
  x <- imported()
  expect_length(x$imp$data$G_list, 3L)
  expect_length(x$sel$data$G_list, 1L)
  expect_identical(nrow(x$sel$data$G_list[[1]]), 60L)
  pw <- hsms_run_pairwise(x$sel, d$pairwise, 1)
  expect_identical(pw$raw$parent_label, x$sel$family_internal)
  expect_length(pw$raw$fit$mom_phase_list, 1L)
  # a pooled call on the whole object would be a different (multi-dam) analysis
  pooled <- HalfSibMap::pairwise_rf(x$imp$data, threads = 1)
  expect_length(pooled$fit$mom_phase_list, 3L)
  expect_false(isTRUE(all.equal(unname(pooled$fit$r), unname(pw$raw$fit$r))))
  # MOM2 selection uses only MOM2 offspring
  s2 <- hsms_select_family(x$imp, "MOM2")
  expect_identical(nrow(s2$data$G_list[[1]]), 50L)
  expect_true(all(grepl("^P2_", hsms_to_original(x$imp$mapping, "sample", s2$offspring))))
})

test_that("uninformative maternal markers are excluded and reported", {
  x <- imported()
  imp <- x$imp
  fi <- x$sel$family_internal
  imp$data$M_list[[fi]][1:2] <- c(0L, NA)   # explicit edit of a COPY for this test
  sel <- hsms_select_family(imp, "MOM1")
  expect_length(sel$markers, 22L)
  expect_setequal(sel$excluded_markers_auto$original, c("m1", "m2"))
  expect_match(sel$excluded_markers_auto$reason[sel$excluded_markers_auto$original == "m1"], "homozygous")
})

test_that("the linkage-LOD cutoff reaches tpt_filter() below and above 5 (no hidden phase filter)", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1)
  for (lod in c(3, 6)) {
    s <- modifyList(d$filter, list(lod = lod))
    f <- hsms_run_filter(pw$raw, s)
    expect_equal(f$summary$lod_used, lod)
    ref <- HalfSibMap::tpt_filter(pw$raw, thresh.LOD.ph = lod, thresh.LOD.rf = lod, thresh.rf = s$thresh_rf,
                                  probs = c(s$prob_low, s$prob_high), diagnostic.plot = FALSE)
    expect_identical(f$kept, ref$markers)
  }
  # the upstream trap this adapter avoids: lowering only thresh.LOD.rf keeps an effective cutoff of 5
  trap <- HalfSibMap::tpt_filter(pw$raw, thresh.LOD.rf = 3, diagnostic.plot = FALSE)
  expect_equal(trap$filter_stats$thresholds$LOD_used, 5)
  expect_true(all(hsms_args_filter(modifyList(d$filter, list(lod = 3)))[c("thresh.LOD.ph", "thresh.LOD.rf")] == 3))
})

test_that("an impossible filter fails clearly and preview equals apply", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1)
  expect_error(hsms_run_filter(pw$raw, modifyList(d$filter, list(lod = 1000))), "No marker pair passes")
  s <- modifyList(d$filter, list(lod = 4))
  expect_identical(hsms_filter_preview(pw$raw, s)$n_after, hsms_run_filter(pw$raw, s)$summary$n_after)
})

test_that("grouping validates k, records the method used, and flags tiny groups", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  expect_error(hsms_run_group(f$raw, modifyList(d$group, list(k = 0))), "positive")
  expect_error(hsms_run_group(f$raw, modifyList(d$group, list(k = 999))), "only")
  g <- hsms_run_group(f$raw, modifyList(d$group, list(weight_by_lod = TRUE)))
  expect_identical(g$summary$method_used, "ward.D2"); expect_true(g$summary$method_switched)
  g20 <- hsms_run_group(f$raw, modifyList(d$group, list(k = 20)))
  expect_gt(g20$summary$n_singletons, 0L)
  single <- names(g20$summary$sizes)[g20$summary$sizes == 1][1]
  o <- hsms_run_order_mds(f$raw, hsms_group_members(g20$raw, single), d$order, 1)
  expect_false(o$mds_estimated); expect_match(o$method, "singleton")
  ph <- hsms_run_phase(f$raw, o$order, d$phase)
  expect_true(ph$singleton); expect_false(hsms_phase_routes(ph)$population)
})

test_that("MDS envelope removal is explicit and every marker is accounted for", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  g <- hsms_run_group(f$raw, d$group)
  s <- modifyList(d$order, list(drop_outside_envelope = TRUE, envelope_k = 1))
  o <- hsms_run_order_mds(f$raw, hsms_group_members(g$raw, 1), s, 1)
  expect_gt(length(o$removed), 0L)
  # when removal would leave too few markers, the package skips it and says so; the warning is kept
  o2 <- hsms_run_order_mds(f$raw, hsms_group_members(g$raw, 1), modifyList(s, list(envelope_k = 0.1)), 1)
  expect_length(o2$removed, 0L); expect_true(any(grepl("Skipping envelope filter", o2$warnings)))
  expect_setequal(c(o$order, o$removed), hsms_group_members(g$raw, 1))
  r <- hsms_reverse_order(o)
  expect_identical(r$order, rev(o$order)); expect_identical(r$reversed_from, o$order)
})

test_that("malformed supplied orders are rejected with a reconciliation report", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  g <- hsms_run_group(f$raw, modifyList(d$group, list(k = 2)))
  labs <- c(`1` = "LG1", `2` = "LG2")
  m1 <- hsms_to_original(x$imp$mapping, "marker", hsms_group_members(g$raw, 1))
  m2 <- hsms_to_original(x$imp$mapping, "marker", hsms_group_members(g$raw, 2))
  ok <- hsms_validate_supplied_order(data.frame(marker_id = rev(m1), linkage_group = "LG1", rank = seq_along(m1)),
                                     g$raw, x$imp$mapping, labs)
  expect_true(ok$ok); expect_identical(ok$groups[["1"]]$order, rev(hsms_group_members(g$raw, 1)))
  bad <- function(df) hsms_validate_supplied_order(df, g$raw, x$imp$mapping, labs)
  expect_false(bad(data.frame(marker_id = c(m1[1], m1[1]), linkage_group = "LG1", rank = 1:2))$ok)
  expect_false(bad(data.frame(marker_id = c(m1[1], "nope"), linkage_group = "LG1", rank = 1:2))$ok)
  expect_false(bad(data.frame(marker_id = c(m1[1], m2[1]), linkage_group = "LG1", rank = 1:2))$ok)
  expect_false(bad(data.frame(marker_id = m1[1:2], linkage_group = "LG1", rank = c("a", "b")))$ok)
  expect_false(bad(data.frame(marker_id = m1[1:2], linkage_group = "LG9", rank = 1:2))$ok)
  expect_false(bad(data.frame(marker = m1[1:2]))$ok)
  part <- bad(data.frame(marker_id = m1[1:2], linkage_group = "1", rank = 2:1))
  expect_true(part$ok); expect_length(part$groups[["1"]]$omitted, length(m1) - 2L)
})

test_that("direct, path-derived and unresolved phase intervals are distinguished and never filled", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  g <- hsms_run_group(f$raw, d$group); o <- hsms_run_order_mds(f$raw, hsms_group_members(g$raw, 1), d$order, 1)
  ph <- hsms_run_phase(f$raw, o$order, modifyList(d$phase, list(min_phase_lod = 4.5)))
  iv <- ph$summary$intervals
  expect_true(all(c("direct", "path", "unresolved") %in% iv$resolved_via))
  expect_true(all(iv$phase[iv$resolved_via == "unresolved"] == "unresolved"))
  expect_true(all(is.na(ph$raw$phase_vec[iv$resolved_via == "unresolved"])))
  expect_false(ph$summary$fully_resolved)
  rt <- hsms_phase_routes(ph)
  expect_false(rt$source_aware); expect_true(rt$population)
})

test_that("map routing: source-aware on resolved phase; blocked on unresolved; population blocks work", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  g <- hsms_run_group(f$raw, d$group); o <- hsms_run_order_mds(f$raw, hsms_group_members(g$raw, 1), d$order, 1)
  ph <- hsms_run_phase(f$raw, o$order, d$phase)
  sa <- hsms_run_source_aware(x$sel, ph, d$source_aware)
  expect_s3_class(sa$raw, "HSMap.sourceaware"); expect_true(sa$fit_table$converged)
  phu <- hsms_run_phase(f$raw, o$order, modifyList(d$phase, list(min_phase_lod = 4.5)))
  expect_error(hsms_run_source_aware(x$sel, phu, d$source_aware), "fully resolved")
  po <- hsms_run_population(x$sel, phu, d$population)
  expect_s3_class(po$raw, "HSMap.map.blocks")
  # blocks are contiguous runs between unresolved intervals (a component may be non-contiguous)
  expect_identical(nrow(po$fit_table), sum(is.na(phu$raw$phase_vec)) + 1L)
  expect_true(all(po$fit_table$status[po$fit_table$n_markers >= 2] == "fitted"))
  expect_true(all(po$fit_table$status[po$fit_table$n_markers < 2] == "not fitted (too few markers)"))
  s <- hsms_map_summary(po)
  expect_true(all(is.na(s$intervals$distance_cM[s$intervals$status != "linked"])))
  expect_true(isTRUE(hsms_crosscheck_block_map(po)))
})

test_that("posteriors use model-specific paths with verified shapes, labels and probabilities", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  g <- hsms_run_group(f$raw, d$group); o <- hsms_run_order_mds(f$raw, hsms_group_members(g$raw, 1), d$order, 1)
  ph <- hsms_run_phase(f$raw, o$order, d$phase)
  sa <- hsms_run_source_aware(x$sel, ph, d$source_aware)
  po <- hsms_run_population(x$sel, ph, d$population)
  a <- hsms_posterior_maternal(sa); b <- hsms_posterior_maternal(po)
  for (m in list(a, b)) {
    expect_identical(dim(m$prob_homolog1), c(60L, length(o$order)))
    expect_identical(colnames(m$prob_homolog1), o$order)
    expect_identical(rownames(m$prob_homolog1), x$sel$offspring)
    expect_true(all(m$prob_homolog1 >= 0 & m$prob_homolog1 <= 1))
  }
  # same orientation convention in both models (homolog 1 carries allele 1 at the first marker)
  expect_gt(stats::cor(as.vector(a$prob_homolog1), as.vector(b$prob_homolog1)), 0.95)
  arr <- po$posteriors[[1]]$gamma
  expect_true(max(abs(arr[1, , ] + arr[2, , ] - 1)) < 1e-8)
  expect_identical(dim(hsms_posterior_sharing(sa)), c(60L, length(o$order)))
  expect_null(hsms_posterior_sharing(po))
  sl <- hsms_posterior_slice(sa, x$imp$mapping, offspring = x$sel$offspring[1:3], markers = o$order[1:4])
  expect_identical(nrow(sl), 12L); expect_true(all(sl$offspring_id %in% c("P1_O1", "P1_O2", "P1_O3")))
})

test_that("an iteration-limited fit is reported as not converged and keeps its warning", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  g <- hsms_run_group(f$raw, d$group); o <- hsms_run_order_mds(f$raw, hsms_group_members(g$raw, 1), d$order, 1)
  ph <- hsms_run_phase(f$raw, o$order, d$phase)
  sa <- hsms_run_source_aware(x$sel, ph, modifyList(d$source_aware, list(maxit = 3)))
  expect_false(sa$fit_table$converged); expect_identical(sa$fit_table$iters, 3L)
  expect_true(any(grepl("did not converge", sa$warnings)))
  expect_true(hsms_payload_nonconverged(sa))
  s <- hsms_map_summary(sa)
  expect_false(s$fit_summary$converged); expect_match(s$fit_summary$warnings, "did not converge")
})

test_that("a failing block leaves other blocks' fits intact and marks the result partial", {
  x <- imported(); pw <- hsms_run_pairwise(x$sel, d$pairwise, 1); f <- hsms_run_filter(pw$raw, d$filter)
  g <- hsms_run_group(f$raw, d$group); o <- hsms_run_order_mds(f$raw, hsms_group_members(g$raw, 1), d$order, 1)
  phu <- hsms_run_phase(f$raw, o$order, modifyList(d$phase, list(min_phase_lod = 4.5)))
  ranges <- hsms_phase_blocks(length(phu$raw$order), phu$raw$phase_vec)
  victim <- phu$raw$order[ranges[[which(lengths(ranges) >= 2)[1]]]]
  real <- HalfSibMap::hmm_map_blocks
  testthat::local_mocked_bindings(
    hmm_map_blocks = function(x, phased, ...) {
      if (identical(phased$order, victim) || length(phased$order) == length(phu$raw$order)) stop("simulated block failure")
      real(x, phased, ...)
    }, .package = "HalfSibMap")
  po <- hsms_run_population(x$sel, phu, d$population)
  expect_true(po$partial)
  expect_identical(sum(po$fit_table$status == "failed"), 1L)
  expect_true(any(po$fit_table$status == "fitted"))
  expect_match(po$fit_table$error[po$fit_table$status == "failed"], "simulated block failure")
  s <- hsms_map_summary(po)
  expect_true(any(s$intervals$note == "block fit failed", na.rm = TRUE))
})
