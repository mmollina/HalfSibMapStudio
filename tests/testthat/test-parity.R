# Independent API parity: a reference analysis written with DIRECT HalfSibMap
# calls (no app adapters, state, or job layer) compared with the artifacts the
# app produced through its background workers. Deterministic computations on
# the same machine, settings and thread count, so tolerances are tight; the
# only symmetry allowed is whole-order reversal for MDS orders.

test_that("app artifacts match a direct HalfSibMap reference analysis", {
  skip_if_fast()
  dp <- demo_project(); p <- dp$proj; dir <- dp$dir
  imp <- hsms_load_current(p, dir, "import")
  o <- function(x) hsms_to_original(imp$mapping, "marker", x)
  # ---- reference: direct calls on family MOM1 only ----
  RcppParallel::setThreadOptions(numThreads = 1L)
  dat <- HalfSibMap::read_HSMap_data(demo_paths()$ped, demo_paths()$geno)
  x <- dat; x$G_list <- dat$G_list["MOM1"]; x$M_list <- dat$M_list["MOM1"]
  ref_tpt <- HalfSibMap::pairwise_rf(x, threads = 1)
  ref_flt <- HalfSibMap::tpt_filter(ref_tpt, thresh.LOD.ph = 5, thresh.LOD.rf = 5, thresh.rf = 0.15,
                                    probs = c(0.05, 1), diagnostic.plot = FALSE)
  ref_grp <- HalfSibMap::group_markers(ref_flt, k = 1, inter = FALSE)

  # two-point matrices (aligned by original marker name)
  app_tpt <- hsms_load_current(p, dir, "tpt_raw")$raw
  for (w in c("r", "lod_r", "lod_ph", "logLik")) {
    A <- app_tpt$fit[[w]]; dimnames(A) <- list(o(rownames(A)), o(colnames(A)))
    B <- ref_tpt$fit[[w]]
    expect_equal(A[rownames(B), colnames(B)], B, tolerance = 1e-10, info = w)
  }
  # filter and groups
  app_flt <- hsms_load_current(p, dir, "tpt_filtered")
  expect_setequal(o(app_flt$kept), ref_flt$markers)
  expect_equal(app_flt$summary$lod_used, ref_flt$filter_stats$thresholds$LOD_used)
  app_grp <- hsms_load_current(p, dir, "grouping")$raw
  expect_identical(as.integer(table(app_grp$groups)), as.integer(table(ref_grp$groups)))
  # MDS order: compared up to whole-order reversal
  app_ord <- hsms_load_current(p, dir, "order", "1")$order
  ref_ord <- HalfSibMap::mds_order(ref_grp, ref_flt, plot_each = FALSE)[[1]]
  expect_true(identical(o(app_ord), as.character(ref_ord)) || identical(o(app_ord), rev(as.character(ref_ord))))
  # Phase and source-aware fit on the SAME fixed order (isolates fit parity from MDS)
  fixed <- o(app_ord)
  ref_ph <- HalfSibMap::phase_from_pairwise(ref_flt, order = fixed, dam = 1)
  app_ph <- hsms_load_current(p, dir, "phase", "1")$raw
  expect_identical(app_ph$phase_vec, ref_ph$phase_vec)       # orientation-invariant adjacent phase
  expect_identical(app_ph$component, ref_ph$component)
  ref_sa <- HalfSibMap::hmm_map_source_aware(x, phased = ref_ph, dam = 1)
  app_sa <- hsms_load_current(p, dir, "map", "1")
  expect_equal(app_sa$raw$r, ref_sa$r, tolerance = 1e-10)
  expect_equal(app_sa$raw$loglik, ref_sa$loglik, tolerance = 1e-10)
  expect_equal(c(app_sa$raw$alpha, app_sa$raw$beta), c(ref_sa$alpha, ref_sa$beta), tolerance = 1e-10)
  expect_identical(app_sa$raw$iters, ref_sa$iters)
  # posterior alignment: app rows/cols are labelled; compare after mapping labels
  P <- hsms_posterior_maternal(app_sa)$prob_homolog1
  rownames(P) <- hsms_to_original(imp$mapping, "sample", rownames(P)); colnames(P) <- o(colnames(P))
  Rf <- ref_sa$gammaM; dimnames(Rf) <- list(rownames(x$G_list[[1]]), fixed)
  expect_equal(P[rownames(Rf), colnames(Rf)], Rf, tolerance = 1e-10)
  # gap-aware summary vs the package's own Haldane distances for the same fit
  s <- hsms_map_summary(app_sa, "haldane", 0.499)
  expect_equal(s$intervals$distance_cM, ref_sa$dist, tolerance = 1e-10)
})

test_that("population blockwise fits match direct hmm_map_blocks()/get_block_map() on an unresolved order", {
  skip_if_fast()
  dp <- demo_project(); p <- dp$proj; dir <- dp$dir
  imp <- hsms_load_current(p, dir, "import")
  sel <- hsms_load_current(p, dir, "selection")
  flt <- hsms_load_current(p, dir, "tpt_filtered")$raw
  ord <- hsms_load_current(p, dir, "order", "1")$order
  ph <- hsms_run_phase(flt, ord, modifyList(dp$defaults$phase, list(min_phase_lod = 4.5)))
  app <- hsms_run_population(sel, ph, dp$defaults$population)
  ref <- HalfSibMap::hmm_map_blocks(sel$data, ph$raw)          # direct call, package defaults
  ref_bm <- HalfSibMap::get_block_map(ref, "haldane")
  s <- hsms_map_summary(app, "haldane", 0.499)
  expect_identical(s$intervals$status, ref_bm$interval_table$status)
  expect_equal(s$intervals$r, ref_bm$interval_table$r, tolerance = 1e-10)
  expect_equal(s$intervals$distance_cM, ref_bm$interval_table$dist_cM, tolerance = 1e-10)
  expect_equal(s$linked_length, ref_bm$total_linked_length, tolerance = 1e-10)
  for (b in which(!vapply(app$block_fits, is.null, logical(1)))) {
    g_ref <- HalfSibMap::calc_haploprob(sel$data, ref$blocks[[b]]$fit)
    expect_equal(app$posteriors[[b]]$gamma, g_ref$gamma, tolerance = 1e-10)
  }
})
