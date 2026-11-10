# Two-point analysis and filtering adapters around HalfSibMap::pairwise_rf()
# and HalfSibMap::tpt_filter().

# Memory model for the two-point stage, calibrated from MEASURED peak resident
# memory (VmHWM) of fresh worker-like R processes (R 4.3.3, Linux x86_64,
# 2026-11-09; see docs/VALIDATION.md). Memory grows with T^2 (T = markers);
# offspring count and thread count had no material effect. Coefficients are
# bytes per T^2 and are rounded up, so the figures are slightly pessimistic:
# against the app's real job workers (peak read from /proc while they ran) the
# estimates were 6-22% above the measured peak for T = 500 to 3000, for both
# the two-point and the filter step, with and without diagnostics
# (tests/testthat/test-resources.R repeats this check on Linux).
#   result object : 5 double + 2 integer T x T matrices = 48 bytes/T^2 (101 with diagnostics)
#   two-point worker peak above R start-up : measured 75-81 (136 with diagnostics)
#   filter worker peak (reloads and subsets the full result) : measured 109-132
#   app session displaying the result : measured 83-103
#   result file (compressed RDS) : measured 37
HSMS_MEM_MODEL <- list(
  r_startup_mib = 380,          # an R worker with HalfSibMap and dependencies loaded (measured 375)
  fixed_mib = 20,               # small-T overhead
  object = c(plain = 48, diag = 101),
  twopoint = c(plain = 82, diag = 140),
  filter = c(plain = 135, diag = 190),
  app = c(plain = 105, diag = 160),
  file = c(plain = 40, diag = 85))

hsms_available_memory <- function() {
  m <- tryCatch(ps::ps_system_memory(), error = function(e) NULL)
  if (is.null(m) || is.null(m$avail) || !is.finite(m$avail)) return(NA_real_)
  as.numeric(m$avail)
}

hsms_pairwise_memory_estimate <- function(n_markers, return_diagnostics = FALSE, n_offspring = 0) {
  T <- as.numeric(n_markers); mm <- HSMS_MEM_MODEL
  k <- if (isTRUE(return_diagnostics)) "diag" else "plain"
  fixed <- mm$fixed_mib * 2^20 + 16 * as.numeric(n_offspring) * T
  startup <- mm$r_startup_mib * 2^20
  list(n_markers = n_markers, n_pairs = T * (T - 1) / 2,
       object_bytes = mm$object[[k]] * T^2,
       worker_peak_bytes = startup + fixed + mm$twopoint[[k]] * T^2,
       filter_peak_bytes = startup + fixed + mm$filter[[k]] * T^2,
       app_bytes = fixed + mm$app[[k]] * T^2,
       file_bytes = mm$file[[k]] * T^2,
       available_bytes = hsms_available_memory(),
       # kept for callers that use the older field names
       output_bytes = mm$object[[k]] * T^2,
       peak_bytes_estimate = startup + fixed + mm$filter[[k]] * T^2,
       note = paste("Calibrated from measured peak memory (R 4.3, Linux); slightly pessimistic.",
                    "Thread count does not change memory use."))
}

# Memory warning: the larger worker (filter step) plus what the app session
# holds must fit comfortably in available memory and within the configured cap.
hsms_pairwise_memory_warning <- function(est, cap_gb = Inf) {
  need <- max(est$worker_peak_bytes, est$filter_peak_bytes) + est$app_bytes
  over_cap <- is.finite(cap_gb) && need > cap_gb * 1024^3
  over_avail <- is.finite(est$available_bytes) && need > 0.8 * est$available_bytes
  list(warn = over_cap || over_avail, need_bytes = need, over_cap = over_cap, over_avail = over_avail)
}

hsms_run_pairwise <- function(sel, settings, threads) {
  args <- hsms_args_pairwise(settings, threads)
  x <- sel$data
  t0 <- proc.time()[["elapsed"]]
  cap <- hsms_capture(do.call(HalfSibMap::pairwise_rf,
                              c(list(x = x, snps = sel$markers, presence_threshold = 1), args)))
  tpt <- cap$value
  if (!inherits(tpt, "HSMap.tpt")) stop("pairwise_rf() did not return HSMap.tpt.")
  if (!identical(tpt$parent_label, names(x$G_list)))
    stop("Two-point result is not labelled with the selected family.")
  if (length(tpt$fit$mom_phase_list) != 1L) stop("Two-point result contains more than one dam.")
  list(raw = tpt, warnings = cap$warnings, messages = cap$messages,
       elapsed = proc.time()[["elapsed"]] - t0, args = args,
       summary = hsms_pairwise_summary(tpt))
}

hsms_pairwise_summary <- function(tpt) {
  R <- tpt$fit$r; L <- tpt$fit$lod_r; P <- tpt$fit$lod_ph
  ut <- upper.tri(R)
  r <- R[ut]
  list(n_markers = length(tpt$markers), n_pairs = sum(ut),
       n_pairs_na = sum(is.na(r)), n_pairs_r_half = sum(!is.na(r) & r >= 0.5),
       r_quantiles = stats::quantile(r, c(0, .25, .5, .75, 1), na.rm = TRUE),
       lod_max = suppressWarnings(max(L[ut], na.rm = TRUE)),
       threads = tpt$threads, time_sec = tpt$time_sec)
}

# Upper-triangle values for distribution plots, optionally subsampled
# deterministically (every k-th pair) when very large.
hsms_pairwise_distribution <- function(tpt, max_pairs = 2e5) {
  ut <- which(upper.tri(tpt$fit$r))
  aggregated <- FALSE
  if (length(ut) > max_pairs) {
    ut <- ut[unique(round(seq(1, length(ut), length.out = max_pairs)))]
    aggregated <- TRUE
  }
  df <- data.frame(r = tpt$fit$r[ut], lod_r = tpt$fit$lod_r[ut], lod_ph = tpt$fit$lod_ph[ut])
  attr(df, "subsampled") <- aggregated
  df
}

# Matrix slice for display. `markers` are internal IDs in display order.
hsms_matrix_slice <- function(tpt, markers, what = c("r", "lod_r", "lod_ph")) {
  what <- match.arg(what)
  M <- tpt$fit[[what]]
  markers <- markers[markers %in% rownames(M)]
  M[markers, markers, drop = FALSE]
}

# Deterministic block-mean aggregation of a square matrix to at most
# `max_cells` x `max_cells` (missing values excluded from each mean; a block
# with no finite values stays NA so missing data is never shown as linkage).
hsms_aggregate_square <- function(M, max_cells = 250) {
  n <- nrow(M)
  if (n <= max_cells) return(list(matrix = M, aggregated = FALSE, block = 1L))
  b <- ceiling(n / max_cells)
  grp <- (seq_len(n) - 1L) %/% b + 1L
  ng <- max(grp)
  out <- matrix(NA_real_, ng, ng)
  for (i in seq_len(ng)) for (j in seq_len(ng)) {
    v <- M[grp == i, grp == j]
    v <- v[is.finite(v)]
    if (length(v)) out[i, j] <- mean(v)
  }
  labs <- vapply(split(rownames(M), grp), function(z) paste0(z[1], "..", z[length(z)]), "")
  dimnames(out) <- list(labs, labs)
  list(matrix = out, aggregated = TRUE, block = b)
}

# Long pair table for an explicitly selected subset of markers.
hsms_pair_table <- function(tpt, markers, map) {
  markers <- markers[markers %in% tpt$markers]
  if (length(markers) < 2L) return(NULL)
  cmb <- utils::combn(markers, 2)
  ph <- tpt$fit$mom_phase_list[[1]]
  data.frame(
    marker_1 = hsms_to_original(map, "marker", cmb[1, ]),
    marker_2 = hsms_to_original(map, "marker", cmb[2, ]),
    r = tpt$fit$r[cbind(cmb[1, ], cmb[2, ])],
    lod_linkage = tpt$fit$lod_r[cbind(cmb[1, ], cmb[2, ])],
    lod_phase = tpt$fit$lod_ph[cbind(cmb[1, ], cmb[2, ])],
    phase = {
      p <- ph[cbind(cmb[1, ], cmb[2, ])]
      ifelse(is.na(p), NA_character_, ifelse(p >= 0.5, "coupling", "repulsion"))
    },
    stringsAsFactors = FALSE)
}

# ---- Filtering --------------------------------------------------------------

hsms_run_filter <- function(tpt_raw, settings) {
  args <- hsms_args_filter(settings)
  rc <- hsms_filter_row_counts(tpt_raw, settings)
  if (!length(rc) || max(rc) == 0)
    stop(sprintf(paste("No marker pair passes the screen (linkage LOD >= %s and r <= %s). In this case",
                       "tpt_filter() keeps every marker, because all markers tie at zero surviving pairs;",
                       "the app does not record that as a filtered result. Lower the LOD cutoff or raise the maximum r."),
                 format(settings$lod), format(settings$thresh_rf)), call. = FALSE)
  res <- tryCatch(hsms_capture(do.call(HalfSibMap::tpt_filter, c(list(x = tpt_raw), args))),
                  error = function(e) e)
  if (inherits(res, "error")) {
    msg <- conditionMessage(res)
    if (grepl("No markers remain", msg)) msg <- paste0(msg, " Lower the linkage-LOD cutoff or raise the maximum RF.")
    stop(msg, call. = FALSE)
  }
  tptf <- res$value
  fs <- tptf$filter_stats
  if (!isTRUE(all.equal(fs$thresholds$LOD_used, settings$lod)))
    stop("Effective linkage-LOD cutoff (", fs$thresholds$LOD_used, ") differs from the requested ",
         settings$lod, "; refusing to record a misleading filter.")
  kept <- tptf$markers
  excluded <- setdiff(tpt_raw$markers, kept)
  list(raw = tptf, warnings = res$warnings, args = args,
       summary = list(n_before = length(tpt_raw$markers), n_after = length(kept),
                      n_excluded = length(excluded), lod_used = fs$thresholds$LOD_used,
                      rf = fs$thresholds$rf, probs = fs$thresholds$probs,
                      diag_markers = fs$thresholds$diag.markers),
       kept = kept, excluded = excluded,
       row_counts = hsms_filter_row_counts(tpt_raw, settings))
}

# Display-only: number of surviving informative pairs per marker under the
# same mask tpt_filter() applies (used for the histogram; the kept set always
# comes from tpt_filter() itself).
hsms_filter_row_counts <- function(tpt_raw, settings) {
  R <- tpt_raw$fit$r; L <- tpt_raw$fit$lod_r
  ok <- !is.na(R) & !is.na(L) & (L >= settings$lod) & (R <= settings$thresh_rf)
  if (!is.null(settings$diag_markers) && !is.na(settings$diag_markers))
    ok <- ok & abs(row(ok) - col(ok)) <= as.integer(settings$diag_markers)
  stats::setNames(rowSums(ok, na.rm = TRUE), rownames(R))
}

# Preview == apply: calls exactly the same adapter.
hsms_filter_preview <- function(tpt_raw, settings) {
  res <- tryCatch(hsms_run_filter(tpt_raw, settings), error = function(e) e)
  if (inherits(res, "error"))
    return(list(ok = FALSE, message = conditionMessage(res), n_before = length(tpt_raw$markers),
                n_after = 0L, row_counts = hsms_filter_row_counts(tpt_raw, settings)))
  list(ok = TRUE, n_before = res$summary$n_before, n_after = res$summary$n_after,
       excluded = res$excluded, row_counts = res$row_counts)
}
