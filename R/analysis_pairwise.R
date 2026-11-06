# Two-point analysis and filtering adapters around HalfSibMap::pairwise_rf()
# and HalfSibMap::tpt_filter().

# Conservative memory estimate for pairwise_rf() output, in bytes.
# Verified against the real return structure (docs/UPSTREAM_API_CONTRACT.md):
# for one family the fit holds 7 T x T matrices (r, lod_r, lod_ph, logLik,
# no_linkage, mom_phase_list[[1]], lod_ph_list[[1]]), each stored as 8-byte
# doubles. Diagnostics add further T x T matrices. The worker then serialises
# the result to disk and the app reads it back, so up to ~3 copies can be live.
hsms_pairwise_memory_estimate <- function(n_markers, return_diagnostics = FALSE) {
  T <- as.numeric(n_markers)
  n_mats <- 7 + if (isTRUE(return_diagnostics)) 8 else 0
  one_copy <- n_mats * 8 * T^2
  list(n_markers = n_markers, n_pairs = T * (T - 1) / 2,
       output_bytes = one_copy, peak_bytes_estimate = 3 * one_copy,
       note = "Estimate from the documented return structure; not a measured peak.")
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
