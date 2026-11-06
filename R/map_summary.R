# App-owned, gap-aware map summary adapter with a stable schema. It applies the
# same interval classification as HalfSibMap::get_block_map() and is checked
# against it for population fits (tests/testthat/test-map-summary.R).
#
# Interval status values:
#   linked                    fitted r < gap_r
#   no_linkage_boundary       fitted r >= gap_r (classified BEFORE any distance
#                             transformation; distance NA)
#   unresolved_phase          relative phase unresolved; not estimated
#   insufficient_information  inside a block that was not fitted (too few
#                             markers, or the block fit failed)
# Positions restart at 0 at the start of every segment; a new segment starts
# after any non-linked interval.

hsms_map_function <- function(name) {
  switch(name, haldane = HalfSibMap::inv_haldane, kosambi = HalfSibMap::inv_kosambi,
         morgan = HalfSibMap::inv_morgan, stop("Unknown map function: ", name))
}

hsms_summarize_map <- function(markers, r, boundary, block, map_function = "haldane",
                               gap_r = 0.499, block_failed = integer(0)) {
  Tn <- length(markers); Ti <- max(Tn - 1L, 0L)
  if (length(r) != Ti || length(boundary) != Ti || length(block) != Tn)
    stop("Map summary inputs are misaligned.")
  mf <- hsms_map_function(map_function)
  status <- character(Ti)
  status[boundary] <- "unresolved_phase"
  nb <- !boundary
  status[nb & !is.finite(r)] <- "insufficient_information"
  status[nb & is.finite(r) & r >= gap_r] <- "no_linkage_boundary"
  status[nb & is.finite(r) & r < gap_r] <- "linked"
  dist <- rep(NA_real_, Ti)
  lk <- status == "linked"
  if (any(lk)) dist[lk] <- mf(r[lk])
  segment <- integer(Tn); pos <- numeric(Tn)
  if (Tn) { segment[1] <- 1L; pos[1] <- 0 }
  for (t in seq_len(Ti)) {
    if (lk[t]) { segment[t + 1L] <- segment[t]; pos[t + 1L] <- pos[t] + dist[t] }
    else { segment[t + 1L] <- segment[t] + 1L; pos[t + 1L] <- 0 }
  }
  # A marker that is alone in its segment has no estimated position relative
  # to anything: keep it, flag it, and do not imply a distance.
  seg_size <- tabulate(segment, nbins = max(segment, 0L))
  positions <- data.frame(rank = seq_len(Tn), marker = markers, phase_block = as.integer(block),
                          segment_id = segment, position_cM = pos,
                          singleton_segment = seg_size[segment] == 1L,
                          stringsAsFactors = FALSE)
  positions$position_cM[positions$singleton_segment] <- NA_real_
  intervals <- data.frame(interval = seq_len(Ti), from_marker = markers[-Tn], to_marker = markers[-1],
                          phase_block = if (Ti) ifelse(boundary, NA_integer_, block[seq_len(Ti)]) else integer(0),
                          segment_id = if (Ti) ifelse(lk, segment[seq_len(Ti)], NA_integer_) else integer(0),
                          r = r, distance_cM = dist, status = status, stringsAsFactors = FALSE)
  intervals$note <- NA_character_
  if (length(block_failed) && Ti)
    intervals$note[intervals$status == "insufficient_information" &
                   intervals$phase_block %in% block_failed] <- "block fit failed"
  seg_len <- if (any(lk)) tapply(dist[lk], segment[seq_len(Ti)][lk], sum) else numeric(0)
  list(positions = positions, intervals = intervals,
       linked_length = sum(dist, na.rm = TRUE),
       n_gaps = sum(!lk), n_segments = if (Tn) max(segment) else 0L,
       segment_lengths = seg_len, map_function = map_function, gap_r = gap_r)
}

# Summary for a map payload (either model), with original IDs and context.
hsms_map_summary <- function(fit, map_function = "haldane", gap_r = 0.499, ctx = list()) {
  if (identical(fit$model, "source_aware") && !identical(map_function, "haldane")) {
    # Allowed as a display conversion of the fitted r; the model is not refitted.
    conv_note <- sprintf("%s distances are a display conversion of the fitted r (the model is not refitted).",
                         map_function)
  } else conv_note <- NULL
  iv <- hsms_map_intervals_raw(fit)
  failed <- if (!is.null(fit$fit_table)) fit$fit_table$block[fit$fit_table$status == "failed"] else integer(0)
  s <- hsms_summarize_map(fit$markers, iv$r, iv$boundary, iv$block, map_function, gap_r, failed)
  map <- ctx$mapping
  orig <- function(x) if (is.null(map)) x else hsms_to_original(map, "marker", x)
  s$positions <- cbind(
    data.frame(family_id = ctx$family %||% NA_character_, mother_id = ctx$mother %||% NA_character_,
               linkage_group = ctx$group_label %||% NA_character_, stringsAsFactors = FALSE),
    transform(s$positions, marker = orig(marker)),
    data.frame(model = fit$model, map_function = map_function, run_id = ctx$run_id %||% NA_character_,
               stringsAsFactors = FALSE))
  names(s$positions)[names(s$positions) == "marker"] <- "marker_id"
  s$intervals <- cbind(
    data.frame(linkage_group = ctx$group_label %||% NA_character_, stringsAsFactors = FALSE),
    transform(s$intervals, from_marker = orig(from_marker), to_marker = orig(to_marker)),
    data.frame(model = fit$model, map_function = map_function, run_id = ctx$run_id %||% NA_character_,
               stringsAsFactors = FALSE))
  s$note <- conv_note
  s$fit_summary <- hsms_fit_summary(fit, s, ctx)
  s
}

hsms_fit_summary <- function(fit, s, ctx) {
  ft <- fit$fit_table
  data.frame(
    linkage_group = ctx$group_label %||% NA_character_,
    model = fit$model,
    block = ft$block,
    n_offspring = ft$n_offspring,
    n_markers = ft$n_markers,
    status = ft$status,
    converged = ft$converged,
    iterations = ft$iters,
    objective_type = ft$objective_type,
    objective_value = ft$objective,
    linked_length_cM_group = s$linked_length,
    n_gaps_group = s$n_gaps,
    map_function = s$map_function,
    alpha = if ("alpha" %in% names(ft)) ft$alpha else NA_real_,
    beta = if ("beta" %in% names(ft)) ft$beta else NA_real_,
    sharing_occupancy = if ("sharing_occupancy" %in% names(ft)) ft$sharing_occupancy else NA_real_,
    warnings = paste(fit$warnings, collapse = " | "),
    error = ft$error,
    elapsed_sec = fit$elapsed,
    run_id = ctx$run_id %||% NA_character_,
    stringsAsFactors = FALSE)
}

# Cross-check against HalfSibMap::get_block_map() for a native blockwise
# container. Returns TRUE or a character description of the disagreement.
hsms_crosscheck_block_map <- function(fit, map_function = "haldane", gap_r = 0.499) {
  if (!inherits(fit$raw, "HSMap.map.blocks")) return(TRUE)
  ref <- HalfSibMap::get_block_map(fit$raw, map.function = map_function, gap_r = gap_r)
  iv <- hsms_map_intervals_raw(fit)
  s <- hsms_summarize_map(fit$markers, iv$r, iv$boundary, iv$block, map_function, gap_r)
  if (!identical(ref$interval_table$status, s$intervals$status)) return("interval status differs")
  if (!isTRUE(all.equal(ref$interval_table$dist_cM, s$intervals$distance_cM))) return("distances differ")
  if (!identical(as.integer(ref$positions$segment), s$positions$segment_id)) return("segments differ")
  keep <- !s$positions$singleton_segment
  if (!isTRUE(all.equal(ref$positions$pos[keep], s$positions$position_cM[keep]))) return("positions differ")
  if (!isTRUE(all.equal(ref$total_linked_length, s$linked_length))) return("linked length differs")
  TRUE
}
