# Map-fitting adapters: source-aware HMM (fully resolved phase only) and the
# population-model blockwise fit (resolved or unresolved phase). The model is
# an explicit choice recorded with every result; the app never substitutes one
# model for another after an error.

hsms_run_source_aware <- function(sel, phase_payload, settings) {
  ph <- phase_payload$raw
  if (is.null(ph)) stop("Source-aware fitting needs at least 2 ordered markers with an assembled phase.")
  if (!isTRUE(ph$fully_resolved) || anyNA(ph$phase_vec))
    stop("Source-aware fitting requires a fully resolved phase; this order has ",
         sum(is.na(ph$phase_vec)), " unresolved interval(s). Use the population model with phase blocks.")
  errs <- hsms_validate_settings("source_aware", settings)
  if (length(errs)) stop(paste(errs, collapse = " "))
  if (!all(ph$order %in% colnames(sel$data$G_list[[1]])))
    stop("The phased order contains markers that are not in the selected family's informative set.")
  args <- hsms_args_source_aware(settings)
  t0 <- proc.time()[["elapsed"]]
  cap <- hsms_capture(do.call(HalfSibMap::hmm_map_source_aware,
                              c(list(x = sel$data, phased = ph, dam = 1L), args)))
  sa <- cap$value
  if (!inherits(sa, "HSMap.sourceaware")) stop("hmm_map_source_aware() returned an unexpected class.")
  Tn <- length(ph$order); n <- nrow(sel$data$G_list[[1]])
  if (!identical(sa$markers, ph$order) || length(sa$r) != Tn - 1L)
    stop("Source-aware result is not aligned with the phased order.")
  if (!identical(dim(sa$gammaM), c(n, Tn)) || !identical(dim(sa$gammaH), c(n, Tn)))
    stop("Source-aware posterior matrices have unexpected dimensions.")
  list(model = "source_aware", raw = sa, warnings = cap$warnings,
       elapsed = proc.time()[["elapsed"]] - t0, args = args,
       offspring = rownames(sel$data$G_list[[1]]), markers = ph$order,
       phase_vec = ph$phase_vec,
       fit_table = data.frame(block = 1L, n_markers = Tn, n_offspring = n, status = "fitted",
                              converged = isTRUE(sa$converged), iters = sa$iters,
                              objective_type = "observed log-likelihood", objective = sa$loglik,
                              alpha = sa$alpha, beta = sa$beta,
                              sharing_occupancy = mean(sa$gammaH),
                              error = NA_character_, stringsAsFactors = FALSE))
}

# Contiguous marker-index ranges split at unresolved (NA) intervals -- the same
# rule hmm_map_blocks() applies (blocks_core.R, .blocks_from_phase).
hsms_phase_blocks <- function(n_markers, phase_vec) {
  if (n_markers <= 1L) return(list(seq_len(n_markers)))
  bnd <- which(is.na(phase_vec))
  Map(function(s, e) s:e, c(1L, bnd + 1L), c(bnd, n_markers))
}

hsms_run_population <- function(sel, phase_payload, settings) {
  ph <- phase_payload$raw
  if (is.null(ph)) stop("Population fitting needs at least 2 ordered markers with an assembled phase.")
  errs <- hsms_validate_settings("population", settings)
  if (length(errs)) stop(paste(errs, collapse = " "))
  args <- hsms_args_population(settings)
  fit_args <- args[c("epsilon", "lambda", "r_start", "tol", "maxit", "paternal_mode")]
  t0 <- proc.time()[["elapsed"]]
  x <- sel$data
  ranges <- hsms_phase_blocks(length(ph$order), ph$phase_vec)

  primary <- tryCatch(
    hsms_capture(do.call(HalfSibMap::hmm_map_blocks,
                         c(list(x = x, phased = ph, min_block_markers = args$min_block_markers,
                                gap_r = args$gap_r), fit_args))),
    error = function(e) e)

  block_fits <- vector("list", length(ranges))
  block_err <- rep(NA_character_, length(ranges))
  warnings <- character(0)
  if (!inherits(primary, "error")) {
    mode <- "single hmm_map_blocks() call"
    container <- primary$value
    if (!inherits(container, "HSMap.map.blocks")) stop("hmm_map_blocks() returned an unexpected class.")
    if (container$n_blocks != length(ranges)) stop("Block structure differs from the phase boundaries.")
    for (b in seq_along(ranges)) block_fits[b] <- list(container$blocks[[b]]$fit)
    warnings <- primary$warnings
    raw <- container
  } else {
    # Isolate blocks so one failing block cannot erase valid results for the
    # others. Each call is the public hmm_map_blocks() on one resolved block,
    # using the same minimal phased object (dam, order, phase_vec) that
    # hmm_map_blocks() constructs internally for each block.
    mode <- "per-block hmm_map_blocks() calls (a whole-order call failed)"
    warnings <- paste("Whole-order hmm_map_blocks() failed:", conditionMessage(primary))
    raw <- list(per_block = vector("list", length(ranges)), whole_order_error = conditionMessage(primary))
    for (b in seq_along(ranges)) {
      idx <- ranges[[b]]
      if (length(idx) < args$min_block_markers) next
      sub <- structure(list(dam = ph$dam, order = ph$order[idx],
                            phase_vec = as.integer(ph$phase_vec[idx[-length(idx)]])),
                       class = "HSMap.phased")
      r <- tryCatch(hsms_capture(do.call(HalfSibMap::hmm_map_blocks,
                                         c(list(x = x, phased = sub, min_block_markers = args$min_block_markers,
                                                gap_r = args$gap_r), fit_args))),
                    error = function(e) e)
      if (inherits(r, "error")) { block_err[b] <- conditionMessage(r); next }
      raw$per_block[b] <- list(r$value)
      block_fits[b] <- list(r$value$blocks[[1]]$fit)
      warnings <- c(warnings, sprintf("block %d: %s", b, r$warnings))
    }
    class(raw) <- "hsms_population_isolated"
  }

  # Per-block posteriors via calc_haploprob() on each fitted single-dam map.
  posteriors <- vector("list", length(ranges))
  for (b in seq_along(ranges)) {
    f <- block_fits[[b]]
    if (is.null(f)) next
    g <- tryCatch(HalfSibMap::calc_haploprob(x, f), error = function(e) e)
    if (inherits(g, "error")) {
      warnings <- c(warnings, sprintf("block %d: inheritance decoding failed: %s", b, conditionMessage(g)))
      next
    }
    posteriors[[b]] <- g
  }

  n <- nrow(x$G_list[[1]])
  ft <- do.call(rbind, lapply(seq_along(ranges), function(b) {
    f <- block_fits[[b]]; nb <- length(ranges[[b]])
    status <- if (!is.null(f)) "fitted" else if (!is.na(block_err[b])) "failed"
              else if (nb < args$min_block_markers) "not fitted (too few markers)" else "not fitted"
    data.frame(block = b, n_markers = nb, n_offspring = n, status = status,
               converged = if (!is.null(f)) isTRUE(f$fit$converged) else NA,
               iters = if (!is.null(f)) as.integer(f$fit$iters) else NA_integer_,
               objective_type = if (!is.null(f)) {
                 if (is.finite(f$fit$penalized_obj %||% NA_real_)) "penalized objective (lambda > 0)" else "observed log-likelihood"
               } else NA_character_,
               objective = if (!is.null(f)) as.numeric(f$fit$objective) else NA_real_,
               logLik = if (!is.null(f)) as.numeric(f$fit$logLik) else NA_real_,
               conv_reason = if (!is.null(f)) as.character(f$fit$conv_reason %||% NA) else NA_character_,
               error = block_err[b], stringsAsFactors = FALSE)
  }))

  list(model = "population", mode = mode, raw = raw, block_fits = block_fits,
       posteriors = posteriors, ranges = ranges, warnings = unique(warnings),
       elapsed = proc.time()[["elapsed"]] - t0, args = args,
       offspring = rownames(x$G_list[[1]]), markers = ph$order, phase_vec = ph$phase_vec,
       fit_table = ft,
       partial = any(ft$status == "failed"))
}

# Per-interval r and phase-block assignment from either model payload.
hsms_map_intervals_raw <- function(fit) {
  Tn <- length(fit$markers)
  if (identical(fit$model, "source_aware")) {
    return(list(r = as.numeric(fit$raw$r), boundary = rep(FALSE, max(Tn - 1L, 0L)),
                block = rep(1L, Tn)))
  }
  r <- rep(NA_real_, max(Tn - 1L, 0L)); block <- integer(Tn)
  for (b in seq_along(fit$ranges)) {
    idx <- fit$ranges[[b]]; block[idx] <- b
    f <- fit$block_fits[[b]]
    if (!is.null(f) && length(idx) > 1L) {
      rv <- as.numeric(f$fit$r)
      if (length(rv) != length(idx) - 1L) stop("Block ", b, " r vector misaligned.")
      r[idx[-length(idx)]] <- rv
    }
  }
  list(r = r, boundary = is.na(fit$phase_vec), block = block)
}
