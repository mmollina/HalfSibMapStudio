# One normalisation interface for maternal inheritance posteriors from either
# model. Output: matrix [offspring x markers] of P(maternal homolog 1), with
# internal IDs as dimnames, plus the phase block of each marker.
#
# Verified orientation (docs/UPSTREAM_API_CONTRACT.md):
#  * source-aware: gammaM is [n x T], rows in the order of
#    x$G_list[[dam]] rows, columns in phased order; it is the posterior
#    probability that homolog 1 (the homolog carrying allele "1" at the first
#    marker of the order) was transmitted.
#  * population: calc_haploprob()$gamma is [2 x T x n]; gamma[1, , ] is the
#    probability of hap0, which carries allele "1" at the first marker of the
#    block (hap0_is_A[1] == 1). Same convention within each block.
# Homolog labels are statistical labels with arbitrary orientation per resolved
# block; they are not comparable across blocks.

hsms_posterior_maternal <- function(fit, tol = 1e-6) {
  n <- length(fit$offspring); Tn <- length(fit$markers)
  if (identical(fit$model, "source_aware")) {
    P <- fit$raw$gammaM
    if (!identical(dim(P), c(n, Tn))) stop("Source-aware gammaM has unexpected dimensions.")
    dimnames(P) <- list(fit$offspring, fit$markers)
    block <- rep(1L, Tn)
  } else {
    P <- matrix(NA_real_, n, Tn, dimnames = list(fit$offspring, fit$markers))
    block <- integer(Tn)
    for (b in seq_along(fit$ranges)) {
      idx <- fit$ranges[[b]]; block[idx] <- b
      g <- fit$posteriors[[b]]
      if (is.null(g)) next
      arr <- g$gamma
      if (!identical(dim(arr), c(2L, length(idx), n))) stop("Block ", b, " posterior array has unexpected dimensions.")
      if (!identical(g$markers, fit$markers[idx])) stop("Block ", b, " posterior markers misaligned.")
      if (!identical(g$samples, fit$offspring)) stop("Block ", b, " posterior samples misaligned.")
      s <- arr[1, , ] + arr[2, , ]
      if (any(abs(s - 1) > tol, na.rm = TRUE)) stop("Block ", b, " homolog probabilities do not sum to one.")
      P[, idx] <- t(matrix(arr[1, , ], nrow = length(idx), ncol = n))
    }
  }
  if (any(P < -tol | P > 1 + tol, na.rm = TRUE)) stop("Posterior probabilities outside [0, 1].")
  list(prob_homolog1 = P, block = block, model = fit$model)
}

# Source-aware only: posterior occupancy of the paternal haplotype-sharing mode.
hsms_posterior_sharing <- function(fit) {
  if (!identical(fit$model, "source_aware")) return(NULL)
  H <- fit$raw$gammaH
  dimnames(H) <- list(fit$offspring, fit$markers)
  H
}

# Source-aware only: per-interval posterior probability of a maternal
# recombination (expected switch) between adjacent markers.
hsms_posterior_recombination <- function(fit) {
  if (!identical(fit$model, "source_aware")) return(NULL)
  X <- fit$raw$xi
  Tn <- length(fit$markers)
  dimnames(X) <- list(fit$offspring, paste(fit$markers[-Tn], fit$markers[-1], sep = "|"))
  X
}

# Long table for a selected slice (offspring subset, marker range), with
# original IDs. Never builds the full table unless asked.
hsms_posterior_slice <- function(fit, mapping, offspring = NULL, markers = NULL,
                                 what = c("maternal", "sharing")) {
  what <- match.arg(what)
  M <- if (what == "maternal") hsms_posterior_maternal(fit)$prob_homolog1 else hsms_posterior_sharing(fit)
  if (is.null(M)) return(NULL)
  blk <- hsms_posterior_maternal(fit)$block
  names(blk) <- fit$markers
  if (!is.null(offspring)) M <- M[intersect(offspring, rownames(M)), , drop = FALSE]
  if (!is.null(markers)) M <- M[, intersect(markers, colnames(M)), drop = FALSE]
  if (!length(M)) return(NULL)
  data.frame(
    offspring_id = hsms_to_original(mapping, "sample", rep(rownames(M), times = ncol(M))),
    marker_id = hsms_to_original(mapping, "marker", rep(colnames(M), each = nrow(M))),
    marker_rank = rep(match(colnames(M), fit$markers), each = nrow(M)),
    phase_block = rep(unname(blk[colnames(M)]), each = nrow(M)),
    value = as.vector(M),
    quantity = if (what == "maternal") "P(maternal homolog 1)" else "P(paternal sharing mode)",
    model = fit$model,
    stringsAsFactors = FALSE)
}
