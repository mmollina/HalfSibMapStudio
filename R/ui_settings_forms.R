# Field metadata for stage settings forms: labels, help, and which fields are
# shown by default ("common") versus under Advanced. Defaults come from
# config/defaults.json, which mirrors the pinned HalfSibMap defaults.

hsms_fields <- function() {
  f <- function(label, help, step = NA, type = "num", choices = NULL, common = FALSE)
    list(label = label, help = help, step = step, type = type, choices = choices, common = common)
  list(
    pairwise = list(
      lambda = f("Regularization lambda (two-point)", "Total pseudocount shrinking each per-marker paternal gametic frequency toward q0 (pairwise_rf lambda). Package default 20 is kept for backward compatibility; it was not chosen by a formal sensitivity study. Not shared with other stages.", 1),
      q0 = f("Prior target q0", "Value the paternal gametic frequency is shrunk toward (pairwise_rf q0). Default 0.5.", 0.05),
      tol = f("Optimizer tolerance", "Tolerance of the bounded local refinement of r (pairwise_rf tol).", 1e-6),
      maxit = f("Iteration cap", "Maximum iterations of the local refinement (pairwise_rf maxit).", 1),
      tiny = f("Log floor", "Numerical floor that avoids log(0) (pairwise_rf tiny).", 1e-12),
      return_diagnostics = f("Return optimizer diagnostics", "Adds per-pair diagnostic matrices to the result (larger output).", type = "lgl")),
    filter = list(
      lod = f("Linkage LOD cutoff", "Keep pairs whose linkage LOD (vs r = 0.5) is at least this value. Passed to tpt_filter() as both thresh.LOD.rf and thresh.LOD.ph, because the package uses max() of the two as its single linkage cutoff; thresh.LOD.ph is not a separate phase filter.", 0.5, common = TRUE),
      thresh_rf = f("Maximum pairwise r", "Keep pairs with recombination fraction at most this value.", 0.01, common = TRUE),
      prob_low = f("Lower row-information quantile", "Markers whose number of surviving informative pairs is below this quantile are removed. This is about surviving pair counts, not genotype missingness or a p-value.", 0.01, common = TRUE),
      prob_high = f("Upper row-information quantile", "Markers above this quantile of surviving pair counts are removed (1 keeps all).", 0.01, common = TRUE),
      diag_markers = f("Diagonal band half-width (markers)", "Keep only pairs within this many positions of each other in the matrix order. Only meaningful relative to an explicit marker order; leave empty to disable.", 1)),
    group = list(
      k = f("Number of linkage groups (k)", "Explicit number of groups to cut the dendrogram into. Choose it from biology (e.g. chromosome number) and the dendrogram.", 1, common = TRUE),
      method = f("Clustering method", "Linkage method for stats::hclust on the symmetrised r matrix.", type = "chr",
                 choices = c("average", "complete", "single", "ward.D2", "mcquitty", "median", "centroid")),
      na_r = f("Fill value for missing r", "Value used for missing r before clustering (0.5 = unlinked).", 0.05),
      weight_by_lod = f("Weight markers by LOD", "Uses mean squared LOD as hclust member weights. The package then switches an 'average' request to 'ward.D2'; the method actually used is recorded.", type = "lgl")),
    order = list(
      itmax = f("SMACOF iteration limit", "Maximum iterations of the MDS fit (mds_order itmax).", 1000),
      drop_outside_envelope = f("Remove markers outside the principal-curve envelope", "If enabled, markers far from the fitted curve are removed from the order and reported as removed. Off by default.", type = "lgl"),
      envelope_k = f("Envelope width multiplier", "Multiplier of the local residual width (mds_order envelope_k).", 0.1),
      envelope_span = f("Envelope smoothing span", "Loess span for the residual width (mds_order envelope_span).", 0.05),
      min_keep = f("Minimum markers kept", "Envelope removal is skipped if it would leave fewer markers (mds_order min_keep).", 1)),
    phase = list(
      min_phase_lod = f("Minimum phase LOD", "An edge counts as phase evidence only when its profiled phase LOD exceeds this value. For one family this equals the linkage LOD of the pair. The package default 0 is a backward-compatibility choice, not a validated threshold; higher values leave more intervals unresolved.", 0.5, common = TRUE),
      tie_tol = f("Tie tolerance", "LODs at or below this value are treated as unresolved ties.", 1e-8),
      max_passes = f("Greedy passes", "Maximum greedy improvement passes per component.", 1),
      tol = f("Improvement tolerance", "Minimum objective improvement for a flip.", 1e-9)),
    source_aware = list(
      epsilon = f("Genotyping error rate", "Fixed hard-call error rate in the emission model (not estimated).", 0.005, common = TRUE),
      tol = f("Convergence tolerance", "EM convergence tolerance.", 1e-6, common = TRUE),
      maxit = f("Iteration cap", "Maximum EM iterations. A fit that reaches the cap is reported as not converged.", 100, common = TRUE),
      lambda = f("Regularization lambda (source-aware pre-fit)", "Pseudocount for paternal frequencies in the population-mode pre-fit used by the source-aware model. Package default 2 (different from the other stages).", 1),
      r_start = f("Starting r", "Starting recombination fraction for every interval.", 0.01),
      alpha_start = f("Starting alpha (enter sharing mode)", "Starting transition probability into the haplotype-sharing mode, per marker step.", 0.001),
      beta_start = f("Starting beta (leave sharing mode)", "Starting transition probability out of the sharing mode, per marker step.", 0.01),
      fit_mode = f("Estimate alpha and beta", "If off, alpha and beta stay at their starting values.", type = "lgl"),
      nmin = f("Kernel support threshold", "Minimum supporting pairs before the population LD kernel departs from independence (nmin).", 1),
      smooth = f("Kernel smoothing", "Cell smoothing for the population kernel.", 0.1)),
    population = list(
      epsilon = f("Genotyping error rate", "Fixed global hard-call error rate (not estimated).", 0.005, common = TRUE),
      tol = f("Convergence tolerance", "EM convergence tolerance.", 1e-6, common = TRUE),
      maxit = f("Iteration cap", "Maximum EM iterations per block.", 100, common = TRUE),
      lambda = f("Regularization lambda (population model)", "Total pseudocount of the paternal gametic-frequency prior. Package default 20 is historical, not a validated recommendation.", 1),
      r_start = f("Starting r", "Starting recombination fraction.", 0.01),
      min_block_markers = f("Minimum markers per block", "Blocks with fewer markers are kept as metadata but not fitted.", 1),
      gap_r = f("No-linkage threshold (block table)", "Fitted r at or above this value is a no-linkage boundary in the package's block table.", 0.001)))
}

hsms_settings_form <- function(ns, stage, values, prefix = stage) {
  fl <- hsms_fields()[[stage]]
  mk <- function(nm) {
    fd <- fl[[nm]]; id <- ns(paste0(prefix, "__", nm)); v <- values[[nm]]
    if (fd$type == "lgl") return(shiny::checkboxInput(id, hsms_label(fd$label, fd$help), isTRUE(v)))
    if (fd$type == "chr") return(shiny::selectInput(id, hsms_label(fd$label, fd$help), fd$choices, selected = v))
    hsms_num(id, fd$label, if (is.null(v)) NA else v, fd$help, step = fd$step)
  }
  common <- names(fl)[vapply(fl, function(x) isTRUE(x$common), logical(1))]
  adv <- setdiff(names(fl), common)
  shiny::tagList(
    lapply(common, mk),
    if (length(adv)) bslib::accordion(open = FALSE, bslib::accordion_panel("Advanced settings", lapply(adv, mk))))
}

hsms_read_form <- function(input, stage, prefix = stage) {
  fl <- hsms_fields()[[stage]]
  out <- lapply(names(fl), function(nm) {
    v <- input[[paste0(prefix, "__", nm)]]
    if (fl[[nm]]$type == "num" && (is.null(v) || is.na(v))) return(NULL)
    v
  })
  names(out) <- names(fl)
  out
}

hsms_update_form <- function(session, stage, values, prefix = stage) {
  fl <- hsms_fields()[[stage]]
  for (nm in names(fl)) {
    id <- paste0(prefix, "__", nm); v <- values[[nm]]
    if (fl[[nm]]$type == "lgl") shiny::updateCheckboxInput(session, id, value = isTRUE(v))
    else if (fl[[nm]]$type == "chr") shiny::updateSelectInput(session, id, selected = v)
    else shiny::updateNumericInput(session, id, value = if (is.null(v)) NA else v)
  }
}

# Compare a form draft with applied values (ignoring unset optional fields).
hsms_form_differs <- function(draft, applied, stage) {
  if (is.null(applied)) return(FALSE)
  a <- hsms_clean_settings(stage, applied); d <- hsms_clean_settings(stage, draft)
  !identical(hsms_signature(a), hsms_signature(d))
}
