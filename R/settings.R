# Stage settings: defaults, validation, and translation into HalfSibMap
# arguments. Pure R.

hsms_app_dir <- function() getOption("hsms.app_dir", normalizePath(".", winslash = "/"))

hsms_defaults <- function(app_dir = hsms_app_dir()) {
  path <- file.path(app_dir, "config", "defaults.json")
  if (!file.exists(path)) stop("Missing config/defaults.json in ", app_dir)
  d <- jsonlite::read_json(path, simplifyVector = TRUE)
  d[["_comment"]] <- NULL
  d
}

# Specification of every user-editable analytical setting. `range` bounds are
# the domains the upstream functions accept (checked in their source), not
# scientific recommendations.
hsms_setting_specs <- function() {
  num <- function(lo, hi, lo_incl = TRUE, hi_incl = TRUE, int = FALSE)
    list(type = if (int) "int" else "num", lo = lo, hi = hi,
         lo_incl = lo_incl, hi_incl = hi_incl)
  lgl <- function() list(type = "lgl")
  chr <- function(choices) list(type = "chr", choices = choices)
  list(
    pairwise = list(
      lambda = num(0, Inf), q0 = num(0, 1), tol = num(0, Inf, lo_incl = FALSE),
      maxit = num(1, 1e6, int = TRUE), tiny = num(0, 1e-3, lo_incl = FALSE),
      return_diagnostics = lgl()),
    filter = list(
      lod = num(0, Inf), thresh_rf = num(0, 0.5), prob_low = num(0, 1),
      prob_high = num(0, 1), diag_markers = c(num(0, Inf, int = TRUE), list(nullable = TRUE))),
    group = list(
      k = num(1, Inf, int = TRUE),
      method = chr(c("average", "complete", "single", "ward.D2", "mcquitty", "median", "centroid")),
      na_r = num(0, 0.5), weight_by_lod = lgl()),
    order = list(
      itmax = num(1, 1e7, int = TRUE), drop_outside_envelope = lgl(),
      envelope_k = num(0, Inf, lo_incl = FALSE), envelope_span = num(0, 1, lo_incl = FALSE),
      min_keep = num(1, Inf, int = TRUE)),
    phase = list(
      min_phase_lod = num(0, Inf), tie_tol = num(0, 1), max_passes = num(1, 1e6, int = TRUE),
      tol = num(0, 1)),
    source_aware = list(
      epsilon = num(0, 1, hi_incl = FALSE), lambda = num(0, Inf),
      alpha_start = num(0, 1, hi_incl = FALSE), beta_start = num(0, 1, FALSE, FALSE),
      r_start = num(0, 0.5, lo_incl = FALSE), tol = num(0, Inf, lo_incl = FALSE),
      maxit = num(1, 1e6, int = TRUE), fit_mode = lgl(), nmin = num(1, Inf, int = TRUE),
      smooth = num(0, Inf)),
    population = list(
      epsilon = num(0, 1, hi_incl = FALSE), lambda = num(0, Inf),
      r_start = num(0, 0.5, lo_incl = FALSE), tol = num(0, Inf, lo_incl = FALSE),
      maxit = num(1, 1e6, int = TRUE), paternal_mode = chr("gametic"),
      min_block_markers = num(2, Inf, int = TRUE), gap_r = num(0, 0.5, lo_incl = FALSE)),
    display = list(gap_r = num(0, 0.5, lo_incl = FALSE),
                   map_function = chr(c("haldane", "kosambi", "morgan")))
  )
}

# Validate one stage's settings list. Returns character vector of problems.
hsms_validate_settings <- function(stage, s) {
  spec <- hsms_setting_specs()[[stage]]
  if (is.null(spec)) stop("Unknown settings stage: ", stage)
  errs <- character(0)
  for (nm in names(spec)) {
    sp <- spec[[nm]]; v <- s[[nm]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) {
      if (isTRUE(sp$nullable)) next
      errs <- c(errs, sprintf("`%s` is required.", nm)); next
    }
    if (length(v) != 1L) { errs <- c(errs, sprintf("`%s` must be a single value.", nm)); next }
    if (sp$type %in% c("num", "int")) {
      if (!is.numeric(v) || !is.finite(v) && !(is.infinite(v) && is.infinite(sp$hi))) {
        errs <- c(errs, sprintf("`%s` must be a finite number.", nm)); next
      }
      if (sp$type == "int" && v != round(v)) errs <- c(errs, sprintf("`%s` must be a whole number.", nm))
      lo_bad <- if (sp$lo_incl) v < sp$lo else v <= sp$lo
      hi_bad <- if (sp$hi_incl) v > sp$hi else v >= sp$hi
      if (lo_bad || hi_bad)
        errs <- c(errs, sprintf("`%s` must be in %s%s, %s%s.", nm,
                                if (sp$lo_incl) "[" else "(", format(sp$lo),
                                format(sp$hi), if (sp$hi_incl) "]" else ")"))
    } else if (sp$type == "lgl") {
      if (!is.logical(v)) errs <- c(errs, sprintf("`%s` must be TRUE or FALSE.", nm))
    } else if (sp$type == "chr") {
      if (!is.character(v) || !v %in% sp$choices)
        errs <- c(errs, sprintf("`%s` must be one of: %s.", nm, paste(sp$choices, collapse = ", ")))
    }
  }
  if (identical(stage, "filter") && !length(errs) && s$prob_low > s$prob_high)
    errs <- c(errs, "The lower row-information quantile must not exceed the upper quantile.")
  errs
}

# Keep only the keys a stage defines (drop UI-only extras), coercing types.
hsms_clean_settings <- function(stage, s) {
  spec <- hsms_setting_specs()[[stage]]
  out <- list()
  for (nm in names(spec)) {
    v <- s[[nm]]
    if (is.null(v)) { out[nm] <- list(NULL); next }
    if (spec[[nm]]$type == "int" && is.numeric(v)) v <- as.integer(round(v))
    if (spec[[nm]]$type == "num" && is.numeric(v)) v <- as.numeric(v)
    out[[nm]] <- v
  }
  out
}

# ---- Translation to the exact upstream arguments --------------------------

hsms_args_pairwise <- function(s, threads) {
  list(lambda = s$lambda, q0 = s$q0, tol = s$tol, maxit = as.integer(s$maxit),
       tiny = s$tiny, return_diagnostics = isTRUE(s$return_diagnostics),
       threads = as.integer(threads))
}

# One user-facing linkage-LOD cutoff. tpt_filter() uses
# max(thresh.LOD.ph, thresh.LOD.rf) as the effective linkage cutoff, so both
# compatibility arguments receive the same value.
hsms_args_filter <- function(s) {
  list(thresh.LOD.ph = s$lod, thresh.LOD.rf = s$lod, thresh.rf = s$thresh_rf,
       probs = c(s$prob_low, s$prob_high),
       diag.markers = if (is.null(s$diag_markers) || is.na(s$diag_markers)) NULL else as.integer(s$diag_markers),
       diagnostic.plot = FALSE)
}

hsms_args_group <- function(s) {
  list(k = as.integer(s$k), inter = FALSE, method = s$method, na_r = s$na_r,
       weight_by_lod = isTRUE(s$weight_by_lod), compare = FALSE)
}

hsms_args_order <- function(s) {
  list(plot_each = FALSE, itmax = as.integer(s$itmax),
       drop_outside_envelope = isTRUE(s$drop_outside_envelope),
       envelope_k = s$envelope_k, envelope_span = s$envelope_span,
       min_keep = as.integer(s$min_keep))
}

hsms_args_phase <- function(s) {
  list(min_phase_lod = s$min_phase_lod, tie_tol = s$tie_tol,
       max_passes = as.integer(s$max_passes), tol = s$tol)
}

hsms_args_source_aware <- function(s) {
  list(epsilon = s$epsilon, lambda = s$lambda, alpha_start = s$alpha_start,
       beta_start = s$beta_start, r_start = s$r_start, tol = s$tol,
       maxit = as.integer(s$maxit), fit_mode = isTRUE(s$fit_mode),
       nmin = as.integer(s$nmin), smooth = s$smooth)
}

hsms_args_population <- function(s) {
  list(epsilon = s$epsilon, lambda = s$lambda, r_start = s$r_start, tol = s$tol,
       maxit = as.integer(s$maxit), paternal_mode = "gametic",
       min_block_markers = as.integer(s$min_block_markers), gap_r = s$gap_r)
}
