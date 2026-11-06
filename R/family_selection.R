# Family selection and exclusions: build the single-family analysis object.
# Families are never pooled: the analysis object contains exactly one dam.

hsms_empty_exclusions <- function() {
  data.frame(kind = character(0), internal = character(0), original = character(0),
             reason = character(0), source = character(0), applied_at = character(0),
             stringsAsFactors = FALSE)
}

hsms_validate_exclusions <- function(ex) {
  need <- names(hsms_empty_exclusions())
  if (!is.data.frame(ex) || !all(need %in% names(ex))) stop("Malformed exclusion table.")
  if (any(!ex$kind %in% c("sample", "marker"))) stop("Exclusion kind must be 'sample' or 'marker'.")
  if (any(!nzchar(ex$reason))) stop("Every exclusion needs a reason.")
  ex[!duplicated(ex[, c("kind", "internal")]), need, drop = FALSE]
}

# family: ORIGINAL family id. exclusions: data frame (internal ids).
hsms_select_family <- function(imp, family, exclusions = hsms_empty_exclusions()) {
  fam <- imp$families[imp$families$family_id == family, , drop = FALSE]
  if (nrow(fam) != 1L) stop("Unknown family: ", family)
  if (!isTRUE(fam$analyzable)) stop("Family ", family, " cannot be analysed: ", fam$notes)
  exclusions <- hsms_validate_exclusions(exclusions)
  dat <- imp$data
  fi <- fam$family_internal
  G <- dat$G_list[[fi]]
  M <- dat$M_list[[fi]]
  map <- imp$mapping

  ex_s <- exclusions$internal[exclusions$kind == "sample"]
  ex_m <- exclusions$internal[exclusions$kind == "marker"]
  unknown_s <- setdiff(ex_s, rownames(G)); unknown_m <- setdiff(ex_m, colnames(G))
  if (length(unknown_s) || length(unknown_m))
    stop("Exclusions refer to IDs not in this family: ",
         paste(utils::head(hsms_to_original(map, "sample", unknown_s), 5), collapse = ", "),
         paste(utils::head(hsms_to_original(map, "marker", unknown_m), 5), collapse = ", "))
  keep_off <- setdiff(rownames(G), ex_s)
  if (length(keep_off) < 1L) stop("All offspring are excluded.")

  G1 <- G[keep_off, , drop = FALSE]
  auto <- data.frame(internal = colnames(G), reason = NA_character_, stringsAsFactors = FALSE)
  auto$reason[is.na(M)] <- "maternal genotype missing (not informative for maternal phase)"
  auto$reason[!is.na(M) & M %in% c(0L, 2L)] <- "maternal genotype homozygous (not informative for maternal phase)"
  no_obs <- colSums(!is.na(G1)) == 0
  auto$reason[is.na(auto$reason) & no_obs] <- "no observed call among retained offspring"
  auto$reason[auto$internal %in% ex_m] <- NA_character_  # manual exclusion reported separately
  auto <- auto[!is.na(auto$reason), , drop = FALSE]
  auto$original <- hsms_to_original(map, "marker", auto$internal)

  keep_mk <- setdiff(colnames(G), c(ex_m, auto$internal))
  analysis <- list(
    G_list = stats::setNames(list(G[keep_off, keep_mk, drop = FALSE]), fi),
    M_list = stats::setNames(list(M[keep_mk]), fi),
    alleles = dat$alleles[dat$alleles$marker_id %in% keep_mk, , drop = FALSE],
    pedigree = dat$pedigree[dat$pedigree$family_id == fi, , drop = FALSE],
    stats = data.frame(family_id = fi, mother_id = fam$mother_internal,
                       n_offspring = length(keep_off), n_markers = length(keep_mk),
                       missing_rate = if (length(keep_mk)) mean(is.na(G[keep_off, keep_mk])) else NA_real_,
                       maternal_het_rate = if (length(keep_mk)) mean(M[keep_mk] == 1L, na.rm = TRUE) else NA_real_,
                       stringsAsFactors = FALSE)
  )
  class(analysis) <- "HSMap.data"
  hsms_verify_selection(analysis, fi)

  list(
    family = family, family_internal = fi,
    mother = fam$mother_id, mother_internal = fam$mother_internal,
    data = analysis,
    offspring = keep_off, markers = keep_mk,
    excluded_samples = ex_s, excluded_markers_manual = ex_m,
    excluded_markers_auto = auto,
    exclusions = exclusions,
    counts = list(raw_offspring = nrow(G), raw_markers = ncol(G),
                  retained_offspring = length(keep_off), retained_markers = length(keep_mk),
                  excluded_offspring = length(ex_s), excluded_markers_manual = length(ex_m),
                  excluded_markers_auto = nrow(auto))
  )
}

hsms_verify_selection <- function(x, fi) {
  if (length(x$G_list) != 1L || length(x$M_list) != 1L)
    stop("Selection must contain exactly one family (no pooling).")
  if (!identical(names(x$G_list), fi)) stop("Selected family label mismatch.")
  if (!identical(colnames(x$G_list[[1]]), names(x$M_list[[1]])))
    stop("Offspring and maternal marker vectors are misaligned.")
  if (any(x$M_list[[1]] != 1L, na.rm = TRUE) || anyNA(x$M_list[[1]]))
    stop("Non-heterozygous maternal markers leaked into the informative set.")
  invisible(TRUE)
}

# Missingness-based exclusion candidates (preview; nothing is applied here).
hsms_missingness_candidates <- function(imp, family, exclusions, max_off_missing, max_marker_missing) {
  sel <- hsms_select_family(imp, family, exclusions)
  G <- sel$data$G_list[[1]]
  off_miss <- rowMeans(is.na(G))
  cand_s <- names(off_miss)[off_miss > max_off_missing]
  G2 <- G[setdiff(rownames(G), cand_s), , drop = FALSE]
  mk_miss <- if (nrow(G2)) colMeans(is.na(G2)) else stats::setNames(rep(1, ncol(G)), colnames(G))
  cand_m <- names(mk_miss)[mk_miss > max_marker_missing]
  map <- imp$mapping
  now <- hsms_now()
  rbind(
    if (length(cand_s)) data.frame(kind = "sample", internal = cand_s,
                                   original = hsms_to_original(map, "sample", cand_s),
                                   reason = sprintf("offspring missing rate %.3f > %.3f", off_miss[cand_s], max_off_missing),
                                   source = "missingness filter", applied_at = now, stringsAsFactors = FALSE),
    if (length(cand_m)) data.frame(kind = "marker", internal = cand_m,
                                   original = hsms_to_original(map, "marker", cand_m),
                                   reason = sprintf("marker missing rate %.3f > %.3f", mk_miss[cand_m], max_marker_missing),
                                   source = "missingness filter", applied_at = now, stringsAsFactors = FALSE),
    hsms_empty_exclusions())
}
