# Descriptive data-quality summaries for one family. These are descriptive
# statistics only (no Hardy-Weinberg test, no genotype correction, no selfing
# detection).

hsms_safe_mean <- function(x) if (length(x)) mean(x) else NA_real_

hsms_qc_summary <- function(imp, family, sel = NULL) {
  fam <- imp$families[imp$families$family_id == family, , drop = FALSE]
  fi <- fam$family_internal
  G <- imp$data$G_list[[fi]]
  M <- imp$data$M_list[[fi]]
  map <- imp$mapping
  ann <- imp$marker_annotation

  off_all <- data.frame(
    offspring_id = hsms_to_original(map, "sample", rownames(G)),
    internal = rownames(G),
    n_markers = ncol(G),
    n_missing = rowSums(is.na(G)),
    missing_rate = if (ncol(G)) rowMeans(is.na(G)) else NA_real_,
    stringsAsFactors = FALSE)
  if (!is.null(sel)) {
    off_all$status <- ifelse(off_all$internal %in% sel$excluded_samples, "excluded", "retained")
    inf <- sel$markers
    off_all$missing_rate_informative <- if (length(inf)) rowMeans(is.na(G[, inf, drop = FALSE])) else NA_real_
  }

  cnt <- function(v) c(n0 = sum(v == 0L, na.rm = TRUE), n1 = sum(v == 1L, na.rm = TRUE),
                       n2 = sum(v == 2L, na.rm = TRUE), n_obs = sum(!is.na(v)))
  cc <- if (ncol(G)) t(apply(G, 2, cnt)) else matrix(0, 0, 4, dimnames = list(NULL, c("n0","n1","n2","n_obs")))
  mk <- data.frame(
    marker_id = hsms_to_original(map, "marker", colnames(G)),
    internal = colnames(G),
    chrom = ann$chrom[match(colnames(G), ann$marker_internal)],
    position = ann$position[match(colnames(G), ann$marker_internal)],
    maternal_call = unname(M),
    maternal_class = ifelse(is.na(M), "missing", ifelse(M == 1L, "heterozygous", "homozygous")),
    n_offspring = nrow(G),
    n_observed = cc[, "n_obs"],
    missing_rate = if (nrow(G)) 1 - cc[, "n_obs"] / nrow(G) else NA_real_,
    freq_0 = ifelse(cc[, "n_obs"] > 0, cc[, "n0"] / cc[, "n_obs"], NA_real_),
    freq_1 = ifelse(cc[, "n_obs"] > 0, cc[, "n1"] / cc[, "n_obs"], NA_real_),
    freq_2 = ifelse(cc[, "n_obs"] > 0, cc[, "n2"] / cc[, "n_obs"], NA_real_),
    stringsAsFactors = FALSE)
  if (!is.null(sel)) {
    mk$status <- "retained"
    mk$status[mk$internal %in% sel$excluded_markers_manual] <- "excluded (manual)"
    auto <- sel$excluded_markers_auto
    mk$status[match(auto$internal, mk$internal)] <- paste0("not informative: ", auto$reason)
  }

  overall <- list(
    family = family, mother = fam$mother_id,
    raw_offspring = nrow(G), raw_markers = ncol(G),
    offspring_missing_rate = if (length(G)) mean(is.na(G)) else NA_real_,
    maternal_heterozygous = sum(M == 1L, na.rm = TRUE),
    maternal_homozygous = sum(M %in% c(0L, 2L)),
    maternal_missing = sum(is.na(M)),
    informative_markers = sum(M == 1L & cc[, "n_obs"] > 0, na.rm = TRUE)
  )
  if (!is.null(sel)) overall <- c(overall, sel$counts)
  list(overall = overall, offspring = off_all, markers = mk)
}
