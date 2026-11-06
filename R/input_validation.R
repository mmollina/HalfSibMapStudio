# Raw input reading and validation, performed BEFORE the upstream reader is
# called. Everything is read as text so identifiers (leading zeros,
# numeric-looking IDs, punctuation, Unicode) are never altered here.

HSMS_MISSING_TOKENS <- c("", "NA", ".")
HSMS_PED_REQUIRED <- c("id", "mother", "father", "generation", "family_id")
HSMS_GENO_REQUIRED <- c("marker_id", "REF", "ALT")
HSMS_GENO_OPTIONAL <- c("chrom", "position")

hsms_issue <- function(severity, code, message, ids = character(0), location = "") {
  data.frame(severity = severity, code = code, message = message,
             ids = paste(utils::head(ids, 50), collapse = "; "),
             n_affected = length(ids), location = location,
             stringsAsFactors = FALSE)
}

hsms_empty_issues <- function() {
  data.frame(severity = character(0), code = character(0), message = character(0),
             ids = character(0), n_affected = integer(0), location = character(0),
             stringsAsFactors = FALSE)
}

# Remove a UTF-8 byte-order mark, byte-wise (safe in non-UTF-8 locales).
hsms_strip_bom <- function(x) {
  if (!length(x) || is.na(x)) return(x)
  out <- sub("^\xef\xbb\xbf", "", x, useBytes = TRUE)
  Encoding(out) <- "UTF-8"
  out
}

# Files are read as UTF-8 text and marked UTF-8 WITHOUT conversion to the
# session's native encoding, so Unicode identifiers survive even when R runs
# in a C/POSIX locale.
# Read a CSV as an all-character table. Returns list(ok, header, data, issues).
hsms_read_raw_csv <- function(path, label) {
  issues <- hsms_empty_issues()
  add <- function(...) issues <<- rbind(issues, hsms_issue(...))
  if (!file.exists(path)) {
    add("error", "file_missing", sprintf("%s file was not found.", label))
    return(list(ok = FALSE, issues = issues))
  }
  if (file.info(path)$size == 0) {
    add("error", "file_empty", sprintf("%s file is empty.", label))
    return(list(ok = FALSE, issues = issues))
  }
  first <- tryCatch(readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) NA_character_)
  if (!length(first) || is.na(first)) {
    add("error", "file_unreadable", sprintf("%s file could not be read as text.", label))
    return(list(ok = FALSE, issues = issues))
  }
  first <- hsms_strip_bom(first)
  if (!grepl(",", first, fixed = TRUE)) {
    delim <- if (grepl("\t", first)) "tab" else if (grepl(";", first)) "semicolon" else "unknown"
    add("error", "unsupported_delimiter",
        sprintf("%s file does not look comma-separated (detected delimiter: %s). Save it as CSV with commas.", label, delim))
    return(list(ok = FALSE, issues = issues))
  }
  header <- scan(text = first, what = "", sep = ",", quote = "\"", quiet = TRUE,
                 na.strings = character(0), strip.white = FALSE, encoding = "UTF-8")
  dat <- tryCatch(
    utils::read.table(path, header = TRUE, sep = ",", quote = "\"", comment.char = "",
                      colClasses = "character", check.names = FALSE,
                      na.strings = character(0), strip.white = FALSE,
                      encoding = "UTF-8", stringsAsFactors = FALSE,
                      blank.lines.skip = TRUE, fill = FALSE),
    error = function(e) e)
  if (inherits(dat, "error")) {
    add("error", "parse_error", sprintf("%s file could not be parsed: %s", label, conditionMessage(dat)))
    return(list(ok = FALSE, issues = issues))
  }
  if (length(header) != ncol(dat)) header <- names(dat)
  header[1] <- hsms_strip_bom(header[1])
  dup <- unique(header[duplicated(header)])
  if (length(dup))
    add("error", "duplicate_header", sprintf("%s file has duplicated column names.", label),
        ids = dup, location = "header")
  empty_h <- which(!nzchar(trimws(header)))
  if (length(empty_h))
    add("error", "empty_header", sprintf("%s file has %d empty column name(s).", label, length(empty_h)),
        ids = as.character(empty_h), location = "header (column numbers)")
  ws <- header[header != trimws(header)]
  if (length(ws))
    add("error", "header_whitespace",
        sprintf("%s file has column names with leading/trailing spaces; they would not match identifiers exactly.", label),
        ids = ws, location = "header")
  names(dat) <- header
  if (!nrow(dat))
    add("error", "no_rows", sprintf("%s file has a header but no data rows.", label))
  list(ok = !any(issues$severity == "error"), header = header, data = dat, issues = issues)
}

# Validate pedigree + genotype files. Pure; does not call HalfSibMap.
hsms_validate_inputs <- function(ped_path, geno_path, missing_tokens = HSMS_MISSING_TOKENS) {
  ped_r <- hsms_read_raw_csv(ped_path, "Pedigree")
  geno_r <- hsms_read_raw_csv(geno_path, "Genotype")
  issues <- rbind(ped_r$issues, geno_r$issues)
  add <- function(...) issues <<- rbind(issues, hsms_issue(...))
  result <- list(ok = FALSE, issues = issues)
  if (!ped_r$ok || !geno_r$ok) { result$issues <- issues; return(result) }

  ped <- ped_r$data; geno <- geno_r$data

  # ---- pedigree ------------------------------------------------------------
  miss_p <- setdiff(HSMS_PED_REQUIRED, names(ped))
  if (length(miss_p))
    add("error", "ped_missing_columns", "Pedigree is missing required columns.", miss_p, "header")
  miss_g <- setdiff(HSMS_GENO_REQUIRED, names(geno))
  if (length(miss_g))
    add("error", "geno_missing_columns", "Genotype file is missing required columns.", miss_g, "header")
  if (length(miss_p) || length(miss_g)) { result$issues <- issues; return(result) }

  extra_p <- setdiff(names(ped), HSMS_PED_REQUIRED)
  if (length(extra_p))
    add("info", "ped_extra_columns", "Extra pedigree columns are kept in the raw file but ignored by the analysis.",
        extra_p, "header")

  rowloc <- function(i) paste0("pedigree row ", i + 1L)  # +1 for header line
  for (col in c("id", "mother", "father", "family_id")) {
    v <- ped[[col]]
    bad <- which(v != trimws(v))
    if (length(bad))
      add("error", "ped_whitespace", sprintf("Pedigree column `%s` has values with leading/trailing spaces.", col),
          v[bad], paste(rowloc(utils::head(bad, 10)), collapse = ", "))
  }
  is_blank <- function(v) v %in% missing_tokens
  if (any(is_blank(ped$id)))
    add("error", "ped_empty_id", "Pedigree has rows with an empty `id`.",
        location = paste(rowloc(utils::head(which(is_blank(ped$id)), 10)), collapse = ", "))
  dup_id <- unique(ped$id[duplicated(ped$id) & !is_blank(ped$id)])
  if (length(dup_id))
    add("error", "ped_duplicate_id", "Pedigree individuals must be unique; these IDs appear more than once.", dup_id)
  if (any(is_blank(ped$family_id)))
    add("error", "ped_empty_family", "Pedigree has rows with an empty `family_id`.",
        ped$id[is_blank(ped$family_id)])
  gen <- ped$generation
  bad_gen <- which(!gen %in% c("1", "2"))
  if (length(bad_gen))
    add("error", "ped_bad_generation",
        "`generation` must be 1 (maternal parent / founder) or 2 (offspring), as read by read_HSMap_data().",
        ped$id[bad_gen], paste(rowloc(utils::head(bad_gen, 10)), collapse = ", "))
  off_rows <- which(gen == "2")
  no_mom <- off_rows[is_blank(ped$mother[off_rows])]
  if (length(no_mom))
    add("error", "ped_offspring_without_mother", "Offspring rows (generation 2) must name their mother.",
        ped$id[no_mom])
  founder_mom <- which(gen == "1" & !is_blank(ped$mother))
  if (length(founder_mom))
    add("warning", "ped_founder_with_mother",
        "Generation-1 rows list a mother; this is ignored by the single-family workflow.", ped$id[founder_mom])
  self_mother <- which(!is_blank(ped$mother) & ped$mother == ped$id)
  if (length(self_mother))
    add("error", "ped_self_mother", "An individual is listed as its own mother.", ped$id[self_mother])
  known_father <- which(!is_blank(ped$father))
  if (length(known_father))
    add("warning", "known_father",
        paste("Some rows record a father. This application fits the unknown-father (open-pollinated)",
              "model only: sire identities and sire genotypes are NOT used. You must explicitly",
              "confirm that you want to proceed under the unknown-father model; the choice is recorded."),
        ped$id[known_father])
  # a mother that also appears as an offspring
  moms <- unique(ped$mother[!is_blank(ped$mother) & gen == "2"])
  mom_as_off <- intersect(moms, ped$id[gen == "2"])
  if (length(mom_as_off))
    add("error", "ped_mother_is_offspring",
        "These IDs are used as a mother and also appear as offspring (generation 2).", mom_as_off)

  # ---- genotypes -----------------------------------------------------------
  mk <- geno$marker_id
  if (any(is_blank(mk)))
    add("error", "geno_empty_marker", "Genotype file has rows with an empty `marker_id`.",
        location = paste0("genotype row ", utils::head(which(is_blank(mk)), 10) + 1L, collapse = ", "))
  bad_ws <- mk[mk != trimws(mk)]
  if (length(bad_ws))
    add("error", "geno_marker_whitespace", "Marker IDs with leading/trailing spaces.", bad_ws)
  dup_mk <- unique(mk[duplicated(mk) & !is_blank(mk)])
  if (length(dup_mk))
    add("error", "geno_duplicate_marker", "Marker IDs must be unique; these appear more than once.", dup_mk)

  meta_cols <- intersect(c(HSMS_GENO_REQUIRED, HSMS_GENO_OPTIONAL), names(geno))
  other_cols <- setdiff(names(geno), meta_cols)
  ped_ids <- ped$id[!is_blank(ped$id)]
  all_people <- unique(c(ped_ids, ped$mother[!is_blank(ped$mother)]))
  sample_cols <- intersect(other_cols, all_people)
  unmatched_cols <- setdiff(other_cols, all_people)
  if (length(unmatched_cols))
    add("warning", "geno_unmatched_columns",
        paste("Genotype columns that do not match any pedigree individual are excluded from the",
              "analysis input (they are not interpreted as samples)."),
        unmatched_cols, "genotype header")
  no_geno <- setdiff(ped_ids, sample_cols)
  if (length(no_geno))
    add("warning", "ped_without_genotypes",
        "Pedigree individuals without a genotype column are excluded from analysis.", no_geno)
  if (!length(sample_cols))
    add("error", "no_matching_samples",
        "No genotype column matches a pedigree `id`; check that sample headers equal the pedigree IDs exactly.")
  if (!nrow(geno))
    add("error", "geno_no_markers", "Genotype file has no marker rows.")

  calls <- NULL
  if (length(sample_cols) && nrow(geno)) {
    G <- as.matrix(geno[, sample_cols, drop = FALSE])
    valid <- matrix(G %in% c("0", "1", "2") | G %in% missing_tokens, nrow(G), ncol(G))
    if (!all(valid)) {
      bad <- which(!valid, arr.ind = TRUE)
      tok <- G[!valid]
      numv <- suppressWarnings(as.numeric(tok))
      klass <- ifelse(is.na(numv), "non-numeric",
                      ifelse(is.infinite(numv), "infinite",
                             ifelse(numv != round(numv), "fractional (dosage?)",
                                    ifelse(numv %in% c(0, 1, 2), "decimal-formatted integer",
                                           "out of range"))))
      for (k in unique(klass)) {
        sel <- which(klass == k)
        locs <- paste0(mk[bad[sel, 1]], "@", sample_cols[bad[sel, 2]], "='", tok[sel], "'")
        add("error", paste0("geno_invalid_", gsub("[^a-z]+", "_", k)),
            sprintf(paste("%d genotype call(s) are %s. Calls must be hard-call integers 0, 1 or 2 (allele count),",
                          "or a missing token (%s). Decimal dosages are not truncated or rounded."),
                    length(sel), k, paste(shQuote(missing_tokens[nzchar(missing_tokens)]), collapse = ", ")),
            locs, "marker@sample")
      }
    } else {
      calls <- matrix(NA_integer_, nrow(G), ncol(G), dimnames = list(mk, sample_cols))
      obs <- !(G %in% missing_tokens)
      calls[obs] <- as.integer(G[obs])
      if (all(is.na(calls)))
        add("error", "geno_all_missing", "Every genotype call is missing.")
      all_na_mk <- mk[rowSums(!is.na(calls)) == 0]
      if (length(all_na_mk))
        add("warning", "geno_marker_all_missing", "Markers with no observed call in any sample.", all_na_mk)
    }
  }

  # ---- family-level checks -------------------------------------------------
  fam_ids <- unique(ped$family_id[!is_blank(ped$family_id)])
  fam_rows <- lapply(fam_ids, function(f) {
    fr <- ped[ped$family_id == f, , drop = FALSE]
    offs <- fr$id[fr$generation == "2"]
    moms_f <- unique(fr$mother[fr$generation == "2" & !is_blank(fr$mother)])
    reasons <- character(0)
    mom <- if (length(moms_f) == 1L) moms_f else NA_character_
    if (!length(moms_f)) reasons <- c(reasons, "no offspring with a recorded mother")
    if (length(moms_f) > 1L)
      reasons <- c(reasons, sprintf("offspring list %d different mothers (%s)", length(moms_f),
                                    paste(moms_f, collapse = ", ")))
    mom_geno <- !is.na(mom) && mom %in% sample_cols
    if (!is.na(mom) && !mom_geno) reasons <- c(reasons, "the mother has no genotype column")
    if (!is.na(mom) && !mom %in% ped$id)
      reasons <- c(reasons, "the mother has no pedigree row (allowed, but check the file)")
    offs_g <- intersect(offs, sample_cols)
    if (!length(offs_g)) reasons <- c(reasons, "no genotyped offspring")
    n_het <- n_hom <- n_mna <- NA_integer_; miss <- NA_real_
    if (!is.null(calls)) {
      if (mom_geno) {
        mv <- calls[, mom]
        n_het <- sum(mv == 1L, na.rm = TRUE); n_hom <- sum(mv %in% c(0L, 2L)); n_mna <- sum(is.na(mv))
        if (n_het < 2L) reasons <- c(reasons, "fewer than 2 markers are heterozygous in the mother")
      }
      if (length(offs_g)) miss <- mean(is.na(calls[, offs_g, drop = FALSE]))
    }
    kf <- sum(!is_blank(fr$father))
    blocking <- setdiff(reasons, "the mother has no pedigree row (allowed, but check the file)")
    data.frame(family_id = f, mother_id = mom, n_offspring_pedigree = length(offs),
               n_offspring_genotyped = length(offs_g), n_markers = nrow(geno),
               maternal_het = n_het, maternal_hom = n_hom, maternal_missing = n_mna,
               offspring_missing_rate = miss, known_father_rows = kf,
               analyzable = !length(blocking) && !is.null(calls),
               notes = paste(reasons, collapse = "; "), stringsAsFactors = FALSE)
  })
  fam_summary <- do.call(rbind, fam_rows)
  if (!is.null(fam_summary) && !any(fam_summary$analyzable) && !any(issues$severity == "error"))
    add("error", "no_analyzable_family",
        "No family can be analysed: each needs exactly one genotyped mother and genotyped offspring.",
        fam_summary$family_id)
  multi_mom <- fam_summary$family_id[grepl("different mothers", fam_summary$notes)]
  if (length(multi_mom))
    add("warning", "family_multiple_mothers",
        "These families list more than one mother and cannot be selected (families are never pooled).",
        multi_mom)

  list(ok = !any(issues$severity == "error"), issues = issues, ped = ped, geno = geno,
       sample_cols = sample_cols, unmatched_cols = unmatched_cols, meta_cols = meta_cols,
       calls = calls, families = fam_summary,
       has_known_father = length(known_father) > 0L,
       missing_tokens = missing_tokens)
}
