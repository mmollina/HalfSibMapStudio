# Reversible identifier mapping and the import adapter around
# HalfSibMap::read_HSMap_data().
#
# read_HSMap_data() uses utils::read.csv() defaults (check.names = TRUE and
# type inference), which rename sample columns such as "001" -> "X001" and read
# a pedigree id "001" as the integer 1. To make identifiers survive exactly, the
# app maps every sample, family and marker ID to a deterministic, syntactically
# safe internal ID, writes normalised CSVs, and calls the reader on those. The
# mapping is stored with the project and used for every display and export.

hsms_build_id_mapping <- function(v) {
  ped <- v$ped
  is_blank <- function(x) x %in% v$missing_tokens
  samples <- unique(c(ped$id[!is_blank(ped$id)], ped$mother[!is_blank(ped$mother)], v$sample_cols))
  fams <- unique(ped$family_id[!is_blank(ped$family_id)])
  markers <- v$geno$marker_id
  mk <- function(kind, orig, prefix, width)
    data.frame(kind = kind, original = orig,
               internal = sprintf(paste0("%s%0", width, "d"), prefix, seq_along(orig)),
               stringsAsFactors = FALSE)
  map <- rbind(mk("sample", samples, "S", max(6, nchar(length(samples)))),
               mk("family", fams, "F", max(4, nchar(length(fams)))),
               mk("marker", markers, "M", max(7, nchar(length(markers)))))
  hsms_check_mapping(map)
  map
}

hsms_check_mapping <- function(map) {
  for (k in unique(map$kind)) {
    m <- map[map$kind == k, ]
    if (anyDuplicated(m$original)) stop("Identifier mapping collision: duplicated original ", k, " IDs.")
    if (anyDuplicated(m$internal)) stop("Identifier mapping collision: duplicated internal ", k, " IDs.")
    if (!all(make.names(m$internal) == m$internal))
      stop("Internal ", k, " IDs are not syntactic; the upstream reader would rename them.")
  }
  invisible(TRUE)
}

# Translate between original and internal IDs (vectorised; NA for unknown).
hsms_to_internal <- function(map, kind, x) {
  m <- map[map$kind == kind, ]
  m$internal[match(x, m$original)]
}
hsms_to_original <- function(map, kind, x) {
  m <- map[map$kind == kind, ]
  m$original[match(x, m$internal)]
}

# Write normalised reader inputs: only recognised columns, internal IDs,
# blank-for-missing calls, and father information removed (recorded policy).
hsms_write_normalized_inputs <- function(v, map, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ped <- v$ped
  is_blank <- function(x) x %in% v$missing_tokens
  pn <- data.frame(
    id = hsms_to_internal(map, "sample", ped$id),
    mother = ifelse(is_blank(ped$mother), "", hsms_to_internal(map, "sample", ped$mother)),
    father = "",
    generation = ped$generation,
    family_id = hsms_to_internal(map, "family", ped$family_id),
    stringsAsFactors = FALSE)
  geno <- v$geno
  gm <- data.frame(marker_id = hsms_to_internal(map, "marker", geno$marker_id),
                   REF = geno$REF, ALT = geno$ALT, stringsAsFactors = FALSE)
  for (oc in intersect(HSMS_GENO_OPTIONAL, names(geno))) gm[[oc]] <- geno[[oc]]
  calls <- v$calls
  cm <- matrix(ifelse(is.na(calls), "", as.character(calls)), nrow(calls))
  colnames(cm) <- hsms_to_internal(map, "sample", colnames(calls))
  gm <- cbind(gm, as.data.frame(cm, stringsAsFactors = FALSE))
  ped_path <- file.path(out_dir, "pedigree_normalized.csv")
  geno_path <- file.path(out_dir, "genotypes_normalized.csv")
  utils::write.csv(pn, ped_path, row.names = FALSE, na = "")
  utils::write.csv(gm, geno_path, row.names = FALSE, na = "")
  list(pedigree = ped_path, genotypes = geno_path)
}

# Full import: validate, map IDs, call read_HSMap_data(), verify alignment.
# Returns an import artifact payload (list) or a list with ok = FALSE.
hsms_import <- function(ped_path, geno_path, work_dir, accept_unknown_father = FALSE,
                        missing_tokens = HSMS_MISSING_TOKENS) {
  v <- hsms_validate_inputs(ped_path, geno_path, missing_tokens)
  if (!v$ok) return(list(ok = FALSE, issues = v$issues, families = v$families))
  if (isTRUE(v$has_known_father) && !isTRUE(accept_unknown_father)) {
    v$issues <- rbind(v$issues, hsms_issue(
      "error", "known_father_not_confirmed",
      "Father information is present. Confirm proceeding under the unknown-father model to continue."))
    return(list(ok = FALSE, issues = v$issues, families = v$families,
                needs_father_confirmation = TRUE))
  }
  map <- hsms_build_id_mapping(v)
  paths <- hsms_write_normalized_inputs(v, map, work_dir)
  dat <- HalfSibMap::read_HSMap_data(paths$pedigree, paths$genotypes)
  hsms_verify_import(dat, v, map)
  fam <- v$families
  fam$family_internal <- hsms_to_internal(map, "family", fam$family_id)
  fam$mother_internal <- hsms_to_internal(map, "sample", fam$mother_id)
  list(
    ok = TRUE,
    data = dat,
    mapping = map,
    issues = v$issues,
    families = fam,
    unmatched_columns = v$unmatched_cols,
    known_father_policy = if (isTRUE(v$has_known_father))
      "father information present; user confirmed the unknown-father model (sire data not used)"
      else "no father information present",
    raw_checksums = c(pedigree = hsms_file_sha256(ped_path), genotypes = hsms_file_sha256(geno_path)),
    normalized_checksums = c(pedigree = hsms_file_sha256(paths$pedigree),
                             genotypes = hsms_file_sha256(paths$genotypes)),
    marker_annotation = data.frame(
      marker_internal = hsms_to_internal(map, "marker", v$geno$marker_id),
      marker_id = v$geno$marker_id, REF = v$geno$REF, ALT = v$geno$ALT,
      chrom = if ("chrom" %in% names(v$geno)) v$geno$chrom else NA_character_,
      position = if ("position" %in% names(v$geno)) v$geno$position else NA_character_,
      stringsAsFactors = FALSE),
    missing_tokens = missing_tokens
  )
}

# Prove that the reader produced exactly the expected object.
hsms_verify_import <- function(dat, v, map) {
  if (!inherits(dat, "HSMap.data")) stop("read_HSMap_data() did not return HSMap.data.")
  fam_int <- hsms_to_internal(map, "family", unique(v$ped$family_id[!v$ped$family_id %in% v$missing_tokens]))
  if (!identical(names(dat$G_list), fam_int))
    stop("Import verification failed: family list differs from the pedigree.")
  mk_int <- hsms_to_internal(map, "marker", v$geno$marker_id)
  for (k in seq_len(nrow(v$families))) {
    fr <- v$families[k, ]
    if (!fr$analyzable) next
    fi <- hsms_to_internal(map, "family", fr$family_id)
    G <- dat$G_list[[fi]]
    off <- v$ped$id[v$ped$family_id == fr$family_id & v$ped$generation == "2"]
    off <- intersect(off, v$sample_cols)
    if (!identical(rownames(G), hsms_to_internal(map, "sample", off)))
      stop("Import verification failed: offspring of family ", fr$family_id, " were renamed or reordered.")
    if (!identical(colnames(G), mk_int))
      stop("Import verification failed: marker columns changed for family ", fr$family_id, ".")
    expect <- t(v$calls[, off, drop = FALSE]); dimnames(expect) <- dimnames(G)
    if (!identical(unname(G), unname(expect)) && !isTRUE(all.equal(unname(G), unname(expect))))
      stop("Import verification failed: genotype calls differ for family ", fr$family_id, ".")
    st <- dat$stats[dat$stats$family_id == fi, ]
    if (!identical(st$mother_id, hsms_to_internal(map, "sample", fr$mother_id)))
      stop("Import verification failed: mother of family ", fr$family_id, " not identified by the reader.")
    mv <- dat$M_list[[fi]]
    if (!identical(unname(as.integer(mv)), unname(v$calls[, fr$mother_id])))
      stop("Import verification failed: maternal genotypes differ for family ", fr$family_id, ".")
  }
  invisible(TRUE)
}
