# Artifact storage: each artifact is artifacts/<id>.rds (payload) plus
# artifacts/<id>.json (metadata). Payloads are raw HalfSibMap objects plus
# app summaries; metadata carries provenance.

hsms_artifact_paths <- function(project_dir, id) {
  list(rds = file.path(project_dir, "artifacts", paste0(id, ".rds")),
       json = file.path(project_dir, "artifacts", paste0(id, ".json")))
}

# Build metadata for a new artifact.
hsms_make_meta <- function(proj, stage, scope, signature, payload_summary = list(),
                           status = "complete", warnings = character(0), run_id = NA_character_,
                           elapsed = NA_real_, settings = list(), inputs = list(), threads = NA_integer_,
                           seed = NA_integer_, id = hsms_id(stage)) {
  list(
    id = id, stage = stage, scope = if (is.null(scope)) NULL else as.character(scope),
    run_id = run_id, created = hsms_now(), elapsed_sec = elapsed,
    signature = signature,
    inputs = inputs,
    input_signatures = lapply(inputs, function(i) proj$artifacts[[i]]$signature %||% NA_character_),
    settings = settings, family = proj$family %||% NA_character_,
    exclusion_revision = proj$exclusion_revision,
    input_checksums = list(pedigree = proj$inputs$pedigree$sha256 %||% NA_character_,
                           genotypes = proj$inputs$genotypes$sha256 %||% NA_character_),
    seed = seed, threads = threads,
    versions = hsms_versions(),
    warnings = as.list(unique(warnings)),
    status = status,
    summary = payload_summary
  )
}

hsms_store_artifact <- function(project_dir, meta, payload = NULL, payload_file = NULL) {
  p <- hsms_artifact_paths(project_dir, meta$id)
  dir.create(dirname(p$rds), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(payload_file)) {
    hsms_atomic_write(p$rds, function(tmp) {
      if (!file.copy(payload_file, tmp, overwrite = TRUE)) stop("Could not copy artifact payload.")
    })
  } else {
    hsms_write_rds(payload, p$rds)
  }
  meta$file <- file.path("artifacts", basename(p$rds))
  meta$sha256 <- hsms_file_sha256(p$rds)
  meta$bytes <- file.info(p$rds)$size
  hsms_write_json(meta, p$json)
  meta
}

# Session-level payload cache (an environment owned by the caller, never part
# of the project object).
hsms_new_cache <- function() new.env(parent = emptyenv())

hsms_load_artifact <- function(project_dir, meta, cache = NULL, verify = FALSE) {
  if (is.null(meta)) return(NULL)
  if (!is.null(cache) && exists(meta$id, envir = cache, inherits = FALSE))
    return(get(meta$id, envir = cache))
  path <- file.path(project_dir, meta$file)
  if (!file.exists(path)) stop("Artifact file missing: ", meta$file)
  if (verify && !identical(hsms_file_sha256(path), meta$sha256))
    stop("Artifact checksum mismatch: ", meta$file)
  obj <- readRDS(path)
  if (!is.null(cache)) assign(meta$id, obj, envir = cache)
  obj
}

hsms_load_current <- function(proj, project_dir, stage, scope = NULL, cache = NULL) {
  id <- hsms_current_id(proj, stage, scope)
  if (is.null(id)) return(NULL)
  hsms_load_artifact(project_dir, proj$artifacts[[id]], cache)
}
