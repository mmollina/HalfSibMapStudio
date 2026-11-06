# Parent-side job management: build immutable requests from the applied
# project state, launch callr::r_bg() workers, poll without blocking, cancel
# through the owned process, and accept results only into the revision that
# requested them. Process handles live in session-scoped objects only; they
# are never stored in the project.

# Build a job request for a stage. `items` is a list of list(scope, ...extra).
hsms_build_request <- function(proj, project_dir, stage, items, extra = list()) {
  job_id <- hsms_id("job")
  job_dir <- file.path(project_dir, "jobs", job_id)
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  art_path <- function(st, sc = NULL) {
    id <- hsms_current_id(proj, st, sc)
    if (is.null(id) || !hsms_is_current(proj, st, sc))
      stop("Prerequisite \"", HSMS_STAGE_LABELS[[st]], "\" is not current.")
    normalizePath(file.path(project_dir, proj$artifacts[[id]]$file), winslash = "/")
  }
  inputs <- switch(stage,
    import = list(pedigree = normalizePath(file.path(project_dir, proj$inputs$pedigree$file), winslash = "/"),
                  genotypes = normalizePath(file.path(project_dir, proj$inputs$genotypes$file), winslash = "/")),
    tpt_raw = list(selection = art_path("selection")),
    tpt_filtered = list(tpt_raw = art_path("tpt_raw")),
    grouping = list(tpt_filtered = art_path("tpt_filtered")),
    order = list(tpt_filtered = art_path("tpt_filtered")),
    phase = list(tpt_filtered = art_path("tpt_filtered")),
    map = list(selection = art_path("selection")),
    stop("Unknown stage ", stage))
  items <- lapply(items, function(it) {
    it$signature <- hsms_expected_signature(proj, stage, it$scope)
    if (is.na(it$signature)) stop("Settings for ", stage, " are incomplete.")
    if (stage %in% HSMS_GROUP_STAGES)
      it$spec <- proj[[paste0(stage, "_spec")]][[as.character(it$scope)]]
    if (stage == "phase") it$inputs <- list(order = art_path("order", it$scope))
    if (stage == "map") it$inputs <- list(phase = art_path("phase", it$scope))
    it
  })
  settings <- if (stage %in% c("tpt_raw")) proj$settings$pairwise
              else if (stage == "tpt_filtered") proj$settings$filter
              else if (stage == "grouping") proj$settings$group
              else if (stage == "order") extra$order_settings
              else NULL
  req <- list(job_id = job_id, stage = stage, items = items, inputs = inputs,
              settings = settings, seed = as.integer(proj$settings$app$seed),
              threads = as.integer(proj$settings$app$threads),
              accept_unknown_father = isTRUE(proj$accept_unknown_father),
              app_dir = normalizePath(hsms_app_dir(), winslash = "/"),
              job_dir = normalizePath(job_dir, winslash = "/"),
              project_id = proj$id,
              halfsibmap_commit = proj$halfsibmap_commit,
              created = hsms_now())
  saveRDS(req, file.path(job_dir, "request.rds"))
  req
}

hsms_worker_bootstrap <- function(request_path, app_dir) {
  source(file.path(app_dir, "R", "core_files.R"), local = FALSE)
  hsms_source_core(app_dir, envir = globalenv())
  hsms_worker_main(request_path)
}

hsms_job_launch <- function(req) {
  log_path <- file.path(req$job_dir, "worker.log")
  proc <- callr::r_bg(
    hsms_worker_bootstrap,
    args = list(request_path = file.path(req$job_dir, "request.rds"), app_dir = req$app_dir),
    libpath = .libPaths(), stdout = log_path, stderr = "2>&1",
    supervise = TRUE, package = FALSE, user_profile = FALSE,
    wd = req$app_dir)
  list(id = req$job_id, stage = req$stage, req = req, proc = proc, started = Sys.time(),
       log_path = log_path, state = "running", cancel_requested = FALSE)
}

# Bounded tail of a log file (never reads more than max_bytes).
hsms_tail_file <- function(path, max_bytes = 20000, max_lines = 200) {
  if (!file.exists(path)) return(character(0))
  sz <- file.info(path)$size
  con <- file(path, "rb"); on.exit(close(con))
  if (sz > max_bytes) seek(con, sz - max_bytes)
  txt <- rawToChar(readBin(con, "raw", min(sz, max_bytes)))
  Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  utils::tail(lines, max_lines)
}

hsms_job_status_file <- function(job) {
  f <- file.path(job$req$job_dir, "status.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::read_json(f), error = function(e) NULL)
}

# Non-blocking poll. Returns the job with updated state:
# running, cancelling, finished, failed, cancelled.
hsms_job_poll <- function(job) {
  alive <- job$proc$is_alive()
  if (alive) {
    job$state <- if (isTRUE(job$cancel_requested)) "cancelling" else "running"
    return(job)
  }
  if (isTRUE(job$cancel_requested)) { job$state <- "cancelled"; return(job) }
  res <- tryCatch({ job$proc$get_result(); NULL }, error = function(e) conditionMessage(e))
  job$worker_error <- res
  job$state <- if (is.null(res)) "finished" else "failed"
  job
}

hsms_job_cancel <- function(job) {
  job$cancel_requested <- TRUE
  if (job$proc$is_alive()) {
    tryCatch(job$proc$kill_tree(), error = function(e) tryCatch(job$proc$kill(), error = function(e2) NULL))
  }
  job$state <- if (job$proc$is_alive()) "cancelling" else "cancelled"
  job
}

hsms_job_manifest <- function(job) {
  f <- file.path(job$req$job_dir, "result.json")
  if (!file.exists(f)) return(NULL)
  jsonlite::read_json(f, simplifyVector = FALSE)
}

# Accept finished (or partially finished) job items into the project. Items
# whose launch signature no longer matches the expected signature are kept
# only as historical artifacts (never current). Returns list(proj, events).
hsms_accept_job <- function(proj, project_dir, job, manifest, accept_partial = TRUE) {
  events <- list()
  req <- job$req
  if (file.exists(job$log_path %||% "")) {
    dir.create(file.path(project_dir, "history", "logs"), recursive = TRUE, showWarnings = FALSE)
    file.copy(job$log_path, file.path(project_dir, "history", "logs", paste0(req$job_id, ".log")), overwrite = TRUE)
  }
  if (is.null(manifest)) return(list(proj = proj, events = events, accepted = 0L))
  accepted <- 0L
  for (it in manifest$items) {
    scope <- it$scope
    if (!isTRUE(it$ok)) {
      proj <- hsms_record_outcome(proj, req$stage, scope, "failed", it$signature, it$error, req$job_id)
      events[[length(events) + 1]] <- list(type = "job_item_failed", stage = req$stage,
                                           scope = scope %||% NA, error = it$error)
      next
    }
    payload_file <- file.path(req$job_dir, it$file)
    if (!file.exists(payload_file)) {
      proj <- hsms_record_outcome(proj, req$stage, scope, "failed", it$signature,
                                  "result file missing", req$job_id)
      next
    }
    payload <- readRDS(payload_file)
    if (req$stage == "import" && !isTRUE(payload$ok)) {
      proj$validation <- list(issues = payload$issues, families = payload$families,
                              needs_father_confirmation = isTRUE(payload$needs_father_confirmation),
                              time = hsms_now())
      proj <- hsms_record_outcome(proj, "import", NULL, "failed", it$signature,
                                  "input validation failed", req$job_id)
      events[[length(events) + 1]] <- list(type = "validation_failed", stage = "import")
      next
    }
    hsms_validate_payload(req$stage, payload)
    fresh <- hsms_job_item_is_fresh(proj, req$stage, scope, it$signature)
    req_item <- Filter(function(x) identical(x$scope, scope), req$items)[[1]]
    # Group stages record their full applied specification (e.g. the order
    # recipe with its seed); global stages record their stage settings.
    settings <- if (req$stage %in% HSMS_GROUP_STAGES) req_item$spec else req$settings
    meta <- hsms_make_meta(proj, req$stage, scope, it$signature,
                           payload_summary = it$summary %||% list(),
                           status = if (fresh) it$status else "historical",
                           warnings = unlist(it$warnings), run_id = req$job_id, elapsed = it$elapsed,
                           settings = settings,
                           inputs = hsms_input_ids(proj, req$stage, scope),
                           threads = req$threads, seed = req$seed)
    meta <- hsms_store_artifact(project_dir, meta, payload_file = payload_file)
    if (fresh) meta$status <- it$status
    proj <- hsms_register_artifact(proj, meta, make_current = fresh)
    if (fresh) accepted <- accepted + 1L
    if (req$stage == "import" && fresh) {
      hsms_write_csv(payload$mapping, file.path(project_dir, "metadata", "identifier_mapping.csv"))
      proj$validation <- list(issues = payload$issues, families = payload$families, time = hsms_now())
    }
    events[[length(events) + 1]] <- list(
      type = if (fresh) "artifact_accepted" else "stale_result_kept_as_historical",
      stage = req$stage, scope = scope %||% NA, artifact = meta$id, status = meta$status)
  }
  list(proj = proj, events = events, accepted = accepted)
}

hsms_input_ids <- function(proj, stage, scope) {
  ids <- switch(stage,
    import = list(),
    tpt_raw = list(selection = hsms_current_id(proj, "selection")),
    tpt_filtered = list(tpt_raw = hsms_current_id(proj, "tpt_raw")),
    grouping = list(tpt_filtered = hsms_current_id(proj, "tpt_filtered")),
    order = list(grouping = hsms_current_id(proj, "grouping"), tpt_filtered = hsms_current_id(proj, "tpt_filtered")),
    phase = list(order = hsms_current_id(proj, "order", scope), tpt_filtered = hsms_current_id(proj, "tpt_filtered")),
    map = list(phase = hsms_current_id(proj, "phase", scope), selection = hsms_current_id(proj, "selection")),
    list())
  Filter(Negate(is.null), ids)
}

# Structural checks before a payload may be accepted.
hsms_validate_payload <- function(stage, p) {
  ok <- switch(stage,
    import = inherits(p$data, "HSMap.data") && is.data.frame(p$mapping),
    tpt_raw = inherits(p$raw, "HSMap.tpt"),
    tpt_filtered = inherits(p$raw, "HSMap.tpt") && length(p$kept) >= 1L,
    grouping = inherits(p$raw, "hsmap_group"),
    order = is.character(p$order) && length(p$order) >= 1L,
    phase = isTRUE(p$singleton) || inherits(p$raw, "HSMap.phased"),
    map = p$model %in% c("source_aware", "population") && is.data.frame(p$fit_table),
    FALSE)
  if (!isTRUE(ok)) stop("Result of stage '", stage, "' failed structural validation; it was not accepted.")
  invisible(TRUE)
}

hsms_cleanup_job_dir <- function(job) {
  unlink(job$req$job_dir, recursive = TRUE)
}
