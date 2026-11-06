# Background worker entry point. Runs in a separate R process started with
# callr::r_bg(). It receives only an immutable request file (paths, settings,
# seed, expected signatures), never the Shiny session. Results are written to
# the job directory as RDS payloads plus a small result.json manifest.

hsms_worker_status <- function(job_dir, stage, message, done = NULL, total = NULL, scope = NULL) {
  hsms_write_json(list(stage = stage, message = message, done = done, total = total,
                       scope = scope, time = hsms_now(), pid = Sys.getpid()),
                  file.path(job_dir, "status.json"))
  cat(sprintf("[%s] %s%s\n", format(Sys.time(), "%H:%M:%S"), message,
              if (!is.null(done) && !is.null(total)) sprintf(" (%s/%s)", done, total) else ""))
  flush.console()
}

hsms_worker_check_env <- function(req) {
  if (!requireNamespace("HalfSibMap", quietly = TRUE))
    stop("HalfSibMap is not installed in the worker library (", paste(.libPaths(), collapse = ", "), ").")
  sha <- utils::packageDescription("HalfSibMap")$RemoteSha %||% NA_character_
  if (!identical(sha, req$halfsibmap_commit))
    stop("Worker HalfSibMap commit ", sha, " differs from the project's pinned commit ", req$halfsibmap_commit, ".")
  invisible(TRUE)
}

# Run one item, capturing errors so a failure in one group/block does not
# discard the others.
hsms_worker_item <- function(req, item, fun) {
  t0 <- proc.time()[["elapsed"]]
  set.seed(req$seed)
  res <- tryCatch(fun(), error = function(e) e)
  el <- proc.time()[["elapsed"]] - t0
  base <- list(scope = item$scope, signature = item$signature, elapsed = el)
  if (inherits(res, "error"))
    return(c(base, list(ok = FALSE, error = conditionMessage(res), status = "failed",
                        warnings = list(), file = NULL)))
  fname <- paste0("payload_", if (is.null(item$scope)) "global" else hsms_safe_filename(item$scope), ".rds")
  hsms_write_rds(res, file.path(req$job_dir, fname))
  warns <- unique(c(res$warnings %||% character(0)))
  status <- if (isTRUE(res$partial)) "partial"
            else if (length(warns) || hsms_payload_nonconverged(res)) "complete_with_warnings" else "complete"
  c(base, list(ok = TRUE, error = NULL, status = status, warnings = as.list(warns),
               file = fname, summary = hsms_payload_brief(req$stage, res)))
}

hsms_payload_nonconverged <- function(res) {
  ft <- res$fit_table
  !is.null(ft) && any(ft$status == "fitted" & !ft$converged, na.rm = TRUE)
}

# Small JSON-safe summary stored in artifact metadata.
hsms_payload_brief <- function(stage, res) {
  switch(stage,
    import = list(ok = isTRUE(res$ok), n_families = nrow(res$families %||% data.frame()),
                  n_markers = nrow(res$marker_annotation %||% data.frame())),
    tpt_raw = list(n_markers = res$summary$n_markers, n_pairs = res$summary$n_pairs,
                   n_pairs_na = res$summary$n_pairs_na, time_sec = res$summary$time_sec,
                   threads = res$summary$threads),
    tpt_filtered = res$summary[c("n_before", "n_after", "n_excluded", "lod_used", "rf")],
    grouping = list(k = res$summary$k, method_used = res$summary$method_used,
                    method_requested = res$summary$method_requested,
                    sizes = as.list(res$summary$sizes), n_singletons = res$summary$n_singletons),
    order = list(n_markers = length(res$order), n_removed = length(res$removed), method = res$method),
    phase = list(n_markers = nrow(res$summary$markers), n_components = res$summary$n_components,
                 fully_resolved = res$summary$fully_resolved, n_unresolved = res$summary$n_unresolved),
    map = list(model = res$model, n_markers = length(res$markers),
               converged = all(res$fit_table$converged[res$fit_table$status == "fitted"]),
               n_blocks = nrow(res$fit_table), n_failed = sum(res$fit_table$status == "failed")),
    list())
}

# Build an export inside the worker from the saved project state.
hsms_worker_export <- function(req) {
  o <- req$settings %||% list()
  op <- hsms_open_project(req$project_dir, verify_checksums = FALSE)
  proj <- op$proj
  dir.create(file.path(req$project_dir, "exports"), showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  base <- paste(hsms_safe_filename(proj$name, 30), hsms_safe_filename(proj$family %||% "nofamily", 20), sep = "_")
  if (identical(o$kind, "replay")) {
    f <- file.path("exports", sprintf("replay_%s_%s.zip", base, stamp))
    hsms_export_replay_bundle(proj, req$project_dir, file.path(req$project_dir, f))
    return(list(kind = "replay", file = f, status = "replay"))
  }
  tmp <- file.path(req$project_dir, "exports", sprintf("results_%s_%s.tmp.zip", base, stamp))
  r <- hsms_export_results(proj, req$project_dir, tmp,
                           include_historical = isTRUE(o$include_historical),
                           include_figures = !isFALSE(o$include_figures),
                           include_posteriors = !isFALSE(o$include_posteriors),
                           include_pairwise_matrices = isTRUE(o$include_pairwise_matrices))
  f <- file.path("exports", sprintf("results_%s_%s_%s.zip", base, r$status, stamp))
  file.rename(tmp, file.path(req$project_dir, f))
  list(kind = "results", file = f, status = r$status, notes = r$notes)
}

hsms_worker_main <- function(request_path) {
  req <- readRDS(request_path)
  job_dir <- req$job_dir
  on.exit(NULL)
  hsms_worker_status(job_dir, req$stage, "Worker started")
  hsms_worker_check_env(req)
  threads <- max(1L, as.integer(req$threads %||% 1L))
  RcppParallel::setThreadOptions(numThreads = threads)
  options(HSMap.n_threads = threads)
  set.seed(req$seed)
  items <- req$items
  n <- length(items)
  load_in <- function(name, item = NULL) {
    p <- if (!is.null(item) && !is.null(item$inputs[[name]])) item$inputs[[name]] else req$inputs[[name]]
    if (is.null(p)) stop("Missing input '", name, "' in job request.")
    readRDS(p)
  }
  out <- vector("list", n)
  for (i in seq_len(n)) {
    it <- items[[i]]
    label <- if (is.null(it$scope)) HSMS_STAGE_LABELS[[req$stage]] else
      paste0(HSMS_STAGE_LABELS[[req$stage]], " - ", it$label %||% it$scope)
    hsms_worker_status(job_dir, req$stage, paste("Running", label), done = i - 1L, total = n, scope = it$scope)
    out[[i]] <- hsms_worker_item(req, it, switch(req$stage,
      import = function() hsms_import(req$inputs$pedigree, req$inputs$genotypes, file.path(job_dir, "normalized"),
                                      accept_unknown_father = isTRUE(req$accept_unknown_father)),
      tpt_raw = function() hsms_run_pairwise(load_in("selection"), req$settings, threads),
      tpt_filtered = function() hsms_run_filter(load_in("tpt_raw")$raw, req$settings),
      grouping = function() hsms_run_group(load_in("tpt_filtered")$raw, req$settings),
      order = function() hsms_run_order_mds(load_in("tpt_filtered")$raw, it$markers, req$settings, req$seed),
      phase = function() hsms_run_phase(load_in("tpt_filtered")$raw, load_in("order", it)$order, it$settings),
      map = function() {
        sel <- load_in("selection"); ph <- load_in("phase", it)
        if (identical(it$model, "source_aware")) hsms_run_source_aware(sel, ph, it$settings)
        else if (identical(it$model, "population")) hsms_run_population(sel, ph, it$settings)
        else stop("Unknown model: ", it$model)
      },
      export = function() hsms_worker_export(req),
      stop("Unknown stage: ", req$stage)))
    hsms_write_json(list(job_id = req$job_id, stage = req$stage, complete = FALSE,
                         items = out[seq_len(i)]), file.path(job_dir, "result.json"))
    msg <- if (isTRUE(out[[i]]$ok)) "finished" else paste("FAILED:", out[[i]]$error)
    hsms_worker_status(job_dir, req$stage, paste(label, msg), done = i, total = n, scope = it$scope)
  }
  hsms_write_json(list(job_id = req$job_id, stage = req$stage, complete = TRUE, items = out),
                  file.path(job_dir, "result.json"))
  hsms_worker_status(job_dir, req$stage, "Worker finished", done = n, total = n)
  "done"
}
