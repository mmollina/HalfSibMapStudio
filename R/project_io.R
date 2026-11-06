# Project persistence: local project store, atomic saves, locking, reopen
# validation, and portable .hsmap.zip bundles. Paths here are on the machine
# running R (which may differ from the browser machine if hosted later).

hsms_projects_dir <- function(defaults = NULL) {
  d <- Sys.getenv("HSMS_PROJECTS_DIR", "")
  if (!nzchar(d) && !is.null(defaults$app$projects_dir) && length(defaults$app$projects_dir) &&
      !is.na(defaults$app$projects_dir)) d <- defaults$app$projects_dir
  if (!nzchar(d)) d <- file.path(tools::R_user_dir("HalfSibMapStudio", which = "data"), "projects")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, winslash = "/", mustWork = TRUE)
}

hsms_project_dirs <- function(project_dir) {
  for (s in c("inputs", "metadata", "artifacts", "history", "exports", "jobs"))
    dir.create(file.path(project_dir, s), recursive = TRUE, showWarnings = FALSE)
  invisible(project_dir)
}

hsms_create_project <- function(name, base_dir, defaults = hsms_defaults()) {
  proj <- hsms_new_project(name, defaults)
  dir <- file.path(base_dir, paste0(hsms_safe_filename(name, 40), "_", substr(proj$id, nchar(proj$id) - 7, nchar(proj$id))))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  hsms_project_dirs(dir)
  hsms_save_project(proj, dir)
  hsms_log_event(dir, "project_created", list(name = name))
  list(proj = proj, dir = normalizePath(dir, winslash = "/"))
}

# ---- events -----------------------------------------------------------------

hsms_log_event <- function(project_dir, type, details = list()) {
  f <- file.path(project_dir, "history", "events.jsonl")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  line <- jsonlite::toJSON(list(time = hsms_now(), type = type, details = details),
                           auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  cat(line, "\n", file = f, append = TRUE, sep = "")
  invisible(TRUE)
}

hsms_read_events <- function(project_dir, n = 500) {
  f <- file.path(project_dir, "history", "events.jsonl")
  if (!file.exists(f)) return(data.frame(time = character(0), type = character(0), details = character(0)))
  lines <- utils::tail(readLines(f, warn = FALSE, encoding = "UTF-8"), n)
  rows <- lapply(lines, function(l) {
    e <- tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(err) NULL)
    if (is.null(e)) return(NULL)
    d <- e$details
    txt <- if (length(d)) paste(vapply(names(d), function(k) {
      v <- d[[k]]; paste0(k, "=", paste(unlist(v), collapse = ","))
    }, ""), collapse = "; ") else ""
    data.frame(time = e$time %||% NA, type = e$type %||% NA, details = txt, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- data.frame(time = character(0), type = character(0), details = character(0))
  out[rev(seq_len(nrow(out))), , drop = FALSE]
}

# ---- save / open --------------------------------------------------------------

hsms_save_project <- function(proj, project_dir) {
  hsms_project_dirs(project_dir)
  ex <- proj$exclusions %||% hsms_empty_exclusions()
  hsms_write_csv(ex, file.path(project_dir, "metadata", "exclusions.csv"))
  hsms_write_json(proj$settings, file.path(project_dir, "metadata", "settings.json"))
  man <- proj
  man$exclusions <- NULL
  man$exclusions_file <- "metadata/exclusions.csv"
  man$exclusions_signature <- hsms_exclusion_signature(ex)
  man$saved <- hsms_now()
  hsms_write_json(man, file.path(project_dir, "project.json"))
  invisible(TRUE)
}

# JSON row-list (as written by jsonlite for a data frame) back to a data frame.
hsms_rows_to_df <- function(x) {
  if (is.null(x) || is.data.frame(x)) return(x)
  if (!length(x)) return(data.frame())
  cols <- unique(unlist(lapply(x, names)))
  out <- lapply(cols, function(cn) {
    v <- lapply(x, function(r) { z <- r[[cn]]; if (is.null(z)) NA else z })
    unlist(v)
  })
  names(out) <- cols
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

hsms_unlist_named <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  v <- unlist(x); if (is.null(names(v))) names(v) <- names(x)
  v
}

# Read and validate a project directory. Returns list(proj, problems, notices).
hsms_open_project <- function(project_dir, verify_checksums = TRUE) {
  pj <- file.path(project_dir, "project.json")
  if (!file.exists(pj)) stop("Not a HalfSibMap Studio project (project.json missing): ", project_dir)
  man <- jsonlite::read_json(pj, simplifyVector = FALSE)
  if (!identical(as.integer(man$schema_version %||% -1L), HSMS_SCHEMA_VERSION))
    stop("Unsupported project schema version ", man$schema_version %||% "unknown",
         " (this app reads version ", HSMS_SCHEMA_VERSION, ").")
  problems <- character(0); notices <- character(0)
  proj <- man
  proj$saved <- NULL; proj$exclusions_file <- NULL
  exf <- file.path(project_dir, "metadata", "exclusions.csv")
  ex <- if (file.exists(exf)) utils::read.csv(exf, colClasses = "character", na.strings = character(0),
                                              check.names = FALSE, encoding = "UTF-8") else hsms_empty_exclusions()
  if (!nrow(ex)) ex <- hsms_empty_exclusions()
  ex <- ex[, names(hsms_empty_exclusions()), drop = FALSE]
  if (!identical(hsms_exclusion_signature(ex), man$exclusions_signature %||% "none"))
    problems <- c(problems, "metadata/exclusions.csv does not match the saved manifest.")
  proj$exclusions <- ex
  proj$exclusions_signature <- NULL
  proj$group_labels <- hsms_unlist_named(proj$group_labels)
  if (!is.null(proj$validation)) {
    proj$validation$issues <- hsms_rows_to_df(proj$validation$issues)
    if (!NROW(proj$validation$issues) || !"severity" %in% names(proj$validation$issues))
      proj$validation$issues <- hsms_empty_issues()
    proj$validation$families <- hsms_rows_to_df(proj$validation$families)
  }
  for (nm in c("order_spec", "phase_spec", "map_spec", "artifacts", "last_outcome", "jobs", "exports"))
    if (is.null(proj[[nm]])) proj[[nm]] <- list()
  for (s in HSMS_GROUP_STAGES) if (is.null(proj$current[[s]])) proj$current[[s]] <- list()
  # settings: unlist scalar lists
  proj$settings <- lapply(proj$settings, function(s) lapply(s, function(v) if (is.list(v) && !length(v)) NULL else v))

  # artifact files + checksums
  for (id in names(proj$artifacts)) {
    meta <- proj$artifacts[[id]]
    f <- file.path(project_dir, meta$file %||% "")
    if (!file.exists(f)) {
      problems <- c(problems, sprintf("Artifact %s (%s) is missing its payload file.", id, meta$stage))
      proj$artifacts[[id]] <- NULL
      next
    }
    if (verify_checksums && !identical(hsms_file_sha256(f), meta$sha256)) {
      problems <- c(problems, sprintf("Artifact %s (%s) failed its checksum and was set aside.", id, meta$stage))
      proj$artifacts[[id]] <- NULL
    }
  }
  # drop current pointers to removed artifacts
  for (s in HSMS_GLOBAL_STAGES) {
    id <- proj$current[[s]]
    if (!is.null(id) && is.null(proj$artifacts[[id]])) proj$current[[s]] <- NULL
  }
  for (s in HSMS_GROUP_STAGES) for (g in names(proj$current[[s]])) {
    id <- proj$current[[s]][[g]]
    if (!is.null(id) && is.null(proj$artifacts[[id]])) proj$current[[s]][[g]] <- NULL
  }
  # inputs
  if (!is.null(proj$inputs)) for (k in c("pedigree", "genotypes")) {
    f <- file.path(project_dir, proj$inputs[[k]]$file)
    if (!file.exists(f)) problems <- c(problems, sprintf("Raw input %s is missing.", k))
    else if (verify_checksums && !identical(hsms_file_sha256(f), proj$inputs[[k]]$sha256))
      problems <- c(problems, sprintf("Raw input %s failed its checksum.", k))
  }
  # compatibility: installed HalfSibMap commit
  installed <- hsms_versions()$HalfSibMap_commit
  if (!identical(installed, proj$halfsibmap_commit)) {
    notices <- c(notices, sprintf(paste(
      "Compatibility event: this project was computed with HalfSibMap commit %s but commit %s is installed.",
      "All stored results are kept for inspection but are marked Outdated; rerun the stages to recompute."),
      substr(proj$halfsibmap_commit %||% "unknown", 1, 10), substr(installed %||% "unknown", 1, 10)))
    proj$halfsibmap_commit <- installed
  }
  # interrupted jobs
  for (j in names(proj$jobs)) {
    if (identical(proj$jobs[[j]]$status, "running") || identical(proj$jobs[[j]]$status, "cancelling")) {
      proj$jobs[[j]]$status <- "interrupted"
      notices <- c(notices, sprintf("Job %s (%s) was interrupted when the project was last closed; rerun it if needed.",
                                    j, proj$jobs[[j]]$stage))
      for (it in proj$jobs[[j]]$items %||% list())
        proj <- hsms_record_outcome(proj, proj$jobs[[j]]$stage, it$scope, "interrupted", it$signature,
                                    "interrupted when the project was closed", j)
    }
  }
  list(proj = proj, problems = problems, notices = notices)
}

hsms_list_projects <- function(base_dir) {
  dirs <- list.dirs(base_dir, recursive = FALSE)
  rows <- lapply(dirs, function(d) {
    pj <- file.path(d, "project.json")
    if (!file.exists(pj)) return(NULL)
    m <- tryCatch(jsonlite::read_json(pj, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    data.frame(name = m$name %||% basename(d), id = m$id %||% NA, family = m$family %||% NA,
               modified = m$modified %||% NA, dir = normalizePath(d, winslash = "/"),
               locked = file.exists(file.path(d, ".lock")), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(data.frame(name = character(0), id = character(0), family = character(0),
                                      modified = character(0), dir = character(0), locked = logical(0)))
  out[order(out$modified, decreasing = TRUE), , drop = FALSE]
}

# ---- locking -------------------------------------------------------------------

hsms_pid_alive <- function(pid) {
  if (is.na(pid)) return(FALSE)
  if (.Platform$OS.type == "windows") {
    out <- tryCatch(system2("tasklist", c("/FI", paste0("PID eq ", pid)), stdout = TRUE, stderr = FALSE),
                    error = function(e) character(0))
    return(any(grepl(paste0("\\b", pid, "\\b"), out)))
  }
  isTRUE(tryCatch(tools::pskill(pid, 0L), error = function(e) FALSE))
}

hsms_lock_project <- function(project_dir, token) {
  lf <- file.path(project_dir, ".lock")
  if (file.exists(lf)) {
    l <- tryCatch(jsonlite::read_json(lf), error = function(e) list())
    same_host <- identical(l$host %||% "", Sys.info()[["nodename"]])
    if (!identical(l$token %||% "", token) && same_host && hsms_pid_alive(as.integer(l$pid %||% NA)) &&
        !identical(as.integer(l$pid %||% NA), Sys.getpid())) {
      stop("This project is open in another HalfSibMap Studio process (pid ", l$pid, ", since ", l$time,
           "). Close it there first.")
    }
    if (!identical(l$token %||% "", token) && identical(as.integer(l$pid %||% NA), Sys.getpid()))
      stop("This project is already open in another browser session of this app. Close it there first.")
  }
  hsms_write_json(list(token = token, pid = Sys.getpid(), host = Sys.info()[["nodename"]], time = hsms_now()), lf)
  invisible(TRUE)
}

hsms_unlock_project <- function(project_dir, token) {
  lf <- file.path(project_dir, ".lock")
  if (!file.exists(lf)) return(invisible(FALSE))
  l <- tryCatch(jsonlite::read_json(lf), error = function(e) list())
  if (identical(l$token %||% "", token)) unlink(lf)
  invisible(TRUE)
}

# ---- inputs ---------------------------------------------------------------------

# Copy raw uploads into the project (never trusting the uploaded file name as
# a path) and record checksums.
hsms_store_inputs <- function(proj, project_dir, ped_path, geno_path, ped_name = "pedigree.csv",
                              geno_name = "genotypes.csv") {
  dir.create(file.path(project_dir, "inputs"), showWarnings = FALSE)
  dst_p <- file.path(project_dir, "inputs", "pedigree.csv")
  dst_g <- file.path(project_dir, "inputs", "genotypes.csv")
  if (!file.copy(ped_path, dst_p, overwrite = TRUE) || !file.copy(geno_path, dst_g, overwrite = TRUE))
    stop("Could not store the uploaded files in the project.")
  proj$inputs <- list(
    pedigree = list(file = "inputs/pedigree.csv", sha256 = hsms_file_sha256(dst_p),
                    original_name = basename(ped_name), bytes = file.info(dst_p)$size),
    genotypes = list(file = "inputs/genotypes.csv", sha256 = hsms_file_sha256(dst_g),
                     original_name = basename(geno_name), bytes = file.info(dst_g)$size),
    stored = hsms_now())
  proj$modified <- hsms_now()
  proj
}

# ---- portable bundles ----------------------------------------------------------------

HSMS_BUNDLE_ALLOWED_TOP <- c("project.json", "bundle.json", "inputs", "metadata", "artifacts", "history")
HSMS_BUNDLE_ALLOWED_EXT <- c("json", "csv", "rds", "jsonl")

# Write a portable project bundle (.hsmap.zip). Options make omissions explicit.
hsms_export_bundle <- function(proj, project_dir, zip_path, include_inputs = TRUE, include_artifacts = TRUE) {
  hsms_save_project(proj, project_dir)
  files <- c("project.json", "metadata/settings.json", "metadata/exclusions.csv")
  if (file.exists(file.path(project_dir, "history", "events.jsonl"))) files <- c(files, "history/events.jsonl")
  if (include_inputs && !is.null(proj$inputs)) files <- c(files, proj$inputs$pedigree$file, proj$inputs$genotypes$file)
  if (include_artifacts) for (m in proj$artifacts) {
    files <- c(files, m$file, sub("\\.rds$", ".json", m$file))
  }
  files <- unique(files[file.exists(file.path(project_dir, files))])
  manifest <- list(format = "hsmap-studio-bundle", bundle_version = 1L,
                   schema_version = HSMS_SCHEMA_VERSION, created = hsms_now(),
                   project_id = proj$id, project_name = proj$name,
                   includes = list(inputs = include_inputs, artifacts = include_artifacts),
                   limitations = c(
                     if (!include_inputs) "Raw inputs omitted: the analysis cannot be recomputed from this bundle.",
                     if (!include_artifacts) "Analysis artifacts omitted: results must be recomputed after import."),
                   versions = hsms_versions(),
                   files = lapply(files, function(f) list(path = f, bytes = file.info(file.path(project_dir, f))$size,
                                                          sha256 = hsms_file_sha256(file.path(project_dir, f)))))
  stage <- tempfile("bundle_"); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (f in files) {
    dir.create(dirname(file.path(stage, f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(project_dir, f), file.path(stage, f))
  }
  hsms_write_json(manifest, file.path(stage, "bundle.json"))
  if (file.exists(zip_path)) unlink(zip_path)
  zip::zip(zip_path, files = c("bundle.json", files), root = stage, mode = "mirror")
  invisible(zip_path)
}

hsms_bundle_path_ok <- function(p) {
  if (!nzchar(p) || grepl("^[/\\\\]", p) || grepl("^[A-Za-z]:", p)) return(FALSE)
  parts <- strsplit(gsub("\\\\", "/", p), "/", fixed = TRUE)[[1]]
  if (any(parts %in% c("..", "."))) return(FALSE)
  if (!parts[1] %in% HSMS_BUNDLE_ALLOWED_TOP) return(FALSE)
  if (grepl("/$", p)) return(TRUE)  # directory entry
  ext <- tolower(tools::file_ext(p))
  ext %in% HSMS_BUNDLE_ALLOWED_EXT
}

# Validate and import a bundle into a NEW application-owned project directory.
hsms_import_bundle <- function(zip_path, base_dir, max_bytes = 2e9) {
  lst <- tryCatch(zip::zip_list(zip_path), error = function(e) stop("Not a readable zip archive."))
  bad <- lst$filename[!vapply(lst$filename, hsms_bundle_path_ok, logical(1))]
  if (length(bad)) stop("Bundle rejected: unexpected or unsafe paths: ", paste(utils::head(bad, 5), collapse = ", "))
  if (sum(lst$uncompressed_size) > max_bytes) stop("Bundle rejected: uncompressed size exceeds the limit.")
  if (!"bundle.json" %in% lst$filename || !"project.json" %in% lst$filename)
    stop("Bundle rejected: bundle.json or project.json missing.")
  stage <- tempfile("bundle_in_", tmpdir = base_dir); dir.create(stage)
  ok <- FALSE
  on.exit(if (!ok) unlink(stage, recursive = TRUE), add = TRUE)
  zip::unzip(zip_path, exdir = stage)
  # re-check: nothing escaped the staging directory
  real <- normalizePath(list.files(stage, recursive = TRUE, full.names = TRUE, all.files = TRUE), winslash = "/")
  if (any(!startsWith(real, normalizePath(stage, winslash = "/"))))
    stop("Bundle rejected: extracted files escaped the target directory.")
  man <- jsonlite::read_json(file.path(stage, "bundle.json"), simplifyVector = FALSE)
  if (!identical(man$format, "hsmap-studio-bundle")) stop("Bundle rejected: unknown format.")
  if (!identical(as.integer(man$schema_version), HSMS_SCHEMA_VERSION))
    stop("Bundle rejected: project schema version ", man$schema_version, " is not supported.")
  for (f in man$files) {
    p <- file.path(stage, f$path)
    if (!file.exists(p)) stop("Bundle rejected: listed file missing: ", f$path)
    if (!identical(hsms_file_sha256(p), f$sha256)) stop("Bundle rejected: checksum mismatch for ", f$path)
  }
  pj <- jsonlite::read_json(file.path(stage, "project.json"), simplifyVector = FALSE)
  dest <- file.path(base_dir, paste0(hsms_safe_filename(pj$name %||% "imported", 40), "_import_",
                                     format(Sys.time(), "%Y%m%d%H%M%S"), "_", basename(tempfile(""))))
  if (!file.rename(stage, dest)) stop("Could not move the imported project into place.")
  ok <- TRUE
  hsms_project_dirs(dest)
  opened <- hsms_open_project(dest)
  if (!isTRUE(man$includes$artifacts))
    opened$notices <- c(opened$notices, "This bundle did not include analysis artifacts; rerun the stages.")
  if (!isTRUE(man$includes$inputs))
    opened$notices <- c(opened$notices, "This bundle did not include raw inputs; the analysis cannot be recomputed.")
  hsms_log_event(dest, "bundle_imported", list(source = basename(zip_path)))
  c(opened, list(dir = normalizePath(dest, winslash = "/")))
}
