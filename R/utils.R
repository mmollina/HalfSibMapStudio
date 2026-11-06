# Small shared helpers. Pure R: no Shiny dependency, so this file is sourced by
# background workers, scripts and the replay bundle as well as by the app.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

hsms_app_version <- function() "0.1.0"

hsms_pinned_halfsibmap_commit <- function() "b3e8b5fdd429c67a3feb7cd6c380dde746377f43"

# Timestamp in ISO-8601 UTC.
hsms_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

# Short random identifier (not used for security).
hsms_id <- function(prefix = "a") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"), "_",
         paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
}

hsms_file_md5 <- function(path) unname(tools::md5sum(path))

hsms_file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# Signature of an R value (used for computational-input signatures). Values
# are canonicalised and serialised to JSON first, so a value that has been
# saved to project.json and read back produces the same signature (vectors and
# JSON arrays, integer and double storage, attribute order all agree).
hsms_signature <- function(x) {
  txt <- jsonlite::toJSON(hsms_canonical(x), auto_unbox = TRUE, digits = NA,
                          null = "null", na = "null")
  digest::digest(as.character(txt), algo = "sha256", serialize = FALSE)
}

hsms_canonical <- function(x) {
  if (is.null(x)) return(NA)
  if (is.data.frame(x)) x <- as.list(x)
  if (is.list(x)) {
    if (!length(x)) return(list())
    if (!is.null(names(x)) && all(nzchar(names(x)))) {
      x <- x[order(names(x))]
      return(lapply(x, hsms_canonical))
    }
    return(lapply(unname(x), hsms_canonical))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    x <- as.numeric(x)
    x[is.finite(x)] <- signif(x[is.finite(x)], 12)
  }
  x <- unname(x)
  if (length(x) > 1L) return(as.list(x))
  x
}

# Write a file atomically: write to a temporary sibling and rename.
hsms_atomic_write <- function(path, writer) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid(), "-", sample.int(1e9, 1))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  writer(tmp)
  if (!file.exists(tmp)) stop("Atomic write failed: temporary file missing for ", path)
  if (file.exists(path)) {
    # file.rename over an existing file is atomic on POSIX; on Windows it can
    # fail, so fall back to replace-with-backup.
    ok <- suppressWarnings(file.rename(tmp, path))
    if (!ok) {
      bak <- paste0(path, ".bak")
      file.copy(path, bak, overwrite = TRUE)
      ok <- file.copy(tmp, path, overwrite = TRUE)
      if (ok) unlink(bak)
    }
  } else {
    ok <- file.rename(tmp, path)
  }
  if (!ok) stop("Could not write ", path)
  invisible(path)
}

hsms_write_json <- function(x, path) {
  hsms_atomic_write(path, function(tmp) {
    jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, digits = NA,
                         null = "null", na = "null")
  })
}

hsms_read_json <- function(path, simplify = FALSE) {
  jsonlite::read_json(path, simplifyVector = simplify)
}

hsms_write_rds <- function(x, path) {
  hsms_atomic_write(path, function(tmp) saveRDS(x, tmp, version = 3))
}

# Write a data frame as UTF-8 CSV with full numeric precision and explicit NA.
hsms_write_csv <- function(df, path, na = "NA") {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_along(df)) {
    if (is.double(df[[j]])) df[[j]] <- hsms_format_full(df[[j]])
  }
  hsms_atomic_write(path, function(tmp) {
    utils::write.csv(df, tmp, row.names = FALSE, na = na, fileEncoding = "UTF-8")
  })
}

# Full-precision text for doubles (17 significant digits round-trip).
hsms_format_full <- function(x) {
  out <- ifelse(is.na(x), NA_character_,
                ifelse(is.infinite(x), ifelse(x > 0, "Inf", "-Inf"),
                       formatC(x, digits = 17, format = "g")))
  # drop trailing representation noise for integers stored as double
  out <- sub("^\\s+", "", out)
  out
}

# Filesystem-safe name derived from an arbitrary label (labels are retained
# verbatim inside files; only file names are sanitised).
hsms_safe_filename <- function(x, max_len = 60) {
  y <- iconv(x, to = "ASCII//TRANSLIT", sub = "_")
  y[is.na(y)] <- "item"
  y <- gsub("[^A-Za-z0-9._-]+", "_", y)
  y <- gsub("^[._]+|[._]+$", "", y)
  y[!nzchar(y)] <- "item"
  substr(y, 1, max_len)
}

# Detect a safe CPU allowance. parallel::detectCores() can return NA.
hsms_cpu_allowance <- function(config_max = NULL) {
  n <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (is.na(n) || n < 1) n <- 1L
  allow <- max(1L, as.integer(n) - 1L)
  if (!is.null(config_max) && is.finite(config_max)) allow <- min(allow, as.integer(config_max))
  max(1L, allow)
}

# Run an expression capturing warnings (retained, not suppressed) and messages.
hsms_capture <- function(expr) {
  warnings <- character(0)
  messages <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, warnings = unique(warnings), messages = messages)
}

hsms_versions <- function() {
  pkg_ver <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                                  error = function(e) NA_character_)
  hsm_desc <- tryCatch(utils::packageDescription("HalfSibMap"), error = function(e) NULL)
  list(
    app_version = hsms_app_version(),
    app_commit = hsms_git_commit(),
    R = as.character(getRversion()),
    platform = R.version$platform,
    HalfSibMap = pkg_ver("HalfSibMap"),
    HalfSibMap_commit = if (is.list(hsm_desc)) hsm_desc$RemoteSha %||% NA_character_ else NA_character_,
    shiny = pkg_ver("shiny"), bslib = pkg_ver("bslib"), callr = pkg_ver("callr"),
    ggplot2 = pkg_ver("ggplot2"), DT = pkg_ver("DT")
  )
}

# Application git commit, when running from a git checkout.
hsms_git_commit <- function(dir = getOption("hsms.app_dir", ".")) {
  head <- file.path(dir, ".git", "HEAD")
  if (!file.exists(head)) return(NA_character_)
  ref <- tryCatch(readLines(head, warn = FALSE)[1], error = function(e) NA_character_)
  if (is.na(ref)) return(NA_character_)
  if (startsWith(ref, "ref: ")) {
    rf <- file.path(dir, ".git", sub("^ref: ", "", ref))
    if (file.exists(rf)) return(readLines(rf, warn = FALSE)[1])
    packed <- file.path(dir, ".git", "packed-refs")
    if (file.exists(packed)) {
      pl <- readLines(packed, warn = FALSE)
      hit <- grep(paste0(" ", sub("^ref: ", "", ref), "$"), pl, value = TRUE)
      if (length(hit)) return(sub(" .*$", "", hit[1]))
    }
    return(NA_character_)
  }
  ref
}

hsms_fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f"))
}

hsms_fmt_bytes <- function(b) {
  if (is.na(b)) return("NA")
  u <- c("B", "KB", "MB", "GB", "TB"); i <- 1
  while (b >= 1024 && i < length(u)) { b <- b / 1024; i <- i + 1 }
  sprintf("%.1f %s", b, u[i])
}
