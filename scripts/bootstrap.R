# HalfSibMap Studio -- dependency bootstrap
#
# Usage (from the repository root):
#   Rscript scripts/bootstrap.R            # restore renv.lock (normal case)
#   Rscript scripts/bootstrap.R --init     # maintainer: fresh install + snapshot
#
# Environment variables:
#   HSMS_CRAN_REPO   CRAN-like repository URL (default: https://cloud.r-project.org).
#                    Posit Package Manager can be used for prebuilt Linux
#                    binaries, e.g.
#                    https://packagemanager.posit.co/cran/__linux__/noble/latest
#
# This script never upgrades packages silently: in the normal mode it only
# restores the exact versions recorded in renv.lock.

local({
  args <- commandArgs(trailingOnly = TRUE)
  init_mode <- "--init" %in% args

  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  `%||%` <- function(a, b) if (is.null(a)) b else a
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Run this script from the HalfSibMap Studio repository root ",
         "(the folder that contains app.R and DESCRIPTION).", call. = FALSE)
  }

  halfsibmap_commit <- "b3e8b5fdd429c67a3feb7cd6c380dde746377f43"
  halfsibmap_git <- "https://github.com/mmollina/HalfSibMap.git"

  msg <- function(...) cat("[bootstrap] ", ..., "\n", sep = "")
  fail <- function(...) {
    cat("[bootstrap] ERROR: ", ..., "\n", sep = "")
    quit(status = 1, save = "no")
  }

  # ---- prerequisites --------------------------------------------------------
  if (getRversion() < "4.2.0")
    fail("R >= 4.2.0 is required (found ", as.character(getRversion()), ").")
  msg("R ", as.character(getRversion()), " on ", R.version$platform)

  is_windows <- identical(.Platform$OS.type, "windows")
  make_ok <- nzchar(Sys.which("make"))
  if (!make_ok) {
    if (is_windows) {
      msg("WARNING: 'make' not found on PATH. HalfSibMap contains compiled C++ ",
          "code and is installed from source; install the Rtools version that ",
          "matches your R (https://cran.r-project.org/bin/windows/Rtools/).")
    } else {
      msg("WARNING: 'make' not found. Install build tools (e.g. 'build-essential' ",
          "on Debian/Ubuntu) before continuing.")
    }
  }
  if (!nzchar(Sys.which("git")))
    msg("WARNING: 'git' not found on PATH; HalfSibMap is installed from its git ",
        "repository at a pinned commit, so git is required.")

  repo <- Sys.getenv("HSMS_CRAN_REPO", "https://cloud.r-project.org")
  options(repos = c(CRAN = repo),
          Ncpus = max(1L, min(4L, parallel::detectCores(logical = FALSE) %||% 1L, na.rm = TRUE)))
  msg("Package repository: ", repo)

  # ---- renv itself ----------------------------------------------------------
  if (!requireNamespace("renv", quietly = TRUE)) {
    msg("Installing renv into the user library ...")
    lib <- Sys.getenv("R_LIBS_USER")
    if (!nzchar(lib)) fail("R_LIBS_USER is not set; cannot install renv.")
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    utils::install.packages("renv", lib = lib)
    .libPaths(c(lib, .libPaths()))
    if (!requireNamespace("renv", quietly = TRUE)) fail("Could not install renv.")
  }

  lockfile <- file.path(root, "renv.lock")

  if (!init_mode) {
    if (!file.exists(lockfile))
      fail("renv.lock not found. Maintainers can create it with ",
           "'Rscript scripts/bootstrap.R --init'.")
    msg("Restoring the locked dependency set (this compiles HalfSibMap) ...")
    ok <- tryCatch({
      renv::restore(project = root, prompt = FALSE)
      TRUE
    }, error = function(e) {
      cat(conditionMessage(e), "\n")
      FALSE
    })
    if (!ok) fail("renv::restore() failed. See the messages above. Common causes: ",
                  "missing compiler/Rtools, no network access to the package ",
                  "repository or github.com, or missing system libraries.")
  } else {
    msg("Maintainer init: installing dependencies and writing renv.lock ...")
    if (!file.exists(file.path(root, "renv", "activate.R")))
      renv::init(project = root, bare = TRUE, restart = FALSE)
    renv::load(project = root, quiet = TRUE)
    options(repos = c(CRAN = repo))
    cran_pkgs <- c("shiny", "bslib", "callr", "processx", "ggplot2", "DT",
                   "jsonlite", "digest", "zip", "htmltools", "testthat",
                   "shinytest2", "chromote", "remotes", "renv",
                   # HalfSibMap imports / link-time dependencies
                   "Rcpp", "RcppParallel", "RcppProgress", "dplyr", "tidyr",
                   "tidyselect", "fields", "dendextend", "princurve", "smacof",
                   "cli", "crayon", "knitr", "rmarkdown")
    renv::install(cran_pkgs, prompt = FALSE)
    msg("Installing HalfSibMap from git at ", halfsibmap_commit, " ...")
    remotes::install_git(halfsibmap_git, ref = halfsibmap_commit,
                         dependencies = FALSE, upgrade = "never",
                         build_vignettes = FALSE)
    renv::snapshot(project = root, type = "explicit", prompt = FALSE)
  }

  # ---- verify ---------------------------------------------------------------
  renv::load(project = root, quiet = TRUE)
  desc <- tryCatch(utils::packageDescription("HalfSibMap"), error = function(e) NULL)
  if (is.null(desc) || inherits(desc, "logical"))
    fail("HalfSibMap is not installed in the project library.")
  sha <- desc$RemoteSha %||% NA_character_
  msg("HalfSibMap ", desc$Version, " (RemoteSha ", sha, ")")
  if (!identical(sha, halfsibmap_commit))
    fail("Installed HalfSibMap commit ", sha, " does not match the pinned commit ",
         halfsibmap_commit, ".")
  for (p in c("shiny", "bslib", "callr", "ggplot2", "DT", "jsonlite", "digest", "zip"))
    if (!requireNamespace(p, quietly = TRUE)) fail("Package '", p, "' is missing.")
  msg("All dependencies are available. Start the app with: Rscript scripts/run_app.R")
})
