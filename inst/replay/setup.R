# Replay bundle setup: verify (default) or install (--install) the recorded
# dependencies. Run from any directory:
#   Rscript /path/to/replay/setup.R            # check only
#   Rscript /path/to/replay/setup.R --install  # install missing pieces into the default library
local({
  args <- commandArgs(trailingOnly = TRUE)
  # Rscript encodes spaces in the script path as "~+~".
  root <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)))
  cfg <- jsonlite::read_json(file.path(root, "settings.json"))
  install <- "--install" %in% args
  need <- c("jsonlite", "digest", "RcppParallel")
  if (getRversion() < "4.2.0") stop("R >= 4.2.0 is required.")
  repos <- c(CRAN = Sys.getenv("HSMS_CRAN_REPO", "https://cloud.r-project.org"))
  for (p in need) if (!requireNamespace(p, quietly = TRUE)) {
    if (!install) stop("Package '", p, "' is missing. Rerun with --install.")
    utils::install.packages(p, repos = repos)
  }
  sha <- tryCatch(utils::packageDescription("HalfSibMap")$RemoteSha, error = function(e) NULL)
  if (!identical(sha, cfg$halfsibmap_commit)) {
    if (!install) stop("HalfSibMap at commit ", cfg$halfsibmap_commit, " is required (found: ",
                       if (is.null(sha)) "not installed" else sha, "). Rerun with --install.")
    if (!requireNamespace("remotes", quietly = TRUE)) utils::install.packages("remotes", repos = repos)
    options(repos = repos)
    remotes::install_git(cfg$halfsibmap_git, ref = cfg$halfsibmap_commit, upgrade = "never",
                         build_vignettes = FALSE)
  }
  cat("Dependencies verified: HalfSibMap", cfg$halfsibmap_commit, "\n")
})
