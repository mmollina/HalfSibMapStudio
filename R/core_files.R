# The pure-R core: files a background worker, a script, or the replay bundle
# can source without Shiny. Order matters only for readability.
hsms_core_files <- function() {
  c("utils.R", "settings.R", "input_validation.R", "identifier_mapping.R",
    "family_selection.R", "qc_summary.R", "analysis_pairwise.R",
    "analysis_group_order.R", "analysis_phase.R", "analysis_map.R",
    "map_summary.R", "analysis_inheritance.R", "project_state.R",
    "artifact_store.R", "worker_entry.R", "job_manager.R", "project_actions.R", "export_analysis.R", "export_figures.R",
    "replay_bundle.R", "project_io.R", "core_files.R")
}

# Source the core into `envir`. Used by workers and scripts; the Shiny app
# loads R/ automatically.
hsms_source_core <- function(app_dir, envir = globalenv()) {
  for (f in hsms_core_files()) {
    p <- file.path(app_dir, "R", f)
    if (file.exists(p)) sys.source(p, envir = envir, keep.source = FALSE)
  }
  options(hsms.app_dir = normalizePath(app_dir, winslash = "/"))
  invisible(TRUE)
}
