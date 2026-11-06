# Replayable analysis bundle: recomputes every completed, current stage from
# the included raw inputs using the real HalfSibMap API and the recorded
# settings and seeds. It never just reloads final RDS files.

HSMS_REPLAY_HELPERS <- c("utils.R", "settings.R", "input_validation.R", "identifier_mapping.R",
                         "family_selection.R", "qc_summary.R", "analysis_pairwise.R",
                         "analysis_group_order.R", "analysis_phase.R", "analysis_map.R",
                         "map_summary.R", "analysis_inheritance.R", "project_state.R",
                         "replay_bundle.R")

# Resolve the chain of order steps for a group (e.g. MDS then reversal).
hsms_order_recipe <- function(proj, art_id, map) {
  steps <- list()
  while (!is.null(art_id)) {
    spec <- proj$artifacts[[art_id]]$settings
    if (identical(spec$source, "reversed")) {
      steps <- c(list(list(source = "reversed")), steps)
      art_id <- spec$base_artifact
    } else if (identical(spec$source, "supplied")) {
      steps <- c(list(list(source = "supplied",
                           order = as.list(hsms_to_original(map, "marker", unlist(spec$order))))), steps)
      art_id <- NULL
    } else {
      steps <- c(list(list(source = "mds", settings = spec$settings, seed = spec$seed)), steps)
      art_id <- NULL
    }
  }
  steps
}

hsms_replay_config <- function(proj, map) {
  cur <- function(s, g = NULL) hsms_is_current(proj, s, g)
  completed <- HSMS_GLOBAL_STAGES[vapply(HSMS_GLOBAL_STAGES, cur, logical(1))]
  groups <- list()
  if (cur("grouping")) for (g in hsms_group_ids(proj)) {
    e <- list(label = unname(hsms_group_label(proj, g)))
    if (cur("order", g)) e$order <- hsms_order_recipe(proj, hsms_current_id(proj, "order", g), map)
    if (cur("phase", g)) e$phase <- proj$phase_spec[[g]]
    if (cur("map", g)) e$map <- proj$map_spec[[g]]
    groups[[g]] <- e
  }
  ex <- proj$exclusions
  list(format = "hsmap-studio-replay", created = hsms_now(), project = proj$name,
       family = proj$family, accept_unknown_father = isTRUE(proj$accept_unknown_father),
       seed = proj$settings$app$seed, threads = proj$settings$app$threads,
       exclusions = lapply(seq_len(nrow(ex)), function(i) list(kind = ex$kind[i], id = ex$original[i], reason = ex$reason[i])),
       pairwise = proj$settings$pairwise, filter = proj$settings$filter, group = proj$settings$group,
       display = proj$settings$display, completed = as.list(completed), groups = groups,
       halfsibmap_commit = proj$halfsibmap_commit,
       halfsibmap_git = "https://github.com/mmollina/HalfSibMap.git")
}

# Standardised comparison outputs (written by the app as expected/ and by
# replay.R as outputs/).
hsms_write_replay_outputs <- function(res, out_dir, display) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  map <- res$mapping
  o <- function(x) hsms_to_original(map, "marker", x)
  if (!is.null(res$tpt_raw)) {
    R <- res$tpt_raw$fit$r; L <- res$tpt_raw$fit$lod_r
    ut <- which(upper.tri(R), arr.ind = TRUE)
    hsms_write_csv(data.frame(marker_1 = o(rownames(R)[ut[, 1]]), marker_2 = o(colnames(R)[ut[, 2]]),
                              r = R[ut], lod_r = L[ut]), file.path(out_dir, "two_point_pairs.csv"))
  }
  if (!is.null(res$tpt_filtered))
    hsms_write_csv(data.frame(marker_id = o(res$tpt_filtered$markers)), file.path(out_dir, "filtered_markers.csv"))
  if (!is.null(res$grouping))
    hsms_write_csv(data.frame(marker_id = o(names(res$grouping$groups)), group = unname(res$grouping$groups)),
                   file.path(out_dir, "groups.csv"))
  rows <- function(f) { l <- Filter(Negate(is.null), lapply(names(res$groups), f)); if (length(l)) do.call(rbind, l) else NULL }
  ord <- rows(function(g) { x <- res$groups[[g]]$order; if (is.null(x)) NULL else
    data.frame(group = g, rank = seq_along(x$order), marker_id = o(x$order)) })
  if (!is.null(ord)) hsms_write_csv(ord, file.path(out_dir, "orders.csv"))
  ph <- rows(function(g) { x <- res$groups[[g]]$phase; if (is.null(x) || !nrow(x$summary$intervals)) NULL else
    data.frame(group = g, interval = x$summary$intervals$interval, phase = x$summary$intervals$phase) })
  if (!is.null(ph)) hsms_write_csv(ph, file.path(out_dir, "phase.csv"))
  pos <- rows(function(g) { x <- res$groups[[g]]$map; if (is.null(x)) NULL else {
    s <- hsms_map_summary(x, display$map_function, display$gap_r, list(mapping = map))
    data.frame(group = g, marker_id = s$positions$marker_id, phase_block = s$positions$phase_block,
               segment_id = s$positions$segment_id, position_cM = s$positions$position_cM) } })
  if (!is.null(pos)) hsms_write_csv(pos, file.path(out_dir, "positions.csv"))
  iv <- rows(function(g) { x <- res$groups[[g]]$map; if (is.null(x)) NULL else {
    s <- hsms_map_summary(x, display$map_function, display$gap_r, list(mapping = map))
    data.frame(group = g, from = s$intervals$from_marker, to = s$intervals$to_marker, r = s$intervals$r,
               distance_cM = s$intervals$distance_cM, status = s$intervals$status) } })
  if (!is.null(iv)) hsms_write_csv(iv, file.path(out_dir, "intervals.csv"))
  ft <- rows(function(g) { x <- res$groups[[g]]$map; if (is.null(x)) NULL else
    data.frame(group = g, model = x$model, block = x$fit_table$block, status = x$fit_table$status,
               converged = x$fit_table$converged, iters = x$fit_table$iters, objective = x$fit_table$objective) })
  if (!is.null(ft)) hsms_write_csv(ft, file.path(out_dir, "fits.csv"))
  invisible(out_dir)
}

# Collect app results in the same shape replay.R produces.
hsms_app_results_for_replay <- function(proj, dir, cache = NULL) {
  ld <- function(s, g = NULL) if (hsms_is_current(proj, s, g)) hsms_load_current(proj, dir, s, g, cache) else NULL
  imp <- ld("import")
  res <- list(mapping = imp$mapping, tpt_raw = ld("tpt_raw")$raw, tpt_filtered = ld("tpt_filtered")$raw,
              grouping = ld("grouping")$raw, groups = list())
  if (!is.null(res$grouping)) for (g in hsms_group_ids(proj))
    res$groups[[g]] <- list(order = ld("order", g), phase = ld("phase", g), map = ld("map", g))
  res
}

hsms_export_replay_bundle <- function(proj, dir, zip_path, cache = NULL) {
  if (!hsms_is_current(proj, "import") || is.null(proj$inputs))
    stop("A current import is required to build a replay bundle.")
  stage <- tempfile("hsms_replay_"); root <- file.path(stage, "replay")
  dir.create(file.path(root, "helpers"), recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  app_dir <- hsms_app_dir()
  for (f in HSMS_REPLAY_HELPERS) file.copy(file.path(app_dir, "R", f), file.path(root, "helpers", f))
  for (f in c("replay.R", "setup.R", "compare.R", "README.md"))
    file.copy(file.path(app_dir, "inst", "replay", f), file.path(root, f))
  dir.create(file.path(root, "inputs")); dir.create(file.path(root, "metadata"))
  file.copy(file.path(dir, proj$inputs$pedigree$file), file.path(root, "inputs", "pedigree.csv"))
  file.copy(file.path(dir, proj$inputs$genotypes$file), file.path(root, "inputs", "genotypes.csv"))
  imp <- hsms_load_current(proj, dir, "import", cache = cache)
  hsms_write_csv(imp$mapping, file.path(root, "metadata", "identifier_mapping.csv"))
  hsms_write_csv(proj$exclusions, file.path(root, "metadata", "exclusions.csv"))
  cfg <- hsms_replay_config(proj, imp$mapping)
  cfg$input_sha256 <- list(pedigree = proj$inputs$pedigree$sha256, genotypes = proj$inputs$genotypes$sha256)
  hsms_write_json(cfg, file.path(root, "settings.json"))
  hsms_write_replay_outputs(hsms_app_results_for_replay(proj, dir, cache), file.path(root, "expected"),
                            proj$settings$display)
  hsms_write_json(hsms_versions(), file.path(root, "metadata", "versions.json"))
  if (file.exists(zip_path)) unlink(zip_path)
  zip::zip(zip_path, files = "replay", root = stage, mode = "mirror")
  invisible(zip_path)
}
