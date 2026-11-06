# Result collection and export (tables, native objects, figures, provenance).
# Tables use original identifiers, full precision, and explicit NA.

hsms_artifact_label <- function(proj, stage, scope = NULL) {
  st <- hsms_stage_status(proj, stage, scope)
  if (st %in% c("complete", "complete_with_warnings", "partial")) st else if (st == "outdated") "historical" else st
}

# Collect everything that exists for one group. include_outdated controls
# whether outdated artifacts are returned (labelled historical).
hsms_group_result <- function(proj, dir, g, cache = NULL, include_outdated = TRUE) {
  g <- as.character(g)
  get <- function(stage) {
    id <- hsms_current_id(proj, stage, g)
    if (is.null(id)) return(NULL)
    cur <- hsms_is_current(proj, stage, g)
    if (!cur && !include_outdated) return(NULL)
    list(payload = hsms_load_artifact(dir, proj$artifacts[[id]], cache), meta = proj$artifacts[[id]],
         current = cur, label = if (cur) proj$artifacts[[id]]$status else "historical")
  }
  list(group = g, label = hsms_group_label(proj, g), order = get("order"), phase = get("phase"), map = get("map"))
}

hsms_export_status <- function(labels) {
  labels <- unlist(labels)
  if (!length(labels)) return("empty")
  if (any(labels == "historical")) return("includes-historical")
  if (any(labels %in% c("partial", "failed"))) return("partial")
  if (any(labels == "complete_with_warnings")) return("complete-with-warnings")
  "complete"
}

hsms_export_results <- function(proj, dir, zip_path, cache = NULL, include_historical = FALSE,
                                include_figures = TRUE, include_posteriors = TRUE,
                                posterior_max_rows = 2e6, include_pairwise_matrices = FALSE) {
  stage_dir <- tempfile("hsms_export_"); dir.create(stage_dir)
  on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)
  P <- function(...) file.path(stage_dir, ...)
  for (s in c("tables", "native", "figures", "provenance", "settings")) dir.create(P(s))
  labels <- list(); notes <- character(0)
  files_note <- list()
  load <- function(stage, scope = NULL) {
    id <- hsms_current_id(proj, stage, scope)
    if (is.null(id)) return(NULL)
    cur <- hsms_is_current(proj, stage, scope)
    if (!cur && !include_historical) {
      notes <<- c(notes, sprintf("%s%s is outdated and was not exported (enable historical export to include it).",
                                 HSMS_STAGE_LABELS[[stage]], if (is.null(scope)) "" else paste0(" [", hsms_group_label(proj, scope), "]")))
      return(NULL)
    }
    labels[[hsms_scope_key(stage, scope)]] <<- if (cur) proj$artifacts[[id]]$status else "historical"
    list(p = hsms_load_artifact(dir, proj$artifacts[[id]], cache), meta = proj$artifacts[[id]], current = cur)
  }
  imp <- load("import")
  map <- if (!is.null(imp)) imp$p$mapping else NULL
  if (!is.null(proj$validation$issues)) hsms_write_csv(proj$validation$issues, P("tables", "validation_report.csv"))
  if (!is.null(imp)) {
    hsms_write_csv(imp$p$families, P("tables", "family_summary.csv"))
    hsms_write_csv(map, P("provenance", "identifier_mapping.csv"))
  }
  sel <- load("selection")
  if (!is.null(sel)) {
    ex <- proj$exclusions
    ex <- rbind(ex, if (nrow(sel$p$excluded_markers_auto))
      data.frame(kind = "marker", internal = sel$p$excluded_markers_auto$internal,
                 original = sel$p$excluded_markers_auto$original,
                 reason = sel$p$excluded_markers_auto$reason, source = "automatic (informativeness)",
                 applied_at = sel$meta$created, stringsAsFactors = FALSE))
    names(ex)[names(ex) == "original"] <- "id"
    hsms_write_csv(ex[, c("kind", "id", "reason", "source", "applied_at")], P("tables", "exclusions.csv"))
    hsms_write_csv(sel$p$qc$offspring[, setdiff(names(sel$p$qc$offspring), "internal")], P("tables", "qc_offspring.csv"))
    hsms_write_csv(sel$p$qc$markers[, setdiff(names(sel$p$qc$markers), "internal")], P("tables", "qc_markers.csv"))
  }
  orig <- function(x) if (is.null(map)) x else hsms_to_original(map, "marker", x)
  tr <- load("tpt_raw")
  if (!is.null(tr)) {
    saveRDS(tr$p$raw, P("native", "two_point_raw_HSMap.tpt.rds"))
    if (include_figures) {
      d <- hsms_pairwise_distribution(tr$p$raw)
      hsms_save_figure(hsms_plot_rf_hist(d), P("figures", "two_point_rf_distribution"))
      hsms_save_figure(hsms_plot_lod_hist(d), P("figures", "two_point_lod_distribution"), width = 9)
    }
    if (include_pairwise_matrices) {
      for (w in c("r", "lod_r", "lod_ph")) {
        M <- tr$p$raw$fit[[w]]; dimnames(M) <- list(orig(rownames(M)), orig(colnames(M)))
        hsms_write_csv(cbind(marker_id = rownames(M), as.data.frame(M, check.names = FALSE)),
                       P("tables", paste0("two_point_matrix_", w, ".csv")))
      }
    }
  }
  tf <- load("tpt_filtered")
  if (!is.null(tf)) {
    saveRDS(tf$p$raw, P("native", "two_point_filtered_HSMap.tpt.rds"))
    fm <- data.frame(marker_id = orig(c(tf$p$kept, tf$p$excluded)),
                     filter_status = c(rep("retained", length(tf$p$kept)), rep("removed by filter", length(tf$p$excluded))),
                     surviving_pairs = unname(tf$p$row_counts[c(tf$p$kept, tf$p$excluded)]),
                     stringsAsFactors = FALSE)
    hsms_write_csv(fm, P("tables", "two_point_filter.csv"))
    if (include_figures)
      hsms_save_figure(hsms_plot_filter_counts(tf$p$row_counts, tf$p$kept), P("figures", "two_point_filter_counts"))
  }
  gr <- load("grouping")
  groups <- character(0)
  if (!is.null(gr)) {
    saveRDS(gr$p$raw, P("native", "grouping_hsmap_group.rds"))
    gv <- gr$p$raw$groups
    ann <- if (!is.null(imp)) imp$p$marker_annotation else NULL
    ga <- data.frame(marker_id = orig(names(gv)), group = unname(gv),
                     group_label = hsms_group_label(proj, unname(gv)),
                     supplied_chrom = if (!is.null(ann)) ann$chrom[match(names(gv), ann$marker_internal)] else NA,
                     supplied_position = if (!is.null(ann)) ann$position[match(names(gv), ann$marker_internal)] else NA,
                     stringsAsFactors = FALSE)
    hsms_write_csv(ga, P("tables", "group_assignments.csv"))
    groups <- as.character(seq_len(gr$p$raw$k))
    if (include_figures)
      hsms_save_figure(function() hsms_draw_dendrogram(gr$p$raw), P("figures", "grouping_dendrogram"), width = 9, height = 5)
  }
  ord_rows <- list(); phm <- list(); phi <- list(); pos <- list(); ivs <- list(); fits <- list(); summaries <- list()
  disp <- proj$settings$display
  for (g in groups) {
    lab <- hsms_group_label(proj, g)
    o <- load("order", g)
    if (!is.null(o)) {
      ord_rows[[g]] <- data.frame(linkage_group = lab, group = g, rank = seq_along(o$p$order),
                                  marker_id = orig(o$p$order), order_source = o$p$method,
                                  result_label = labels[[hsms_scope_key("order", g)]], stringsAsFactors = FALSE)
      if (length(o$p$removed))
        ord_rows[[paste0(g, "_rm")]] <- data.frame(linkage_group = lab, group = g, rank = NA_integer_,
                                                   marker_id = orig(o$p$removed), order_source = "removed by MDS envelope",
                                                   result_label = labels[[hsms_scope_key("order", g)]], stringsAsFactors = FALSE)
      if (!is.null(o$p$omitted) && length(o$p$omitted))
        ord_rows[[paste0(g, "_om")]] <- data.frame(linkage_group = lab, group = g, rank = NA_integer_,
                                                   marker_id = orig(unlist(o$p$omitted)), order_source = "omitted from supplied order",
                                                   result_label = labels[[hsms_scope_key("order", g)]], stringsAsFactors = FALSE)
    }
    ph <- load("phase", g)
    if (!is.null(ph)) {
      if (!is.null(ph$p$raw)) saveRDS(ph$p$raw, P("native", sprintf("phase_%s_HSMap.phased.rds", hsms_safe_filename(lab))))
      m <- ph$p$summary$markers; m$marker <- orig(m$marker)
      phm[[g]] <- cbind(linkage_group = lab, m, result_label = labels[[hsms_scope_key("phase", g)]])
      iv <- ph$p$summary$intervals
      if (nrow(iv)) {
        iv$from <- orig(iv$from); iv$to <- orig(iv$to)
        phi[[g]] <- cbind(linkage_group = lab, iv, result_label = labels[[hsms_scope_key("phase", g)]])
        if (include_figures) hsms_save_figure(hsms_plot_phase(ph$p$summary$intervals),
                                              P("figures", sprintf("phase_%s", hsms_safe_filename(lab))), height = 2.6)
      }
    }
    mp <- load("map", g)
    if (!is.null(mp)) {
      fit <- mp$p
      saveRDS(fit$raw, P("native", sprintf("map_%s_%s.rds", hsms_safe_filename(lab),
                                            if (fit$model == "source_aware") "HSMap.sourceaware" else "HSMap.map.blocks")))
      ctx <- list(family = proj$family, mother = if (!is.null(sel)) sel$p$mother else NA, group_label = lab,
                  run_id = mp$meta$run_id, mapping = map)
      s <- hsms_map_summary(fit, disp$map_function, disp$gap_r, ctx)
      rl <- labels[[hsms_scope_key("map", g)]]
      summaries[[g]] <- s
      pos[[g]] <- cbind(s$positions, result_label = rl)
      ivs[[g]] <- cbind(s$intervals, result_label = rl)
      fits[[g]] <- cbind(s$fit_summary, result_label = rl,
                         settings_json = as.character(jsonlite::toJSON(mp$meta$settings, auto_unbox = TRUE, digits = NA)))
      if (include_figures) {
        hsms_save_figure(hsms_plot_map_group(s), P("figures", sprintf("map_%s", hsms_safe_filename(lab))),
                         height = 2 + 1.2 * s$n_segments)
        hsms_save_figure(hsms_plot_intervals(s), P("figures", sprintf("intervals_%s", hsms_safe_filename(lab))))
      }
      if (include_posteriors) {
        nrows <- length(fit$offspring) * length(fit$markers)
        if (nrows <= posterior_max_rows) {
          hsms_write_csv(hsms_posterior_slice(fit, map, what = "maternal"),
                         P("tables", sprintf("inheritance_maternal_%s.csv", hsms_safe_filename(lab))))
          if (fit$model == "source_aware")
            hsms_write_csv(hsms_posterior_slice(fit, map, what = "sharing"),
                           P("tables", sprintf("paternal_sharing_%s.csv", hsms_safe_filename(lab))))
        } else notes <- c(notes, sprintf("Posterior table for %s (%d rows) exceeds the export limit; the full posteriors are in native/.", lab, nrows))
      }
    }
  }
  bind <- function(l) if (length(l)) do.call(rbind, unname(l)) else NULL
  if (length(ord_rows)) hsms_write_csv(bind(ord_rows), P("tables", "marker_orders.csv"))
  if (length(phm)) hsms_write_csv(bind(phm), P("tables", "phase_markers.csv"))
  if (length(phi)) hsms_write_csv(bind(phi), P("tables", "phase_intervals.csv"))
  if (length(pos)) hsms_write_csv(bind(pos), P("tables", "marker_positions.csv"))
  if (length(ivs)) hsms_write_csv(bind(ivs), P("tables", "adjacent_intervals.csv"))
  if (length(fits)) hsms_write_csv(bind(fits), P("tables", "fit_summary.csv"))
  if (include_figures && length(summaries)) {
    ov <- hsms_plot_map_overview(summaries)
    if (!is.null(ov)) hsms_save_figure(ov, P("figures", "map_overview"), width = max(6, 2 + 1.5 * length(summaries)))
  }
  # settings and provenance
  hsms_write_json(hsms_settings_export(proj), P("settings", "applied_settings.json"))
  hsms_write_json(hsms_versions(), P("provenance", "versions.json"))
  writeLines(utils::capture.output(utils::sessionInfo()), P("provenance", "sessionInfo.txt"))
  hsms_write_json(proj$inputs, P("provenance", "inputs.json"))
  ev <- file.path(dir, "history", "events.jsonl")
  if (file.exists(ev)) file.copy(ev, P("provenance", "events.jsonl"))
  logs <- list.files(file.path(dir, "history", "logs"), full.names = TRUE)
  if (length(logs)) { dir.create(P("provenance", "job_logs")); file.copy(logs, P("provenance", "job_logs")) }
  status <- hsms_export_status(labels)
  warn_rows <- Filter(function(m) length(m$warnings) > 0L, proj$artifacts)
  manifest <- list(format = "hsmap-studio-results", created = hsms_now(), project = proj$name,
                   family = proj$family %||% NA, export_status = status,
                   result_labels = labels, notes = as.list(notes),
                   warnings = lapply(warn_rows, function(m) list(stage = m$stage, scope = m$scope %||% NA,
                                                                 warnings = m$warnings)),
                   units = list(position_cM = "centiMorgan within segment (restarts after each gap)",
                                distance_cM = "centiMorgan; NA for gaps", r = "recombination fraction"),
                   versions = hsms_versions())
  hsms_write_json(manifest, P("manifest.json"))
  writeLines(hsms_results_readme(status, notes), P("README.txt"))
  files <- list.files(stage_dir, recursive = TRUE)
  if (file.exists(zip_path)) unlink(zip_path)
  zip::zip(zip_path, files = files, root = stage_dir, mode = "mirror")
  invisible(list(path = zip_path, status = status, notes = notes))
}

hsms_settings_export <- function(proj) {
  list(project = proj$name, family = proj$family, accept_unknown_father = proj$accept_unknown_father,
       app = proj$settings$app, pairwise = proj$settings$pairwise,
       filter = c(proj$settings$filter, list(effective_thresh.LOD.ph = proj$settings$filter$lod,
                                             effective_thresh.LOD.rf = proj$settings$filter$lod)),
       group = proj$settings$group, display = proj$settings$display,
       group_labels = as.list(proj$group_labels),
       order = proj$order_spec, phase = proj$phase_spec, map = proj$map_spec,
       exclusions = proj$exclusions,
       halfsibmap_commit = proj$halfsibmap_commit)
}

hsms_results_readme <- function(status, notes) {
  c("HalfSibMap Studio results export",
    "================================",
    "",
    sprintf("Export status: %s", status),
    "  complete               all exported results are current and complete",
    "  complete-with-warnings current results, some with warnings (e.g. non-converged fits)",
    "  partial                some groups/blocks failed or were not fitted",
    "  includes-historical    contains OUTDATED results, labelled 'historical' in result_label columns",
    "",
    "tables/      CSV tables with original identifiers and full numeric precision; NA = not available.",
    "             marker_positions.csv: position_cM restarts at 0 in every segment; gaps have no distance.",
    "             adjacent_intervals.csv: status is linked, no_linkage_boundary, unresolved_phase,",
    "             or insufficient_information; distance_cM is NA for every non-linked interval.",
    "native/      unmodified HalfSibMap result objects (RDS; read with readRDS()).",
    "figures/     PNG (300 dpi) and PDF versions of the figures.",
    "settings/    applied stage-specific settings, per-group order/phase/model specifications.",
    "provenance/  versions, session info, input checksums, identifier mapping, history and job logs.",
    "",
    "Maternal homolog labels ('homolog 1') are statistical labels with arbitrary orientation within",
    "each resolved phase block; they are not comparable across blocks and do not identify a",
    "grandparent. Paternal sharing states are a statistical description of haplotype-sharing",
    "patterns; they do not identify fathers or prove identity by descent.",
    if (length(notes)) c("", "Notes:", paste("-", notes)))
}
