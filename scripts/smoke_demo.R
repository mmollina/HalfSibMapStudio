# Headless end-to-end smoke test of the real workflow on the bundled demo,
# using the same adapters, project state and background job layer as the app.
#   Rscript scripts/smoke_demo.R
local({
  app_dir <- normalizePath(".", winslash = "/")
  if (!file.exists(file.path(app_dir, "R", "core_files.R"))) stop("Run from the repository root.")
  source(file.path(app_dir, "R", "core_files.R"))
  hsms_source_core(app_dir)
  d <- hsms_defaults(app_dir)
  base <- tempfile("hsms_smoke_"); dir.create(base)
  t_all <- proc.time()[["elapsed"]]
  timing <- list()
  step <- function(name, expr) {
    t0 <- proc.time()[["elapsed"]]; v <- force(expr)
    timing[[name]] <<- proc.time()[["elapsed"]] - t0
    cat(sprintf("  %-28s %6.2f s\n", name, timing[[name]])); v
  }
  cr <- hsms_create_project("Smoke demo", base, d)
  proj <- cr$proj; dir <- cr$dir
  ped  <- system.file("extdata", "example_pedigree.csv",  package = "HalfSibMap")
  geno <- system.file("extdata", "example_genotypes.csv", package = "HalfSibMap")
  proj <- hsms_action_set_inputs(proj, dir, ped, geno, "example_pedigree.csv", "example_genotypes.csv")
  cache <- hsms_new_cache()
  proj <- step("import (worker)", hsms_run_stage_sync(proj, dir, "import"))
  stopifnot(hsms_is_current(proj, "import"))
  proj <- step("select family MOM1", hsms_action_select_family(proj, dir, d$demo$family, cache))
  proj <- step("two-point (worker)", hsms_run_stage_sync(proj, dir, "tpt_raw"))
  proj <- hsms_apply_settings(proj, "filter", modifyList(d$filter, list(lod = 3)))
  proj <- step("filter (worker)", hsms_run_stage_sync(proj, dir, "tpt_filtered"))
  proj <- hsms_apply_settings(proj, "group", modifyList(d$group, list(k = d$demo$k)))
  proj <- step("grouping (worker)", hsms_run_stage_sync(proj, dir, "grouping"))
  groups <- hsms_group_ids(proj)
  for (g in groups) proj <- hsms_set_order_spec_mds(proj, g, d$order)
  proj <- step("MDS order (worker)", hsms_run_stage_sync(proj, dir, "order",
                                         hsms_group_items(proj, dir, "order", groups, cache),
                                         extra = list(order_settings = d$order)))
  for (g in groups) proj <- hsms_set_phase_spec(proj, g, d$phase)
  proj <- step("phase (worker)", hsms_run_stage_sync(proj, dir, "phase", hsms_group_items(proj, dir, "phase", groups)))
  for (g in groups) proj <- hsms_set_map_spec(proj, g, "source_aware", d$source_aware)
  proj <- step("source-aware map (worker)", hsms_run_stage_sync(proj, dir, "map", hsms_group_items(proj, dir, "map", groups)))
  for (g in groups) stopifnot(hsms_stage_status(proj, "map", g) %in% c("complete", "complete_with_warnings"))
  fit <- hsms_load_current(proj, dir, "map", "1")
  imp <- hsms_load_current(proj, dir, "import")
  s <- hsms_map_summary(fit, "haldane", 0.499, list(mapping = imp$mapping, group_label = "LG1"))
  cat(sprintf("Map: %d markers, linked length %.2f cM, %d gaps, converged=%s\n",
              nrow(s$positions), s$linked_length, s$n_gaps, fit$fit_table$converged))
  hsms_save_project(proj, dir)
  re <- hsms_open_project(dir)
  stopifnot(length(re$problems) == 0L, hsms_is_current(re$proj, "map", "1"))
  cat("Reopen: all stages current without recomputation.\n")
  cat(sprintf("Total %.1f s\n", proc.time()[["elapsed"]] - t_all))
  invisible(list(proj = proj, dir = dir, timing = timing))
})
