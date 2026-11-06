# High-level project actions shared by the Shiny server, scripts and tests.
# Cheap actions run in-process; heavy stages go through the job layer.

# Store new raw inputs; previous analysis becomes outdated via signatures.
hsms_action_set_inputs <- function(proj, project_dir, ped_path, geno_path, ped_name, geno_name,
                                   accept_unknown_father = FALSE) {
  proj <- hsms_store_inputs(proj, project_dir, ped_path, geno_path, ped_name, geno_name)
  proj$accept_unknown_father <- isTRUE(accept_unknown_father)
  proj$validation <- NULL
  hsms_log_event(project_dir, "inputs_stored", list(pedigree = ped_name, genotypes = geno_name,
                                                    pedigree_sha256 = proj$inputs$pedigree$sha256,
                                                    genotypes_sha256 = proj$inputs$genotypes$sha256))
  proj
}

hsms_action_select_family <- function(proj, project_dir, family, cache = NULL) {
  imp <- hsms_load_current(proj, project_dir, "import", cache = cache)
  if (is.null(imp) || !hsms_is_current(proj, "import")) stop("Import the data first.")
  fam <- imp$families[imp$families$family_id == family, ]
  if (nrow(fam) != 1L) stop("Unknown family.")
  if (!isTRUE(fam$analyzable)) stop("Family ", family, " cannot be analysed: ", fam$notes)
  if (!identical(proj$family, family)) {
    proj$family <- family
    proj$exclusions <- hsms_empty_exclusions()
    proj$exclusion_revision <- proj$exclusion_revision + 1L
    hsms_log_event(project_dir, "family_selected", list(family = family))
  }
  hsms_action_apply_selection(proj, project_dir, cache)
}

# (Re)build the selection artifact for the applied family + exclusions.
hsms_action_apply_selection <- function(proj, project_dir, cache = NULL) {
  imp <- hsms_load_current(proj, project_dir, "import", cache = cache)
  sig <- hsms_expected_signature(proj, "selection")
  if (hsms_is_current(proj, "selection")) return(proj)
  t0 <- proc.time()[["elapsed"]]
  sel <- hsms_select_family(imp, proj$family, proj$exclusions)
  qc <- hsms_qc_summary(imp, proj$family, sel)
  # The selection object itself (plus QC) is the payload, so adapters can use
  # it directly (sel$data is the single-family HSMap.data).
  payload <- c(sel, list(qc = qc))
  meta <- hsms_make_meta(proj, "selection", NULL, sig,
                         payload_summary = list(family = proj$family, counts = sel$counts),
                         elapsed = proc.time()[["elapsed"]] - t0,
                         settings = list(family = proj$family, n_exclusions = nrow(proj$exclusions)),
                         inputs = list(import = hsms_current_id(proj, "import")),
                         run_id = "in-process")
  meta <- hsms_store_artifact(project_dir, meta, payload)
  proj <- hsms_register_artifact(proj, meta)
  hsms_log_event(project_dir, "selection_applied", list(family = proj$family, counts = sel$counts))
  proj
}

hsms_action_set_exclusions <- function(proj, project_dir, exclusions, cache = NULL) {
  exclusions <- hsms_validate_exclusions(exclusions)
  old <- proj$exclusions
  proj$exclusions <- exclusions
  proj$exclusion_revision <- proj$exclusion_revision + 1L
  hsms_log_event(project_dir, "exclusions_applied",
                 list(n_before = nrow(old), n_after = nrow(exclusions),
                      added = setdiff(exclusions$internal, old$internal),
                      removed = setdiff(old$internal, exclusions$internal)))
  hsms_action_apply_selection(proj, project_dir, cache)
}

# Register an in-process artifact for a group stage (supplied/reversed order).
hsms_action_store_order <- function(proj, project_dir, g, order_payload, spec) {
  g <- as.character(g)
  proj$order_spec[[g]] <- spec
  sig <- hsms_expected_signature(proj, "order", g)
  meta <- hsms_make_meta(proj, "order", g, sig,
                         payload_summary = list(n_markers = length(order_payload$order),
                                                n_removed = length(order_payload$removed),
                                                method = order_payload$method),
                         settings = spec, inputs = list(grouping = hsms_current_id(proj, "grouping")),
                         run_id = "in-process", seed = proj$settings$app$seed)
  meta <- hsms_store_artifact(project_dir, meta, order_payload)
  proj <- hsms_register_artifact(proj, meta)
  hsms_log_event(project_dir, "order_applied", list(group = g, source = spec$source))
  proj
}

hsms_action_reverse_order <- function(proj, project_dir, g, cache = NULL) {
  g <- as.character(g)
  if (!hsms_is_current(proj, "order", g)) stop("Group ", g, " has no current order to reverse.")
  base_id <- hsms_current_id(proj, "order", g)
  base <- hsms_load_artifact(project_dir, proj$artifacts[[base_id]], cache)
  rev <- hsms_reverse_order(base)
  spec <- list(source = "reversed", base_artifact = base_id,
               base_signature = proj$artifacts[[base_id]]$signature)
  hsms_action_store_order(proj, project_dir, g, rev, spec)
}

hsms_action_apply_supplied_orders <- function(proj, project_dir, recon) {
  if (!isTRUE(recon$ok)) stop("The supplied order has unresolved problems.")
  for (g in names(recon$groups)) {
    go <- recon$groups[[g]]
    payload <- list(order = go$order, removed = character(0), method = "supplied",
                    mds_estimated = FALSE, omitted = go$omitted,
                    inclusion_rule = "Only listed markers are used; omitted group members are excluded from this group's map.")
    spec <- list(source = "supplied", order = as.list(go$order), omitted = as.list(go$omitted))
    proj <- hsms_action_store_order(proj, project_dir, g, payload, spec)
  }
  proj
}

# ---- Synchronous runner for scripts/tests (blocks; never used by Shiny) ----

hsms_run_stage_sync <- function(proj, project_dir, stage, items = list(list(scope = NULL)), extra = list(),
                                timeout = 600) {
  req <- hsms_build_request(proj, project_dir, stage, items, extra)
  job <- hsms_job_launch(req)
  job$proc$wait(timeout * 1000)
  if (job$proc$is_alive()) { job <- hsms_job_cancel(job); stop("Job timed out.") }
  job <- hsms_job_poll(job)
  if (job$state == "failed") stop("Worker failed: ", job$worker_error, "\n",
                                  paste(hsms_tail_file(job$log_path), collapse = "\n"))
  man <- hsms_job_manifest(job)
  acc <- hsms_accept_job(proj, project_dir, job, man)
  for (e in acc$events) hsms_log_event(project_dir, e$type, e)
  hsms_cleanup_job_dir(job)
  acc$proj
}

# Specs for group stages, set when the user runs the stage for that group.
hsms_set_order_spec_mds <- function(proj, g, order_settings) {
  proj$order_spec[[as.character(g)]] <- list(source = "mds", settings = hsms_clean_settings("order", order_settings),
                                             seed = as.integer(proj$settings$app$seed))
  proj
}
hsms_set_phase_spec <- function(proj, g, phase_settings) {
  proj$phase_spec[[as.character(g)]] <- hsms_clean_settings("phase", phase_settings)
  proj
}
hsms_set_map_spec <- function(proj, g, model, settings) {
  proj$map_spec[[as.character(g)]] <- list(model = model, settings = hsms_clean_settings(model, settings))
  proj
}

# Build items for group stages.
hsms_group_items <- function(proj, project_dir, stage, groups, cache = NULL) {
  grp <- if (stage == "order") hsms_load_current(proj, project_dir, "grouping", cache = cache) else NULL
  lapply(as.character(groups), function(g) {
    it <- list(scope = g, label = hsms_group_label(proj, g))
    if (stage == "order") it$markers <- hsms_group_members(grp$raw, g)
    if (stage == "phase") it$settings <- proj$phase_spec[[g]]
    if (stage == "map") { it$model <- proj$map_spec[[g]]$model; it$settings <- proj$map_spec[[g]]$settings }
    it
  })
}
