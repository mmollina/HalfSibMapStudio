# Project state: a plain, serialisable list (never reactive values, process
# handles or environments). All functions here are pure: they take a project
# and return a modified project.
#
# Staleness model. Every stage has an EXPECTED signature computed recursively
# from the applied settings and inputs only:
#   import       <- input checksums, father policy
#   selection    <- import, family, exclusion table
#   tpt_raw      <- selection, pairwise settings
#   tpt_filtered <- tpt_raw, filter settings
#   grouping     <- tpt_filtered, group settings
#   order[g]     <- grouping, g, the group's order spec (MDS settings+seed,
#                   supplied order, or reversal of a base order)
#   phase[g]     <- order[g], the group's phase settings
#   map[g]       <- phase[g], the group's model and model settings
# An artifact is CURRENT iff its recorded signature equals the expected
# signature now. Any upstream change therefore makes every descendant
# outdated automatically, and display-only state (labels, gap threshold for
# summaries, map function, table sorting) never enters a signature.

HSMS_SCHEMA_VERSION <- 1L
HSMS_GLOBAL_STAGES <- c("import", "selection", "tpt_raw", "tpt_filtered", "grouping")
HSMS_GROUP_STAGES <- c("order", "phase", "map")
HSMS_STAGE_LABELS <- c(import = "Import", selection = "Family and exclusions",
                       tpt_raw = "Two-point analysis", tpt_filtered = "Two-point filter",
                       grouping = "Linkage groups", order = "Marker order",
                       phase = "Maternal phase", map = "Map fit", export = "Export")
HSMS_PARENT <- c(selection = "import", tpt_raw = "selection", tpt_filtered = "tpt_raw",
                 grouping = "tpt_filtered", order = "grouping", phase = "order", map = "phase")

hsms_new_project <- function(name, defaults = hsms_defaults(), id = hsms_id("p")) {
  list(
    schema_version = HSMS_SCHEMA_VERSION,
    id = id, name = name, created = hsms_now(), modified = hsms_now(),
    app_version = hsms_app_version(),
    halfsibmap_commit = hsms_pinned_halfsibmap_commit(),
    settings = list(
      app = list(threads = as.integer(defaults$app$threads), seed = as.integer(defaults$app$seed)),
      pairwise = defaults$pairwise, filter = defaults$filter, group = defaults$group,
      display = defaults$display),
    inputs = NULL,
    accept_unknown_father = FALSE,
    family = NULL,
    exclusions = hsms_empty_exclusions(),
    exclusion_revision = 0L,
    group_labels = character(0),
    order_spec = list(), phase_spec = list(), map_spec = list(),
    artifacts = list(),
    current = list(order = list(), phase = list(), map = list()),
    last_outcome = list(),
    jobs = list(),
    exports = list(),
    validation = NULL,
    demo = FALSE
  )
}

hsms_scope_key <- function(stage, scope = NULL) {
  if (stage %in% HSMS_GROUP_STAGES) paste0(stage, ":", scope) else stage
}

hsms_current_id <- function(proj, stage, scope = NULL) {
  if (stage %in% HSMS_GROUP_STAGES) proj$current[[stage]][[as.character(scope)]] %||% NULL
  else proj$current[[stage]] %||% NULL
}

hsms_set_current <- function(proj, stage, scope, id) {
  if (stage %in% HSMS_GROUP_STAGES) {
    if (is.null(proj$current[[stage]])) proj$current[[stage]] <- list()
    proj$current[[stage]][[as.character(scope)]] <- id
  } else proj$current[[stage]] <- id
  proj
}

hsms_exclusion_signature <- function(ex) {
  if (!nrow(ex)) return("none")
  ex <- ex[order(ex$kind, ex$internal), c("kind", "internal")]
  hsms_signature(as.list(ex))
}

# Expected signature for a stage/scope under the current applied state, or NA
# if a prerequisite is not specified yet.
hsms_expected_signature <- function(proj, stage, scope = NULL) {
  commit <- proj$halfsibmap_commit
  if (stage == "import") {
    if (is.null(proj$inputs)) return(NA_character_)
    return(hsms_signature(list("import", proj$inputs$pedigree$sha256, proj$inputs$genotypes$sha256,
                               isTRUE(proj$accept_unknown_father), commit)))
  }
  parent <- HSMS_PARENT[[stage]]
  ps <- hsms_expected_signature(proj, parent, scope)
  if (is.na(ps)) return(NA_character_)
  switch(stage,
    selection = if (is.null(proj$family)) NA_character_ else
      hsms_signature(list("selection", ps, proj$family, hsms_exclusion_signature(proj$exclusions))),
    tpt_raw = hsms_signature(list("tpt_raw", ps, hsms_clean_settings("pairwise", proj$settings$pairwise))),
    tpt_filtered = hsms_signature(list("tpt_filtered", ps, hsms_clean_settings("filter", proj$settings$filter))),
    grouping = hsms_signature(list("grouping", ps, hsms_clean_settings("group", proj$settings$group))),
    order = {
      spec <- proj$order_spec[[as.character(scope)]]
      if (is.null(spec)) NA_character_ else hsms_signature(list("order", ps, as.character(scope), spec))
    },
    phase = {
      spec <- proj$phase_spec[[as.character(scope)]]
      if (is.null(spec)) NA_character_ else hsms_signature(list("phase", ps, spec))
    },
    map = {
      spec <- proj$map_spec[[as.character(scope)]]
      if (is.null(spec)) NA_character_ else hsms_signature(list("map", ps, spec))
    },
    stop("Unknown stage ", stage))
}

hsms_artifact_meta <- function(proj, id) if (is.null(id)) NULL else proj$artifacts[[id]]

hsms_is_current <- function(proj, stage, scope = NULL) {
  id <- hsms_current_id(proj, stage, scope)
  if (is.null(id)) return(FALSE)
  meta <- proj$artifacts[[id]]
  if (is.null(meta)) return(FALSE)
  exp <- hsms_expected_signature(proj, stage, scope)
  !is.na(exp) && identical(meta$signature, exp)
}

# Stage status: not_run, ready, blocked, complete, complete_with_warnings,
# partial, outdated, failed, cancelled. (running/cancelling come from the job
# layer.)
hsms_stage_status <- function(proj, stage, scope = NULL) {
  if (stage != "import") {
    parent <- HSMS_PARENT[[stage]]
    if (!hsms_is_current(proj, parent, scope)) {
      id <- hsms_current_id(proj, stage, scope)
      return(if (is.null(id)) "blocked" else "outdated")
    }
  }
  id <- hsms_current_id(proj, stage, scope)
  oc <- proj$last_outcome[[hsms_scope_key(stage, scope)]]
  exp <- hsms_expected_signature(proj, stage, scope)
  if (!is.null(oc) && !is.na(exp) && identical(oc$signature, exp) &&
      oc$status %in% c("failed", "cancelled", "interrupted") &&
      (is.null(id) || !hsms_is_current(proj, stage, scope)))
    return(oc$status)
  if (is.null(id)) return(if (stage == "import" && is.null(proj$inputs)) "not_run" else "ready")
  if (!hsms_is_current(proj, stage, scope)) return("outdated")
  proj$artifacts[[id]]$status %||% "complete"
}

hsms_group_ids <- function(proj) {
  if (!hsms_is_current(proj, "grouping")) {
    id <- hsms_current_id(proj, "grouping")
    if (is.null(id)) return(character(0))
  }
  meta <- proj$artifacts[[hsms_current_id(proj, "grouping")]]
  as.character(seq_len(meta$summary$k %||% 0L))
}

hsms_group_label <- function(proj, g) {
  g <- as.character(g)
  lab <- proj$group_labels[g]
  ifelse(is.na(lab) | !nzchar(lab), paste0("LG", g), lab)
}

# Record a stage outcome that produced no accepted artifact (failure, cancel).
hsms_record_outcome <- function(proj, stage, scope, status, signature, message = NULL, job_id = NULL) {
  proj$last_outcome[[hsms_scope_key(stage, scope)]] <-
    list(status = status, signature = signature, message = message, job_id = job_id, time = hsms_now())
  proj
}

# Register an accepted artifact. `meta` must contain id, stage, scope,
# signature, status, file, sha256.
hsms_register_artifact <- function(proj, meta, make_current = TRUE) {
  proj$artifacts[[meta$id]] <- meta
  if (make_current) {
    proj <- hsms_set_current(proj, meta$stage, meta$scope, meta$id)
    proj$last_outcome[[hsms_scope_key(meta$stage, meta$scope)]] <- NULL
  }
  proj$modified <- hsms_now()
  proj
}

# Decide whether a finished job item may become current: only if the expected
# signature it was launched against is still the expected signature now
# (guards against stale completions, rapid resubmission and late results).
hsms_job_item_is_fresh <- function(proj, stage, scope, launched_signature) {
  exp <- hsms_expected_signature(proj, stage, scope)
  !is.na(exp) && identical(exp, launched_signature)
}

# Applying settings: commit new applied settings for a global stage.
hsms_apply_settings <- function(proj, stage, settings) {
  errs <- hsms_validate_settings(stage, settings)
  if (length(errs)) stop(paste(errs, collapse = " "))
  proj$settings[[stage]] <- hsms_clean_settings(stage, settings)
  proj$modified <- hsms_now()
  proj
}

hsms_settings_differ <- function(a, b, stage) {
  !identical(hsms_canonical(hsms_clean_settings(stage, a)), hsms_canonical(hsms_clean_settings(stage, b)))
}

# Prerequisite message for a stage (for disabled controls).
hsms_prereq_message <- function(proj, stage, scope = NULL) {
  if (is.null(proj)) return("Create or open a project first (stage 1).")
  if (stage == "import") return(NULL)
  parent <- HSMS_PARENT[[stage]]
  if (hsms_is_current(proj, parent, scope)) return(NULL)
  sprintf("Requires a current %s result. Complete or rerun \"%s\" first.",
          tolower(HSMS_STAGE_LABELS[[parent]]), HSMS_STAGE_LABELS[[parent]])
}

# All descendants that are currently outdated (for history messages).
hsms_outdated_list <- function(proj) {
  out <- character(0)
  for (s in HSMS_GLOBAL_STAGES) {
    if (!is.null(hsms_current_id(proj, s)) && !hsms_is_current(proj, s)) out <- c(out, HSMS_STAGE_LABELS[[s]])
  }
  for (s in HSMS_GROUP_STAGES) for (g in names(proj$current[[s]])) {
    if (!hsms_is_current(proj, s, g)) out <- c(out, paste0(HSMS_STAGE_LABELS[[s]], " (", hsms_group_label(proj, g), ")"))
  }
  out
}
