# Application server: session-scoped context, navigation, autosave, and the
# background job lifecycle. Nothing mutable is stored globally.

hsms_combine_status <- function(st) {
  st <- unlist(st)
  if (!length(st)) return("blocked")
  for (s in c("running", "cancelling", "failed", "interrupted", "cancelled", "outdated")) if (any(st == s)) return(s)
  done <- st %in% c("complete", "complete_with_warnings", "partial")
  if (all(done)) {
    if (any(st == "partial")) return("partial")
    if (any(st == "complete_with_warnings")) return("complete_with_warnings")
    return("complete")
  }
  first <- st[!done][1]
  if (first %in% c("ready", "not_run")) return("ready")
  if (any(done)) return("ready")
  "blocked"
}

hsms_server <- function(input, output, session) {
  defaults <- hsms_defaults()
  ctx <- new.env(parent = emptyenv())
  ctx$session <- session
  ctx$defaults <- defaults
  ctx$proj <- shiny::reactiveVal(NULL)
  ctx$proj_id <- shiny::reactiveVal("")      # changes only when another project is opened
  ctx$dir <- shiny::reactiveVal(NULL)
  ctx$cache <- hsms_new_cache()
  ctx$save_state <- shiny::reactiveVal("No project open")
  ctx$job_rv <- shiny::reactiveVal(NULL)       # id of the active job (or NULL)
  ctx$job_view <- shiny::reactiveVal(NULL)     # display info for the active job
  ctx$jobs_env <- new.env(parent = emptyenv()) # process handle of the owned job
  ctx$events_tick <- shiny::reactiveVal(0)
  ctx$demo_chain <- shiny::reactiveVal(NULL)
  ctx$after_accept <- list()                   # stage -> callback(proj) set by modules
  ctx$projects_dir <- hsms_projects_dir(defaults)
  ctx$token <- session$token

  shiny::observe({ p <- ctx$proj(); ctx$proj_id(if (is.null(p)) "" else p$id) })

  ctx$notify <- function(msg, type = "message", duration = 6) {
    shiny::showNotification(msg, type = type, duration = duration, session = session)
  }
  ctx$log <- function(type, details = list()) {
    d <- shiny::isolate(ctx$dir())
    if (!is.null(d)) tryCatch(hsms_log_event(d, type, details), error = function(e) NULL)
    ctx$events_tick(shiny::isolate(ctx$events_tick()) + 1)
  }
  # Commit a new project state and autosave the manifest.
  ctx$set_proj <- function(p) {
    ctx$proj(p)
    d <- shiny::isolate(ctx$dir())
    if (is.null(p) || is.null(d)) return(invisible(FALSE))
    ok <- tryCatch({ hsms_save_project(p, d); TRUE }, error = function(e) {
      ctx$save_state(paste("Save failed:", conditionMessage(e))); FALSE })
    if (ok) ctx$save_state(paste("Saved", format(Sys.time(), "%H:%M:%S")))
    invisible(ok)
  }
  ctx$load <- function(stage, scope = NULL) {
    p <- ctx$proj(); d <- ctx$dir()
    if (is.null(p) || is.null(d)) return(NULL)
    tryCatch(hsms_load_current(p, d, stage, scope, ctx$cache), error = function(e) {
      ctx$notify(paste("Could not load", stage, "result:", conditionMessage(e)), "error"); NULL })
  }
  ctx$busy <- function() !is.null(shiny::isolate(ctx$job_rv()))
  ctx$open_project <- function(dir, p, notices = character(0)) {
    hsms_lock_project(dir, ctx$token)
    old <- shiny::isolate(ctx$dir())
    if (!is.null(old) && !identical(old, dir)) hsms_unlock_project(old, ctx$token)
    rm(list = ls(ctx$cache), envir = ctx$cache)
    ctx$dir(dir); ctx$proj(p)
    ctx$set_proj(p)
    for (n in notices) ctx$notify(n, "warning", duration = 15)
    ctx$log("project_opened", list(name = p$name))
  }
  ctx$close_project <- function() {
    if (ctx$busy()) ctx$cancel_job()
    d <- shiny::isolate(ctx$dir()); p <- shiny::isolate(ctx$proj())
    if (!is.null(d) && !is.null(p)) { tryCatch(hsms_save_project(p, d), error = function(e) NULL); hsms_unlock_project(d, ctx$token) }
    ctx$dir(NULL); ctx$proj(NULL); ctx$save_state("No project open")
    rm(list = ls(ctx$cache), envir = ctx$cache)
  }

  # ---- jobs -------------------------------------------------------------------
  ctx$launch <- function(stage, items = list(list(scope = NULL)), extra = list(), label = NULL) {
    if (ctx$busy()) { ctx$notify("A job is already running. Wait for it or cancel it first.", "warning"); return(FALSE) }
    p <- shiny::isolate(ctx$proj()); d <- shiny::isolate(ctx$dir())
    if (is.null(p)) return(FALSE)
    req <- tryCatch(hsms_build_request(p, d, stage, items, extra), error = function(e) e)
    if (inherits(req, "error")) { ctx$notify(conditionMessage(req), "error"); return(FALSE) }
    job <- tryCatch(hsms_job_launch(req), error = function(e) e)
    if (inherits(job, "error")) { ctx$notify(paste("Could not start the worker:", conditionMessage(job)), "error"); return(FALSE) }
    job$label <- label %||% HSMS_STAGE_LABELS[[stage]]
    ctx$jobs_env$active <- job
    p$jobs[[job$id]] <- list(id = job$id, stage = stage, label = job$label, status = "running",
                             started = hsms_now(), threads = req$threads, seed = req$seed,
                             items = lapply(req$items, function(it) list(scope = it$scope, signature = it$signature)))
    ctx$set_proj(p)
    ctx$log("job_started", list(job = job$id, stage = stage, n_items = length(items)))
    ctx$job_rv(job$id)
    session$sendCustomMessage("hsms-busy", list(busy = TRUE))
    TRUE
  }
  ctx$cancel_job <- function() {
    job <- ctx$jobs_env$active
    if (is.null(job)) return(invisible(FALSE))
    ctx$jobs_env$active <- hsms_job_cancel(job)
    ctx$log("job_cancel_requested", list(job = job$id))
    invisible(TRUE)
  }

  finalize_job <- function(job) {
    p <- ctx$proj(); d <- ctx$dir()
    man <- hsms_job_manifest(job)
    acc <- tryCatch(hsms_accept_job(p, d, job, man), error = function(e) e)
    if (inherits(acc, "error")) {
      ctx$notify(paste("Result rejected:", conditionMessage(acc)), "error", duration = 12)
      acc <- list(proj = p, events = list(), accepted = 0L)
    }
    p <- acc$proj
    done_scopes <- vapply(man$items %||% list(), function(it) as.character(it$scope %||% "global"), "")
    for (it in job$req$items) {
      sc <- as.character(it$scope %||% "global")
      if (sc %in% done_scopes) next
      st <- if (job$state == "cancelled") "cancelled" else "failed"
      p <- hsms_record_outcome(p, job$stage, it$scope, st, it$signature,
                               if (st == "failed") job$worker_error %||% "worker failed" else "cancelled by user", job$id)
    }
    n_fail <- sum(vapply(man$items %||% list(), function(it) !isTRUE(it$ok), logical(1)))
    status <- if (job$state == "cancelled") "cancelled" else if (job$state == "failed") "failed"
              else if (n_fail) "completed with failures" else "completed"
    p$jobs[[job$id]]$status <- status
    p$jobs[[job$id]]$finished <- hsms_now()
    p$jobs[[job$id]]$elapsed_sec <- as.numeric(difftime(Sys.time(), job$started, units = "secs"))
    if (!is.null(job$worker_error)) p$jobs[[job$id]]$error <- job$worker_error
    ctx$set_proj(p)
    for (e in acc$events) ctx$log(e$type, e)
    ctx$log(paste0("job_", gsub(" ", "_", status)), list(job = job$id, stage = job$stage))
    hsms_cleanup_job_dir(job)
    ctx$jobs_env$active <- NULL
    ctx$job_view(NULL)
    ctx$job_rv(NULL)
    session$sendCustomMessage("hsms-busy", list(busy = FALSE))
    msg <- sprintf("%s: %s.", job$label, status)
    if (job$state == "failed") msg <- paste(msg, job$worker_error %||% "")
    ctx$notify(msg, if (status == "completed") "message" else if (status == "cancelled") "warning" else "error",
               duration = if (status == "completed") 5 else 12)
    cb <- ctx$after_accept[[job$stage]]
    if (!is.null(cb)) tryCatch(cb(p), error = function(e) NULL)
    demo_continue(status)
  }

  shiny::observe({
    ctx$job_rv()
    job <- ctx$jobs_env$active
    if (is.null(job)) return()
    shiny::invalidateLater(as.integer(defaults$app$poll_ms))
    job <- hsms_job_poll(job)
    ctx$jobs_env$active <- job
    st <- hsms_job_status_file(job)
    ctx$job_view(list(id = job$id, label = job$label, stage = job$stage, state = job$state,
                      message = st$message %||% "Starting worker", done = st$done, total = st$total,
                      elapsed = as.numeric(difftime(Sys.time(), job$started, units = "secs")),
                      log = hsms_tail_file(job$log_path, max_lines = defaults$app$log_max_lines)))
    if (job$state %in% c("finished", "failed", "cancelled")) shiny::isolate(finalize_job(job))
  })

  session$onSessionEnded(function() {
    job <- ctx$jobs_env$active
    if (!is.null(job) && job$proc$is_alive()) tryCatch(job$proc$kill_tree(), error = function(e) job$proc$kill())
    d <- shiny::isolate(ctx$dir()); p <- shiny::isolate(ctx$proj())
    if (!is.null(d) && !is.null(p)) {
      if (!is.null(job)) {
        p$jobs[[job$id]]$status <- "cancelled (session ended)"
        for (it in job$req$items) p <- hsms_record_outcome(p, job$stage, it$scope, "cancelled", it$signature,
                                                             "session ended", job$id)
        tryCatch(hsms_log_event(d, "job_cancelled_session_end", list(job = job$id)), error = function(e) NULL)
        hsms_cleanup_job_dir(job)
      }
      tryCatch(hsms_save_project(p, d), error = function(e) NULL)
      hsms_unlock_project(d, ctx$token)
    }
  })

  # ---- demo workflow (explicit, recorded preset stages) -----------------------
  demo_steps <- c("import", "selection", "tpt_raw", "tpt_filtered", "grouping", "order", "phase", "map")
  ctx$start_demo <- function(full = TRUE) {
    ctx$demo_chain(list(full = full, step = "import"))
  }
  demo_continue <- function(status) {
    ch <- shiny::isolate(ctx$demo_chain())
    if (is.null(ch)) return()
    if (status != "completed") {
      ctx$demo_chain(NULL)
      ctx$notify("Demo workflow stopped because a stage did not complete. Inspect the job log.", "error", 12)
      return()
    }
    p <- shiny::isolate(ctx$proj()); d <- shiny::isolate(ctx$dir())
    nxt <- demo_steps[match(ch$step, demo_steps) + 1L]
    if (ch$step == "import") {
      p2 <- tryCatch(hsms_action_select_family(p, d, defaults$demo$family, ctx$cache), error = function(e) e)
      if (inherits(p2, "error")) { ctx$demo_chain(NULL); ctx$notify(conditionMessage(p2), "error"); return() }
      ctx$set_proj(p2); p <- p2
      ctx$log("demo_step", list(step = "selection", family = defaults$demo$family))
      if (!isTRUE(ch$full)) { ctx$demo_chain(NULL); ctx$notify("Demo data loaded and family MOM1 selected.", duration = 6); return() }
      nxt <- "tpt_raw"
    }
    if (is.na(nxt)) { ctx$demo_chain(NULL); ctx$notify("Demo workflow finished. Open 'Inheritance and results'.", duration = 8); return() }
    ctx$demo_chain(list(full = TRUE, step = nxt))
    groups <- hsms_group_ids(p)
    ctx$log("demo_step", list(step = nxt))
    switch(nxt,
      tpt_raw = ctx$launch("tpt_raw", label = "Demo: two-point analysis"),
      tpt_filtered = ctx$launch("tpt_filtered", label = "Demo: two-point filter"),
      grouping = {
        p <- hsms_apply_settings(p, "group", modifyList(p$settings$group, list(k = defaults$demo$k)))
        ctx$set_proj(p); ctx$launch("grouping", label = "Demo: linkage groups (k = 1, demo setting)")
      },
      order = {
        for (g in groups) p <- hsms_set_order_spec_mds(p, g, defaults$order)
        ctx$set_proj(p)
        ctx$launch("order", hsms_group_items(p, d, "order", groups, ctx$cache),
                   list(order_settings = defaults$order), label = "Demo: MDS order")
      },
      phase = {
        for (g in groups) p <- hsms_set_phase_spec(p, g, defaults$phase)
        ctx$set_proj(p); ctx$launch("phase", hsms_group_items(p, d, "phase", groups), label = "Demo: phase assembly")
      },
      map = {
        for (g in groups) {
          ph <- hsms_load_current(p, d, "phase", g, ctx$cache)
          rt <- hsms_phase_routes(ph)
          if (rt$source_aware) p <- hsms_set_map_spec(p, g, "source_aware", defaults$source_aware)
          else if (rt$population) p <- hsms_set_map_spec(p, g, "population", defaults$population)
        }
        fit_groups <- names(p$map_spec)[names(p$map_spec) %in% groups]
        ctx$set_proj(p); ctx$launch("map", hsms_group_items(p, d, "map", fit_groups), label = "Demo: map fit")
      })
  }

  # ---- shell outputs ------------------------------------------------------------
  output$header_status <- shiny::renderUI({
    p <- ctx$proj()
    if (is.null(p)) return(shiny::tags$span(class = "hsms-hdr-item text-muted", "No project open"))
    shiny::tagList(
      shiny::tags$span(class = "hsms-hdr-item", shiny::tags$strong("Project: "), p$name),
      shiny::tags$span(class = "hsms-hdr-item", shiny::tags$strong("Family: "), p$family %||% "not selected"),
      shiny::tags$span(class = paste("hsms-hdr-item", if (grepl("failed", ctx$save_state())) "text-danger"),
                       ctx$save_state()))
  })
  output$jobs_button_label <- shiny::renderUI({
    jv <- ctx$job_view()
    if (is.null(jv)) "Jobs" else shiny::tags$span(shiny::tags$span(class = "hsms-spinner", `aria-hidden` = "true"),
                                                  if (jv$state == "cancelling") "Cancelling..." else "Job running")
  })
  shiny::observeEvent(input$toggle_jobs, bslib::toggle_sidebar("jobs_sidebar"))

  nav_status <- shiny::reactive({
    p <- ctx$proj(); jv <- ctx$job_view()
    if (is.null(p)) return(c(project = "ready", stats::setNames(rep("blocked", 7), names(HSMS_NAV)[-1])))
    gs <- hsms_group_ids(p)
    gstat <- function(stage) if (!length(gs)) "blocked" else lapply(gs, function(g) hsms_stage_status(p, stage, g))
    run <- function(stages) !is.null(jv) && jv$stage %in% stages
    st <- c(
      project = "complete",
      data = if (run("import")) "running" else hsms_stage_status(p, "import"),
      quality = hsms_stage_status(p, "selection"),
      twopoint = if (run(c("tpt_raw", "tpt_filtered"))) "running" else
        hsms_combine_status(list(hsms_stage_status(p, "tpt_raw"), hsms_stage_status(p, "tpt_filtered"))),
      groups = if (run(c("grouping", "order"))) "running" else
        hsms_combine_status(c(list(hsms_stage_status(p, "grouping")), if (hsms_is_current(p, "grouping")) gstat("order"))),
      phasemaps = if (run(c("phase", "map"))) "running" else
        if (!hsms_is_current(p, "grouping")) "blocked" else hsms_combine_status(c(gstat("phase"), gstat("map"))),
      results = if (any(vapply(gs, function(g) hsms_is_current(p, "map", g), logical(1)))) "available" else "blocked",
      export = if (hsms_is_current(p, "import")) "available" else "blocked")
    if (!is.null(st[["data"]]) && st[["data"]] == "not_run") st[["data"]] <- "ready"
    st
  })
  output$nav <- shiny::renderUI({
    st <- nav_status(); cur <- input$stage %||% "project"
    shiny::tagList(lapply(seq_along(HSMS_NAV), function(i) {
      key <- names(HSMS_NAV)[i]
      shiny::tags$button(
        type = "button", class = paste("hsms-nav-item", if (identical(cur, key)) "active"),
        `aria-current` = if (identical(cur, key)) "page" else NULL,
        onclick = sprintf("Shiny.setInputValue('nav_click', '%s', {priority: 'event'})", key),
        shiny::tags$span(class = "hsms-nav-num", i),
        shiny::tags$span(class = "hsms-nav-label", HSMS_NAV[[i]]),
        hsms_status_badge(st[[key]]))
    }))
  })
  ctx$go <- function(stage) bslib::nav_select("stage", stage, session = session)
  shiny::observeEvent(input$nav_click, ctx$go(input$nav_click))

  # ---- settings and help modals -----------------------------------------------
  shiny::observeEvent(input$open_settings, {
    p <- ctx$proj()
    allow <- hsms_cpu_allowance(defaults$app$max_threads)
    shiny::showModal(shiny::modalDialog(
      title = "Project settings", easyClose = TRUE, size = "m",
      if (is.null(p)) hsms_notice("Open a project to change its settings.") else shiny::tagList(
        hsms_num("set_threads", "Worker threads", p$settings$app$threads,
                 sprintf("Threads used by RcppParallel inside the background worker (two-point analysis). Allowed: 1 to %d on this machine. Thread count does not change results.", allow),
                 min = 1, max = allow, step = 1),
        hsms_num("set_seed", "Random seed", p$settings$app$seed,
                 "Recorded with every job and set before each stochastic step. MDS ordering records the seed in its order specification.",
                 min = 1, step = 1),
        shiny::p(class = "text-muted small", sprintf("Projects are stored on the machine running R, in: %s", ctx$projects_dir))),
      footer = shiny::tagList(shiny::modalButton("Close"),
                              if (!is.null(p)) shiny::actionButton("apply_settings", "Apply settings", class = "btn-primary"))))
  })
  shiny::observeEvent(input$apply_settings, {
    p <- ctx$proj(); allow <- hsms_cpu_allowance(defaults$app$max_threads)
    th <- as.integer(input$set_threads); sd <- as.integer(input$set_seed)
    if (is.na(th) || th < 1 || th > allow) { ctx$notify(sprintf("Threads must be between 1 and %d.", allow), "error"); return() }
    if (is.na(sd) || sd < 1) { ctx$notify("The seed must be a positive whole number.", "error"); return() }
    p$settings$app$threads <- th; p$settings$app$seed <- sd
    ctx$set_proj(p); ctx$log("settings_applied", list(threads = th, seed = sd))
    shiny::removeModal(); ctx$notify("Settings applied.")
  })
  shiny::observeEvent(input$open_help, {
    shiny::showModal(shiny::modalDialog(title = "How HalfSibMap Studio works", easyClose = TRUE, size = "l",
                                        hsms_help_content(), footer = shiny::modalButton("Close")))
  })

  # ---- modules ------------------------------------------------------------------
  mod_project_server("project", ctx)
  mod_data_server("data", ctx)
  mod_qc_server("qc", ctx)
  mod_pairwise_server("twopoint", ctx)
  mod_group_order_server("groups", ctx)
  mod_phase_map_server("phasemaps", ctx)
  mod_results_server("results", ctx)
  mod_exports_server("export", ctx)
  mod_jobs_server("jobs", ctx)
}

hsms_help_content <- function() {
  shiny::tagList(
    shiny::p("HalfSibMap Studio is a graphical interface to the HalfSibMap R package (pinned commit ",
             shiny::tags$code(substr(hsms_pinned_halfsibmap_commit(), 1, 10)), "). All statistics are computed by HalfSibMap; the app validates inputs, runs each stage in a background R process, keeps results tied to the settings that produced them, and exports reproducible outputs."),
    shiny::tags$h5("Workflow"),
    shiny::tags$ol(
      shiny::tags$li(shiny::strong("Project: "), "create, open, or import a project; or load the simulated demo."),
      shiny::tags$li(shiny::strong("Data: "), "upload pedigree and genotype CSVs, validate them, and select ONE maternal family. Families are never pooled."),
      shiny::tags$li(shiny::strong("Quality review: "), "inspect missingness and maternal genotypes; apply reversible exclusions."),
      shiny::tags$li(shiny::strong("Two-point analysis: "), "pairwise recombination fractions and LOD scores, then filtering."),
      shiny::tags$li(shiny::strong("Groups and order: "), "linkage groups and marker order (MDS or a supplied order)."),
      shiny::tags$li(shiny::strong("Phase and maps: "), "maternal phase assembly; source-aware model for fully resolved phase, population model with phase blocks otherwise."),
      shiny::tags$li(shiny::strong("Inheritance and results: "), "gap-aware maps and per-offspring maternal inheritance probabilities."),
      shiny::tags$li(shiny::strong("Export: "), "tables, figures, native objects, project bundles, and a replayable R script.")),
    shiny::tags$h5("Status badges"),
    shiny::p("Complete, Warnings (e.g. a non-converged fit), Partial (some blocks failed), Outdated (an upstream setting or input changed; the old result is kept for inspection), Failed, Cancelled, Blocked (a prerequisite is missing), Ready (can be run)."),
    shiny::tags$h5("Scientific notes"),
    shiny::tags$ul(
      shiny::tags$li("Package defaults (for example two-point lambda = 20, minimum phase LOD = 0) are shown as defaults, not as validated recommendations."),
      shiny::tags$li("No-linkage and unresolved-phase intervals are gaps: their distance is NA, and map positions restart after them."),
      shiny::tags$li("Homolog 1 is a statistical label within a resolved phase block, not an identified grandparent."),
      shiny::tags$li("Paternal sharing states describe haplotype-sharing patterns; they do not identify fathers or prove identity by descent."),
      shiny::tags$li("Confirmed selfed offspring should be removed by upstream quality control: maternal-specific transmission is not identifiable for selfs.")),
    shiny::p(class = "text-muted small", "Full documentation: docs/USER_GUIDE.md and docs/DATA_FORMATS.md in the application folder."))
}
