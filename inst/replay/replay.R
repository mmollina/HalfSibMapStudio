# Replay the analysis from the included raw inputs with the recorded settings
# and seeds, using the real HalfSibMap API. Run `Rscript setup.R` first.
#   Rscript /path/to/replay/replay.R [--out=/path/to/outputs]
local({
  args <- commandArgs(trailingOnly = TRUE)
  # Rscript encodes spaces in the script path as "~+~".
  root <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)))
  out_arg <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
  out_dir <- if (length(out_arg)) out_arg[1] else file.path(root, "outputs")
  for (f in list.files(file.path(root, "helpers"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = globalenv(), keep.source = FALSE)
  cfg <- jsonlite::read_json(file.path(root, "settings.json"), simplifyVector = FALSE)
  sha <- utils::packageDescription("HalfSibMap")$RemoteSha
  if (!identical(sha, cfg$halfsibmap_commit))
    stop("Installed HalfSibMap (", sha, ") differs from the recorded commit ", cfg$halfsibmap_commit,
         ". Run setup.R --install.")
  inp <- function(f) file.path(root, "inputs", f)
  for (k in c("pedigree", "genotypes"))
    if (!identical(hsms_file_sha256(inp(paste0(k, ".csv"))), cfg$input_sha256[[k]]))
      stop("Input file ", k, " does not match its recorded checksum.")
  completed <- unlist(cfg$completed)
  seed <- as.integer(cfg$seed); threads <- as.integer(cfg$threads)
  RcppParallel::setThreadOptions(numThreads = threads)
  log <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
  log("Importing (identifier-safe)")
  imp <- hsms_import(inp("pedigree.csv"), inp("genotypes.csv"), file.path(tempdir(), "normalized"),
                     accept_unknown_father = isTRUE(cfg$accept_unknown_father))
  if (!isTRUE(imp$ok)) stop("Input validation failed in replay.")
  saved_map <- utils::read.csv(file.path(root, "metadata", "identifier_mapping.csv"), colClasses = "character",
                               check.names = FALSE, na.strings = character(0), encoding = "UTF-8")
  if (!identical(saved_map$original, imp$mapping$original) || !identical(saved_map$internal, imp$mapping$internal))
    stop("Identifier mapping differs from the saved mapping.")
  res <- list(mapping = imp$mapping, groups = list())
  if (!"selection" %in% completed) { log("No family selection recorded; stopping after import."); return(invisible()) }
  ex <- hsms_empty_exclusions()
  for (e in cfg$exclusions)
    ex <- rbind(ex, data.frame(kind = e$kind, internal = hsms_to_internal(imp$mapping, e$kind, e$id),
                               original = e$id, reason = e$reason, source = "replay", applied_at = "",
                               stringsAsFactors = FALSE))
  sel <- hsms_select_family(imp, cfg$family, ex)
  log("Family", cfg$family, ":", sel$counts$retained_offspring, "offspring,", sel$counts$retained_markers, "informative markers")
  if ("tpt_raw" %in% completed) {
    log("Two-point analysis"); set.seed(seed)
    tpt <- hsms_run_pairwise(sel, cfg$pairwise, threads); res$tpt_raw <- tpt$raw
  }
  if ("tpt_filtered" %in% completed) {
    log("Two-point filter"); set.seed(seed)
    res$tpt_filtered <- hsms_run_filter(res$tpt_raw, cfg$filter)$raw
  }
  if ("grouping" %in% completed) {
    log("Grouping"); set.seed(seed)
    res$grouping <- hsms_run_group(res$tpt_filtered, cfg$group)$raw
  }
  for (g in names(cfg$groups)) {
    gc <- cfg$groups[[g]]; r <- list()
    if (!is.null(gc$order)) {
      ord <- NULL
      for (st in gc$order) {
        if (st$source == "mds") {
          set.seed(seed)
          ord <- hsms_run_order_mds(res$tpt_filtered, hsms_group_members(res$grouping, g), st$settings, as.integer(st$seed))
        } else if (st$source == "supplied") {
          ord <- list(order = hsms_to_internal(imp$mapping, "marker", unlist(st$order)), removed = character(0), method = "supplied")
        } else if (st$source == "reversed") ord <- hsms_reverse_order(ord)
      }
      r$order <- ord
      log("Group", gc$label, ": order of", length(ord$order), "markers (", ord$method, ")")
    }
    if (!is.null(gc$phase)) { set.seed(seed); r$phase <- hsms_run_phase(res$tpt_filtered, r$order$order, gc$phase) }
    if (!is.null(gc$map)) {
      set.seed(seed)
      r$map <- if (gc$map$model == "source_aware") hsms_run_source_aware(sel, r$phase, gc$map$settings)
               else hsms_run_population(sel, r$phase, gc$map$settings)
      log("Group", gc$label, ": fitted", gc$map$model, "model")
    }
    res$groups[[g]] <- r
  }
  hsms_write_replay_outputs(res, out_dir, cfg$display)
  saveRDS(res, file.path(out_dir, "replay_results.rds"))
  log("Replay complete. Outputs in", out_dir)
})
