# Maternal phase assembly adapter around HalfSibMap::phase_from_pairwise().

hsms_run_phase <- function(tptf, order, settings) {
  order <- as.character(order)
  if (length(order) < 1L) stop("The order is empty.")
  if (length(tptf$fit$mom_phase_list) != 1L) stop("Phase assembly expects a single-family two-point result.")
  if (length(order) == 1L) {
    # phase_from_pairwise() needs >= 2 markers; a singleton keeps its metadata only.
    return(list(raw = NULL, singleton = TRUE, warnings = character(0), args = NULL,
                summary = hsms_phase_summary_singleton(order)))
  }
  args <- hsms_args_phase(settings)
  cap <- hsms_capture(do.call(HalfSibMap::phase_from_pairwise,
                              c(list(tpt = tptf, order = order, dam = 1L), args)))
  ph <- cap$value
  if (inherits(ph, "HSMap.phased.multi") || !inherits(ph, "HSMap.phased"))
    stop("phase_from_pairwise() did not return a single-dam HSMap.phased object.")
  if (!identical(ph$dam, tptf$parent_label[1]))
    stop("Phase result belongs to dam '", ph$dam, "', not the selected family.")
  if (!identical(ph$order, order))
    stop("phase_from_pairwise() dropped or reordered markers: ",
         paste(utils::head(setdiff(order, ph$order), 5), collapse = ", "))
  list(raw = ph, singleton = FALSE, warnings = cap$warnings, args = args,
       summary = hsms_phase_summary(ph))
}

hsms_phase_summary_singleton <- function(order) {
  list(markers = data.frame(rank = 1L, marker = order, component = 1L, homolog_label = NA_integer_,
                            stringsAsFactors = FALSE),
       intervals = data.frame(interval = integer(0), from = character(0), to = character(0),
                              phase = character(0), resolved_via = character(0),
                              direct_lod = numeric(0), direct_supported = logical(0),
                              stringsAsFactors = FALSE),
       n_components = 1L, component_sizes = 1L, fully_resolved = TRUE,
       n_unresolved = 0L, converged = NA, route = "singleton")
}

hsms_phase_summary <- function(ph) {
  o <- ph$order; Tn <- length(o)
  intervals <- data.frame(
    interval = seq_len(Tn - 1L), from = o[-Tn], to = o[-1],
    phase = ifelse(is.na(ph$phase_vec), "unresolved",
                   ifelse(ph$phase_vec == 1L, "coupling", "repulsion")),
    resolved_via = ph$resolved_via,
    direct_lod = ph$direct_lod,
    direct_supported = ph$direct_supported,
    stringsAsFactors = FALSE)
  markers <- data.frame(rank = seq_len(Tn), marker = o, component = ph$component,
                        homolog_label = ph$clusters, stringsAsFactors = FALSE)
  route <- if (isTRUE(ph$fully_resolved)) "resolved" else "unresolved"
  list(markers = markers, intervals = intervals,
       n_components = ph$n_components, component_sizes = ph$component_sizes,
       fully_resolved = isTRUE(ph$fully_resolved),
       n_unresolved = sum(is.na(ph$phase_vec)),
       converged = ph$converged, objective = ph$objective,
       weighted_agreement = ph$weighted_agreement, route = route)
}

# Which model routes are available for a phase result.
hsms_phase_routes <- function(phase_payload) {
  s <- phase_payload$summary
  n <- nrow(s$markers)
  if (isTRUE(phase_payload$singleton) || n < 2L)
    return(list(source_aware = FALSE, population = FALSE,
                reason = "A single marker has no interval to estimate; its metadata is kept but no map is fitted."))
  if (isTRUE(s$fully_resolved))
    return(list(source_aware = TRUE, population = TRUE,
                reason = "Phase is fully resolved: the source-aware model and the population model are both available."))
  list(source_aware = FALSE, population = TRUE,
       reason = sprintf(paste("Phase is unresolved at %d interval(s) (%d components). Whole-order source-aware",
                              "fitting needs a fully resolved phase and is not available; the population model",
                              "with phase-block handling fits each resolved block separately."),
                        s$n_unresolved, s$n_components))
}
