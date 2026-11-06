# Grouping and ordering adapters around HalfSibMap::group_markers() and
# HalfSibMap::mds_order(), plus supplied-order validation and reversal.

hsms_run_group <- function(tptf, settings) {
  n <- length(tptf$markers)
  k <- as.integer(settings$k)
  if (is.na(k) || k < 1L) stop("The number of groups must be a positive whole number.")
  if (n < 2L) stop("At least 2 retained markers are needed to build groups (", n, " retained).")
  if (k > n) stop("Requested ", k, " groups but only ", n, " markers are retained.")
  args <- hsms_args_group(settings)
  cap <- hsms_capture(do.call(HalfSibMap::group_markers, c(list(w = tptf), args)))
  grp <- cap$value
  if (!inherits(grp, "hsmap_group")) stop("group_markers() did not return hsmap_group.")
  if (!identical(names(grp$groups), tptf$markers)) stop("Group assignments are misaligned with markers.")
  sizes <- as.integer(table(factor(grp$groups, levels = seq_len(grp$k))))
  list(raw = grp, warnings = cap$warnings, args = args,
       summary = list(k = grp$k, method_requested = settings$method, method_used = grp$method,
                      method_switched = !identical(settings$method, grp$method),
                      sizes = stats::setNames(sizes, seq_len(grp$k)),
                      n_singletons = sum(sizes == 1L), n_markers = n))
}

hsms_group_members <- function(grp, g) names(grp$groups)[grp$groups == as.integer(g)]

# Order one group's markers by MDS (or record a documented bypass for groups
# with fewer than 3 markers, where mds_order() returns its input unchanged).
hsms_run_order_mds <- function(tptf, markers, settings, seed) {
  markers <- as.character(markers)
  if (!length(markers)) stop("The group has no markers.")
  set.seed(seed)
  if (length(markers) < 3L) {
    ord <- markers
    return(list(order = ord, removed = character(0),
                method = if (length(markers) == 1L) "singleton (no ordering possible)"
                         else "two markers (order taken as given; no MDS estimate)",
                mds_estimated = FALSE, warnings = character(0), args = NULL))
  }
  args <- hsms_args_order(settings)
  cap <- hsms_capture(do.call(HalfSibMap::mds_order, c(list(x = markers, tpt = tptf), args)))
  res <- cap$value
  if (!inherits(res, "hsmap_mds") || length(res) != 1L) stop("mds_order() returned an unexpected object.")
  ord <- as.character(res[[1]])
  removed <- as.character(attr(res[[1]], "removed") %||% character(0))
  if (!setequal(c(ord, removed), markers) || anyDuplicated(ord))
    stop("mds_order() result does not account for every marker of the group.")
  list(order = ord, removed = removed, method = "mds", mds_estimated = TRUE,
       warnings = cap$warnings, args = args, raw = res)
}

hsms_reverse_order <- function(order_payload) {
  out <- order_payload
  out$order <- rev(order_payload$order)
  out$method <- paste0(order_payload$method, " (reversed)")
  out$reversed_from <- order_payload$order
  out
}

# Validate a user-supplied order table (columns marker_id, linkage_group, rank;
# original IDs) against current groups. Returns a reconciliation report.
# `group_labels`: named character vector, names = group ids, values = display labels.
hsms_validate_supplied_order <- function(df, grp, map, group_labels) {
  issues <- character(0)
  need <- c("marker_id", "linkage_group", "rank")
  if (!is.data.frame(df) || !all(need %in% names(df)))
    return(list(ok = FALSE, issues = paste("The order file needs columns:", paste(need, collapse = ", ")),
                groups = list()))
  df <- df[, need]
  df[] <- lapply(df, as.character)
  rk <- suppressWarnings(as.numeric(df$rank))
  if (anyNA(rk) || any(!is.finite(rk))) issues <- c(issues, "Some ranks are missing or not numeric.")
  if (anyDuplicated(df$marker_id))
    issues <- c(issues, paste("Markers listed more than once:",
                              paste(utils::head(unique(df$marker_id[duplicated(df$marker_id)]), 10), collapse = ", ")))
  df$internal <- hsms_to_internal(map, "marker", df$marker_id)
  unknown <- df$marker_id[is.na(df$internal) | !df$internal %in% names(grp$groups)]
  if (length(unknown))
    issues <- c(issues, paste("Markers not among the current grouped markers:",
                              paste(utils::head(unknown, 10), collapse = ", ")))
  lab_to_id <- c(stats::setNames(names(group_labels), group_labels),
                 stats::setNames(names(group_labels), names(group_labels)))
  df$group_id <- unname(lab_to_id[df$linkage_group])
  bad_g <- unique(df$linkage_group[is.na(df$group_id)])
  if (length(bad_g))
    issues <- c(issues, paste("Unknown linkage_group values (use a group number or its label):",
                              paste(bad_g, collapse = ", ")))
  groups <- list()
  for (g in unique(stats::na.omit(df$group_id))) {
    sub <- df[df$group_id == g & !is.na(df$internal), ]
    members <- hsms_group_members(grp, g)
    wrong <- sub$marker_id[!sub$internal %in% members]
    if (length(wrong))
      issues <- c(issues, sprintf("Group %s: markers belonging to another group: %s", g,
                                  paste(utils::head(wrong, 10), collapse = ", ")))
    rr <- suppressWarnings(as.numeric(sub$rank))
    if (anyDuplicated(rr)) issues <- c(issues, sprintf("Group %s: duplicated ranks.", g))
    ord <- sub$internal[order(rr)]
    omitted <- setdiff(members, sub$internal)
    groups[[g]] <- list(group = g, order = ord, n_listed = nrow(sub),
                        omitted = omitted,
                        omitted_original = hsms_to_original(map, "marker", omitted))
  }
  if (!length(groups) && !length(issues)) issues <- c(issues, "The file contains no usable rows.")
  list(ok = !length(issues), issues = issues, groups = groups)
}
