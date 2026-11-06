# App-owned scientific figures (ggplot2). Used by the interface and by exports.
# Gaps are never drawn as continuous lines; aggregated heatmaps say so.

HSMS_STATUS_COLORS <- c(linked = "#0f766e", no_linkage_boundary = "#c53030",
                        unresolved_phase = "#b7791f", insufficient_information = "#718096")
HSMS_PHASE_COLORS <- c(coupling = "#2b6cb0", repulsion = "#d97706", unresolved = "#9aa5b1")


hsms_gg <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 13),
                   plot.caption = ggplot2::element_text(colour = "#52606d", hjust = 0, size = 9))
}

hsms_plot_rf_hist <- function(dist_df) {
  sub <- if (isTRUE(attr(dist_df, "subsampled"))) "Evenly spaced subsample of pairs (display only)." else NULL
  n_na <- sum(is.na(dist_df$r))
  ggplot2::ggplot(dist_df[!is.na(dist_df$r), , drop = FALSE], ggplot2::aes(x = r)) +
    ggplot2::geom_histogram(binwidth = 0.01, boundary = 0, fill = "#0f766e", colour = "white", linewidth = 0.1) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", colour = "#c53030") +
    ggplot2::annotate("text", x = 0.5, y = Inf, label = "no linkage (r = 0.5)", vjust = 1.5, hjust = 1.05,
                      colour = "#c53030", size = 3.3) +
    ggplot2::scale_x_continuous(breaks = seq(0, 0.5, 0.1)) + ggplot2::coord_cartesian(xlim = c(0, 0.51)) +
    ggplot2::labs(title = "Pairwise maternal recombination fractions", x = "Recombination fraction (r)",
                  y = "Marker pairs",
                  caption = paste(c(sprintf("%d pair(s) with no estimate (NA) are not shown.", n_na), sub), collapse = " ")) +
    hsms_gg()
}

hsms_plot_lod_hist <- function(dist_df) {
  long <- rbind(data.frame(value = dist_df$lod_r, type = "Linkage LOD (vs r = 0.5)"),
                data.frame(value = dist_df$lod_ph, type = "Phase LOD (profiled)"))
  long <- long[is.finite(long$value), ]
  ggplot2::ggplot(long, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 50, fill = "#2b6cb0", colour = "white", linewidth = 0.1) +
    ggplot2::facet_wrap(~type, ncol = 2) +
    ggplot2::labs(title = "LOD score distributions", x = "LOD", y = "Marker pairs",
                  caption = "For a single family the profiled phase LOD equals the linkage LOD wherever a phase is called.") +
    hsms_gg()
}

# Square-matrix heatmap; missing values drawn in a distinct grey.
hsms_plot_matrix <- function(M, what = c("r", "lod_r", "lod_ph"), labels = NULL, aggregated = FALSE,
                             block = 1L, title = NULL, max_labels = 60) {
  what <- match.arg(what)
  n <- nrow(M)
  lab <- labels %||% rownames(M)
  df <- data.frame(i = rep(seq_len(n), times = n), j = rep(seq_len(n), each = n), value = as.vector(M))
  fill_lab <- switch(what, r = "r", lod_r = "Linkage LOD", lod_ph = "Phase LOD")
  sc <- if (what == "r") {
    ggplot2::scale_fill_gradientn(colours = c("#7f1d1d", "#f59e0b", "#fef3c7", "#e0f2fe", "#1e3a8a"),
                                  values = c(0, 0.15, 0.3, 0.42, 1), limits = c(0, 0.5),
                                  na.value = "#d1d5db", name = fill_lab)
  } else ggplot2::scale_fill_viridis_c(option = "magma", direction = -1, na.value = "#d1d5db", name = fill_lab)
  brk <- if (n <= max_labels) seq_len(n) else unique(round(seq(1, n, length.out = max_labels)))
  ggplot2::ggplot(df, ggplot2::aes(x = j, y = i, fill = value)) +
    ggplot2::geom_raster() + sc +
    ggplot2::scale_x_continuous(breaks = brk, labels = lab[brk], expand = c(0, 0)) +
    ggplot2::scale_y_reverse(breaks = brk, labels = lab[brk], expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title %||% paste("Pairwise", fill_lab), x = NULL, y = NULL,
                  caption = paste0(if (aggregated) sprintf("Aggregated view: each cell is the mean of a %d x %d block of marker pairs (display only; exports keep full resolution). ", block, block) else "",
                                   "Grey = no estimate (missing), not linkage.")) +
    hsms_gg() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
                   axis.text.y = ggplot2::element_text(size = 7), panel.grid = ggplot2::element_blank())
}

hsms_plot_missingness <- function(values, what = "offspring") {
  df <- data.frame(x = values[is.finite(values)])
  ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = 0.02, boundary = 0, fill = "#52606d", colour = "white", linewidth = 0.1) +
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
    ggplot2::labs(title = sprintf("Missing-call proportion by %s", what), x = "Proportion of calls missing",
                  y = if (what == "offspring") "Offspring" else "Markers",
                  caption = sprintf("Denominator: %s.", if (what == "offspring") "markers in the file for this family" else "offspring in the family")) +
    hsms_gg()
}

hsms_plot_filter_counts <- function(counts, kept) {
  df <- data.frame(count = counts, status = ifelse(names(counts) %in% kept, "retained", "removed"))
  ggplot2::ggplot(df, ggplot2::aes(x = count, fill = status)) +
    ggplot2::geom_histogram(bins = 40, colour = "white", linewidth = 0.1, position = "stack") +
    ggplot2::scale_fill_manual(values = c(retained = "#0f766e", removed = "#d97706"), name = NULL) +
    ggplot2::labs(title = "Surviving informative pairs per marker",
                  x = "Pairs passing the LOD and RF screen (row count)", y = "Markers",
                  caption = "Row-information quantiles act on these counts, not on genotype missingness.") +
    hsms_gg()
}

hsms_draw_dendrogram <- function(grp, labels = NULL) {
  hc <- grp$hc
  op <- graphics::par(mar = c(2, 4, 3, 1)); on.exit(graphics::par(op))
  plot(hc, labels = FALSE, hang = -1, main = sprintf("Marker dendrogram (%s linkage)", grp$method),
       xlab = "", sub = "", ylab = "Symmetrised r (distance)")
  if (grp$k > 1L && grp$k < length(grp$markers))
    stats::rect.hclust(hc, k = grp$k, border = c("#0f766e", "#d97706", "#2b6cb0", "#9b2c2c", "#52606d")[
      (seq_len(grp$k) - 1L) %% 5L + 1L])
}

hsms_plot_phase <- function(intervals, labels = NULL) {
  if (!nrow(intervals)) return(NULL)
  df <- intervals
  df$phase <- factor(df$phase, levels = c("coupling", "repulsion", "unresolved"))
  df$via <- ifelse(df$resolved_via == "path", "P", ifelse(df$resolved_via == "direct", "D", "?"))
  ggplot2::ggplot(df, ggplot2::aes(x = interval, y = 1, fill = phase)) +
    ggplot2::geom_tile(height = 0.8, colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = via), size = 3, colour = "white") +
    ggplot2::scale_fill_manual(values = HSMS_PHASE_COLORS, drop = FALSE, name = "Adjacent phase") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::labs(title = "Maternal phase between adjacent markers", x = "Interval (between consecutive ordered markers)",
                  y = NULL, caption = "D = supported by the direct adjacent edge; P = derived through a path in the phase component; ? = unresolved (no estimate).") +
    hsms_gg() + ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# Detailed map for one group: one row per segment; positions restart per segment.
hsms_plot_map_group <- function(summary, max_labels = 40, title = NULL) {
  pos <- summary$positions
  if (!nrow(pos)) return(NULL)
  pos$segment_lab <- factor(paste0("Segment ", pos$segment_id, " (block ", pos$phase_block, ")"),
                            levels = unique(paste0("Segment ", pos$segment_id, " (block ", pos$phase_block, ")")))
  pos$pos_plot <- ifelse(is.na(pos$position_cM), 0, pos$position_cM)
  thin <- if (nrow(pos) > max_labels) seq(1, nrow(pos), by = ceiling(nrow(pos) / max_labels)) else seq_len(nrow(pos))
  pos$lab <- ifelse(seq_len(nrow(pos)) %in% thin, pos$marker_id, "")
  seg <- stats::aggregate(pos_plot ~ segment_lab, pos, function(z) c(min(z), max(z)))
  seg <- data.frame(segment_lab = seg$segment_lab, x0 = seg$pos_plot[, 1], x1 = seg$pos_plot[, 2])
  ggplot2::ggplot(pos, ggplot2::aes(x = pos_plot, y = 0)) +
    ggplot2::geom_segment(data = seg, ggplot2::aes(x = x0, xend = x1, y = 0, yend = 0), inherit.aes = FALSE,
                          linewidth = 2.2, colour = "#cbd2d9", lineend = "round") +
    ggplot2::geom_point(ggplot2::aes(shape = singleton_segment), size = 2.2, colour = "#0f766e") +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = c(`FALSE` = "positioned",
                                `TRUE` = "unestimated (alone in segment)"), name = NULL) +
    ggplot2::geom_text(ggplot2::aes(label = lab), angle = 60, hjust = 0, vjust = 0, nudge_y = 0.06, size = 2.8,
                       check_overlap = TRUE) +
    ggplot2::facet_wrap(~segment_lab, ncol = 1, scales = "free_x") +
    ggplot2::scale_y_continuous(limits = c(-0.2, 1), breaks = NULL) +
    ggplot2::labs(title = title %||% sprintf("Maternal map: %.1f cM linked (%s), %d gap(s), %d segment(s)",
                                             summary$linked_length, tools::toTitleCase(summary$map_function),
                                             summary$n_gaps, summary$n_segments),
                  x = sprintf("Position within segment (cM, %s)", tools::toTitleCase(summary$map_function)), y = NULL,
                  caption = "Each panel is a separate segment; positions restart after every gap (unresolved phase or no linkage). No distance is implied across gaps.") +
    hsms_gg() + ggplot2::theme(legend.position = "bottom")
}

# Overview across groups: each segment drawn as its own bar, side by side.
hsms_plot_map_overview <- function(summaries) {
  rows <- lapply(names(summaries), function(g) {
    p <- summaries[[g]]$positions
    if (is.null(p) || !nrow(p)) return(NULL)
    data.frame(group = p$linkage_group, segment = p$segment_id, pos = p$position_cM)
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || !nrow(df)) return(NULL)
  df$group <- factor(df$group, levels = unique(df$group))
  df$seg <- factor(df$segment)
  bars <- stats::aggregate(pos ~ group + seg, df[!is.na(df$pos), ], function(z) c(min(z), max(z)))
  bars <- data.frame(group = bars$group, seg = bars$seg, y0 = bars$pos[, 1], y1 = bars$pos[, 2])
  ggplot2::ggplot(df, ggplot2::aes(x = seg, y = pos)) +
    ggplot2::geom_segment(data = bars, ggplot2::aes(x = seg, xend = seg, y = y0, yend = y1), inherit.aes = FALSE,
                          linewidth = 5, colour = "#cbd2d9", lineend = "round") +
    ggplot2::geom_point(shape = 95, size = 6, colour = "#0f766e", na.rm = TRUE) +
    ggplot2::scale_y_reverse() +
    ggplot2::facet_grid(~group, scales = "free_x", space = "free_x") +
    ggplot2::labs(title = "Map overview", x = "Segment within linkage group", y = "Position within segment (cM)",
                  caption = "Segments are separated by gaps (unresolved phase or no linkage) and are not placed on a common scale.") +
    hsms_gg()
}

hsms_plot_intervals <- function(summary) {
  iv <- summary$intervals
  if (!nrow(iv)) return(NULL)
  iv$r_plot <- ifelse(is.na(iv$r), 0.5, iv$r)
  iv$estimated <- ifelse(is.na(iv$r), "no estimate", "estimated")
  ggplot2::ggplot(iv, ggplot2::aes(x = interval, y = r_plot, colour = status, shape = estimated)) +
    ggplot2::geom_hline(yintercept = summary$gap_r, linetype = "dashed", colour = "#c53030") +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = HSMS_STATUS_COLORS, name = "Interval status") +
    ggplot2::scale_shape_manual(values = c(estimated = 16, `no estimate` = 4), name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 0.52)) +
    ggplot2::labs(title = "Adjacent-interval recombination fractions", x = "Interval", y = "Fitted r",
                  caption = sprintf("Dashed line: no-linkage threshold r = %.3f. Crosses at the top mark intervals with no estimate (drawn at 0.5 for visibility only).", summary$gap_r)) +
    hsms_gg()
}

hsms_plot_posterior_heatmap <- function(long, title, fill_name, caption = NULL) {
  if (is.null(long) || !nrow(long)) return(NULL)
  long$offspring_id <- factor(long$offspring_id, levels = rev(unique(long$offspring_id)))
  long$marker_f <- factor(long$marker_id, levels = unique(long$marker_id[order(long$marker_rank)]))
  long$block_lab <- paste("Phase block", long$phase_block)
  nm <- length(unique(long$marker_id)); no <- length(unique(long$offspring_id))
  ggplot2::ggplot(long, ggplot2::aes(x = marker_f, y = offspring_id, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2(low = "#2b6cb0", mid = "#f7fafc", high = "#d97706", midpoint = 0.5,
                                  limits = c(0, 1), na.value = "#9aa5b1", name = fill_name) +
    ggplot2::facet_grid(~block_lab, scales = "free_x", space = "free_x") +
    ggplot2::labs(title = title, x = "Marker (map order)", y = "Offspring",
                  caption = caption) +
    hsms_gg() +
    ggplot2::theme(axis.text.x = if (nm <= 60) ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7) else ggplot2::element_blank(),
                   axis.text.y = if (no <= 40) ggplot2::element_text(size = 7) else ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

hsms_plot_profile <- function(long, offspring) {
  d <- long[long$offspring_id %in% offspring, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(x = marker_rank, y = value, colour = offspring_id,
                                  group = interaction(offspring_id, phase_block))) +
    ggplot2::geom_hline(yintercept = 0.5, colour = "#cbd2d9") +
    ggplot2::geom_line() + ggplot2::geom_point(size = 1.4) +
    ggplot2::facet_grid(~paste("Phase block", phase_block), scales = "free_x", space = "free_x") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = "Inheritance profile", x = "Marker rank in map order", y = "P(maternal homolog 1)",
                  colour = "Offspring",
                  caption = "Lines never cross phase-block boundaries; homolog labels are not comparable across blocks.") +
    hsms_gg()
}

# Save a ggplot (or base-graphics function) as PNG (300 dpi) and PDF.
hsms_save_figure <- function(plot, path_base, width = 8, height = 5) {
  dir.create(dirname(path_base), recursive = TRUE, showWarnings = FALSE)
  out <- c(png = paste0(path_base, ".png"), pdf = paste0(path_base, ".pdf"))
  if (is.function(plot)) {
    grDevices::png(out[["png"]], width = width, height = height, units = "in", res = 300)
    plot(); grDevices::dev.off()
    grDevices::pdf(out[["pdf"]], width = width, height = height)
    plot(); grDevices::dev.off()
  } else {
    ggplot2::ggsave(out[["png"]], plot, width = width, height = height, dpi = 300, bg = "white")
    ggplot2::ggsave(out[["pdf"]], plot, width = width, height = height, device = grDevices::pdf)
  }
  out
}
