# Shared UI helpers (Shiny-only; not sourced by workers).

HSMS_STATUS_TEXT <- c(
  not_run = "Not run", ready = "Ready", blocked = "Blocked", running = "Running",
  cancelling = "Cancelling", complete = "Complete", complete_with_warnings = "Warnings",
  partial = "Partial", outdated = "Outdated", failed = "Failed", cancelled = "Cancelled",
  interrupted = "Interrupted", historical = "Historical", available = "Available", none = "-")

hsms_status_badge <- function(status, label = NULL) {
  status <- status %||% "none"
  txt <- label %||% (HSMS_STATUS_TEXT[[status]] %||% status)
  shiny::tags$span(class = paste("hsms-badge", paste0("hsms-badge-", status)), txt)
}

hsms_help <- function(text, title = NULL) {
  bslib::popover(
    shiny::tags$button(type = "button", class = "hsms-help", `aria-label` = paste("Help:", title %||% "more information"), "?"),
    text, title = title, placement = "right")
}

hsms_label <- function(label, help = NULL) {
  if (is.null(help)) return(label)
  shiny::tags$span(label, hsms_help(help, label))
}

hsms_num <- function(id, label, value, help = NULL, min = NA, max = NA, step = NA, width = "100%") {
  shiny::numericInput(id, hsms_label(label, help), value = value, min = min, max = max, step = step, width = width)
}

hsms_empty_state <- function(title, text, ...) {
  shiny::div(class = "hsms-empty", shiny::tags$h5(title), shiny::p(text), ...)
}

hsms_notice <- function(..., type = c("info", "warning", "danger", "success")) {
  type <- match.arg(type)
  shiny::div(class = paste0("hsms-notice hsms-notice-", type), role = if (type == "danger") "alert" else "status", ...)
}

hsms_prereq_ui <- function(msg) {
  if (is.null(msg)) return(NULL)
  hsms_notice(type = "info", shiny::tags$strong("Prerequisite: "), msg)
}

hsms_kv <- function(...) {
  x <- list(...)
  shiny::tags$dl(class = "hsms-kv", lapply(names(x), function(k)
    shiny::tagList(shiny::tags$dt(k), shiny::tags$dd(x[[k]] %||% "NA"))))
}

# DT table with server-side processing, search, and full-precision download
# handled separately (display rounding only).
hsms_dt <- function(df, digits = 4, page_length = 15, selection = "none", scroll_y = "380px", ...) {
  if (is.null(df) || !nrow(df)) df <- data.frame(Message = "No rows")
  num_cols <- which(vapply(df, function(z) is.double(z), logical(1)))
  dt <- DT::datatable(df, rownames = FALSE, selection = selection, filter = "none",
                      class = "compact stripe hover",
                      options = list(pageLength = page_length, scrollX = TRUE, scrollY = scroll_y,
                                     scrollCollapse = TRUE, deferRender = TRUE,
                                     language = list(emptyTable = "No rows")), ...)
  if (length(num_cols)) dt <- DT::formatSignif(dt, num_cols, digits = digits)
  dt
}

hsms_download_link <- function(id, label = "Download CSV") {
  shiny::downloadLink(id, label, class = "hsms-dl")
}

# Unapplied-changes indicator.
hsms_draft_badge <- function(differs) {
  if (isTRUE(differs)) shiny::tags$span(class = "hsms-badge hsms-badge-outdated", "Unapplied changes") else NULL
}

hsms_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#ffffff", fg = "#1f2933",
    primary = "#0f766e", secondary = "#52606d", success = "#2f855a",
    info = "#2b6cb0", warning = "#b7791f", danger = "#c53030",
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                                       "Helvetica Neue", "Arial", "sans-serif"),
    code_font = bslib::font_collection("ui-monospace", "SFMono-Regular", "Consolas", "Menlo", "monospace"),
    "body-bg" = "#f4f6f8", "border-radius" = "0.4rem", "font-size-base" = "0.92rem",
    "card-cap-bg" = "#ffffff")
}

# ggplot theme for app figures.
hsms_gg_theme <- function(base = 12) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = base + 1),
                   plot.caption = ggplot2::element_text(colour = "#52606d", hjust = 0),
                   legend.position = "right")
}


# Update a select input, keeping the current value if still valid, otherwise
# selecting the first choice (a selectize input never stays silently empty).
hsms_update_select <- function(session, id, choices, current) {
  vals <- unname(choices)
  sel <- if (!is.null(current) && length(current) && current %in% vals) current else if (length(vals)) vals[1] else character(0)
  shiny::updateSelectInput(session, id, choices = choices, selected = sel)
}
