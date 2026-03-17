#' Get QFeatures object from conduit object
#' @param conduit_obj The conduit object
#' @return QFeatures object
get_qf <- function(conduit_obj) {
  slot(conduit_obj, "QFeatures")
}

#' Get combined metrics from conduit object
#' @param conduit_obj The conduit object
#' @return Combined metrics data frame
get_combined_metrics <- function(conduit_obj) {
  slot(conduit_obj, "combined_metrics")
}

#' Get row data from QFeatures object
#' @param qf QFeatures object
#' @return Row data data frame
get_row_data <- function(qf) {
  SummarizedExperiment::rowData(qf)
}

#' Get column data from QFeatures object
#' @param qf QFeatures object
#' @return Column data data frame
get_col_data <- function(qf) {
  SummarizedExperiment::colData(qf)
}

#' Create a cache environment for storing computed values
#' @return A new environment for caching
create_cache_env <- function() {
  new.env(parent = emptyenv())
}

#' columnDefs entry that truncates long cell text with a tooltip, matching the
#' view-assay table style. Pass to conduit_datatable(column_defs = ...).
conduit_truncate_column_defs <- function(max_chars = 40, max_width_px = 220) {
  list(list(
    targets     = "_all",
    render      = DT::JS(
      "function(data, type, row) {",
      sprintf("  if (type === 'display' && data !== null && String(data).length > %d) {", max_chars),
      "    return '<span title=\"' + String(data).replace(/\"/g, '&quot;') + '\">'",
      sprintf("         + String(data).substring(0, %d) + '\u2026</span>';", max_chars),
      "  }",
      "  return data;",
      "}"
    ),
    createdCell = DT::JS(
      "function(td) {",
      sprintf("  td.style.maxWidth = '%dpx';", max_width_px),
      "  td.style.overflow = 'hidden';",
      "  td.style.textOverflow = 'ellipsis';",
      "  td.style.whiteSpace = 'nowrap';",
      "}"
    )
  ))
}

#' Consistent DT datatable with app styling
#' @param data Data frame to display
#' @param page_length Default rows per page
#' @param round_digits Digits to round numeric (double) columns to; set to NA to skip rounding
#' @param column_defs Optional list passed to options$columnDefs (e.g. conduit_truncate_column_defs())
#' @param ... Additional arguments passed to DT::datatable
conduit_datatable <- function(data, page_length = 15, round_digits = 2, column_defs = NULL, ...) {
  opts <- list(
    pageLength = page_length,
    scrollX    = TRUE,
    dom        = '<"conduit-dt-top d-flex justify-content-between align-items-center mb-2"f>t<"conduit-dt-bottom d-flex justify-content-between align-items-center mt-2"ip>',
    language   = list(
      search            = "",
      searchPlaceholder = "Search\u2026",
      paginate          = list(previous = "\u2039", `next` = "\u203a"),
      info              = "Showing _START_\u2013_END_ of _TOTAL_"
    )
  )
  if (!is.null(column_defs)) {
    opts$columnDefs <- column_defs
  }
  dt <- DT::datatable(
    data,
    class   = "table table-hover table-sm",
    options = opts,
    ...
  )
  double_cols <- names(data)[sapply(data, is.double)]
  if (!is.na(round_digits) && length(double_cols) > 0) {
    dt <- DT::formatRound(dt, columns = double_cols, digits = round_digits)
  }
  dt
}

#' Minimal ggplot shown when a plot is waiting for the user to apply settings
#' @param message Text to display in the plot area
waiting_plot <- function(message = "Apply settings to generate plot") {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0.5,
      label = message,
      size  = 4.5,
      color = "#888888",
      hjust = 0.5,
      vjust = 0.5,
      fontface = "italic"
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}

#' Show a notification with consistent styling
#' @param message The message to display
#' @param type The type of notification (error, warning, success)
show_notification <- function(message, type = "default") {
  shiny::showNotification(
    message,
    type = type,
    duration = if (type == "error") NULL else 5
  )
} 