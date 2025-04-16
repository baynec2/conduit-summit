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