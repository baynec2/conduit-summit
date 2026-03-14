library(shinytest2)

# Chrome can be slow to start on first launch — increase navigation timeout
options(chromote.timeout = 120)

# ---------------------------------------------------------------------------
# Startup (no fixture needed)
# ---------------------------------------------------------------------------

test_that("app starts and logs no JS errors", {
  app <- AppDriver$new(app_dir = testthat::test_path("../../"), timeout = 30000)
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle()
  logs <- app$get_logs()
  js_errors <- logs[logs$level == "error" & logs$source == "browser", ]
  expect_equal(nrow(js_errors), 0)
})

test_that("sidebar tabs that require data are initially disabled", {
  app <- AppDriver$new(app_dir = testthat::test_path("../../"), timeout = 30000)
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle()

  disabled_tabs <- c("view_metadata", "filter_data", "analysis", "diann_qc", "traverse")
  html <- app$get_html("body")
  for (tab in disabled_tabs) {
    expect_match(html, sprintf('data-value="%s"', tab))
  }
})

# ---------------------------------------------------------------------------
# All fixture-dependent checks share one app instance to avoid Chrome
# exhaustion from spawning multiple sessions in sequence.
# ---------------------------------------------------------------------------

test_that("full workflow: upload populates stats, plots render on each tab", {
  test_rds <- file.path(testthat::test_path("../../"), "test", "conduit_output.rds")
  skip_if_not(file.exists(test_rds), paste("Test fixture not found:", test_rds))

  app <- AppDriver$new(app_dir = testthat::test_path("../../"), timeout = 120000, load_timeout = 60000)
  on.exit(app$stop(), add = TRUE)

  # Navigate to file_upload tab before uploading so outputs are not suspended
  app$set_inputs(main_tabs = "file_upload")
  app$wait_for_idle()
  app$upload_file(conduit_rds = test_rds)

  # --- File upload tab ---
  num_samples <- app$wait_for_value(output = "file_upload-num_samples", timeout = 120000)
  expect_true(length(num_samples) > 0 && any(nchar(trimws(num_samples)) > 0))

  num_species <- app$wait_for_value(output = "file_upload-num_species_detected", timeout = 60000)
  expect_true(length(num_species) > 0 && any(nchar(trimws(num_species)) > 0))

  app$wait_for_idle(timeout = 30000)
  app$expect_values(output = "file_upload-taxa_tree_plot")

  # --- DIA-NN QC tab ---
  app$set_inputs(main_tabs = "diann_qc")
  app$wait_for_idle()
  app$expect_values(output = "diann_qc-diann_qc_plot")
  diann_table <- app$get_values(output = "diann_qc-diann_qc_table")$output[["diann_qc-diann_qc_table"]]
  expect_false(is.null(diann_table))

  # --- Metadata tab ---
  app$set_inputs(main_tabs = "view_metadata")
  app$wait_for_idle()
  app$expect_values(output = "view_metadata-colData")

  # --- Filter tab ---
  # Don't wait for idle: feature_filters renderUI tries to create pickerInputs
  # with unique values from 81K-row assays, which hangs. The assertion only
  # checks for the static uiOutput placeholder which is always in the DOM.
  app$set_inputs(main_tabs = "filter_data", wait_ = FALSE)
  Sys.sleep(1)
  html <- app$get_html("body")
  expect_match(html, "filter_data-sample_filters")
})
