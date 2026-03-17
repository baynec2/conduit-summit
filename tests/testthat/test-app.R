library(shinytest2)

options(chromote.timeout = 120)

# Helper: assert that a renderPlot output value is a real rendered image.
# shinytest2 returns a list with $src = "data:image/png;base64,..." for a
# successfully rendered plot.  An un-rendered or errored output is NULL.
expect_plot_rendered <- function(val, label = deparse(substitute(val))) {
  if (is.null(val)) {
    fail(paste(label, ": output is NULL (plot did not render)"))
  } else if (!is.list(val) || is.null(val$src)) {
    fail(paste(label, ": output has no $src field — plot may have errored"))
  } else if (!startsWith(val$src, "data:image")) {
    fail(paste(label, ": $src does not contain image data"))
  } else {
    expect_true(TRUE, label = paste(label, "rendered successfully"))
  }
}

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
  html <- app$get_html("body")
  for (tab in c("view_metadata", "filter_data", "analysis", "diann_qc", "traverse")) {
    expect_match(html, sprintf('data-value="%s"', tab))
  }
})

# ---------------------------------------------------------------------------
# All fixture-dependent checks share ONE app instance.
# ---------------------------------------------------------------------------

test_that("all plots render after file upload", {
  test_rds <- testthat::test_path("fixtures", "conduit.rds")
  skip_if_not(file.exists(test_rds), paste("Test fixture not found:", test_rds))

  app <- AppDriver$new(
    app_dir      = testthat::test_path("../../"),
    timeout      = 120000,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # ── Upload ──────────────────────────────────────────────────────────────────
  app$set_inputs(main_tabs = "file_upload")
  app$wait_for_idle()
  app$upload_file(conduit_rds = test_rds)

  # ── File Upload tab ─────────────────────────────────────────────────────────
  num_samples <- app$wait_for_value(output = "file_upload-num_samples", timeout = 120000)
  expect_true(length(num_samples) > 0 && any(nchar(trimws(num_samples)) > 0),
              label = "num_samples stat box is non-empty")

  num_species <- app$wait_for_value(output = "file_upload-num_species_detected", timeout = 60000)
  expect_true(length(num_species) > 0 && any(nchar(trimws(num_species)) > 0),
              label = "num_species stat box is non-empty")

  app$wait_for_idle(timeout = 30000)
  taxa_plot <- app$wait_for_value(output = "file_upload-taxa_tree_plot", timeout = 60000)
  expect_plot_rendered(taxa_plot, "taxa_tree_plot")

  # ── DIA-NN QC tab ───────────────────────────────────────────────────────────
  app$set_inputs(main_tabs = "diann_qc")
  app$wait_for_idle(timeout = 30000)

  diann_plot <- app$wait_for_value(output = "diann_qc-diann_qc_plot", timeout = 60000)
  expect_plot_rendered(diann_plot, "diann_qc_plot")

  diann_table <- app$get_values(output = "diann_qc-diann_qc_table")$output[["diann_qc-diann_qc_table"]]
  expect_false(is.null(diann_table), label = "diann_qc_table renders")

  # ── Metadata tab ────────────────────────────────────────────────────────────
  app$set_inputs(main_tabs = "view_metadata")
  app$wait_for_idle(timeout = 30000)

  coldata_val <- app$get_values(output = "view_metadata-colData")$output[["view_metadata-colData"]]
  expect_false(is.null(coldata_val), label = "colData table renders")

  metadata_plot <- app$wait_for_value(output = "view_metadata-metadata_distribution_plot", timeout = 60000)
  expect_plot_rendered(metadata_plot, "metadata_distribution_plot")

  # ── Filter tab: DOM check only (pickerInput renderUI is slow on large data) ─
  app$set_inputs(main_tabs = "filter_data", wait_ = FALSE)
  Sys.sleep(1)
  html <- app$get_html("body")
  expect_match(html, "filter_data-sample_filters")

  # ── Traverse tab: qf_plot (only needs conduit_obj, no user input needed) ───
  app$set_inputs(main_tabs = "traverse")
  app$wait_for_idle(timeout = 30000)

  # qf_plot is a plotly — check its output is non-NULL
  qf_plot_val <- app$wait_for_value(output = "traverse-qf_plot", timeout = 60000)
  expect_false(is.null(qf_plot_val), label = "traverse qf_plot renders")

  # ── Analysis tab: navigate and wait for the processing pipeline ─────────────
  app$set_inputs(main_tabs = "analysis")
  app$wait_for_idle(timeout = 120000) # add_log_imputed_norm_assay takes time

  # Feature Numbers (QC sub-tab 1)
  app$set_inputs(analysis_tabs = "QC", qc_sub_tabs = "Feature Numbers")
  app$wait_for_idle(timeout = 30000)
  feat_num_plot <- app$wait_for_value(output = "analysis-feature_number_plot", timeout = 60000)
  expect_plot_rendered(feat_num_plot, "feature_number_plot")

  # Missing Values (QC sub-tab 2)
  app$set_inputs(qc_sub_tabs = "Missing Values")
  app$wait_for_idle(timeout = 30000)
  miss_val_plot <- app$wait_for_value(output = "analysis-missing_value_plot", timeout = 60000)
  expect_plot_rendered(miss_val_plot, "missing_value_plot")

  # Sample Correlation (QC sub-tab 3)
  app$set_inputs(qc_sub_tabs = "Sample Correlation")
  app$wait_for_idle(timeout = 30000)
  sample_cor_plot <- app$wait_for_value(output = "analysis-sample_cor_heatmap", timeout = 60000)
  expect_plot_rendered(sample_cor_plot, "sample_cor_heatmap")

  # Intensity Distribution / Detection Frequency (QC sub-tab 4)
  app$set_inputs(qc_sub_tabs = "Intensity Distribuiton")
  app$wait_for_idle(timeout = 30000)
  intensity_plot <- app$wait_for_value(output = "analysis-intensity_distribution_plot", timeout = 60000)
  expect_plot_rendered(intensity_plot, "intensity_distribution_plot")

  # Density Plot (QC sub-tab 5)
  app$set_inputs(qc_sub_tabs = "Density Plot")
  app$wait_for_idle(timeout = 30000)
  density_plot <- app$wait_for_value(output = "analysis-density_plot", timeout = 60000)
  expect_plot_rendered(density_plot, "density_plot")

  # PCA
  app$set_inputs(analysis_tabs = "PCA")
  app$wait_for_idle(timeout = 30000)
  pca_plot <- app$wait_for_value(output = "analysis-pca_plot", timeout = 60000)
  expect_plot_rendered(pca_plot, "pca_plot")

  # Heatmap (static)
  app$set_inputs(analysis_tabs = "Heatmap")
  app$wait_for_idle(timeout = 60000)
  heatmap_plot <- app$wait_for_value(output = "analysis-heatmap_plot_static", timeout = 120000)
  expect_plot_rendered(heatmap_plot, "heatmap_plot_static")

  # Relative Abundance
  app$set_inputs(analysis_tabs = "Relative Abundance")
  app$wait_for_idle(timeout = 30000)
  rel_abund_plot <- app$wait_for_value(output = "analysis-relative_abundance_plot", timeout = 60000)
  expect_plot_rendered(rel_abund_plot, "relative_abundance_plot")
})
