library(shiny)
library(conduitR)
library(waiter)
library(digest)
# Server
server <- function(input, output, session) {
  ##############################################################################
  # User Experience
  ##############################################################################
  # Hide the Enrichment menu item immediately after load
  # Delay hiding until sidebar is rendered
  shiny::observe({
    shinyjs::runjs(
      "
      setTimeout(function() {
        var el = document.querySelector('a[data-value=\"enrichment\"]');
        if (el) {
          el.parentElement.style.display = 'none';
        }
      }, 500);
    "
    )
  })
  # Hide the Pathway Tab
  shiny::observe({
    shinyjs::runjs(
      "
    setTimeout(function() {
      var pathwayTab = document.querySelector('a[data-value=\"pathway\"]');
      if (pathwayTab) {
        pathwayTab.parentElement.style.display = 'none';
      }
    }, 500);
  "
    )
  })

  # Show progress using Hostess on load
  # hostess <- Hostess$new("loader")
  #
  # shiny::observe({
  #   for (i in 1:10) {
  #     Sys.sleep(0.2)
  #     hostess$set(i * 10)
  #   }
  #   waiter_hide()
  # })

  # Observe the plot theme input and update the global theme
  observeEvent(input$plot_theme, {
    set_plot_theme(input$plot_theme)
  })

  # ##############################################################################
  # # Hiding menu items initially, then have them appear once data is loaded
  # ##############################################################################
  ## Initially Hide Tabs ##
  # Disable sidebar tabs at startup
  shiny::observe({
    tabs_to_disable <- c(
      "view_metadata",
      "filter_data",
      "analysis",
      "diann_qc",
      "traverse"
    )
    lapply(tabs_to_disable, function(tab) {
      shinyjs::runjs(sprintf(
        "$('.sidebar-menu a[data-value=\"%s\"]').addClass('disabled-tab');",
        tab
      ))
    })
  })

  shiny::observe({
    req(input$conduit.rds) # Only proceeds when file is uploaded
    tabs_to_enable <- c(
      "view_metadata",
      "filter_data",
      "analysis",
      "diann_qc",
      "traverse"
    )
    lapply(tabs_to_enable, function(tab) {
      shinyjs::runjs(sprintf(
        "$('.sidebar-menu a[data-value=\"%s\"]').removeClass('disabled-tab');",
        tab
      ))
    })
  })

  ###############################################################################
  # Loading + Manipulating Data
  ###############################################################################
  # Read in data from .rds conduit output
  conduit_obj <- reactive({
    req(input$conduit.rds) # Ensure file is uploaded
    readRDS(input$conduit.rds$datapath)
  })

  # Extracting the QFeatures object
  qf <- reactive(slot(conduit_obj(), "QFeatures"))

  metrics <- reactive({
    req(conduit_obj())
    slot(conduit_obj(), "metrics")
  })

  rowData <- reactive({
    req(qf())
    SummarizedExperiment::rowData(qf()) # ✅ Fixed conduit_obj()()
  })

  colData <- reactive({
    req(qf())
    SummarizedExperiment::colData(qf()) # ✅ Fixed conduit_obj()()
  })

  ##############################################################################
  # Populating the Statistics Box
  ##############################################################################

  # Extract statistics from QFeatures object
  output$num_samples <- renderText({
    req(conduit_obj())
    nrow(colData()) # Number of samples
  })

  output$num_species_detected <- renderText({
    req(conduit_obj())
    metrics()$protein_coverage_species |>
      dplyr::pull(taxon) |>
      length()
  })

  output$per_species_detected <- renderText({
    req(conduit_obj())
    metrics()$protein_coverage_species |>
      dplyr::pull(taxon) |>
      length()
  })

  output$num_protein_detected <- renderText({
    req(rowData())
    nrow(rowData())
  })

  output$per_proteins_detected <- renderText({
    req(conduit_obj())
    nrow(SummarizedExperiment::rowData(qf()[["protein_groups"]]))
  })

  ##############################################################################
  # Creating the Heat Tree and Data Table
  ##############################################################################

  # Generating Plot
  taxa_tree_plot <- reactive({
    conduitR::plot_percent_detected_taxa_tree(conduit_obj())
  })

  # Rendering plot
  output$taxa_tree_plot <- renderPlot({
    taxa_tree_plot()
  })

  protein_taxonomy_table <- reactive({
    req(metrics())
    dplyr::arrange(metrics()$protein_coverage_species, dplyr::desc(coverage))
  })

  output$protein_taxonomy <- DT::renderDT({
    protein_taxonomy_table()
  })

  # Download Table
  output$download_protein_taxonomy <- downloadHandler(
    filename = function() {
      paste0("protein_taxonomy_coverage.csv")
    },
    content = function(file) {
      # Save the reactive table as CSV
      readr::write_csv(protein_taxonomy_table(), file)
    }
  )
  ################################################################################
  # Header
  ################################################################################

  # Aggregation choices (assays)

  selected_assay <- reactiveVal(NULL)

  possible_assays <- reactive({
    req(qf())
    names(qf())
  })

  observeEvent(
    qf(),
    {
      default_choice <- if ("protein_groups" %in% possible_assays()) {
        "protein_groups"
      } else {
        possible_assays()[[1]]
      }
      if (is.null(selected_assay())) {
        selected_assay(default_choice)
      }
    },
    once = TRUE
  )

  output$agg_level_choices_ui <- renderUI({
    req(conduit_obj(), qf())

    selectInput(
      "agg_level_choices",
      "Choose QFeatures Assay",
      choices = possible_assays(),
      selected = selected_assay()
    )
  })

  observeEvent(input$agg_level_choices, {
    selected_assay(input$agg_level_choices)
  })

  ################################################################################
  # DIA-NN QC Tab
  ################################################################################
  output$diann_qc_columns_ui <- renderUI({
    req(metrics()) # Ensure metrics() is available before proceeding
    selectInput(
      "diann_qc_metric",
      "Choose QC metric to plot",
      choices = names(metrics()$diann_stats), # Ensure colData() is reactive
      selected = "Proteins.Identified"
    )
  })

  diann_qc_plot <- reactive({
    req(
      conduit_obj(),
      input$diann_qc_metric
    )
    conduitR::plot_conduit_metric_diann(conduit_obj(), input$diann_qc_metric)
  })

  output$diann_qc_plot <- renderPlot({
    diann_qc_plot()
  })

  diann_stats <- reactive({
    require(metrics())
    metrics()$diann_stats
  })

  output$diann_qc_table <- DT::renderDT({
    diann_stats()
  })

  # Download Table
  output$download_diann_stats <- downloadHandler(
    filename = function() {
      paste0("diann_stats.csv")
    },
    content = function(file) {
      # Save the reactive table as CSV
      readr::write_csv(diann_stats(), file)
    }
  )
  ################################################################################
  # Metadata Tab
  ################################################################################
  output$num_continuous_variables <- renderText({
    req(colData())
    sum(sapply(colData(), is.numeric))
  })

  output$num_discrete_variables <- renderText({
    req(colData())
    sum(sapply(colData(), function(x) !is.numeric(x)))
  })

  output$colData <- DT::renderDataTable({
    req(colData())
    DT::datatable(
      as.data.frame(colData()),
      options = list(scrollX = TRUE)
    )
  })

  # Download Table
  output$download_colData_table <- downloadHandler(
    filename = function() {
      paste0("colData.csv")
    },
    content = function(file) {
      # Save the reactive table as CSV
      readr::write_csv(as.data.frame(colData()), file)
    }
  )

  output$metadata_variable_choices_to_plot_ui <- renderUI({
    req(colData()) # Ensure colData() is available before proceeding
    selectInput(
      "metadata_variable_choices_to_plot",
      "Choose variable to plot",
      choices = names(colData()), # Ensure colData() is reactive
      selected = NULL
    )
  })

  metadata_distribution_plot <- reactive({
    req(
      conduit_obj(),
      input$metadata_variable_choices_to_plot,
      colData()
    ) # Ensure colData() is available before proceeding
    conduitR::plot_colData_distribution(
      conduit_obj(),
      input$metadata_variable_choices_to_plot
    )
  })

  output$metadata_distribution_plot <- renderPlot({
    metadata_distribution_plot()
  })

  ##############################################################################
  # Data Filter
  ##############################################################################
  # Get sample and feature variables dynamically
  sample_vars <- reactive({
    req(colData()) # Ensure colData() is available
    colnames(colData()) # Extract sample metadata column names
  })

  feature_vars <- reactive({
    req(qf(), selected_assay()) # Ensure qf() and selected assay exist
    colnames(SummarizedExperiment::rowData(qf()[[selected_assay()]])) # Get feature metadata columns
  })

  # Create dynamic UI for sample metadata filters
  output$sample_filters <- renderUI({
    req(input$conduit.rds, sample_vars(), colData()) # Ensure sample variables exist
    lapply(sample_vars(), function(var) {
      vals <- unique(colData()[[var]]) # Use reactive colData()
      pickerInput(
        paste0("sample_", var),
        label = var,
        choices = vals,
        selected = vals,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    })
  })

  # Create dynamic UI for feature metadata filters
  output$feature_filters <- renderUI({
    req(qf(), input$conduit.rds, selected_assay()) # Ensure qf() and selected assay exist

    # Validate assay selection
    if (!(selected_assay() %in% names(qf()))) {
      return(NULL) # Prevents errors if assay name is invalid
    }

    lapply(feature_vars(), function(var) {
      vals <- unique(SummarizedExperiment::rowData(qf()[[selected_assay()]])[[
        var
      ]])
      pickerInput(
        paste0("feature_", var),
        label = var,
        choices = vals,
        selected = vals,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    })
  })

  # Reactive function to filter QFeatures object
  filtered_qf <- reactive({
    req(qf(), colData(), selected_assay(), sample_vars(), feature_vars())

    qf_filtered <- qf() # Copy original QFeatures object

    # Apply sample filters
    for (var in sample_vars()) {
      sel <- input[[paste0("sample_", var)]]
      if (!is.null(sel) && length(sel) > 0) {
        keep_samples <- SummarizedExperiment::colData(qf_filtered)[[var]] %in%
          sel # ✅ Keep colData() as originally written
        if (!any(keep_samples)) {
          return(NULL)
        } # Prevent errors if no samples are selected
        qf_filtered <- qf_filtered[, keep_samples] # ✅ Remove `drop = FALSE`
      }
    }

    # Apply feature filters **only for the selected assay**
    if (!(selected_assay() %in% names(qf_filtered))) {
      return(NULL)
    } # ✅ Check if assay exists

    se <- qf_filtered[[selected_assay()]]
    if (is.null(se) || nrow(se) == 0) {
      return(NULL)
    } # ✅ Ensure se is valid

    for (var in feature_vars()) {
      sel <- input[[paste0("feature_", var)]]
      if (!is.null(sel) && length(sel) > 0) {
        keep_features <- SummarizedExperiment::rowData(se)[[var]] %in% sel
        if (!any(keep_features)) {
          return(NULL)
        } # Prevent errors if no features are selected
        se <- se[keep_features, ] # ✅ Ensure proper subsetting
      }
    }

    qf_filtered[[selected_assay()]] <- se # ✅ Update filtered QFeatures object
    qf_filtered # ✅ Return filtered QFeatures object
  })

  # Show filtered QFeatures object structure (instead of datatable)
  output$filtered_qfeatures <- renderPrint({
    req(filtered_qf())
    attr(filtered_qf(), "ExperimentList")
  })
  ##############################################################################
  # Add Log Transformation, Imputation, Normalization, and Relative Abundance
  ##############################################################################
  final_qf <- reactive({
    req(
      filtered_qf(),
      selected_assay(),
      input$log_base,
      input$imputation_method,
      input$normalization_method
    )

    message <- HTML(paste0(
      "<p>Your <b>",
      selected_assay(),
      "</b> data is being processed with the following settings:</p>",
      "<ul>",
      "<li><b>Log base:</b> ",
      input$log_base,
      "</li>",
      "<li><b>Imputation method:</b> ",
      input$imputation_method,
      "</li>",
      "<li><b>Normalization method:</b> ",
      input$normalization_method,
      "</li>",
      "</ul>",
      "<p>Please wait while the data is being processed...</p>",
      "<p><i>If you’d like to change any of the options above, you can do so from the tabs at the top of the screen.</i></p>"
    ))

    shinyalert::shinyalert(
      title = "Data Processing",
      text = message,
      type = "info",
      html = TRUE,
      showCancelButton = FALSE,
      closeOnClickOutside = FALSE,
      showConfirmButton = FALSE,
      size = "l"
    )

    # Compute updated QFeatures object
    new_qf <- conduitR::add_log_imputed_norm_assay(
      filtered_qf(),
      assay = selected_assay(),
      base = input$log_base,
      impute_method = input$imputation_method,
      norm_method = input$normalization_method
    )

    # Store relative abundance information for the taxonomic data
    if (
      selected_assay() %in%
        c(
          "domain",
          "kingdom",
          "phylum",
          "class",
          "order",
          "family",
          "genus",
          "species"
        )
    ) {
      new_qf <- conduitR::add_relative_abundance_assay(new_qf, selected_assay())
    }

    # removeModal()  # Done processing
    shinyalert::closeAlert()
    new_qf
  })

  ##############################################################################
  # Analysis
  ##############################################################################
  # We need to define what input we actually want to show the user. We don't
  # Want them to have to select from a long options of assays log2, log2_imputed,
  # log_2_imputed_norm, etc - so we will just show the "processed assay"
  # For some of the plots (mostly the QC plots), it makes sense to look at the
  # Non-normalized values, this way we will have all of the assays stored in the qfeatures object,
  # and can handle this behind the scenes as to not confuse the user.
  processed_assay <- reactive({
    req(final_qf(), selected_assay())
    # Logic to define the processed assay.
    if (input$normalization_method == "none") {
      paste0(selected_assay(), "_log", input$log_base, "_imputed")
    } else {
      paste0(
        selected_assay(),
        "_log",
        input$log_base,
        "_imputed_",
        input$normalization_method
      )
    }
  })

  # What are the names of our colData?
  final_colData_names <- reactive({
    req(final_qf())
    names(SummarizedExperiment::colData(final_qf()))
  })

  # What are the names of our colData?
  final_rowData_names <- reactive({
    req(final_qf())
    names(SummarizedExperiment::rowData(final_qf()[[processed_assay()]]))
  })

  ##############################################################################
  # QC Plots
  ##############################################################################
  ### Feature number plot ###

  feature_number_plot <- reactive({
    req(final_qf(), selected_assay())
    DEP::plot_coverage(final_qf()[[selected_assay()]]) +
      ggplot2::ylab(paste0("Number of ", selected_assay()))
  })
  output$feature_number_plot <- renderPlot({
    feature_number_plot()
  })

  ### Missing Value Heatmap ###

  # UI for selecting colData color variables
  output$miss_val_heatmap_col_color_choices_ui <- renderUI({
    req(final_colData_names())
    selectInput(
      "miss_val_heatmap_col_color_choices",
      "Choose columns to color",
      choices = c(final_colData_names(), NULL),
      selected = NULL,
      multiple = TRUE
    )
  })

  # UI for selecting rowData color variables
  output$miss_val_heatmap_row_color_choices_ui <- renderUI({
    req(final_rowData_names())
    selectInput(
      "miss_val_heatmap_row_color_choices",
      "Choose row annotations",
      choices = c(final_rowData_names(), NULL),
      selected = NULL,
      multiple = TRUE
    )
  })

  # Creating missing value plot
  missing_value_plot <- reactive({
    conduitR::plot_missing_val_heatmap(
      final_qf(),
      assay_name = selected_(),
      col_color_variables = input$miss_val_heatmap_col_color_choices,
      row_color_variables = input$miss_val_heatmap_row_color_choices,
      scale = FALSE
    )
  })

  output$missing_value_plot <- renderPlot({
    missing_value_plot()
  })

  ### Sample Correlation ###

  # UI for selecting color variables
  output$sample_cor_heatmap_color_choices_ui <- renderUI({
    req(final_colData_names())
    selectInput(
      "sample_cor_heatmap_color_choices",
      "Choose annotation",
      choices = final_colData_names(),
      selected = NULL,
      multiple = TRUE
    )
  })
  # Creating Plot
  sample_cor_heatmap <- reactive({
    conduitR::plot_sample_cor_heatmap(
      final_qf(),
      assay_name = processed_assay(),
      sample_annotation_variables = input$sample_cor_heatmap_color_choices
    )
  })

  output$sample_cor_heatmap <- renderPlot({
    sample_cor_heatmap()
  })

  ### Intensity Distribution ###

  output$intensity_distribution_plot <- renderPlot({
    DEP::plot_detect(final_qf()[[selected_assay()]])
  })

  ### Density Plot ###

  # Rendering the UI Color Choice Selection
  output$density_plot_color_choice_ui <- renderUI({
    req(final_colData_names())
    selectInput(
      "density_plot_color_choice",
      "Choose color variable",
      choices = final_colData_names(),
      selected = ""
    )
  })

  # Creating Plot
  density_plot <- reactive({
    req(
      final_qf(),
      selected_assay(),
      input$log_base,
      input$density_plot_color_choice
    )

    conduitR::plot_density(
      final_qf(),
      assay_name = selected_assay(),
      input$log_base,
      input$density_plot_color_choice
    )
  })

  output$density_plot <- renderPlot({
    density_plot()
  })

  ##############################################################################
  # PCA
  ##############################################################################
  # Rendering the UI Color Choice Selection
  output$pca_plot_color_choice_ui <- renderUI({
    req(final_colData_names())
    selectInput(
      "pca_plot_color_choice",
      "Choose color variable",
      choices = c("none" = "", final_colData_names(), NULL),
      selected = ""
    )
  })

  # Rendering the UI Shape Choice Selection
  output$pca_plot_shape_choice_ui <- renderUI({
    req(final_colData_names())
    selectInput(
      "pca_plot_shape_choice",
      "Choose shape variable",
      choices = c("none" = "", final_colData_names()),
      NULL,
      selected = ""
    )
  })

  pca_plot <- reactive({
    conduitR::plot_biplot(
      final_qf(),
      assay_name = processed_assay(),
      color = input$pca_plot_color_choice,
      shape = input$pca_plot_shape_choice,
      facet_formula = as.formula(input$pca_plot_formula)
    )
  })

  # Rendering the PCA plot
  output$pca_plot <- renderPlot({
    pca_plot()
  })

  ##############################################################################
  # Heatmap Plotting
  ##############################################################################
  # UI for selecting number of variables:
  output$heatmap_feature_number_ui <- renderUI({
    req(final_qf(), processed_assay())
    sliderInput(
      "heatmap_feature_number",
      "Max N Of Rows To Show",
      min = 1,
      max = nrow(SummarizedExperiment::rowData(final_qf()[[processed_assay()]])),
      value = 1000
    )
  })

  # UI for selecting color variables
  output$heatmap_col_color_choices_ui <- renderUI({
    req(final_colData_names())
    selectInput(
      "heatmap_col_color_choices",
      "Choose columns to color",
      choices = final_colData_names(),
      selected = NULL,
      multiple = TRUE
    )
  })

  output$heatmap_row_color_choices_ui <- renderUI({
    req(final_rowData_names())
    selectInput(
      "heatmap_row_color_choices",
      "Choose row annotations",
      choices = final_rowData_names(),
      selected = NULL,
      multiple = TRUE
    )
  })

  # Dynamically swap between interactive and static outputs
  output$heatmap_plot_ui <- renderUI({
    if (input$heatmap_plot_type == "interactive") {
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("heatmap_plotly", height = "600px"),
        type = 8,
        caption = "One interactive heatmap coming up...",
        color = "#15131efe"
      )
    } else {
      shinycssloaders::withSpinner(
        plotOutput("heatmap_plot_static", height = "600px"),
        type = 8,
        caption = "One static heatmap is on the way...",
        color = "#15131efe"
      )
    }
  })

  heatmap_qf <- reactive({
    req(final_qf(), processed_assay(), input$heatmap_feature_number)

    qf <- final_qf()
    assay_name <- processed_assay()
    assay_obj <- qf[[assay_name]]
    mat <- SummarizedExperiment::assay(qf[[assay_name]])

    # Compute variances
    variances <- apply(mat, 1, var, na.rm = TRUE)
    n <- min(input$heatmap_feature_number, nrow(mat))
    top_n_idx <- order(variances, decreasing = TRUE)[seq_len(n)]

    # Subset the assay
    assay_obj <- assay_obj[top_n_idx, ]

    # Reconstruct QFeatures with just the one modified assay
    qf_single <- qf[NULL] # Drop all assays
    qf_single[[assay_name]] <- assay_obj # Add back just the one

    qf_single
  })

  # Shared args for both plot functions
  heatmap_args <- reactive({
    list(
      qf = heatmap_qf(),
      assay_name = processed_assay(),
      col_color_variables = input$heatmap_col_color_choices,
      row_color_variables = input$heatmap_row_color_choices
    )
  })

  # Plotting the heatmaps!
  # Static heatmap
  heatmap_plot_static <- reactive({
    req(input$heatmap_plot_type == "static", heatmap_args())
    do.call(conduitR::plot_heatmap, heatmap_args())
  })

  output$heatmap_plot_static <- renderPlot({
    heatmap_plot_static()
  })

  # Interactive heatmap
  output$heatmap_plotly <- plotly::renderPlotly({
    req(input$heatmap_plot_type == "interactive", heatmap_args())
    do.call(conduitR::plot_heatmaply, heatmap_args())
  })

  ##############################################################################
  # Relative Abundance
  ##############################################################################

  # Selecting the relative abundance assay
  relative_abundance_assay <- reactive({
    paste0(selected_assay(), "_rel_abundance")
  })

  relative_abundance_plot <- reactive({
    req(final_qf(), relative_abundance_assay())
    if (
      relative_abundance_assay() %in%
        paste0(
          c(
            "domain",
            "kingdom",
            "phylum",
            "class",
            "order",
            "family",
            "geuns",
            "species"
          ),
          "_rel_abundance"
        )
    ) {
      conduitR::plot_relative_abundance(
        final_qf(),
        assay_name = relative_abundance_assay(),
        facet_formula = input$relative_abundance_plot_formula
      )
    } else {
      message <- "Relative Abundance Plots only Supported For Taxonomic Aggregations"
      ggplot2::ggplot() +
        ggplot2::theme_void() + # Remove all axes and background
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = message,
          size = 6,
          hjust = 0.5,
          vjust = 0.5
        ) +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1)
    }
  })

  # Plotting the relative abundance
  output$relative_abundance_plot <- renderPlot({
    relative_abundance_plot()
  })

  ##############################################################################
  # Statistics
  ##############################################################################

  # Show Possible Contrasts
  output$possible_contrasts <- renderText({
    req(final_qf(), processed_assay(), input$limma_formula)
    # Retrieve the possible contrast terms from the first renderUI
    contrast_terms <- conduitR::find_possible_contrast_terms(
      final_qf(),
      processed_assay(),
      as.formula(input$limma_formula)
    )
  })

  # Perform and Process Statistics
  limma_stats_results <- reactive({
    req(
      final_qf(),
      processed_assay(),
      input$limma_formula,
      input$limma_contrast
    )

    # Getting the results of the statistics
    results <- conduitR::perform_limma_analysis(
      final_qf(),
      assay_name = processed_assay(),
      formula = as.formula(input$limma_formula),
      contrast = input$limma_contrast
    )$top_table
  })

  # Generate Statistics Table.
  # This is somewhat complicated because we want to render a table that looks nice.
  # This means it shouldn't show too many significant figures because it gets hard to look at
  # However, we need sig figs for the p values specifically because they are all so low that
  # there will be a ton with a p.adj of 0 on the volcano plot, which is not desired.
  output$limma_statistics_table <- DT::renderDataTable({
    DT::datatable(
      limma_stats_results() |>
        dplyr::mutate(dplyr::across(
          where(is.character),
          ~ ifelse(
            nchar(.) > 15,
            paste0("<span title='", ., "'>", substr(., 1, 15), "...</span>"),
            .
          )
        )),
      escape = FALSE, # Allow HTML for tooltips
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE
      )
    ) |>
      DT::formatStyle(
        columns = names(limma_stats_results()),
        `white-space` = "normal",
        `word-wrap` = "break-word"
      )
  })

  # Download Table
  output$download_limma_stats_table <- downloadHandler(
    filename = function() {
      paste0("limma_statistics.csv")
    },
    content = function(file) {
      # Save the reactive table as CSV
      readr::write_csv(as.data.frame(limma_stats_results()), file)
    }
  )

  # Render options for coloring the points on the graph.
  output$limma_volcano_color_ui <- renderUI({
    req(final_rowData_names())
    selectInput(
      "limma_volcano_color",
      "Choose how the points are colored",
      choices = c("none" = "", final_rowData_names()),
      selected = "",
      multiple = FALSE
    )
  })

  # Plot volcano plot

  limma_volcano_plot <- reactive({
    req(
      limma_stats_results(),
      input$limma_fc_threshold,
      input$limma_p_threshold,
      input$limma_volcano_facet_formula
    )

    conduitR::plot_volcano(
      limma_stats_results(),
      facet_formula = as.formula(input$limma_volcano_facet_formula),
      color_by = input$limma_volcano_color,
      pval_threshold = input$limma_p_threshold
    ) +
      ggplot2::geom_vline(
        xintercept = input$limma_fc_threshold,
        linetype = "dashed",
        color = "red"
      ) +
      ggplot2::geom_vline(
        xintercept = -input$limma_fc_threshold,
        linetype = "dashed",
        color = "red"
      )
  })
  output$limma_volcano_plot <- renderPlot({
    limma_volcano_plot()
  })

  # Selected Feature Plot

  # UI Generation

  final_colData_and_rowData_names <- reactive({
    unique(c(final_colData_names(), final_rowData_names()))
  })

  # X axis
  output$selected_feature_plot_x_axis_ui <- renderUI({
    req(final_colData_and_rowData_names())
    selectInput(
      "selected_feature_plot_x_axis",
      "Choose x axis",
      choices = final_colData_and_rowData_names(),
      multiple = FALSE
    )
  })

  # Facet wrap
  output$selected_feature_plot_facet_formula_ui <- renderUI({
    textInput(
      "selected_feature_plot_facet_formula",
      "Choose facet formula",
      value = "~ NULL"
    )
  })

  # Color
  output$selected_feature_plot_color_ui <- renderUI({
    req(final_colData_and_rowData_names())
    selectInput(
      "selected_feature_plot_color",
      "Choose color",
      choices = c("none" = "", final_colData_and_rowData_names()),
      multiple = FALSE,
      selected = ""
    )
  })

  # Shape
  output$selected_feature_plot_shape_ui <- renderUI({
    req(final_colData_and_rowData_names())
    selectInput(
      "selected_feature_plot_shape",
      "Choose shape",
      choices = c("none" = "", final_colData_and_rowData_names()),
      multiple = FALSE,
      selected = ""
    )
  })

  # Features are selected from the data table
  selected_features <- reactive({
    req(
      limma_stats_results(),
      input$limma_statistics_table_rows_selected
    )
    limma_stats_results() |>
      dplyr::slice(input$limma_statistics_table_rows_selected) |>
      dplyr::pull(id)
  })

  # Creating Plot
  selected_feature_plot <- reactive({
    req(
      selected_features(),
      processed_assay(),
      input$selected_feature_plot_x_axis,
      input$selected_feature_plot_color,
      input$selected_feature_plot_shape,
      input$selected_feature_plot_facet_formula
    )

    conduitR::plot_selected_features(
      final_qf(),
      assay_name = processed_assay(),
      features = selected_features(),
      x_axis = input$selected_feature_plot_x_axis,
      color_by = input$selected_feature_plot_color,
      shape = input$selected_feature_plot_shape,
      facet_formula = as.formula(input$selected_feature_plot_facet_formula)
    )
  })

  output$selected_feature_plot <- renderPlot({
    selected_feature_plot()
  })
  ##############################################################################
  # Enrichment Analysis
  ##############################################################################

  # Generating the Tab that will let you navigate to enrichment analysis
  observeEvent(input$enrichment_analysis_button, {
    updateTabItems(session, "main_tabs", "enrichment")
  })

  # Render UI for Enrichment Analysis Choices To Match Plot Types Dynamically
  output$enrichment_plot_options_ui <- renderUI({
    req(input$enrichment_type)
    if (input$enrichment_type == "gsea") {
      selectInput(
        "enrichment_plot_type",
        "Choose Enrichment Plot Type",
        choices = c("ridgeplot", "dotplot", "treeplot", "upsetplot"),
        selected = "ridgeplot"
      )
    } else if (input$enrichment_type == "ora") {
      selectInput(
        "enrichment_plot_type",
        "Choose Enrichment Plot Type",
        choices = c(
          "barplot",
          "dotplot",
          "treeplot",
          "upsetplot",
          "cnetplot"
        ),
        selected = "barplot"
      )
    }
  })
  # Define the direction to consider
  output$enrichment_direction_ui <- renderUI({
    req(input$enrichment_type)
    if (input$enrichment_type == "gsea") {
      selectInput(
        "enrichment_direction",
        "Direction of Change for Enrichment",
        choices = c("both"),
        multiple = FALSE,
        selected = "both"
      )
    } else if (input$enrichment_type == "ora") {
      selectInput(
        "enrichment_direction",
        "Direction of Change for Enrichment",
        choices = c("up", "down"),
        multiple = FALSE,
        selected = "up"
      )
    }
  })

  # Perform Enrichment
  enrichment_results <- reactive({
    req(
      limma_stats_results(),
      conduit_obj(),
      input$annotation_type,
      input$enrichment_type
    )

    # Perform GSEA
    if (input$enrichment_type == "gsea") {
      conduitR::perform_gsea(
        limma_stats_results(),
        conduit = conduit_obj(),
        annotation_type = input$annotation_type,
        ranking_column = "logFC"
      )
      # Perform ORA
    } else if (input$enrichment_type == "ora") {
      req(
        input$enrichment_direction,
        input$limma_fc_threshold
      )
      conduitR::perform_ora(
        limma_stats_results(),
        direction = input$enrichment_direction,
        conduit = conduit_obj(),
        annotation_type = input$annotation_type,
        adj_pval_threshold = input$limma_p_threshold,
        logFC_threshold = if (input$enrichment_direction == "down") {
          input$limma_fc_threshold * -1
        } else {
          input$limma_fc_threshold
        }
      )
    }
  })

  # Printing a Summary of Enrichment Object
  output$enrichment_summary <- renderPrint({
    req(enrichment_results())
    enrichment_results()
  })

  # Generating the requested plot

  enrichment_plot <- reactive({
    req(enrichment_results())
    if (input$enrichment_plot_type == "ridgeplot") {
      enrichplot::ridgeplot(enrichment_results())
    } else if (input$enrichment_plot_type == "dotplot") {
      enrichplot::dotplot(enrichment_results())
    } else if (input$enrichment_plot_type == "treeplot") {
      pw <- enrichplot::pairwise_termsim(enrichment_results())
      enrichplot::treeplot(pw)
    } else if (input$enrichment_plot_type == "barplot") {
      barplot(enrichment_results())
    } else if (input$enrichment_plot_type == "cnetplot") {
      enrichplot::cnetplot(enrichment_results())
    }
  })
  output$enrichment_plot <- renderPlot({
    enrichment_plot()
  })

  # Return to Analysis
  observeEvent(input$enrichment_return_to_stats_button, {
    updateTabItems(session, "main_tabs", "analysis")
    updateTabsetPanel(session, "analysis_tabs", selected = "stats")
  })

  ##############################################################################
  # Pathway Viewing
  ##############################################################################
  # Generating the Tab that will let you navigate to pathway analysis
  observeEvent(input$pathway_analysis_button, {
    updateTabItems(session, "main_tabs", "pathway")
  })

  # Updating UI with avalible KEGG pathways so user can choose one.
  output$pathway_select_ui <- renderUI({
    req(conduit_obj())

    # Extracting information on what pathways are present in conduit
    kegg_pathways <- conduit_obj()@annotations |>
      dplyr::filter(annotation_type == "kegg_pathway") |>
      dplyr::select(organism_id, term, description) |>
      dplyr::distinct()

    # Adding taxonomy to the pathways
    taxonomy <- conduit_obj()@taxonomy |>
      dplyr::select(organism_id, species)

    # Joining together
    kegg_pathway_with_taxa <- dplyr::right_join(
      taxonomy,
      kegg_pathways,
      by = "organism_id"
    ) |>
      # making a human readable id with the species + description
      dplyr::mutate(id = paste0(species, ": ", description))

    possible_kegg_ids <- kegg_pathway_with_taxa$term

    names(possible_kegg_ids) <- kegg_pathway_with_taxa$id

    selectInput(
      "selected_kegg_pathway",
      "Select Kegg Pathway To Show",
      choices = possible_kegg_ids,
      multiple = FALSE
    )
  })

  pathway_plot <- reactive({
    req(
      limma_stats_results(),
      input$selected_kegg_pathway
    )

    plot_kegg_pathway(
      stats_results = limma_stats_results(),
      kegg_pathway_id = input$selected_kegg_pathway
    )
  })
  # Plotting selected kegg plot
  output$pathway_plot <- plotly::renderPlotly({
    plotly::ggplotly(pathway_plot())
  })

  # Enableing travel back to Statistics tab.
  observeEvent(input$pathway_return_to_stats_button, {
    updateTabItems(session, "main_tabs", "analysis")
    updateTabsetPanel(session, "analysis_tabs", selected = "stats")
  })

  ##############################################################################
  # Traverse
  ##############################################################################
  # This tab will allow the user to traverse across the assay links for their
  # given selection.
  observe({
    req(qf())
    req(input$traverse_features)

    # Check which assays contain the selected feature and have >0 rows
    available_assays <- names(qf())

    updateSelectInput(
      inputId = "traverse_assay",
      choices = available_assays
    )
  })

  traverse_data <- reactive({
    req(qf())
    req(input$traverse_features)
    req(input$traverse_assay)

    se <- qf()[input$traverse_features, ][[input$traverse_assay]]

    cd <- colData() |>
      as.data.frame() |>
      tibble::rownames_to_column("sample")

    assay <- as.data.frame(SummarizedExperiment::assay(se)) |>
      tibble::rownames_to_column("feature_id") |>
      tidyr::pivot_longer(
        cols = everything()[-1],
        names_to = "sample",
        values_to = "intensity"
      )

    dat <- dplyr::left_join(assay, cd, by = "sample")

    dat
  })

  observe({
    req(colData())

    # Check which assays contain the selected feature and have >0 rows
    available_variables <- names(colData())

    updateSelectInput(
      inputId = "traverse_xaxis",
      choices = available_variables
    )
  })

  output$qf_plot <- plotly::renderPlotly({
    req(conduit_obj())
    plot(conduit_obj()@QFeatures, interactive = TRUE)
  })

  output$traverse_info <- DT::renderDT({
    req(qf())
    req(input$traverse_features)
    se <- qf()[input$traverse_features, ]
    dims <- sapply(SummarizedExperiment::assays(se), dim)
    rownames(dims) <- c("# Features", "# Samples")
    number_of_features <- dims[1, ]
    df <- as.data.frame(number_of_features)
  })

  traverse_plot <- reactive({
    p1 <- traverse_data() |>
      ggplot2::ggplot(ggplot2::aes(
        !!rlang::sym(input$traverse_xaxis),
        y = intensity
      )) +
      ggplot2::geom_point() +
      ggplot2::ylab(paste(input$traverse_assay, " Intensity"))

    p1
  })

  output$traverse_plot <- renderPlot({
    traverse_plot()
  })

  ##############################################################################
  # Outcome Prediction
  ##############################################################################

  final_colData <- reactive({
    req(final_qf())
    SummarizedExperiment::colData(final_qf())
  })

  numeric_colData_names <- reactive({
    req(final_colData(), final_colData_names())
    final_colData_names()[sapply(final_colData(), is.numeric)]
  })

  non_numeric_colData_names <- reactive({
    req(final_colData(), final_colData_names())
    final_colData_names()[sapply(final_colData(), function(x) !is.numeric(x))]
  })

  # UI For Selecting the Outcome variable
  observe({
    req(non_numeric_colData_names())
    updateSelectInput(
      session,
      inputId = "outcome_var",
      choices = non_numeric_colData_names(),
      selected = NULL
    )
  })

  predict_classification_list <- eventReactive(input$run_classification_model, {
    req(
      final_qf(),
      processed_assay(),
      input$outcome_var,
      input$split_ratio,
      input$model_type,
      input$cv_folds,
      input$random_seed
    )

    set.seed(random_seed)

    message <- HTML(paste0(
      "<p>Your <b>",
      selected_assay(),
      "</b> data is being used to generate a classification model with the following parameters:</p>",
      "<ul>",
      "<li><b>Model Type:</b> ",
      input$model_type,
      "</li>",
      "<li><b>Outcome Variable:</b> ",
      input$outcome_var,
      "</li>",
      "<li><b>Train/Test Split Percentage:</b> ",
      input$split_ratio,
      "</li>",
      "<li><b>Cross-Validation Folds:</b> ",
      input$cv_folds,
      "</li>",
      "</ul>",
      "<p>Please wait while the model is generated...</p>"
    ))

    shinyalert::shinyalert(
      title = "Generating Model",
      text = message,
      type = "info",
      html = TRUE,
      showCancelButton = FALSE,
      closeOnClickOutside = FALSE,
      showConfirmButton = FALSE,
      size = "l"
    )

    result <- conduitR::predict_classification(
      final_qf(),
      assay_name = processed_assay(),
      outcome = input$outcome_var,
      train_percent = input$split_ratio,
      model_type = input$model_type,
      v = input$cv_folds
    )

    shinyalert::closeAlert()
    result
  })

  # Confusion matrix plot
  output$confusion_matrix_plot <- renderPlot({
    req(predict_classification_list())
    plot_confusion_matrix(predict_classification_list())
  })

  # Creating test plot

  output$test_plot <- renderPlot({
    req(predict_classification_list(), input$model_plot_type)
    if (input$model_plot_type == "ROC") {
      plot_roc(
        predict_classification_list(),
        "test"
      )
    } else {
      plot_precision_recall(
        predict_classification_list(),
        "test"
      )
    }
  })

  # Creating Training Plot
  output$train_plot <- renderPlot({
    req(predict_classification_list(), input$model_plot_type)
    if (input$model_plot_type == "ROC") {
      plot_roc(
        predict_classification_list(),
        "training"
      )
    } else {
      plot_precision_recall(
        predict_classification_list(),
        "training"
      )
    }
  })

  # Feature importance

  # Dynamically render the slider only when data is ready
  output$features_to_show_slider <- renderUI({
    req(predict_classification_list())
    max_val <- max(
      length(predict_classification_list()$importance$feature),
      na.rm = TRUE
    )

    sliderInput(
      "features_to_show_slider_ui",
      "Select rank of features to show",
      min = 1,
      max = max_val,
      value = c(0, max_val)
    )
  })

  # Plotting feature importance
  output$feature_importance_plot <- renderPlot({
    req(
      predict_classification_list(),
      input$features_to_show_slider_ui
    )
    plot_feature_importance(
      predict_classification_list(),
      input$features_to_show_slider_ui[1],
      input$features_to_show_slider_ui[2]
    )
  })

  ##############################################################################
  # Handling Plot Downloads
  ##############################################################################
  # Determining what the current plot is and what to show the user.
  current_plot <- reactive({
    req(input$main_tabs)

    main_tab <- input$main_tabs
    analysis_tab <- input$analysis_tabs

    # --- Non-analysis sidebar tabs ---
    if (main_tab != "analysis") {
      switch(
        main_tab,
        "file_upload" = taxa_tree_plot(),
        "diann_qc" = diann_qc_plot(),
        "view_metadata" = metadata_distribution_plot(),
        "enrichment" = enrichment_plot(),
        "pathway" = pathway_plot(),
        "traverse" = traverse_plot(),
        NULL
      )
    } else if (main_tab == "analysis" && !is.null(analysis_tab)) {
      # --- Analysis tab and nested subtabs ---
      switch(
        analysis_tab,
        "QC" = {
          req(input$qc_sub_tabs)
          switch(
            input$qc_sub_tabs,
            "Feature Numbers" = feature_number_plot(),
            "Missing Values" = missing_value_plot(),
            "Sample Correlation" = sample_cor_heatmap(),
            "Intensity Distribution" = intensity_distribution_plot(),
            "Density Plot" = density_plot(),
            NULL
          )
        },
        "PCA" = pca_plot(),
        "Heatmap" = heatmap_plot_static(),
        "Relative Abundance" = relative_abundance_plot(),
        "Statistics" = limma_volcano_plot(),
        "Classification Prediction" = {
          req(input$class_sub_tabs)
          switch(
            input$class_sub_tabs,
            "Confusion Matrix" = confusion_matrix_plot_reactive(),
            "Test Plot" = test_plot_reactive(),
            "Train Plot" = train_plot_reactive(),
            "Feature Importance" = feature_importance_plot_reactive(),
            NULL
          )
        },
        NULL
      )
    } else {
      NULL
    }
  })
  # Handling the download of the selected plots.
  output$download_current_plot <- downloadHandler(
    filename = function() {
      if (input$main_tabs == "analysis") {
        paste0(input$analysis_tabs, ".", input$plot_file_format)
      } else {
        paste0(input$main_tabs, ".", input$plot_file_format)
      }
    },
    content = function(file) {
      req(current_plot())

      plot_obj <- current_plot()
      fmt <- input$plot_file_format

      if ("gg" %in% class(plot_obj)) {
        # ggplot object
        ggplot2::ggsave(
          filename = file,
          plot = plot_obj,
          width = input$plot_width,
          height = input$plot_height,
          device = fmt
        )
      } else if ("pheatmap" %in% class(plot_obj)) {
        # pheatmap object
        pheatmap::pheatmap(
          plot_obj$mat,
          color = plot_obj$color,
          cluster_rows = plot_obj$cluster_rows,
          cluster_cols = plot_obj$cluster_cols,
          filename = file,
          width = input$plot_width,
          height = input$plot_height,
          units = "in",
          dpi = 300
        )
      } else if ("sechm" %in% class(plot_obj)) {
        # sechm object (grid/grob)
        if (fmt == "png") {
          png(
            file,
            width = input$plot_width,
            height = input$plot_height,
            units = "in",
            res = 300
          )
        } else if (fmt == "pdf") {
          pdf(file, width = input$plot_width, height = input$plot_height)
        } else {
          stop("Unsupported file type for sechm object")
        }

        grid::grid.newpage()
        grid::grid.draw(plot_obj)
        dev.off()
      } else {
        stop("Unknown plot type; cannot save")
      }
    }
  )
}
