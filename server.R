library(shiny)
library(conduitR)
# Server
function(input, output, session) {
  ###############################################################################
  # Loading + Manipulating Data
  ###############################################################################
  # Read in data from .rds conduit output
  conduit_obj <- reactive({
    req(input$conduit.rds) # Ensure file is uploaded
    # Read the RDS file
    readRDS(input$conduit.rds$datapath)
  })

  combined_metrics <- reactive({
    req(conduit_obj())
    slot(conduit_obj(), "combined_metrics")
  })

  ##############################################################################
  # Populating the Statistics Box
  ##############################################################################

  # Extract statistics from QFeatures object
  output$num_samples <- renderText({
    req(conduit_obj())
    nrow(SummarizedExperiment::colData(slot(conduit_obj(), "QFeatures"))) # Number of samples
  })

  output$num_sample_md_variables <- renderText({
    req(conduit_obj())
    ncol(SummarizedExperiment::colData(slot(conduit_obj(), "QFeatures"))) # Number of features/variables
  })

  output$num_species_detected <- renderText({
    req(conduit_obj())
    combined_metrics() |>
      dplyr::filter(metric == "species") |>
      dplyr::pull(n_detected)
  })

  output$per_species_detected <- renderText({
    req(conduit_obj())
    combined_metrics() |>
      dplyr::filter(metric == "species") |>
      dplyr::pull(per_detected)
  })

  output$num_proteins_detected <- renderText({
    req(conduit_obj())
    combined_metrics() |>
      dplyr::filter(metric == "protein_id") |>
      dplyr::pull(n_detected)
  })

  output$per_proteins_detected <- renderText({
    req(conduit_obj())
    combined_metrics() |>
      dplyr::filter(metric == "protein_id") |>
      dplyr::pull(per_detected)
  })
  ##############################################################################
  # Creating the Heat Tree and Data Table
  ##############################################################################
  pg_to_consider <- reactiveVal() # Use reactiveVal to store a single reactive value

  observe({
    pg_to_consider(input$pg_to_consider) # Update the reactiveVal
  })

  output$taxa_tree_plot <- renderPlot({
    conduitR::plot_percent_detected_taxa_tree(conduit_obj(),
      type = pg_to_consider(),
      layout = "automatic"
    )
  })


  output$protein_taxonomy <- DT::renderDT(
    conduitR::calc_percent_proteins_detected(conduit_obj(), type = pg_to_consider())
  )
################################################################################
  # Header
################################################################################
    rowData = reactive({
    req(conduit_obj())
    SummarizedExperiment::rowData(slot(conduit_obj(), "QFeatures")) # ✅ Fixed conduit_obj()()
  })

  colData = reactive({
    req(conduit_obj())
    SummarizedExperiment::colData(slot(conduit_obj(), "QFeatures")) # ✅ Fixed conduit_obj()()
  })

  output$plot_color_choices_ui <- renderUI({
    req(colData(), rowData()) # Ensure data is available
    selectInput("plot_color_choices", "Choose Color Variable",
                choices = c(names(colData()), names(rowData())),
                selected = NULL)
  })

  output$plot_shape_choices_ui <- renderUI({
    req(colData(), rowData()) # Ensure data is available
    selectInput("plot_shape_choices", "Choose Shape Variable",
                choices = c(names(colData()), names(rowData())),
                selected = NULL)
  })

  output$agg_level_choices_ui <- renderUI({
    req(conduit_obj())
    selectInput("agg_level_choices", "Choose QFeatures Assay",
                choices = names(slot(conduit_obj(),"QFeatures")),
                selected = "protein_group"
    )
  })
################################################################################
  # Metadata Tab
################################################################################

  output$num_continuous_variables <- renderText({
    req(colData())
    sum(sapply(colData(),is.numeric))
  })

  output$num_discrete_variables <- renderText({
    req(colData())
    sum(sapply(colData(),function(x) !is.numeric(x)))
  })


  output$colData <- DT::renderDataTable({
    req(colData())
    DT::datatable(
      as.data.frame(colData()),
      options = list(scrollX = TRUE)
    )
    })

  output$metadata_variable_choices_to_plot_ui <- renderUI({
    req(colData())  # Ensure colData() is available before proceeding
    selectInput("metadata_variable_choices_to_plot",
                "Choose variable to plot",
                choices = names(colData()),  # Ensure colData() is reactive
                selected = NULL)
  })

  output$metadata_distribution_plot <- renderPlot({
    req(colData())  # Ensure colData() is available before proceeding
    conduitR::plot_colData_distribution(conduit_obj(),input$metadata_variable_choices_to_plot)
  })
  ##############################################################################
  # Data Filter
  ##############################################################################
  # Ensure qf is reactive
  qf <- reactive(slot(conduit_obj(), "QFeatures"))

  # Get sample and feature variables dynamically
  sample_vars <- reactive({
    req(colData())  # Ensure colData() is available
    colnames(colData())  # Extract sample metadata column names
  })

  feature_vars <- reactive({
    req(qf(), input$agg_level_choices)  # Ensure qf() and selected assay exist
    colnames(SummarizedExperiment::rowData(qf()[[input$agg_level_choices]]))  # Get feature metadata columns
  })

  # Create dynamic UI for sample metadata filters
  output$sample_filters <- renderUI({
    req(sample_vars())  # Ensure sample variables exist
    lapply(sample_vars(), function(var) {
      vals <- unique(colData()[[var]])  # Use reactive colData()
      pickerInput(paste0("sample_", var), label = var, choices = vals,
                  selected = vals, multiple = TRUE, options = list(`actions-box` = TRUE))
    })
  })

  # Create dynamic UI for feature metadata filters
  output$feature_filters <- renderUI({
    req(qf(), input$agg_level_choices)  # Ensure qf() and selected assay exist

    # Validate assay selection
    if (!(input$agg_level_choices %in% names(qf()))) {
      return(NULL)  # Prevents errors if assay name is invalid
    }

    # Get feature metadata columns
    feature_vars <- colnames(SummarizedExperiment::rowData(qf()[[input$agg_level_choices]]))

    lapply(feature_vars, function(var) {
      vals <- unique(SummarizedExperiment::rowData(qf()[[input$agg_level_choices]])[[var]])
      pickerInput(paste0("feature_", var), label = var, choices = vals,
                  selected = vals, multiple = TRUE, options = list(`actions-box` = TRUE))
    })
  })

  # Reactive function to filter QFeatures object
  filtered_qf <- reactive({
    req(qf(), input$agg_level_choices, sample_vars(), feature_vars())  # Ensure all needed inputs exist
    qf_filtered <- qf()  # Copy original QFeatures object

    # Apply sample filters using reactive colData()
    for (var in sample_vars()) {
      sel <- input[[paste0("sample_", var)]]
      if (!is.null(sel) && length(sel) > 0) {
        keep_samples <- colData()[[var]] %in% sel  # Use reactive colData()
        qf_filtered <- qf_filtered[, keep_samples, drop = FALSE]
      }
    }

    # Apply feature filters **only for the selected assay**
    selected_assay <- input$agg_level_choices
    if (!is.null(selected_assay)) {
      se <- qf_filtered[[selected_assay]]
      for (var in feature_vars()) {
        sel <- input[[paste0("feature_", var)]]
        if (!is.null(sel) && length(sel) > 0) {
          keep_features <- SummarizedExperiment::rowData(se)[[var]] %in% sel
          se <- se[keep_features, , drop = FALSE]
        }
      }
      qf_filtered[[selected_assay]] <- se  # Update the filtered assay
    }

    qf_filtered  # Return filtered QFeatures object
  })

  # Show filtered QFeatures object structure (instead of datatable)
  output$filtered_output <- renderPrint({
    req(filtered_qf())
    filtered_qf()  # Display high-level structure of the QFeatures object
  })
  }


