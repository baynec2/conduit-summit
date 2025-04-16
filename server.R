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
    shinyjs::runjs("
      setTimeout(function() {
        var el = document.querySelector('a[data-value=\"enrichment\"]');
        if (el) {
          el.parentElement.style.display = 'none';
        }
      }, 500);
    ")
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
    tabs_to_disable <- c("view_metadata", "filter_data", "analysis")
    lapply(tabs_to_disable, function(tab) {
      shinyjs::runjs(sprintf("$('.sidebar-menu a[data-value=\"%s\"]').addClass('disabled-tab');", tab))
    })
  })

  shiny::observe({
    req(input$conduit.rds)  # Only proceeds when file is uploaded

    tabs_to_enable <- c("view_metadata", "filter_data", "analysis")
    lapply(tabs_to_enable, function(tab) {
      shinyjs::runjs(sprintf("$('.sidebar-menu a[data-value=\"%s\"]').removeClass('disabled-tab');", tab))
    })
  })
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

  shiny::observe({
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

  # Aggregation choices (assays)
  output$agg_level_choices_ui <- renderUI({
    req(conduit_obj(),qf())
    selectInput("agg_level_choices", "Choose QFeatures Assay",
                choices = names(slot(conduit_obj(),"QFeatures")),
                selected = names(slot(conduit_obj(),"QFeatures"))[[3]]
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
    req(input$conduit.rds,sample_vars(),colData())  # Ensure sample variables exist
    lapply(sample_vars(), function(var) {
      vals <- unique(colData()[[var]])  # Use reactive colData()
      pickerInput(paste0("sample_", var), label = var, choices = vals,
                  selected = vals, multiple = TRUE, options = list(`actions-box` = TRUE))
    })
  })

  # Create dynamic UI for feature metadata filters
  output$feature_filters <- renderUI({
    req(qf(),input$conduit.rds, input$agg_level_choices)  # Ensure qf() and selected assay exist

    # Validate assay selection
    if (!(input$agg_level_choices %in% names(qf()))) {
      return(NULL)  # Prevents errors if assay name is invalid
    }

    lapply(feature_vars(), function(var) {
      vals <- unique(SummarizedExperiment::rowData(qf()[[input$agg_level_choices]])[[var]])
      pickerInput(paste0("feature_", var), label = var, choices = vals,
                  selected = vals, multiple = TRUE, options = list(`actions-box` = TRUE))
    })
  })

  # Reactive function to filter QFeatures object
  filtered_qf <- reactive({
    req(qf(), colData(), input$agg_level_choices, sample_vars(), feature_vars())

    qf_filtered <- qf()  # Copy original QFeatures object

    # Apply sample filters
    for (var in sample_vars()) {
      sel <- input[[paste0("sample_", var)]]
      if (!is.null(sel) && length(sel) > 0) {
        keep_samples <- SummarizedExperiment::colData(qf_filtered)[[var]] %in% sel  # ✅ Keep colData() as originally written
        if (!any(keep_samples)) return(NULL)  # Prevent errors if no samples are selected
        qf_filtered <- qf_filtered[, keep_samples]  # ✅ Remove `drop = FALSE`
      }
   }

    # Apply feature filters **only for the selected assay**
    selected_assay <- input$agg_level_choices
    if (!(selected_assay %in% names(qf_filtered))) return(NULL)  # ✅ Check if assay exists

    se <- qf_filtered[[selected_assay]]
    if (is.null(se) || nrow(se) == 0) return(NULL)  # ✅ Ensure se is valid

    for (var in feature_vars()) {
      sel <- input[[paste0("feature_", var)]]
      if (!is.null(sel) && length(sel) > 0) {
        keep_features <- SummarizedExperiment::rowData(se)[[var]] %in% sel
        if (!any(keep_features)) return(NULL)  # Prevent errors if no features are selected
        se <- se[keep_features, ]  # ✅ Ensure proper subsetting
      }
    }

    qf_filtered[[selected_assay]] <- se  # ✅ Update filtered QFeatures object
    qf_filtered  # ✅ Return filtered QFeatures object
  })


  # Show filtered QFeatures object structure (instead of datatable)
  output$filtered_qfeatures <- renderPrint({
    req(filtered_qf())
    attr(filtered_qf(),"ExperimentList")
  })
  ##############################################################################
  # Add Log Transformation, Imputation, Normalization, and Relative Abundance
  ##############################################################################
  final_qf <- reactive({
    req(
      filtered_qf(),
      input$log_base,
      input$imputation_method,
      input$normalization_method
    )

    # Compute updated QFeatures object
    new_qf <- conduitR::add_log_imputed_norm_assays(
      filtered_qf(),
      base = input$log_base,
      impute_method = input$imputation_method,
      norm_method = input$normalization_method
    )

    # Store relative abundance information for the taxonomic data
    new_qf <- conduitR::add_relative_abundance_assays(new_qf)
    new_qf
  })

  ##############################################################################
  # Analysis
  ##############################################################################

  ##############################################################################
  # QC Plots
  ##############################################################################

  # Feature number plot
  output$feature_number_plot <- renderPlot({
    DEP::plot_coverage(final_qf()[[input$agg_level_choices]])+
      ggplot2::ylab(paste0("Number of ",input$agg_level_choices))
  })

  # Missing Values

  # UI for selecting color variables
  output$miss_val_heatmap_col_color_choices_ui <- renderUI({
    req(colData())
    selectInput("miss_val_heatmap_col_color_choices", "Choose columns to color",
                choices = names(colData()),
                selected = NULL,
                multiple = TRUE)
  })

  output$miss_val_heatmap_row_color_choices_ui <- renderUI({
    req(final_qf(), input$agg_level_choices)
    selectInput("miss_val_heatmap_row_color_choices", "Choose row annotations",
                choices = colnames(SummarizedExperiment::rowData(final_qf()[[input$agg_level_choices]])),
                selected = NULL,
                multiple = TRUE)
  })

  # Plot
  output$missing_value_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                                  detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.25)
                       }
    conduitR::plot_missing_val_heatmap(final_qf(),
                                       input$agg_level_choices,
                                       input$miss_val_heatmap_col_color_choices,
                                       input$miss_val_heatmap_row_color_choices)
  })
    })

  # Sample Correlation
  # UI for selecting color variables
  output$sample_cor_heatmap_color_choices_ui <- renderUI({
    req(colData())
    selectInput("sample_cor_heatmap_color_choices", "Choose annotation",
                choices = names(colData()),
                selected = NULL,
                multiple = TRUE)
  })

  # Plot
  output$sample_cor_heatmap <- renderPlot({

    # Defining the assay name
    assay_name = paste0(input$agg_level_choices,"_log",input$log_base,"_imputed")

    # Plotting the correlation
    conduitR::plot_sample_cor_heatmap(final_qf(),
                                      assay_name,
                                      input$sample_cor_heatmap_color_choices)

    })


  # Intensity Distributions
  output$intensity_distribution_plot <- renderPlot({
    DEP::plot_detect(final_qf()[[paste0(input$agg_level_choices,"_log",input$log_base)]])
  })

  # Density Plot
  # Rendering the UI Color Choice Selection
  output$density_plot_color_choice_ui <- renderUI({
    req(colData(),req(input$agg_level_choices))
    selectInput("density_plot_color_choice", "Choose color variable",
                choices = c(names(colData()),
                            names(SummarizedExperiment::rowData(final_qf()[[input$agg_level_choices]]))),
                selected = "")
  })

  output$density_plot <- renderPlot({
    conduitR::plot_density(final_qf(),
                           input$agg_level_choices,
                           input$log_base,
                           input$density_plot_color_choice)

  })

  ##############################################################################
  # PCA
  ##############################################################################
  # Rendering the UI Color Choice Selection
  output$pca_plot_color_choice_ui <- renderUI({
    req(colData(),req(input$agg_level_choices))
    selectInput("pca_plot_color_choice", "Choose color variable",
                choices = c("none" = "",
                            names(colData()),
                            names(SummarizedExperiment::rowData(final_qf()[[input$agg_level_choices]]))),
                selected = "")
  })

  # Rendering the UI Shape Choice Selection
  output$pca_plot_shape_choice_ui <- renderUI({
    req(colData(),req(input$agg_level_choices))
    selectInput("pca_plot_shape_choice", "Choose shape variable",
                choices = c("none" = "",
                            names(colData()),
                            names(SummarizedExperiment::rowData(final_qf()[[input$agg_level_choices]]))),
                selected = "")
  })

  # Rendering the PCA plot
  output$pca_plot <- renderPlot({

    conduitR::plot_biplot(final_qf(),
                          assay_name =paste0(input$agg_level_choices,"_log",
                                             input$log_base,
                                             "_imputed"),
                          color = input$pca_plot_color_choice,
                          shape = input$pca_plot_shape_choice,
                          facet_formula = as.formula(input$pca_plot_formula)
                          )

  })

  ##############################################################################
  # Heatmap Plotting
  ##############################################################################

  # UI for selecting color variables
  output$heatmap_col_color_choices_ui <- renderUI({
    req(colData())
    selectInput("heatmap_col_color_choices", "Choose columns to color",
                choices = names(colData()),
                selected = NULL,
                multiple = TRUE)
  })

  output$heatmap_row_color_choices_ui <- renderUI({
    req(final_qf(), input$agg_level_choices)
    selectInput("heatmap_row_color_choices", "Choose row annotations",
                choices = colnames(SummarizedExperiment::rowData(final_qf()[[input$agg_level_choices]])),
                selected = NULL,
                multiple = TRUE)
  })

  # Dynamically swap between interactive and static outputs
  output$heatmap_plot_ui <- renderUI({
    if (input$heatmap_plot_type == "interactive") {
      plotly::plotlyOutput("heatmap_plotly")
    } else {
      plotOutput("heatmap_plot_static")
    }
  })

  # Shared args for both plot functions
  heatmap_args <- reactive({
    list(
      qf = final_qf(),
      assay_name = paste0(input$agg_level_choices, "_log", input$log_base, "_imputed"),
      col_color_variables = input$heatmap_col_color_choices,
      row_color_variables = input$heatmap_row_color_choices
    )
  })

  # Plotting the heatmaps!

  # Static heatmap
  output$heatmap_plot_static <- renderPlot({
    req(input$heatmap_plot_type == "static")
    do.call(conduitR::plot_heatmap, heatmap_args())
  })

  # Interactive heatmap
  output$heatmap_plotly <- plotly::renderPlotly({
    req(input$heatmap_plot_type == "interactive")
    do.call(conduitR::plot_heatmaply, heatmap_args())
  })

  ##############################################################################
  # Relative Abundance
  ##############################################################################
  # UI for selecting facet formula
  output$relative_abundance_plot <-
    renderPlot({
      req(final_qf())
      req(input$agg_level_choices)  # Ensure the assay is selected)
      conduitR::plot_relative_abundance(final_qf(),
                                        assay_name = paste0(input$agg_level_choices,
                                                           "_rel_abundance"),
                                        facet_formula = input$relative_abundance_plot_formula)
    })

  ##############################################################################
  # Statistics
  ##############################################################################

  # Possible Contrast output of text
  output$possible_contrasts <- renderText({
    req(final_qf(),input$limma_formula,input$agg_level_choices)
    # Retrieve the possible contrast terms from the first renderUI
    contrast_terms <- conduitR::find_possible_contrast_terms(
      final_qf(),
      paste0(input$agg_level_choices, "_log", input$log_base, "_imputed"),
      as.formula(input$limma_formula)
    )
  })

  # Extract stats
  limma_stats_results <- reactive({
    assay_name = paste0(input$agg_level_choices, "_log", input$log_base, "_imputed")

   results=conduitR::perform_limma_analysis(final_qf(),
                                     assay_name,
                                     formula = as.formula(input$limma_formula),
                                     contrast = input$limma_contrast
                                     )

  # Adding the missing rowData to the stats.
   conduitR::add_rowdata_to_limma_results(results$top_table,
                                          final_qf(),
                                          assay_name) |>
     dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 2))


  })

  # Generate Statistics Table
  output$limma_statistics_table <- DT::renderDataTable({
    DT::datatable(
      limma_stats_results() |>
        dplyr::mutate(dplyr::across(
          where(is.character),
          ~ ifelse(nchar(.) > 15,
                   paste0("<span title='", ., "'>", substr(., 1, 15), "...</span>"),
                   .)
        )),
      escape = FALSE,  # Allow HTML for tooltips
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

  # Render options for coloring the points on the graph.
  output$limma_volcano_color_ui <- renderUI({
    assay_name = paste0(input$agg_level_choices, "_log", input$log_base, "_imputed")
    rowData = SummarizedExperiment::rowData(final_qf()[[assay_name]])
    selectInput("limma_volcano_color",
                "Choose how the points are colored",
                choices = c("none" = "",colnames(rowData)),
                selected = "",
                multiple = FALSE
    )
  })

  # Plot volcano plot
  output$limma_volcano_plot <- renderPlot({
    conduitR::plot_volcano(limma_stats_results(),
                           facet_formula = as.formula(input$limma_volcano_facet_formula),
                           color_by = input$limma_volcano_color,
                           pval_threshold =input$volcano_p_threshold
                           )
    })

  observeEvent(input$enrichment_analysis_button, {
    updateTabItems(session, "main_tabs", "enrichment")
  })

  # Selected Feature Plot

  # UI Generation

  # X axis
  output$selected_feature_plot_x_axis_ui <- renderUI({
    req(colData())
    selectInput("selected_feature_plot_x_axis", "Choose x axis",
                choices = c(names(colData())),
                multiple = FALSE)
  })

  # Facet wrap
  output$selected_feature_plot_facet_formula_ui <- renderUI({
    req(colData())
    textInput("selected_feature_plot_facet_formula",
              "Choose facet formula",
              value = "~ id")
  })

  # Color

  output$selected_feature_plot_color_ui <- renderUI({
    req(colData())
    selectInput("selected_feature_plot_color", "Choose color",
                choices = c("none"="",names(colData())),
                multiple = FALSE,
                selected = "")
  })

  # Shape
  output$selected_feature_plot_shape_ui <- renderUI({
    req(colData())
    selectInput("selected_feature_plot_shape", "Choose shape",
                choices = c("none" = "",names(colData())),
                multiple = FALSE,
                selected = "")
  })

  # Features are selected from the data table
  selected_features<- reactive({
    req(limma_stats_results(),
        input$limma_statistics_table_rows_selected)
    limma_stats_results() |>
      dplyr::slice(input$limma_statistics_table_rows_selected) |>
      dplyr::pull(id)
  })

  # Plotting the feature plot
  output$selected_feature_plot <- renderPlot({

   req(selected_features(),
       input$agg_level_choices,
       input$selected_feature_plot_data_type_ui,
       input$selected_feature_plot_x_axis,
       input$selected_feature_plot_color,
       input$selected_feature_plot_shape)

    assay_name = paste0(input$agg_level_choices,input$selected_feature_plot_data_type_ui)

    conduitR::plot_selected_features(final_qf(),
                                     assay_name,
                                     features = selected_features(),
                                     x_axis = input$selected_feature_plot_x_axis,
                                     color_by = input$selected_feature_plot_color,
                                     shape = input$selected_feature_plot_shape,
                                    )

  })
  ##############################################################################
  # Outcome Prediction
  ##############################################################################
  # Dynamically update predictor and outcome variable inputs
  # shiny::observe({
  #   df <- filtered_data()
  #   shiny::updateSelectInput(session, "predict_var", choices = names(df))
  #   shiny::updateSelectInput(session, "predictors_to_include", choices = names(df))
  # })
  #
  # modeling_results <- shiny::eventReactive(input$run_model, {
  #   shiny::req(filtered_data(), input$predict_var, input$model_type)
  #
  #   set.seed(input$random_seed)
  #   df <- filtered_data()
  #
  #   # Determine predictors
  #   outcome <- input$predict_var
  #   predictors <- if (!is.null(input$predictors_to_include) && length(input$predictors_to_include) > 0) {
  #     input$predictors_to_include
  #   } else {
  #     setdiff(names(df), outcome)
  #   }
  #
  #   df <- df |>
  #     dplyr::select(dplyr::all_of(c(outcome, predictors))) |>
  #     dplyr::filter(!is.na(.data[[outcome]]))
  #   df[[outcome]] <- as.factor(df[[outcome]])
  #
  #   # Data split
  #   split <- rsample::initial_split(df, prop = input$split_ratio / 100, strata = rlang::sym(outcome))
  #   train <- rsample::training(split)
  #   test <- rsample::testing(split)
  #
  #   # Preprocessing recipe
  #   rec <- recipes::recipe(stats::as.formula(paste(outcome, "~ .")), data = train) |>
  #     recipes::step_normalize(recipes::all_numeric_predictors())
  #
  #   # Dynamic model specification
  #   model_spec <- switch(input$model_type,
  #                        "Logistic Regression" = parsnip::logistic_reg() |>
  #                          parsnip::set_engine("glm") |>
  #                          parsnip::set_mode("classification"),
  #                        "Random Forest" = parsnip::rand_forest(mode = "classification", trees = 500) |>
  #                          parsnip::set_engine("ranger"),
  #                        "XGBoost" = parsnip::boost_tree(trees = 100) |>
  #                          parsnip::set_engine("xgboost") |>
  #                          parsnip::set_mode("classification"),
  #                        stop("Unknown model type selected")
  #   )
  #
  #   # Workflow
  #   wf <- workflows::workflow() |>
  #     workflows::add_recipe(rec) |>
  #     workflows::add_model(model_spec)
  #
  #   # Fit model
  #   fit <- workflows::fit(wf, data = train)
  #
  #   # Predictions
  #   train_preds <- predict(fit, train, type = "prob") |>
  #     dplyr::bind_cols(predict(fit, train), train[, outcome, drop = FALSE])
  #
  #   test_preds <- predict(fit, test, type = "prob") |>
  #     dplyr::bind_cols(predict(fit, test), test[, outcome, drop = FALSE])
  #
  #   list(
  #     fit = fit,
  #     outcome = outcome,
  #     train_preds = train_preds,
  #     test_preds = test_preds
  #   )
  # })
  #
  # output$roc_train <- shiny::renderPlot({
  #   shiny::req(modeling_results())
  #   preds <- modeling_results()$train_preds
  #   truth_col <- modeling_results()$outcome
  #   roc_obj <- pROC::roc(preds[[truth_col]], preds$.pred_1)
  #   plot(roc_obj, main = "Training ROC Curve")
  # })
  #
  # output$roc_test <- shiny::renderPlot({
  #   shiny::req(modeling_results())
  #   preds <- modeling_results()$test_preds
  #   truth_col <- modeling_results()$outcome
  #   roc_obj <- pROC::roc(preds[[truth_col]], preds$.pred_1)
  #   plot(roc_obj, main = "Test ROC Curve")
  # })
  #
  # output$feature_importance <- shiny::renderPlot({
  #   shiny::req(modeling_results())
  #   fit <- modeling_results()$fit
  #   vip::vip(fit$fit$fit)
  # })
    }


