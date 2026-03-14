conduit_analysis_server <- function(id, final_qf, processed_assay, selected_assay,
                                    final_colData_names, final_rowData_names, session_parent,
                                    log_base) {
  moduleServer(id, function(input, output, session) {

    ##########################################################################
    # QC Plots
    ##########################################################################

    feature_number_plot <- reactive({
      req(final_qf(), selected_assay())
      conduitR::plot_features_per_sample(final_qf(), assay = selected_assay()) +
        ggplot2::ylab(paste0("Number of ", selected_assay()))
    })

    output$feature_number_plot <- renderPlot({
      feature_number_plot()
    })

    output$miss_val_heatmap_col_color_choices_ui <- renderUI({
      req(final_colData_names())
      selectInput(
        session$ns("miss_val_heatmap_col_color_choices"),
        "Choose columns to color",
        choices = c(final_colData_names(), NULL),
        selected = NULL,
        multiple = TRUE
      )
    })

    output$miss_val_heatmap_row_color_choices_ui <- renderUI({
      req(final_rowData_names())
      selectInput(
        session$ns("miss_val_heatmap_row_color_choices"),
        "Choose row annotations",
        choices = c(final_rowData_names(), NULL),
        selected = NULL,
        multiple = TRUE
      )
    })

    # The missing-value heatmap must use the log-only assay (before imputation)
    # so that NAs are still present and visible.  The processed_assay has had
    # all NAs filled in, which causes sechm to throw "two distinct break values".
    log_assay <- reactive({
      req(processed_assay())
      sub("_imputed.*$", "", processed_assay())
    })

    missing_value_plot <- reactive({
      req(final_qf(), log_assay())
      conduitR::plot_missing_val_heatmap(
        final_qf(),
        assay_name = log_assay(),
        col_color_variables = input$miss_val_heatmap_col_color_choices,
        row_color_variables = input$miss_val_heatmap_row_color_choices
      )
    })

    output$missing_value_plot <- renderPlot({
      missing_value_plot()
    })

    output$sample_cor_heatmap_color_choices_ui <- renderUI({
      req(final_colData_names())
      selectInput(
        session$ns("sample_cor_heatmap_color_choices"),
        "Choose annotation",
        choices = final_colData_names(),
        selected = NULL,
        multiple = TRUE
      )
    })

    sample_cor_heatmap <- reactive({
      req(final_qf(), processed_assay())
      conduitR::plot_sample_cor_heatmap(
        final_qf(),
        assay_name = processed_assay(),
        sample_annotation_variables = input$sample_cor_heatmap_color_choices
      )
    })

    output$sample_cor_heatmap <- renderPlot({
      sample_cor_heatmap()
    })

    output$intensity_distribution_plot <- renderPlot({
      req(final_qf(), selected_assay())
      se  <- final_qf()[[selected_assay()]]
      mat <- SummarizedExperiment::assay(se)
      n_detected <- rowSums(!is.na(mat))
      df <- data.frame(n_detected = n_detected)
      ggplot2::ggplot(df, ggplot2::aes(x = n_detected)) +
        ggplot2::geom_histogram(binwidth = 1, fill = "#3B528BFF", color = "white") +
        ggplot2::labs(
          x = "Number of Samples Feature Is Detected In",
          y = "Number of Features",
          title = paste("Detection Frequency —", selected_assay())
        ) +
        ggplot2::theme_minimal()
    })

    output$density_plot_color_choice_ui <- renderUI({
      req(final_colData_names())
      selectInput(
        session$ns("density_plot_color_choice"),
        "Choose color variable",
        choices = final_colData_names(),
        selected = final_colData_names()[[1]]
      )
    })

    density_plot <- reactive({
      req(final_qf(), selected_assay(), log_base(), input$density_plot_color_choice)
      conduitR::plot_density(
        final_qf(),
        assay_name = selected_assay(),
        log_base(),
        input$density_plot_color_choice
      )
    })

    output$density_plot <- renderPlot({
      density_plot()
    })

    ##########################################################################
    # PCA
    ##########################################################################

    output$pca_plot_color_choice_ui <- renderUI({
      req(final_colData_names())
      selectInput(
        session$ns("pca_plot_color_choice"),
        "Choose color variable",
        choices = c("none" = "", final_colData_names(), NULL),
        selected = ""
      )
    })

    output$pca_plot_shape_choice_ui <- renderUI({
      req(final_colData_names())
      selectInput(
        session$ns("pca_plot_shape_choice"),
        "Choose shape variable",
        choices = c("none" = "", final_colData_names()),
        NULL,
        selected = ""
      )
    })

    pca_plot <- reactive({
      req(final_qf(), processed_assay())
      conduitR::plot_biplot(
        final_qf(),
        assay_name = processed_assay(),
        color = input$pca_plot_color_choice,
        shape = input$pca_plot_shape_choice,
        facet_formula = as.formula(input$pca_plot_formula)
      )
    })

    output$pca_plot <- renderPlot({
      pca_plot()
    })

    ##########################################################################
    # Heatmap
    ##########################################################################

    output$heatmap_feature_number_ui <- renderUI({
      req(final_qf(), processed_assay())
      sliderInput(
        session$ns("heatmap_feature_number"),
        "Max N Of Rows To Show",
        min = 1,
        max = nrow(SummarizedExperiment::rowData(final_qf()[[processed_assay()]])),
        value = 1000
      )
    })

    output$heatmap_col_color_choices_ui <- renderUI({
      req(final_colData_names())
      selectInput(
        session$ns("heatmap_col_color_choices"),
        "Choose columns to color",
        choices = final_colData_names(),
        selected = NULL,
        multiple = TRUE
      )
    })

    output$heatmap_row_color_choices_ui <- renderUI({
      req(final_rowData_names())
      selectInput(
        session$ns("heatmap_row_color_choices"),
        "Choose row annotations",
        choices = final_rowData_names(),
        selected = NULL,
        multiple = TRUE
      )
    })

    output$heatmap_plot_ui <- renderUI({
      if (input$heatmap_plot_type == "interactive") {
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(NS(id, "heatmap_plotly"), height = "600px"),
          type = 8,
          caption = "One interactive heatmap coming up...",
          color = "#15131efe"
        )
      } else {
        shinycssloaders::withSpinner(
          plotOutput(NS(id, "heatmap_plot_static"), height = "600px"),
          type = 8,
          caption = "One static heatmap is on the way...",
          color = "#15131efe"
        )
      }
    })

    heatmap_qf <- reactive({
      req(final_qf(), processed_assay(), input$heatmap_feature_number)
      qf_obj <- final_qf()
      assay_name <- processed_assay()
      assay_obj <- qf_obj[[assay_name]]
      mat <- SummarizedExperiment::assay(qf_obj[[assay_name]])
      variances <- apply(mat, 1, var, na.rm = TRUE)
      n <- min(input$heatmap_feature_number, nrow(mat))
      top_n_idx <- order(variances, decreasing = TRUE)[seq_len(n)]
      assay_obj <- assay_obj[top_n_idx, ]
      qf_single <- qf_obj[NULL]
      qf_single[[assay_name]] <- assay_obj
      qf_single
    })

    heatmap_args <- reactive({
      list(
        qf = heatmap_qf(),
        assay_name = processed_assay(),
        col_color_variables = input$heatmap_col_color_choices,
        row_color_variables = input$heatmap_row_color_choices
      )
    })

    heatmap_plot_static <- reactive({
      req(input$heatmap_plot_type == "static", heatmap_args())
      # scale=FALSE: sechm (used by plot_heatmap) accepts a logical
      do.call(conduitR::plot_heatmap, c(heatmap_args(), list(scale = FALSE)))
    })

    output$heatmap_plot_static <- renderPlot({
      heatmap_plot_static()
    })

    output$heatmap_plotly <- plotly::renderPlotly({
      req(input$heatmap_plot_type == "interactive", heatmap_args())
      # scale="none": heatmaply expects a character, not a logical
      do.call(conduitR::plot_heatmaply, c(heatmap_args(), list(scale = "none")))
    })

    ##########################################################################
    # Relative Abundance
    ##########################################################################

    relative_abundance_assay <- reactive({
      paste0(selected_assay(), "_rel_abundance")
    })

    relative_abundance_plot <- reactive({
      req(final_qf(), relative_abundance_assay())
      if (relative_abundance_assay() %in% paste0(
        c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species"),
        "_rel_abundance"
      )) {
        conduitR::plot_relative_abundance(
          final_qf(),
          assay_name = relative_abundance_assay(),
          facet_formula = input$relative_abundance_plot_formula
        )
      } else {
        msg <- "Relative Abundance Plots only Supported For Taxonomic Aggregations"
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg, size = 6, hjust = 0.5, vjust = 0.5) +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
      }
    })

    output$relative_abundance_plot <- renderPlot({
      relative_abundance_plot()
    })

    ##########################################################################
    # Statistics
    ##########################################################################

    output$possible_contrasts <- renderText({
      req(final_qf(), processed_assay(), input$limma_formula)
      conduitR::find_possible_contrast_terms(
        final_qf(),
        processed_assay(),
        as.formula(input$limma_formula)
      )
    })

    limma_stats_results <- reactive({
      req(final_qf(), processed_assay(), input$limma_formula, input$limma_contrast)
      conduitR::perform_limma_analysis(
        final_qf(),
        assay_name = processed_assay(),
        formula = as.formula(input$limma_formula),
        contrast = input$limma_contrast
      )$top_table
    })

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
        escape = FALSE,
        options = list(scrollX = TRUE, autoWidth = TRUE)
      ) |>
        DT::formatStyle(
          columns = names(limma_stats_results()),
          `white-space` = "normal",
          `word-wrap` = "break-word"
        )
    })

    output$download_limma_stats_table <- downloadHandler(
      filename = function() { "limma_statistics.csv" },
      content = function(file) {
        readr::write_csv(as.data.frame(limma_stats_results()), file)
      }
    )

    output$limma_volcano_color_ui <- renderUI({
      req(final_rowData_names())
      selectInput(
        session$ns("limma_volcano_color"),
        "Choose how the points are colored",
        choices = c("none" = "", final_rowData_names()),
        selected = "",
        multiple = FALSE
      )
    })

    limma_volcano_plot <- reactive({
      req(limma_stats_results(), input$limma_fc_threshold, input$limma_p_threshold, input$limma_volcano_facet_formula)
      conduitR::plot_volcano(
        limma_stats_results(),
        facet_formula = as.formula(input$limma_volcano_facet_formula),
        color_by = input$limma_volcano_color,
        pval_threshold = input$limma_p_threshold
      ) +
        ggplot2::geom_vline(xintercept = input$limma_fc_threshold, linetype = "dashed", color = "red") +
        ggplot2::geom_vline(xintercept = -input$limma_fc_threshold, linetype = "dashed", color = "red")
    })

    output$limma_volcano_plot <- renderPlot({
      limma_volcano_plot()
    })

    # Selected Feature Plot
    final_colData_and_rowData_names <- reactive({
      unique(c(final_colData_names(), final_rowData_names()))
    })

    output$selected_feature_plot_x_axis_ui <- renderUI({
      req(final_colData_and_rowData_names())
      selectInput(session$ns("selected_feature_plot_x_axis"), "Choose x axis",
        choices = final_colData_and_rowData_names(), multiple = FALSE)
    })

    output$selected_feature_plot_facet_formula_ui <- renderUI({
      textInput(session$ns("selected_feature_plot_facet_formula"), "Choose facet formula", value = "~ NULL")
    })

    output$selected_feature_plot_color_ui <- renderUI({
      req(final_colData_and_rowData_names())
      selectInput(session$ns("selected_feature_plot_color"), "Choose color",
        choices = c("none" = "", final_colData_and_rowData_names()), multiple = FALSE, selected = "")
    })

    output$selected_feature_plot_shape_ui <- renderUI({
      req(final_colData_and_rowData_names())
      selectInput(session$ns("selected_feature_plot_shape"), "Choose shape",
        choices = c("none" = "", final_colData_and_rowData_names()), multiple = FALSE, selected = "")
    })

    selected_features <- reactive({
      req(limma_stats_results(), input$limma_statistics_table_rows_selected)
      limma_stats_results() |>
        dplyr::slice(input$limma_statistics_table_rows_selected) |>
        dplyr::pull(id)
    })

    selected_feature_plot <- reactive({
      req(selected_features(), processed_assay(), input$selected_feature_plot_x_axis,
          input$selected_feature_plot_color, input$selected_feature_plot_shape,
          input$selected_feature_plot_facet_formula)
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

    # Tab navigation to enrichment/pathway (uses parent session)
    observeEvent(input$enrichment_analysis_button, {
      updateTabItems(session_parent, "main_tabs", "enrichment")
    })

    observeEvent(input$pathway_analysis_button, {
      updateTabItems(session_parent, "main_tabs", "pathway")
    })

    ##########################################################################
    # Returns for download handler, enrichment, and pathway
    ##########################################################################
    return(list(
      limma_stats_results = limma_stats_results,
      limma_volcano_plot = limma_volcano_plot,
      limma_fc_threshold = reactive(input$limma_fc_threshold),
      limma_p_threshold = reactive(input$limma_p_threshold),
      feature_number_plot = feature_number_plot,
      missing_value_plot = missing_value_plot,
      sample_cor_heatmap = sample_cor_heatmap,
      density_plot = density_plot,
      pca_plot = pca_plot,
      heatmap_plot_static = heatmap_plot_static,
      relative_abundance_plot = relative_abundance_plot,
      selected_feature_plot = selected_feature_plot
    ))
  })
}
