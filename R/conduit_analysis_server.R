conduit_analysis_server <- function(id, final_qf, processed_assay, selected_assay,
                                    final_colData_names, final_rowData_names, session_parent,
                                    log_base, conduit_obj) {
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
      sub("(_log[0-9]+).*$", "\\1", processed_assay())
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

    intensity_distribution_plot <- reactive({
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

    output$intensity_distribution_plot <- renderPlot({
      intensity_distribution_plot()
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
      log_nm     <- paste0(selected_assay(), "_log", log_base())
      imputed_nm <- paste0(log_nm, "_", input$imputation_method)
      conduitR::plot_density(
        final_qf(),
        log_assay     = log_nm,
        imputed_assay = imputed_nm,
        color         = input$density_plot_color_choice
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
          color = "#2f2a20fe"
        )
      } else {
        shinycssloaders::withSpinner(
          plotOutput(NS(id, "heatmap_plot_static"), height = "600px"),
          type = 8,
          caption = "One static heatmap is on the way...",
          color = "#2f2a20fe"
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
      req(final_qf(), relative_abundance_assay(), conduit_obj())
      qf_in <- slot(conduit_obj(), "QFeatures")
      spec  <- conduitR::aggregation_targets(qf_in)[[selected_assay()]]
      if (!is.null(spec) && identical(spec$kind, "taxonomic")) {
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
    # Explore
    ##########################################################################

    # Long-format data: intensity + all colData + all rowData columns
    explore_long_data <- reactive({
      req(final_qf(), processed_assay())
      se  <- final_qf()[[processed_assay()]]
      mat <- SummarizedExperiment::assay(se)

      df <- as.data.frame(mat) |>
        tibble::rownames_to_column("feature") |>
        tidyr::pivot_longer(-feature, names_to = "sample", values_to = "intensity")

      col_df <- as.data.frame(SummarizedExperiment::colData(se)) |>
        tibble::rownames_to_column("sample")

      row_df <- as.data.frame(SummarizedExperiment::rowData(se)) |>
        tibble::rownames_to_column("feature")

      df |>
        dplyr::left_join(col_df, by = "sample") |>
        dplyr::left_join(row_df, by = "feature", suffix = c("", "_feature"))
    })

    # Holds the last successfully applied summarised data frame (NULL = no grouping applied)
    explore_summarized_data <- reactiveVal(NULL)

    observeEvent(input$explore_apply, {
      df         <- req(explore_long_data())
      group_vars <- intersect(input$explore_group_by, names(df))
      if (length(group_vars) == 0) {
        explore_summarized_data(NULL)
        return()
      }
      fn_name   <- if (!is.null(input$explore_summary_fn)) input$explore_summary_fn else "mean"
      value_col <- paste0(fn_name, "_intensity")
      grouped   <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_vars)))
      result <- if (fn_name == "n") {
        dplyr::summarise(grouped, !!value_col := dplyr::n(), .groups = "drop")
      } else {
        summary_fn <- switch(fn_name,
          mean   = function(x) mean(x,   na.rm = TRUE),
          median = function(x) median(x, na.rm = TRUE),
          sum    = function(x) sum(x,    na.rm = TRUE),
          sd     = function(x) sd(x,     na.rm = TRUE)
        )
        dplyr::summarise(grouped, !!value_col := summary_fn(intensity), .groups = "drop")
      }
      explore_summarized_data(result)
    })

    # Data actually sent to the plot: summarised if Apply was used, otherwise raw
    explore_plot_data <- reactive({
      req(explore_long_data())
      summarized <- explore_summarized_data()
      if (is.null(summarized)) explore_long_data() else summarized
    })

    # Column names that exist in the current plot data
    explore_plot_cols <- reactive({
      req(explore_plot_data())
      names(explore_plot_data())
    })

    # Group-by: selectize so users can type variable names; starts blank
    output$explore_group_by_ui <- renderUI({
      req(explore_long_data())
      choices <- setdiff(names(explore_long_data()), "intensity")
      selectizeInput(session$ns("explore_group_by"), "Group by",
        choices  = choices,
        selected = NULL,
        multiple = TRUE,
        options  = list(create = TRUE, placeholder = "Select or type variable names...")
      )
    })

    output$explore_x_axis_ui <- renderUI({
      req(explore_plot_cols())
      cols      <- explore_plot_cols()
      default_x <- intersect(final_colData_names(), cols)
      default_x <- if (length(default_x) > 0) default_x[[1]] else cols[[1]]
      selectInput(session$ns("explore_x_axis"), "X axis",
        choices = cols, selected = default_x)
    })

    output$explore_y_axis_ui <- renderUI({
      req(explore_plot_cols(), input$explore_plot_type)
      if (input$explore_plot_type == "histogram") return(NULL)
      cols      <- explore_plot_cols()
      default_y <- if ("intensity" %in% cols) "intensity" else cols[[length(cols)]]
      selectInput(session$ns("explore_y_axis"), "Y axis",
        choices = cols, selected = default_y)
    })

    output$explore_color_ui <- renderUI({
      req(explore_plot_cols())
      selectInput(session$ns("explore_color"), "Color variable",
        choices = c("none" = "", explore_plot_cols()), selected = "")
    })

    output$explore_shape_ui <- renderUI({
      req(explore_plot_cols(), input$explore_plot_type)
      if (input$explore_plot_type != "scatter") return(NULL)
      selectInput(session$ns("explore_shape"), "Shape variable",
        choices = c("none" = "", explore_plot_cols()), selected = "")
    })

    explore_plot <- reactive({
      req(explore_plot_data(), input$explore_x_axis, input$explore_plot_type)

      df        <- explore_plot_data()
      plot_type <- input$explore_plot_type
      x_var     <- input$explore_x_axis
      color_var <- input$explore_color

      p <- ggplot2::ggplot(df) + ggplot2::aes(x = .data[[x_var]])

      if (plot_type != "histogram") {
        req(input$explore_y_axis)
        p <- p + ggplot2::aes(y = .data[[input$explore_y_axis]])
      }

      if (!is.null(color_var) && nzchar(color_var)) {
        p <- p + ggplot2::aes(color = .data[[color_var]])
      }

      p <- p + switch(plot_type,
        scatter   = ggplot2::geom_point(alpha = 0.7),
        bar       = ggplot2::geom_bar(stat = "identity"),
        line      = ggplot2::geom_line(ggplot2::aes(group = 1)),
        boxplot   = ggplot2::geom_boxplot(),
        violin    = ggplot2::geom_violin(),
        histogram = ggplot2::geom_histogram(bins = 30, fill = "#3B528BFF", color = "white")
      )

      if (plot_type == "scatter" && !is.null(input$explore_shape) && nzchar(input$explore_shape)) {
        p <- p + ggplot2::aes(shape = .data[[input$explore_shape]])
      }

      facet_str <- gsub("\\s+", "", input$explore_facet_formula)
      if (!is.null(facet_str) && nzchar(facet_str) && !facet_str %in% c("~NULL", "~null")) {
        tryCatch(
          p <- p + ggplot2::facet_wrap(as.formula(input$explore_facet_formula)),
          error = function(e) NULL
        )
      }

      p + theme_conduit()
    })

    output$explore_plot <- renderPlot({
      explore_plot()
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

    limma_stats_results <- eventReactive(input$run_limma, {
      req(final_qf(), processed_assay(), input$limma_formula, input$limma_contrast)
      conduitR::perform_limma_analysis(
        final_qf(),
        assay_name = processed_assay(),
        formula = as.formula(input$limma_formula),
        contrast = input$limma_contrast
      )$top_table
    })

    output$limma_statistics_table <- DT::renderDT({
      shiny::validate(
        shiny::need(input$run_limma > 0, "Fill in the formula and contrast, then click \u201cRun Analysis\u201d to populate the table.")
      )
      conduit_datatable(
        limma_stats_results(),
        column_defs = conduit_truncate_column_defs()
      )
    })

    output$download_limma_stats_table <- downloadHandler(
      filename = function() { "limma_statistics.csv" },
      content = function(file) {
        readr::write_csv(as.data.frame(limma_stats_results()), file)
      }
    )

    ##########################################################################
    # Enrichment and Pathway (nested inside Analysis card)
    # Initialized here so threshold reactives are available to the volcano plot
    ##########################################################################
    enrichment_results <- conduit_enrichment_server(
      "enrichment",
      conduit_obj         = conduit_obj,
      limma_stats_results = limma_stats_results,
      fc_threshold        = reactive(input$limma_fc_threshold),
      p_threshold         = reactive(input$limma_p_threshold),
      session_parent      = session_parent
    )
    enrichment_plot <- enrichment_results$enrichment_plot

    pathway_plot <- conduit_pathway_server(
      "pathway",
      conduit_obj         = conduit_obj,
      limma_stats_results = limma_stats_results,
      session_parent      = session_parent
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

    # Track proteins selected by clicking the volcano plot
    volcano_clicked_ids <- reactiveVal(character(0))

    observeEvent(input$run_limma, {
      volcano_clicked_ids(character(0))
    })

    observeEvent(input$clear_volcano_selection, {
      volcano_clicked_ids(character(0))
    })

    observeEvent(plotly::event_data("plotly_click", source = "volcano"), {
      click <- plotly::event_data("plotly_click", source = "volcano")
      req(click, limma_stats_results())
      matched <- limma_stats_results() |>
        dplyr::filter(
          abs(logFC - click$x) < 1e-9,
          abs(neg_log10.adj.P.Val - click$y) < 1e-9
        ) |>
        dplyr::pull(id)
      if (length(matched) > 0) {
        current <- volcano_clicked_ids()
        id <- matched[1]
        if (id %in% current) {
          volcano_clicked_ids(current[current != id])
        } else {
          volcano_clicked_ids(c(current, id))
        }
      }
    })

    limma_volcano_plot <- reactive({
      req(limma_stats_results(), input$limma_volcano_facet_formula)
      fc <- if (!is.null(input$limma_fc_threshold)) input$limma_fc_threshold else 0
      pv <- if (!is.null(input$limma_p_threshold))  input$limma_p_threshold  else 0.05

      color_by  <- if (!is.null(input$limma_volcano_color) && input$limma_volcano_color != "") input$limma_volcano_color else NULL
      facet_str <- input$limma_volcano_facet_formula

      data <- limma_stats_results()

      # Build hover tooltip (after arrange so indices match)
      tip <- paste0("<b>", data$id, "</b>")
      if ("Protein.Names" %in% names(data))
        tip <- paste0(tip, "<br>Protein: ", data$Protein.Names)
      if ("species" %in% names(data))
        tip <- paste0(tip, "<br>Species: ", data$species)
      if ("Genes" %in% names(data))
        tip <- paste0(tip, "<br>Gene: ", data$Genes)

      # Reference lines as layout shapes
      hline_y <- -log10(pv)
      shapes <- list(
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = hline_y, y1 = hline_y,
             line = list(color = "#888888", dash = "dash", width = 1)),
        list(type = "line", y0 = 0, y1 = 1, yref = "paper",
             x0 =  fc, x1 =  fc,
             line = list(color = "#888888", dash = "dash", width = 1)),
        list(type = "line", y0 = 0, y1 = 1, yref = "paper",
             x0 = -fc, x1 = -fc,
             line = list(color = "#888888", dash = "dash", width = 1))
      )

      use_facet <- !is.null(facet_str) && facet_str != "" && facet_str != "~NULL"
      facet_col <- if (use_facet) trimws(sub("^~", "", facet_str)) else NULL

      make_trace <- function(d, t, src = NULL) {
        if (!is.null(color_by)) {
          plotly::plot_ly(source = src) |>
            plotly::add_trace(
              data      = d,
              type      = "scattergl",
              mode      = "markers",
              x         = ~logFC,
              y         = ~neg_log10.adj.P.Val,
              color     = ~.data[[color_by]],
              text      = t,
              hoverinfo = "text",
              marker    = list(opacity = 0.35, size = 6)
            )
        } else {
          plotly::plot_ly(
            data      = d,
            type      = "scattergl",
            mode      = "markers",
            x         = ~logFC,
            y         = ~neg_log10.adj.P.Val,
            text      = t,
            hoverinfo = "text",
            marker    = list(color = "rgba(0,0,0,0.35)", size = 6),
            source    = src
          )
        }
      }

      if (use_facet && !is.null(facet_col) && facet_col %in% names(data)) {
        groups <- split(seq_len(nrow(data)), data[[facet_col]])
        p <- plotly::subplot(
          lapply(names(groups), function(grp) {
            idx      <- groups[[grp]]
            sub_data <- data[idx, ]
            sub_tip  <- tip[idx]
            make_trace(sub_data, sub_tip) |>
              plotly::layout(
                shapes = shapes,
                annotations = list(list(
                  text = grp, x = 0.5, xref = "paper", xanchor = "center",
                  y = 1.02, yref = "paper", showarrow = FALSE
                ))
              )
          }),
          shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE
        )
      } else {
        p <- make_trace(data, tip, src = "volcano") |>
          plotly::layout(
            xaxis  = list(title = "log<sub>2</sub> Fold Change"),
            yaxis  = list(title = "-log<sub>10</sub> adj. p-value"),
            shapes = shapes,
            legend = list(orientation = "v")
          )
      }

      p
    })

    output$limma_volcano_plot <- plotly::renderPlotly({
      if (input$run_limma == 0) {
        return(plotly::plotly_empty() |>
          plotly::layout(
            annotations = list(list(
              text      = "Fill in the formula and contrast, then click \u201cRun Analysis\u201d",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14, color = "#888888")
            ))
          )
        )
      }
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
      textInput(session$ns("selected_feature_plot_facet_formula"), "Choose facet formula", value = "~ rowid")
    })

    output$selected_feature_plot_color_ui <- renderUI({
      req(final_colData_and_rowData_names())
      selectInput(session$ns("selected_feature_plot_color"), "Choose color",
        choices = c("none" = "none", final_colData_and_rowData_names()), multiple = FALSE, selected = "none")
    })

    output$selected_feature_plot_shape_ui <- renderUI({
      req(final_colData_and_rowData_names())
      selectInput(session$ns("selected_feature_plot_shape"), "Choose shape",
        choices = c("none" = "none", final_colData_and_rowData_names()), multiple = FALSE, selected = "none")
    })

    output$selected_feature_plot_data_type_ui <- renderUI({
      req(final_qf(), selected_assay())
      # Derive available assay variants by scanning names(qf) for those starting
      # with the selected base assay name (e.g. protein_groups_log2_MinDet_none)
      base     <- selected_assay()
      all_names <- names(final_qf())
      derived  <- grep(paste0("^", base, "_log[0-9]+"), all_names, value = TRUE)
      choices  <- c(base, derived)
      selected <- if (processed_assay() %in% choices) processed_assay() else base
      selectInput(
        session$ns("selected_feature_plot_data_type"), "Data type",
        choices  = choices,
        selected = selected
      )
    })

    selected_features <- reactive({
      req(limma_stats_results())
      table_ids <- character(0)
      if (!is.null(input$limma_statistics_table_rows_selected)) {
        table_ids <- limma_stats_results() |>
          dplyr::slice(input$limma_statistics_table_rows_selected) |>
          dplyr::pull(id)
      }
      combined <- unique(c(table_ids, volcano_clicked_ids()))
      req(length(combined) > 0)
      combined
    })

    selected_feature_plot <- reactive({
      req(selected_features(), processed_assay(), input$selected_feature_plot_x_axis,
          input$selected_feature_plot_facet_formula)
      conduitR::plot_selected_features(
        final_qf(),
        assay_name = processed_assay(),
        features = selected_features(),
        x_axis = input$selected_feature_plot_x_axis,
        color_by = if (input$selected_feature_plot_color != "none") input$selected_feature_plot_color else NULL,
        shape = if (input$selected_feature_plot_shape != "none") input$selected_feature_plot_shape else NULL,
        facet_formula = as.formula(input$selected_feature_plot_facet_formula)
      )
    })

    output$selected_feature_plot <- renderPlot({
      if (input$run_limma == 0) {
        return(waiting_plot("Run analysis, then select a feature from the table or click a volcano point"))
      }
      if (is.null(input$limma_statistics_table_rows_selected) && length(volcano_clicked_ids()) == 0) {
        return(waiting_plot("Click a point on the volcano plot or select a row from the statistics table"))
      }
      selected_feature_plot()
    })

    # Tab navigation to enrichment/pathway (within the analysis card)

    ##########################################################################
    # Returns for download handler, enrichment, and pathway
    ##########################################################################
    return(list(
      limma_stats_results = limma_stats_results,
      limma_volcano_plot = limma_volcano_plot,
      feature_number_plot = feature_number_plot,
      missing_value_plot = missing_value_plot,
      sample_cor_heatmap = sample_cor_heatmap,
      intensity_distribution_plot = intensity_distribution_plot,
      density_plot = density_plot,
      pca_plot = pca_plot,
      heatmap_plot_static = heatmap_plot_static,
      relative_abundance_plot = relative_abundance_plot,
      selected_feature_plot = selected_feature_plot,
      enrichment_plot = enrichment_plot,
      pathway_plot = pathway_plot,
      explore_plot = explore_plot
    ))
  })
}
