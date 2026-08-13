library(shiny)
library(conduitR)
library(waiter)
library(digest)
library(future)
future::plan(multisession)

# ── Basecamp handoff ─────────────────────────────────────────────────────────
# Conduit-Basecamp runs this app inside the pipeline's container and points
# CONDUIT_RDS at a finished run's output, which is already on the bind the
# workflow wrote it to. When it is set the app opens straight into that run.
# When it is empty — anyone running Summit on its own — the Home tab's upload
# button drives everything exactly as before.
#
# Uploading is not a workable substitute under Basecamp: the browse dialog
# would show the *container's* filesystem rather than anything the user
# recognizes, and Shiny's upload path copies the whole object through the
# browser (capped at 500MB in ui.R) when the file is already on local disk.
conduit_rds_path <- Sys.getenv("CONDUIT_RDS")

# Server
server <- function(input, output, session) {
  ##############################################################################
  # User Experience
  ##############################################################################
  # Observe the plot theme input and update the global theme
  observeEvent(input$plot_theme, {
    set_plot_theme(input$plot_theme)
  })

  # About page navigation shortcuts
  observeEvent(input$goto_upload,           { bslib::nav_select("main_tabs", "file_upload") })
  observeEvent(input$step_goto_file_upload, { bslib::nav_select("main_tabs", "file_upload") })
  observeEvent(input$goto_help,                  { bslib::nav_select("main_tabs", "help") })
  observeEvent(input$goto_help_from_upload,      { bslib::nav_select("main_tabs", "help") })
  observeEvent(input$step_goto_database,    { bslib::nav_select("main_tabs", "database") })
  observeEvent(input$step_goto_diann_qc,    { bslib::nav_select("main_tabs", "diann_qc") })
  observeEvent(input$step_goto_metadata,    { bslib::nav_select("main_tabs", "view_metadata") })
  observeEvent(input$step_goto_filter,      { bslib::nav_select("main_tabs", "filter_data") })
  observeEvent(input$step_goto_view_assay,  { bslib::nav_select("main_tabs", "view_assay") })
  observeEvent(input$step_goto_analysis,    { bslib::nav_select("main_tabs", "analysis") })
  observeEvent(input$step_goto_traverse,    { bslib::nav_select("main_tabs", "traverse") })
  observeEvent(input$goto_ai_from_about,    { bslib::nav_select("main_tabs", "ai") })

  # Whether we have data to show, from either source. Deliberately cheap: the
  # gates below only need to know that an object is coming, and making them
  # depend on conduit_obj() would force the .rds to be read inside an observer,
  # where a bad path surfaces as a dead session instead of a message.
  data_ready <- reactive({
    nzchar(conduit_rds_path) || !is.null(input$conduit_rds)
  })

  # Under Basecamp the run has already been chosen, so the upload control has
  # nothing useful to do — and its browse dialog would show the container's
  # filesystem. Hide it rather than invite a dead end.
  observe({
    if (nzchar(conduit_rds_path)) shinyjs::hide("conduit_rds")
  })

  # Workflow step locking — steps 2-8 locked until data is loaded
  locked_steps <- c("step_2", "step_3", "step_4", "step_5", "step_6", "step_7", "step_8")
  observe({
    lapply(locked_steps, function(id) shinyjs::addClass(id, "workflow-locked"))
    shinyjs::disable("goto_ai_from_about")
  })
  observe({
    req(data_ready())
    lapply(locked_steps, function(id) shinyjs::removeClass(id, "workflow-locked"))
    shinyjs::enable("goto_ai_from_about")
  })

  ##############################################################################
  # Tab visibility — hide data-dependent tabs until data is loaded
  ##############################################################################
  data_tabs <- c("database", "diann_qc", "view_metadata", "filter_data", "view_assay", "analysis", "traverse", "ai")

  observe({
    lapply(data_tabs, function(tab) bslib::nav_hide("main_tabs", tab))
    bslib::nav_hide("main_tabs", "provenance")
  })

  observe({
    req(data_ready())
    lapply(data_tabs, function(tab) bslib::nav_show("main_tabs", tab, select = FALSE))
  })

  # Show Provenance tab only when the uploaded object has a populated provenance slot
  observe({
    req(conduit_obj())
    prov <- tryCatch(slot(conduit_obj(), "provenance"), error = function(e) NULL)
    if (!is.null(prov)) {
      bslib::nav_show("main_tabs", "provenance", select = FALSE)
    } else {
      bslib::nav_hide("main_tabs", "provenance")
    }
  })


  ###############################################################################
  # Loading + Manipulating Data
  ###############################################################################
  # Announce the load and the defaults it will be processed with. Closed again
  # by final_qf() once processing finishes.
  announce_loading <- function(display_name) {
    shinyalert::shinyalert(
      title = "Loading Data",
      text  = HTML(paste0(
        "<p>Your <b>", display_name, "</b> file is being loaded.</p>",
        "<p>Once loaded, data will be processed with the following default settings:</p>",
        "<ul>",
        "<li><b>Log base:</b> ", input$log_base, "</li>",
        "<li><b>Imputation method:</b> ", input$imputation_method, "</li>",
        "<li><b>Normalization method:</b> ", input$normalization_method, "</li>",
        "</ul>",
        "<p>Please wait...</p>",
        "<p><i>You can change any of these settings from the top bar controls.</i></p>"
      )),
      type                = "info",
      html                = TRUE,
      showCancelButton    = FALSE,
      closeOnClickOutside = FALSE,
      showConfirmButton   = FALSE,
      size                = "l"
    )
  }

  # The object under analysis, from whichever source supplied it. Everything
  # downstream — qf, metrics, rowData, colData, and every tab module — derives
  # from this one reactive, so the Basecamp handoff needs no plumbing past here.
  conduit_obj <- reactive({
    if (nzchar(conduit_rds_path)) {
      validate(need(
        file.exists(conduit_rds_path),
        paste0("CONDUIT_RDS names a file this app cannot see: ", conduit_rds_path)
      ))
      # No modal here — see the processing observer below. A preloaded run is
      # read before the user has done anything, so a blocking announcement
      # would land on a UI they have not touched yet.
      showNotification(
        paste0("Opening ", basename(conduit_rds_path)),
        type = "message", duration = 6
      )
      return(readRDS(conduit_rds_path))
    }

    req(input$conduit_rds) # Ensure file is uploaded
    announce_loading(input$conduit_rds$name)
    readRDS(input$conduit_rds$datapath)
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
  # Database Tab
  ##############################################################################
  taxa_tree_plot <- conduit_database_server(
    "database",
    conduit_obj = conduit_obj,
    qf = qf,
    metrics = metrics,
    colData = colData,
    rowData = rowData
  )
  ################################################################################
  # Header
  ################################################################################

  # Aggregation choices: base assays + registered aggregation targets.
  # Targets are resolved on demand via conduitR::aggregate_assay_by_annotation()
  # in qf_with_selected() below — no pre-aggregated assays needed.

  selected_assay <- reactiveVal(NULL)

  base_assays <- reactive({
    req(qf())
    names(qf())
  })

  agg_targets <- reactive({
    req(qf())
    conduitR::aggregation_targets(qf())
  })

  possible_assays <- reactive({
    c(base_assays(), names(agg_targets()))
  })

  observeEvent(
    qf(),
    {
      default_choice <- if ("protein_groups" %in% base_assays()) {
        "protein_groups"
      } else {
        base_assays()[[1]]
      }
      if (is.null(selected_assay())) {
        selected_assay(default_choice)
      }
    },
    once = TRUE
  )

  output$agg_level_choices_ui <- renderUI({
    req(conduit_obj(), qf())

    targets <- agg_targets()
    choices <- if (length(targets) == 0) {
      base_assays()
    } else {
      taxonomic  <- names(targets)[vapply(targets, function(t) identical(t$kind, "taxonomic"),  logical(1))]
      functional <- names(targets)[vapply(targets, function(t) identical(t$kind, "functional"), logical(1))]
      other      <- setdiff(names(targets), c(taxonomic, functional))
      groups <- list("Base assays" = base_assays())
      if (length(taxonomic))  groups[["Taxonomic"]]  <- taxonomic
      if (length(functional)) groups[["Functional"]] <- functional
      if (length(other))      groups[["Other"]]      <- other
      groups
    }

    shinyWidgets::pickerInput(
      "agg_level_choices",
      label    = "Choose Assay / Aggregation Target",
      choices  = choices,
      selected = selected_assay(),
      options  = shinyWidgets::pickerOptions(liveSearch = TRUE, size = 12, container = "body")
    )
  })

  output$include_unassigned_ui <- renderUI({
    req(selected_assay())
    is_target <- selected_assay() %in% names(agg_targets())
    checkboxInput(
      "include_unassigned",
      "Include unannotated features (sum as 'Unassigned')",
      value = isTRUE(input$include_unassigned)
    ) |>
      tagAppendAttributes(
        class = if (!is_target) "text-muted",
        style = if (!is_target) "opacity: 0.5; pointer-events: none;" else NULL
      )
  })

  observeEvent(input$agg_level_choices, {
    selected_assay(input$agg_level_choices)
  })

  # qf_with_selected: when the user picks an aggregation target, materialize
  # it via aggregate_assay_by_annotation; when they pick a base assay, this
  # is just qf().
  qf_with_selected <- reactive({
    req(qf(), selected_assay())
    sa <- selected_assay()
    if (sa %in% base_assays()) return(qf())
    spec <- agg_targets()[[sa]]
    if (is.null(spec)) return(qf())
    conduitR::aggregate_assay_by_annotation(
      qf(),
      i          = spec$from,
      fcol       = sa,
      include_na = if (isTRUE(input$include_unassigned)) "group" else "drop"
    )
  })

  ################################################################################
  # Provenance Tab
  ################################################################################
  conduit_provenance_server("provenance", conduit_obj = conduit_obj)
  ################################################################################
  # DIA-NN QC Tab
  ################################################################################
  diann_qc_plot <- conduit_diann_qc_server("diann_qc", conduit_obj = conduit_obj, metrics = metrics)
  ################################################################################
  # Metadata Tab
  ################################################################################
  metadata_distribution_plot <- conduit_metadata_server("view_metadata", conduit_obj = conduit_obj, colData = colData)

  ##############################################################################
  # Data Filter
  ##############################################################################
  filtered_qf <- conduit_filter_data_server(
    "filter_data",
    qf = qf_with_selected,
    colData = colData,
    selected_assay = selected_assay
  )
  ##############################################################################
  # Add Log Transformation, Imputation, Normalization, and Relative Abundance
  ##############################################################################
  final_qf <- eventReactive(list(input$run_processing, filtered_qf()), {
    req(
      filtered_qf(),
      selected_assay(),
      input$log_base,
      input$imputation_method,
      input$normalization_method
    )

    # Only show the processing modal when the user explicitly clicked the button;
    # silent re-runs (e.g. on data load or filter changes) skip the modal.
    show_modal <- isTRUE(input$run_processing > 0)

    if (show_modal) {
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
    }

    # Compute updated QFeatures object
    new_qf <- conduitR::add_log_imputed_norm_assay(
      filtered_qf(),
      assay = selected_assay(),
      base = input$log_base,
      impute_method = input$imputation_method,
      norm_method = input$normalization_method
    )

    # Store relative abundance information for taxonomic aggregation targets.
    # Dispatched on the registry's `kind` so adding new taxonomic ontologies
    # in conduitR doesn't require updates here.
    spec <- agg_targets()[[selected_assay()]]
    if (!is.null(spec) && identical(spec$kind, "taxonomic")) {
      new_qf <- conduitR::add_relative_abundance_assay(new_qf, selected_assay())
    }

    shinyalert::closeAlert()
    new_qf
  }, ignoreInit = FALSE)

  # Drive the first processing pass for a preloaded run.
  #
  # final_qf() is an eventReactive and therefore lazy: it computes only when
  # something reads it, and the things that read it are outputs inside the
  # data tabs — which Shiny suspends while their tab is unselected. On an
  # upload that is harmless, because the user is already clicking through the
  # UI by the time the object exists. Opening straight into a run, nobody has
  # clicked anything, so without this the assay stays unprocessed until the
  # first tab visit.
  #
  # try() because an observer that throws takes the session down with it; a
  # failure here should surface through the outputs that show the data, not
  # as a dead app.
  observe({
    req(nzchar(conduit_rds_path))
    try(final_qf(), silent = TRUE)
  })

  ##############################################################################
  # Analysis
  ##############################################################################
  processed_assay <- reactive({
    req(final_qf(), selected_assay())
    paste0(selected_assay(), "_log", input$log_base,
           "_", input$imputation_method,
           "_", input$normalization_method)
  })

  final_colData_names <- reactive({
    req(filtered_qf())
    names(SummarizedExperiment::colData(filtered_qf()))
  })

  final_rowData_names <- reactive({
    req(filtered_qf(), selected_assay())
    names(SummarizedExperiment::rowData(filtered_qf()[[selected_assay()]]))
  })

  conduit_view_assay_server(
    "view_assay",
    final_qf       = final_qf,
    selected_assay = selected_assay,
    processed_assay = processed_assay
  )

  analysis_outputs <- conduit_analysis_server(
    "analysis",
    final_qf = final_qf,
    processed_assay = processed_assay,
    selected_assay = selected_assay,
    final_colData_names = final_colData_names,
    final_rowData_names = final_rowData_names,
    session_parent = session,
    log_base = reactive(input$log_base),
    conduit_obj = conduit_obj
  )

  limma_stats_results <- analysis_outputs$limma_stats_results
  enrichment_plot     <- analysis_outputs$enrichment_plot
  pathway_plot        <- analysis_outputs$pathway_plot

  ##############################################################################
  # Traverse
  ##############################################################################
  traverse_plot <- conduit_traverse_server(
    "traverse",
    conduit_obj = conduit_obj,
    qf = qf,
    colData = colData
  )

  ##############################################################################
  # AI Chat
  ##############################################################################
  conduit_ai_server(
    "ai",
    conduit_obj         = conduit_obj,
    final_qf            = final_qf,
    processed_assay     = processed_assay,
    final_colData_names = final_colData_names,
    final_rowData_names = final_rowData_names
  )

  ##############################################################################
  # Outcome Prediction
  ##############################################################################

  conduit_prediction_server(
    "prediction",
    final_qf            = final_qf,
    processed_assay     = processed_assay,
    final_colData_names = final_colData_names
  )

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
        "database" = taxa_tree_plot(),
        "diann_qc" = diann_qc_plot(),
        "view_metadata" = metadata_distribution_plot(),
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
            "Feature Numbers" = analysis_outputs$feature_number_plot(),
            "Missing Values" = analysis_outputs$missing_value_plot(),
            "Sample Correlation" = analysis_outputs$sample_cor_heatmap(),
            "Intensity Distribution" = analysis_outputs$intensity_distribution_plot(),
            "Density Plot" = analysis_outputs$density_plot(),
            NULL
          )
        },
        "PCA" = analysis_outputs$pca_plot(),
        "Heatmap" = analysis_outputs$heatmap_plot_static(),
        "Relative Abundance" = analysis_outputs$relative_abundance_plot(),
        "Statistics" = analysis_outputs$limma_volcano_plot(),
        "Enrichment" = enrichment_plot(),
        "Pathway" = pathway_plot(),
        "Classification Prediction" = NULL,
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
