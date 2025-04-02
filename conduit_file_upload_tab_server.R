conduit_file_upload_tab_server <- function() {

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
    nrow(colData(slot(conduit_obj(), "QFeatures"))) # Number of samples
  })

  output$num_sample_md_variables <- renderText({
    req(conduit_obj())
    ncol(colData(slot(conduit_obj(), "QFeatures"))) # Number of features/variables
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

  observe({
    if (pg_to_consider() == "multiple_proteins_in_group") {
      output$taxa_tree_plot <- renderPlot({
        conduitR::plot_percent_detected_taxa_tree(conduit_obj(), layout = "automatic")
      })
      output$protein_taxonomy <- DT::renderDT(
        conduitR::calc_percent_proteins_detected(
          database_protein_taxonomy = slot(conduit_obj(), "database_protein_taxonomy"),
          detected_protein_taxonomy = slot(conduit_obj(), "detected_protein_taxonomy") # ✅ Fixed spelling
        )
      )
    } else {
      output$taxa_tree_plot <- renderPlot({
        conduitR::plot_percent_detected_unique_taxa_tree(conduit_obj(), layout = "automatic")
      })
    }
  })
}
