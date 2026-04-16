conduit_provenance_server <- function(id, conduit_obj) {
  moduleServer(id, function(input, output, session) {

    prov <- reactive({
      req(conduit_obj())
      slot(conduit_obj(), "provenance")
    })

    # ── Value boxes ───────────────────────────────────────────────────────────

    output$version <- renderText({
      req(prov())
      validate(need(!is.null(prov()$workflow_version), "N/A"))
      as.character(prov()$workflow_version)
    })

    output$date <- renderText({
      req(prov())
      validate(need(!is.null(prov()$generated_date), "N/A"))
      format(prov()$generated_date, "%Y-%m-%d")
    })

    output$uniprot <- renderText({
      req(prov())
      validate(need(!is.null(prov()$uniprotkb_release), "N/A"))
      as.character(prov()$uniprotkb_release)
    })

    # ── Config text outputs ───────────────────────────────────────────────────

    output$txt_snakemake <- renderText({
      req(prov())
      validate(need(!is.null(prov()$config$snakemake_yaml), "No Snakemake config found in provenance."))
      df <- prov()$config$snakemake_yaml
      paste(paste0(df$parameter, ": ", df$value), collapse = "\n")
    })

    output$txt_diann_lib <- renderText({
      req(prov())
      validate(need(!is.null(prov()$config$diann_spectral_library_cfg), "No DIA-NN library config found in provenance."))
      paste(prov()$config$diann_spectral_library_cfg$value, collapse = "\n")
    })

    output$txt_diann_run <- renderText({
      req(prov())
      validate(need(!is.null(prov()$config$diann_run_cfg), "No DIA-NN run config found in provenance."))
      paste(prov()$config$diann_run_cfg$value, collapse = "\n")
    })

    output$txt_runtime <- renderText({
      req(prov())
      validate(need(!is.null(prov()$config$runtime), "No runtime config found in provenance."))
      df <- prov()$config$runtime
      paste(paste0(df$parameter, ": ", df$value), collapse = "\n")
    })

    # ── Download buttons (shown only when provenance exists) ──────────────────

    output$download_buttons_ui <- renderUI({
      req(prov())
      validate(need(!is.null(prov()$config), ""))
      tagList(
        downloadButton(session$ns("dl_snakemake"), "Snakemake Config"),
        downloadButton(session$ns("dl_diann_lib"), "DIA-NN Library"),
        downloadButton(session$ns("dl_diann_run"), "DIA-NN Run"),
        downloadButton(session$ns("dl_runtime"),   "Runtime")
      )
    })

    output$dl_snakemake <- downloadHandler(
      filename = function() "snakemake_config.yaml",
      content  = function(file) {
        df <- prov()$config$snakemake_yaml
        writeLines(paste(paste0(df$parameter, ": ", df$value), collapse = "\n"), file)
      }
    )

    output$dl_diann_lib <- downloadHandler(
      filename = function() "diann_spectral_library.cfg",
      content  = function(file) writeLines(prov()$config$diann_spectral_library_cfg$value, file)
    )

    output$dl_diann_run <- downloadHandler(
      filename = function() "diann_run.cfg",
      content  = function(file) writeLines(prov()$config$diann_run_cfg$value, file)
    )

    output$dl_runtime <- downloadHandler(
      filename = function() "runtime.txt",
      content  = function(file) {
        df <- prov()$config$runtime
        writeLines(paste(paste0(df$parameter, ": ", df$value), collapse = "\n"), file)
      }
    )

    invisible(NULL)
  })
}
