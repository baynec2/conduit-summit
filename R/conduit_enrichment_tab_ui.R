conduit_enrichment_tab_ui <- function() {
    tabItem("enrichment",
      fluidRow(
        column(
          6,
          plotOutput("volcanoplot")
        ),
        column(
          6,
          box(
            title = "Enrichment Options",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            fluidRow(selectInput("enrichment_data", "data_to_plot",
              choices = c("all", "sig_values")
            )),
            fluidRow(selectInput("enrichment_quant", "Values to plot",
              choices = c("raw", "imputed")
            ))
          )
        )
      ),
      fluidRow(
        plotOutput("enrichment_results")
      )
  )
}
