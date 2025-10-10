conduit_diann_qc_tab_ui = function(){
  tabItem(
      tabPanel(
        "DIA-NN QC",
        fluidRow(
          box(
            title = "Plot Options",
            status = "primary",
            solidHeader = TRUE,
            width = 12,  # Make the box take up the full row
            fluidRow(
              column(
                width = 3,
                selectInput("diann_qc_columns", "Select metric to plot",
                            choices = c("static", "interactive"),
                            selected = "static"
                )
              ),
              column(
                width = 3,
                uiOutput("diann_qc_columns_ui")
          )
        ),
        fluidRow(
          uiOutput("diann_qc_plot_ui"),
        )
          )
        )
      )
  )
}
