conduit_header_ui <- function() {
  shinydashboardPlus::dashboardHeader(
    title = tags$img(src = "conduit_box.png", height = "50px"),
    leftUi = tagList(
      dropdownBlock(
        title = "Plot Options",
        id = "plot_options",
        icon = icon("magnifying-glass-chart"),
        uiOutput("plot_color_choices_ui"),
        uiOutput("plot_shape_choices_ui")
      ),
      dropdownBlock(
        title = "Aggregation Level",
        id = "aggregation_level",
        icon = icon("sliders-h"),
        uiOutput("agg_level_choices_ui")
      ),
      dropdownBlock(
        title = "Advanced Settings",
        id = "advanced_settings",
        icon = icon("cogs"),
        selectInput("imputation_method", "Choose imputation method:",
                    choices = MsCoreUtils::imputeMethods(),
                    selected = "min"
        ),
        numericInput(
          "log_base",
          "Choose log base",
          value = 2,
          min = 1,
          max = 10
        ),
        selectInput("normalization_method", "Choose normalization method:",
                    choices = c("none", MsCoreUtils::normalizeMethods()),
                    selected = c("none")
        )
      )
    )
  )
}
