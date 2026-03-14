conduit_header_ui <- function() {
  shinydashboardPlus::dashboardHeader(
    title = tags$img(src = "conduit-summit-top.png", height = "50px"),
    leftUi = tagList(
      dropdownBlock(
        title = "Plot Options",
        id = "plot_options",
        icon = icon("magnifying-glass-chart"),
        selectInput("plot_theme",
                    "Select Plot Theme",
                    choices = c("ggprism",
                                "theme_classic",
                                "theme_bw",
                                "theme_dark",
                                "theme_void",
                                "theme_light",
                                "theme_minimal")
        ),
        selectInput("color_palette",
                    "Select a color palette:",
                    choices = c("viridis", "plasma", "magma", "cividis"),
                    selected = "viridis"
        ),
        selectInput("plot_file_format",
                    "Select a plot file format:",
                    choices = c("pdf", "png", "svg", "jpeg","tiff","eps"),
                    selected = "pdf"
        ),
        numericInput("plot_height",
                     "Plot height (inches):",
                     value = 7,  # Default value
                     min = 1,    # Minimum allowed width
                     max = 50,   # Maximum allowed width
                     step = 0.5  # Increment step
        ),
        numericInput("plot_width",
                     "Plot width (inches):",
                     value = 7,  # Default value
                     min = 1,    # Minimum allowed width
                     max = 50,   # Maximum allowed width
                     step = 0.5  # Increment step
        )
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
        ),
        numericInput(
          "min_n",
          "Filter out features with fewer than this many observations",
          value = 1,
          min = 1,
          max = 1000
        ),
        actionButton("run_processing", "Apply Processing", icon = icon("play"),
                     class = "btn-primary btn-block")
      )
    )
  )
}
