# Returns a single nav_item() containing all right-side navbar controls.
conduit_header_ui <- function() {
  bslib::nav_item(
    div(
      class = "d-flex gap-2 align-items-center",
      bslib::popover(
        trigger = actionButton(
          "plot_opts_btn",
          tagList(icon("chart-bar"), " Plot Options"),
          class = "btn-outline-light btn-sm"
        ),
        title = "Plot Options",
        selectInput("plot_theme", "Select Plot Theme",
          choices  = c("theme_conduit", "ggprism", "theme_classic", "theme_bw",
                       "theme_dark", "theme_void", "theme_light", "theme_minimal"),
          selected = "theme_conduit"
        ),
        selectInput("color_palette", "Select a color palette:",
          choices = c("viridis", "plasma", "magma", "cividis"), selected = "viridis"
        ),
        selectInput("plot_file_format", "Select a plot file format:",
          choices = c("pdf", "png", "svg", "jpeg", "tiff", "eps"), selected = "pdf"
        ),
        numericInput("plot_height", "Plot height (inches):",
          value = 7, min = 1, max = 50, step = 0.5
        ),
        numericInput("plot_width", "Plot width (inches):",
          value = 7, min = 1, max = 50, step = 0.5
        )
      ),
      bslib::popover(
        trigger = actionButton(
          "agg_btn",
          tagList(icon("sliders-h"), " Aggregation"),
          class = "btn-outline-light btn-sm"
        ),
        title = "Aggregation Level",
        uiOutput("agg_level_choices_ui")
      ),
      bslib::popover(
        trigger = actionButton(
          "adv_btn",
          tagList(icon("cogs"), " Advanced"),
          class = "btn-outline-light btn-sm"
        ),
        title = "Advanced Settings",
        selectInput("imputation_method", "Choose imputation method:",
          choices = MsCoreUtils::imputeMethods(), selected = "min"
        ),
        numericInput("log_base", "Choose log base", value = 2, min = 1, max = 10),
        selectInput("normalization_method", "Choose normalization method:",
          choices = c("none", MsCoreUtils::normalizeMethods()), selected = "none"
        ),
        numericInput("min_n", "Filter out features with fewer than this many observations",
          value = 1, min = 1, max = 1000
        ),
        actionButton("run_processing", "Apply Processing", icon = icon("play"),
          class = "btn-warning w-100"
        )
      ),
      downloadButton("download_current_plot", "",
        icon  = icon("download"),
        class = "btn-outline-light btn-sm",
        title = "Save Current Plot"
      )
    )
  )
}
