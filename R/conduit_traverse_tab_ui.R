conduit_traverse_tab_ui <- function(id = "traverse") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title  = "Controls",
      width  = 280,
      bg     = "#faf6ee",
      fg     = "#3b352a",
      open   = "open",
      p(
        class = "text-muted small mb-2",
        "Explore linked intensities across assays and hierarchical feature relationships."
      ),
      textInput(ns("traverse_features"), label = "Feature (copy & paste)"),
      selectInput(ns("traverse_assay"), label = "Assay", choices = NULL),
      selectInput(ns("traverse_xaxis"), label = "X-axis variable", choices = NULL),
      hr(),
      actionButton(
        ns("apply_traverse"), "Apply",
        icon  = icon("play"),
        class = "btn-primary w-100"
      )
    ),
    tagList(
      bslib::card(
        full_screen = TRUE,
        class       = "card-light",
        bslib::card_header("Feature Intensities Across Assays (Interactive)"),
        bslib::card_body(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("qf_plot"), height = "480px"),
            type = 8, caption = "Loading feature hierarchy...", color = "#2f2a20"
          )
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          full_screen = TRUE,
          class       = "card-light",
          bslib::card_header("Feature Intensities (Static)"),
          bslib::card_body(
            shinycssloaders::withSpinner(
              plotOutput(ns("traverse_plot"), height = "460px"),
              type = 8, caption = "Loading intensity plot...", color = "#2f2a20"
            )
          )
        ),
        bslib::card(
          full_screen = TRUE,
          class       = "card-light",
          bslib::card_header("Feature Details"),
          bslib::card_body(
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("traverse_info")),
              type = 8, caption = "Loading feature details...", color = "#2f2a20"
            )
          )
        )
      )
    )
  )
}
