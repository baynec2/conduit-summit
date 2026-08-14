conduit_metadata_tab_ui <- function(id = "view_metadata") {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        title    = "# of Variables",
        value    = textOutput(ns("num_sample_md_variables")),
        showcase = icon("table"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "# Continuous",
        value    = textOutput(ns("num_continuous_variables")),
        showcase = icon("wave-square"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "# Discrete",
        value    = textOutput(ns("num_discrete_variables")),
        showcase = icon("list"),
        theme    = "primary"
      )
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title    = "Plot Controls",

        width    = 260,
        bg       = "#faf6ee",
        fg       = "#3b352a",
        uiOutput(ns("metadata_variable_choices_to_plot_ui"))
      ),
      tagList(
        bslib::card(
          full_screen = TRUE,
          class       = "card-light",
          bslib::card_header("Distribution of Selected Variable"),
          bslib::card_body(
            plotOutput(ns("metadata_distribution_plot"), height = "420px")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          class       = "card-light",
          bslib::card_header("Sample Metadata"),
          bslib::card_body(
            DT::dataTableOutput(ns("colData"), height = "380px")
          ),
          bslib::card_footer(
            downloadButton(ns("download_colData_table"), "Download Table")
          )
        )
      )
    )
  )
}
