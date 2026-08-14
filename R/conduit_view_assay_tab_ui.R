conduit_view_assay_tab_ui <- function(id = "view_assay") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Options",
      width = 260,
      bg    = "#faf6ee",
      fg    = "#3b352a",
      open  = "open",
      shinyWidgets::pickerInput(
        ns("assay_to_show"),
        "Assay to display",
        choices  = NULL,
        selected = NULL,
        options  = shinyWidgets::pickerOptions(liveSearch = TRUE, size = 8, container = "body")
      ),
      checkboxInput(
        ns("include_row_data"),
        "Include feature metadata columns",
        value = TRUE
      ),
      hr(),
      downloadButton(
        ns("download_assay_csv"),
        tagList(icon("download"), " Download CSV"),
        class = "btn-primary btn-sm w-100"
      )
    ),
    tagList(
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::value_box(
          title           = "Samples",
          value           = textOutput(ns("n_samples")),
          showcase        = icon("vials"),
          theme           = "primary",
          height          = "90px",
          showcase_layout = bslib::showcase_left_center(width = "30%")
        ),
        bslib::value_box(
          title           = "Features",
          value           = textOutput(ns("n_features")),
          showcase        = icon("atom"),
          theme           = "primary",
          height          = "90px",
          showcase_layout = bslib::showcase_left_center(width = "30%")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        class       = "card-light",
        bslib::card_header(textOutput(ns("table_title"), inline = TRUE)),
        bslib::card_body(
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("assay_table")),
            type = 8, caption = "Loading assay data...", color = "#2f2a20"
          )
        )
      )
    )
  )
}
