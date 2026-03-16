conduit_filter_data_tab_ui <- function(id = "filter_data") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title  = "Filters",
      width  = 310,
      bg     = "#f8f9fb",
      fg     = "#1a1a2e",
      open   = "open",
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = "Sample Filters",
          icon  = icon("vials"),
          uiOutput(ns("sample_filters"))
        ),
        bslib::accordion_panel(
          title = "Feature Filters",
          icon  = icon("atom"),
          uiOutput(ns("feature_filters"))
        )
      ),
      hr(),
      actionButton(
        ns("apply_filters"),
        tagList(icon("filter"), " Apply Filters"),
        class = "btn-primary btn-sm w-100 mb-2"
      ),
      actionButton(
        ns("clear_all_filters"),
        tagList(icon("rotate-left"), " Clear All Filters"),
        class = "btn-outline-secondary btn-sm w-100"
      )
    ),

    # ── Main area ─────────────────────────────────────────────────────────────
    tagList(
      htmltools::div(
        style = "flex: 0 0 auto;",
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::value_box(
            title            = "Samples Retained",
            value            = textOutput(ns("n_samples_retained")),
            showcase         = icon("vials"),
            theme            = "primary",
            height           = "90px",
            showcase_layout  = bslib::showcase_left_center(width = "30%")
          ),
          bslib::value_box(
            title            = "Features Retained",
            value            = textOutput(ns("n_features_retained")),
            showcase         = icon("atom"),
            theme            = "primary",
            height           = "90px",
            showcase_layout  = bslib::showcase_left_center(width = "30%")
          )
        )
      ),
      bslib::card(
        full_screen = TRUE,
        class       = "card-light",
        bslib::card_header("Filter Impact"),
        bslib::card_body(
          uiOutput(ns("filter_impact_ui"))
        )
      )
    )
  )
}
