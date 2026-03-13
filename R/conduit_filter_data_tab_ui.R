conduit_filter_data_tab_ui <- function() {
  tabItem(
    "filter_data",

    # Metadata Filter Box
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        background = NULL,
        status = "primary",
        title = "Metadata (Sample) Filter",
        uiOutput("sample_filters")  # dynamic sample filter pickers
      )
    ),

    # Filtered result output (optional preview)
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        background = NULL,
        status = "primary",
        title = "Filtered Data Preview",
        verbatimTextOutput("filtered_qfeatures")  # shows filtered QFeatures structure
      )
    )
  )
}
