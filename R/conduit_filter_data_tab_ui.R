conduit_filter_data_tab_ui = function(){
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
        uiOutput("sample_filters")
      )
    ),

    # Feature Filter Box
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        background = NULL,
        status = "primary",
        title = "Feature Filter (Per Assay)",
        uiOutput("feature_filters")
      )
    ),

    # Filtered result output (optional)
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        background = NULL,
        status = "primary",
        title = "Filtered Data Preview",
        verbatimTextOutput("filtered_qfeatures")
      )
    )
  )
}
