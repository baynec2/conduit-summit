conduit_nav_link <- function(input_id, tab_value, icon_name, label) {
  actionLink(
    inputId   = input_id,
    label     = tagList(icon(icon_name), " ", label),
    class     = "nav-link",
    `data-tab` = tab_value
  )
}

conduit_sidebar_ui <- function() {
  bslib::sidebar(
    id    = "main_sidebar",
    width = 220,
    bg    = "#18150f",
    fg    = "#efe6d2",
    open  = "always",
    tags$nav(
      class = "nav flex-column conduit-nav mt-1",
      conduit_nav_link("tab_about",         "about",         "info-circle",     "About"),
      conduit_nav_link("tab_file_upload",   "file_upload",   "cloud-upload-alt","File Upload"),
      conduit_nav_link("tab_diann_qc",      "diann_qc",      "check",           "DIA-NN QC"),
      conduit_nav_link("tab_view_metadata", "view_metadata", "id-card",         "View Metadata"),
      conduit_nav_link("tab_filter_data",   "filter_data",   "filter",          "Filter Data"),
      conduit_nav_link("tab_analysis",      "analysis",      "chart-line",      "Analysis"),
      conduit_nav_link("tab_traverse",      "traverse",      "sitemap",         "Traverse"),
      conduit_nav_link("tab_help",          "help",          "question-circle", "Help")
    ),
    tags$hr(style = "border-color: rgba(255,255,255,0.15); margin: 10px 0;"),
    downloadButton("download_current_plot", "Save Current Plot",
                   class = "btn-outline-light btn-sm w-100")
  )
}
