conduit_sidebar_ui <- function() {
  shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "main_tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem(
        "File Upload",
        tabName = "file_upload",
        icon = icon("cloud-upload-alt")
      ),
      menuItem(
        "View Metadata",
        tabName = "view_metadata",
        icon = icon("id-card")
      ),
      menuItem("Filter Data", tabName = "filter_data", icon = icon("filter")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      menuItem(
        "Enrichment",
        tabName = "enrichment",
        icon = icon("network-wired")
      ),
      menuItem("Pathway", tabName = "pathway", icon = icon("diagram-project"))
    ),
    downloadButton(
      "download_current_plot",
      label = span("Save Current Plot", class = "sidebar-label"),
      class = "btn-block",
      style = "background-color: white; color: black; border: 1px solid #ccc;"
    ),
    tags$head(
      tags$style(HTML(
        "
        .sidebar-collapse .sidebar .sidebar-label {
          display: none !important;
        }
      "
      ))
    )
  )
}
