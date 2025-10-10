conduit_sidebar_ui = function(){
  shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "main_tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("File Upload", tabName = "file_upload", icon = icon("cloud-upload-alt")),
      menuItem("DIA-NN QC", tabName = "diann_qc", icon = icon("check")),
      menuItem("View Metadata", tabName = "view_metadata", icon = icon("id-card")),
      menuItem("Filter Data", tabName = "filter_data", icon = icon("filter")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Traverse",tabName = "traverse", icon = icon("sitemap")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      menuItem("Enrichment", tabName = "enrichment", icon = icon("network-wired"))
      ),
    # Wrap button in a div so it's clickable
      div(
        style = "padding: 10px; text-align: center; pointer-events: all;",
        downloadButton("save_plot", "Save Current Plot", class = "btn-block")
    )
  )
}
