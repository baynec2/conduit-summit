conduit_sidebar_ui = function(){
  shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("File Upload", tabName = "file_upload", icon = icon("cloud-upload-alt")),
      menuItem("View Metadata", tabName = "view_metadata", icon = icon("id-card")),
      menuItem("Filter Data", tabName = "filter_data", icon = icon("filter")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  )
}
