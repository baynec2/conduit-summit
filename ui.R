library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets) # For dropdown menus
library(fresh)
library(QFeatures)
library(shiny)
# Sourcing all R files.
all_r_files = list.files("R/",full.names = TRUE)
lapply(X = all_r_files,FUN = source)

# Setting theme
conduit_theme = create_conduit_theme()

# Setting max upload size to 500MB
options(shiny.maxRequestSize = 500 * 1024^2)

# UI
ui <- shinydashboardPlus::dashboardPage(
  header = conduit_header_ui(),
  sidebar = conduit_sidebar_ui(),
  body = dashboardBody(
    use_theme(conduit_theme),
    tabItems(
      conduit_about_tab_ui(),
      conduit_file_upload_tab_ui(),
      conduit_metadata_tab_ui(),
      conduit_filter_data_tab_ui(),
      conduit_analysis_tab_ui(),
      conduit_help_tab_ui()
    )
  ),
  footer = conduit_footer_ui()
)
