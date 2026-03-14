library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets) # For dropdown menus
library(fresh)
library(QFeatures)
library(shiny)
library(waiter)
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
    shinyjs::useShinyjs(),
    # Making a nice disconnect message.
    shinydisconnect::disconnectMessage(
      text = "Something went wrong! Try refreshing the page.",
      refresh = "Refresh",
      background = "#15131efe",
      colour = "#FFFFFF",
      refreshColour = "#f3b24bff",
      overlayColour = "#15131efe",
      overlayOpacity = 1,
      width = "full",
      top = "center",
      size = 24,
      css = ""
    ),
    # Greying out tabs
    tags$style(HTML("
  .disabled-tab {
    pointer-events: none;
    color: #aaa !important;
  }
")),
   # # Setting up the loading screen for when the app is loading.
   # # This breaks Data filter for some reason
   #  useWaiter(),
   #  autoWaiter(
   #    color = "#15131efe",
   #    html = tagList(
   #      spin_loaders(color = "#f3b24bff"),
   #      br(),
   #      br(),
   #      c(
   #        "Crunching numbers..."
   #      )
   #    )
   #  ),
    use_theme(conduit_theme),
    tabItems(
      conduit_about_tab_ui(),
      conduit_file_upload_tab_ui("file_upload"),
      conduit_diann_qc_tab_ui("diann_qc"),
      conduit_metadata_tab_ui("view_metadata"),
      conduit_filter_data_tab_ui("filter_data"),
      conduit_analysis_tab_ui("analysis"),
      conduit_traverse_tab_ui("traverse"),
      conduit_enrichment_tab_ui("enrichment"),
      conduit_pathway_tab_ui("pathway"),
      conduit_help_tab_ui()
    )
  ),
  footer = conduit_footer_ui()
)
