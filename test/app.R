library(shiny)
library(shinydashboardPlus)
library(shinyWidgets)  # For dropdown menus

library(fresh)
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

# Define UI for application
ui <- shinydashboardPlus::dashboardPage(
  header = dashboardHeader(title = "Conduit"),
  # Sidebar layout with a sidebar and main panel
  sidebar = shinydashboardPlus::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("File Upload", tabName = "file_upload", icon = icon("cloud-upload-alt")),
      menuItem("Analysis", tabName = "analysis", icon = icon("cogs"))

    ),

    # Collapsible sidebar sections
    sidebarMenu(
      menuItem("Analysis Settings", tabName = "analysis_settings", icon = icon("sliders-h"), startExpanded = TRUE),
      menuItem("Aggregation Level", tabName = "aggregation_level", icon = icon("layers"))
    )
  ),

  # Body of the dashboard
  body = dashboardBody(
    tabItems(
      # About Tab
      tabItem("about",
              h3("About"),
              p("Welcome to the Metaproteomics Analysis Tool. This tool helps in the analysis of metaproteomic data."),
              p("Select the options from the sidebar to begin.")
      ),

      # Analysis Tab
      tabItem("analysis",
              h3("Analysis Settings and Aggregation Level"),

              # Dropdowns for Analysis Settings and Aggregation Level
              selectInput("analysis_settings", "Analysis Settings", choices = c("Option 1", "Option 2")),
              selectInput("aggregation_level", "Aggregation Level", choices = c("Level 1", "Level 2"))
      ),

      # QC Tab
      tabItem("qc",
              h3("Quality Control")
      ),

      # Clustering Tab
      tabItem("clustering",
              h3("Clustering Analysis")
      ),

      # Statistics Tab
      tabItem("statistics",
              h3("Statistical Analysis")
      ),

      # Biomarkers Tab
      tabItem("biomarkers",
              h3("Biomarker Identification")
      ),

      # Pathway Analysis Tab
      tabItem("pathway_analysis",
              h3("Pathway Analysis")
      ),

      # File Upload Tab
      tabItem("file_upload",
              h3("Upload Your File"),
              fileInput("file1", "Choose CSV File", accept = ".csv")
      )
    )
  ),

  # Control the theme, including additional custom options
  freshTheme =  mytheme,   # Change the font to Roboto,
  footer = dashboardFooter("Developed by Charlie Bayne at the Gonzalez Lab UCSD")
  )


# Define server logic
server <- function(input, output) {

  # Server-side logic can be added here

}

# Run the application
shinyApp(ui = ui, server = server)
