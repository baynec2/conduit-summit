# Loading necessary ackages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets) # For dropdown menus
library(fresh)

# Creating the conduit theme
conduit_theme <- create_theme(
  adminlte_color(
    light_blue = "black"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#D8DEE3",
    dark_hover_bg = "#008080ff",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

# Define UI for application
ui <- dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = tags$img(src = "conduit_box.png", height = "50px"),
    # Add dropdowns to the left using tags$li()
    # Dropdown 1: Analysis Settings
    leftUi = tagList(
      dropdownBlock(
        title = "Color by",
        id = "color_by",
        icon = icon("palette"),
        checkboxGroupInput("setting_options", "Choose Settings:", choices = c("Option 1", "Option 2"))
      ),
      # Dropdown 2: Aggregation Level
      dropdownBlock(
        title = "Aggregation Level",
        id = "aggregation_level",
        icon = icon("sliders-h"),
        selectInput("agg_level", "Select Level of Peptide Aggregation:",
          choices = c(
            "protein", "peptide", "superkingdom", "kingdom",
            "phylum", "class", "order", "family", "genus", "species",
            "go", "kegg", "eggnog"
          )
        )
      ),
      dropdownBlock(
        title = "Advanced Settings",
        id = "advanced_settings",
        icon = icon("cogs"),
        checkboxGroupInput("setting_options", "Choose Settings:",
          choices = c("Option 1", "Option 2")
        )
      )
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("File Upload", tabName = "file_upload", icon = icon("cloud-upload-alt")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  body = dashboardBody(
    use_theme(conduit_theme), # Apply theme
    tabItems(
      # About Tab
      tabItem(
        "about",
        fluidRow(
          column(
            width = 6,offset = 2, # Centers the flip box
            flipBox(
              id = "about_flipbox",
              front = div(
                tags$img(src = "conduit_circle.png",
                         height = "1000px",
                         style = "display: block; margin-left: auto; margin-right: auto;")
              ),
              back = div(
                p("Will add something here")
              )
            )
          )
        )
      ),
      ################################################################################
      # Analyis Panel
      ################################################################################
      tabItem(
        "analysis",
        # Add sub-tabs within Analysis
        tabsetPanel(
          tabPanel(
            "QC",
            fluidPage(
              tabsetPanel(
                tabPanel(
                  "Feature Numbers",
                  box("feature_number_plot")
                ),
                tabPanel(
                  "Missing Values",
                  box("missing_values_plot")
                ),
                tabPanel(
                  "Sample Correlation",
                  box("sample_correlation_plot")
                ),
                tabPanel(
                  "Density Plot",
                  box("density_plot")
                ),
                tabPanel(
                  "Sample Coverage",
                  box("sample_coverage_plot")
                )
              )
            )
          ),
          tabPanel(
            "Clustering",
            fluidRow(
              column(
                width = 6,
                box(title = "PCA", id = "pca_plot")
              ),
              column(
                width = 6,
                box(title = "Heatmap", id = "heatmap_plot")
              )
            )
          ),
          tabPanel(
            "Statistics",
            fluidRow(
              column(
                width = 3,
                textInput(
                  inputId = "limma_formula",
                  label = "Enter formula for limma analysis"
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  inputId = "contrasts",
                  "Select Contrast",
                  choices = "placeholder"
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                box(title = "Statistics", id = "statistics_df")
              ),
              column(
                width = 6,
                box(title = "Volcano Plot", id = "volcano_plot")
              ),
            )
          ),
          tabPanel(
            "Biomarkers",
            fluidRow(
              column(
                width = 3,
                selectizeInput(
                  inputId = "prediction",
                  label = "Variable to Predict",
                  choices = "placeholder"
                )
              ),
              column(width = 3, sliderInput(
                inputId = "per_training",
                label = "Percent Training Set Split",
                min = 50,
                max = 100,
                value = 75
              )),
              column(
                width = 3,
                selectInput(
                  inputId = "model",
                  label = "model to use",
                  choices = "placeholder"
                )
              )
            )
          ),
          tabPanel(
            "Pathway Analysis",
            h4("Pathway Analysis"),
            p("Perform pathway enrichment analysis.")
          )
        )
      ),
      ###############################################################################
      # File Upload tab
      ###############################################################################
      tabItem(
        "file_upload",
        # Main body where the user will upload files.
        h3("Conduit is best when used in combination Probiomecatalyst"),
        fileInput("qf", "Choose .rda File", accept = ".rda"),
        h3("Alternatively, you can use conduit with matrices generated
           with an alternative workflow below"),
        fileInput(
          inputId = "matrix_input",
          multiple = TRUE,
          label = "Upload all matrices",
          accept = c(".csv", ".txt", ".tsv")
        ),
        h3("Upload colData here"),
        fileInput(
          inputId = "coldata_input",
          label = "upload all",
          accept = c(".csv", ".txt", ".tsv")
        ),
        # Bottom Box that will show the user what they have uploaded
        box(
          solidHeader = FALSE,
          title = "Uploaded File Stats",
          background = NULL,
          width = 12,
          status = "danger",
          footer = fluidRow(
            column(
              width = 3,
              descriptionBlock(
                header = "# Samples",
                text = "X",
                rightBorder = TRUE,
                marginBottom = FALSE
              )
            ),
            column(
              width = 3,
              descriptionBlock(
                header = "# of Variables",
                text = "X",
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            ),
            column(
              width = 3,
              descriptionBlock(
                header = "# of Organisms",
                text = "X",
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            ),
            column(
              width = 3,
              descriptionBlock(
                header = "# of Proteins",
                text = "X",
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            ),
            column(
              width = 3,
              descriptionBlock(
                header = "# of Peptides",
                text = "X",
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            )
          )
        )
      )
    )
  ),
  footer = dashboardFooter(
    right = "Developed by Charlie Bayne at the Gonzalez Lab UCSD",
    left = tagList(
      socialButton(href = "https://github.com", icon = icon("github"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Server-side logic can be added here
}

# Run the application
shinyApp(ui = ui, server = server)
