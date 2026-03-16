library(shiny)
library(bslib)
library(shinyWidgets)
library(QFeatures)
library(MsCoreUtils)

# Sourcing all R files.
all_r_files <- list.files("R/", full.names = TRUE)
lapply(X = all_r_files, FUN = source)

# Setting max upload size to 500MB
options(shiny.maxRequestSize = 500 * 1024^2)

ui <- bslib::page_navbar(
  id              = "main_tabs",
  title           = NULL,
  bg              = "#15131e",
  inverse         = TRUE,
  collapsible     = TRUE,
  underline       = FALSE,
  fillable        = FALSE,
  padding         = "1.25rem",
  theme           = create_conduit_theme(),
  header      = tagList(
    shinyjs::useShinyjs(),
    shinydisconnect::disconnectMessage(
      text           = "Something went wrong! Try refreshing the page.",
      refresh        = "Refresh",
      background     = "#15131e",
      colour         = "#FFFFFF",
      refreshColour  = "#f3b24b",
      overlayColour  = "#15131e",
      overlayOpacity = 1,
      width          = "full",
      top            = "center",
      size           = 24,
      css            = ""
    )
  ),
  footer = conduit_footer_ui(),

  # ── Navigation tabs ──────────────────────────────────────────────────────
  bslib::nav_panel("About",         value = "about",         icon = icon("house"),            conduit_about_tab_ui()),
  bslib::nav_panel("Database",      value = "database",      icon = icon("database"),         conduit_database_tab_ui("database")),
  bslib::nav_panel("DIA-NN QC",     value = "diann_qc",      icon = icon("check-circle"),     conduit_diann_qc_tab_ui("diann_qc")),
  bslib::nav_panel("View Metadata", value = "view_metadata", icon = icon("table"),            conduit_metadata_tab_ui("view_metadata")),
  bslib::nav_panel("Filter Data",   value = "filter_data",   icon = icon("filter"),           conduit_filter_data_tab_ui("filter_data")),
  bslib::nav_panel("View Assay",    value = "view_assay",    icon = icon("table-cells"),      conduit_view_assay_tab_ui("view_assay")),
  bslib::nav_panel("Analysis",      value = "analysis",      icon = icon("chart-line"),       conduit_analysis_tab_ui("analysis")),
  bslib::nav_panel("Traverse",      value = "traverse",      icon = icon("sitemap"),          conduit_traverse_tab_ui("traverse")),
  bslib::nav_panel("Help",          value = "help",          icon = icon("circle-question"),  conduit_help_tab_ui()),

  # ── Right-side controls ──────────────────────────────────────────────────
  bslib::nav_spacer(),
  conduit_header_ui()
)
