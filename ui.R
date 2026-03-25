library(shiny)
library(bslib)
library(shinyWidgets)
library(QFeatures)
library(MsCoreUtils)
library(ellmer)
library(shinychat)

# Sourcing all R files.
all_r_files <- list.files("R/", full.names = TRUE)
lapply(X = all_r_files, FUN = source)

# Setting max upload size to 500MB
options(shiny.maxRequestSize = 500 * 1024^2)

ui <- bslib::page_navbar(
  id              = "main_tabs",
  title           = NULL,
  bg              = "#000000",
  inverse         = TRUE,
  collapsible     = TRUE,
  underline       = FALSE,
  fillable        = FALSE,
  padding         = "1.25rem",
  theme           = create_conduit_theme(),
  header      = tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML("
      #ss-connect-dialog a.ss-github-link::before { content: '' !important; }
      #ss-connect-dialog a.ss-github-link {
        font-size: 13px !important;
        display: inline !important;
        margin-top: 0 !important;
        color: #f3b24b !important;
      }
    ")),
    tags$script(HTML("
      (function() {
        var observer = new MutationObserver(function() {
          var dialog = document.getElementById('ss-connect-dialog');
          if (dialog && !dialog.dataset.linkAdded) {
            dialog.dataset.linkAdded = 'true';
            var p = document.createElement('p');
            p.style.cssText = 'margin-top:12px; font-size:13px; color:#ffffff; opacity:0.75;';
            p.innerHTML = 'If this keeps happening, please <a class=\"ss-github-link\" href=\"https://github.com/baynec2/conduit-summit/issues\" target=\"_blank\">raise an issue on GitHub</a>.';
            dialog.appendChild(p);
          }
        });
        observer.observe(document.body, { childList: true, subtree: true });
      })();
    ")),
    shinydisconnect::disconnectMessage(
      text           = "Your session has disconnected or an unexpected error occurred.",
      refresh        = "Refresh Page",
      background     = "#000000",
      colour         = "#ffffff",
      refreshColour  = "#f3b24b",
      overlayColour  = "#000000",
      overlayOpacity = 0.92,
      width          = 520,
      top            = "center",
      size           = 16,
      css            = "font-family: inherit; letter-spacing: 0.01em;"
    )
  ),
  footer = conduit_footer_ui(),

  # ── Navigation tabs ──────────────────────────────────────────────────────
  bslib::nav_panel("About",         value = "about",         icon = icon("house"),            conduit_about_tab_ui()),
  bslib::nav_panel("AI",            value = "ai",            icon = icon("robot"),            conduit_ai_tab_ui("ai")),
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
