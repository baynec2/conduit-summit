conduit_ai_tab_ui <- function(id = "ai") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "AI Settings",
      width = 280,
      bg = "#faf6ee",
      fg = "#3b352a",
      selectInput(
        ns("provider"),
        "Provider",
        choices  = c("OpenAI", "Anthropic", "Google Gemini", "Ollama (local)"),
        selected = "Anthropic"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'Ollama (local)'", ns("provider")),
        tags$div(
          class = "d-flex align-items-center gap-1 mb-1",
          tags$label("API Key", class = "form-label mb-0"),
          bslib::popover(
            trigger = tags$span(icon("circle-question"), style = "font-size:0.85em; color:#6f6552; cursor:pointer;"),
            title = "API Key",
            tags$p("Auto-filled from ", tags$code("ANTHROPIC_API_KEY"), ", ", tags$code("OPENAI_API_KEY"), ", or ", tags$code("GEMINI_API_KEY"), " environment variables if set."),
            tags$p("Your key is held in memory for this session only — never written to disk or logged. For local use, set it in ", tags$code(".Renviron"), " (and add that file to ", tags$code(".gitignore"), ").")
          )
        ),
        passwordInput(ns("api_key"), label = NULL,
          value = "",
          placeholder = "sk-...")
      ),
      uiOutput(ns("model_select_ui")),
      hr(),
      actionButton(ns("connect"), "Connect", class = "btn-primary w-100"),
      uiOutput(ns("connection_status")),
      uiOutput(ns("context_display")),
      actionButton(ns("clear_chat"), "Clear Chat", class = "btn-outline-secondary w-100 mt-2",
                   icon = icon("trash")),
    ),
    shinychat::chat_ui(
      ns("chat"),
      placeholder = "Ask about your conduit data...",
      height = "100%",
      fill = TRUE
    )
  )
}
