conduit_ai_tab_ui <- function(id = "ai") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "AI Settings",
      width = 280,
      bg = "#f8f9fb",
      fg = "#1a1a2e",
      selectInput(
        ns("provider"),
        "Provider",
        choices  = c("OpenAI", "Anthropic", "Google Gemini", "Ollama (local)"),
        selected = "Anthropic"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'Ollama (local)'", ns("provider")),
        passwordInput(ns("api_key"), "API Key",
          value = "",
          placeholder = "sk-..."),
        helpText(
          "Auto-filled from ", tags$code("ANTHROPIC_API_KEY"), ",",
          tags$code("OPENAI_API_KEY"), ", or ", tags$code("GEMINI_API_KEY"),
          " environment variables if set."
        )
      ),
      uiOutput(ns("model_select_ui")),
      hr(),
      actionButton(ns("connect"), "Connect", class = "btn-primary w-100"),
      uiOutput(ns("connection_status")),
      uiOutput(ns("context_display")),
      actionButton(ns("clear_chat"), "Clear Chat", class = "btn-outline-secondary w-100 mt-2",
                   icon = icon("trash")),
      hr(),
      helpText(
        "Your API key is held in memory for this session only — it is never written to disk, logged, or",
        " included in any output. It is transmitted only to your chosen API provider to authenticate requests.",
        " For local use, set it as an environment variable in ", tags$code(".Renviron"),
        " and ensure that file is listed in ", tags$code(".gitignore"), "."
      )
    ),
    shinychat::chat_ui(
      ns("chat"),
      placeholder = "Ask about your conduit data...",
      height = "100%",
      fill = TRUE
    )
  )
}
