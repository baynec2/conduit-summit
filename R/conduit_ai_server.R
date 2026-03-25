conduit_ai_server <- function(id, conduit_obj, final_qf, processed_assay,
                               final_colData_names, final_rowData_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Pre-fill API key from environment variables ───────────────────────────
    observeEvent(input$provider, {
      key <- switch(input$provider,
        "Anthropic"      = Sys.getenv("ANTHROPIC_API_KEY"),
        "OpenAI"         = Sys.getenv("OPENAI_API_KEY"),
        "Google Gemini"  = Sys.getenv("GEMINI_API_KEY"),
        ""
      )
      if (nzchar(key)) updateTextInput(session, "api_key", value = key)
    }, ignoreInit = FALSE)

    # ── Dynamic model choices by provider ────────────────────────────────────
    output$model_select_ui <- renderUI({
      provider <- input$provider
      req(provider)
      switch(provider,
        "OpenAI" = selectInput(
          ns("model"), "Model",
          choices = c(
            "gpt-4o-mini"  = "gpt-4o-mini",
            "gpt-4.1-mini" = "gpt-4.1-mini",
            "gpt-4o"       = "gpt-4o",
            "gpt-4.1"      = "gpt-4.1"
          )
        ),
        "Anthropic" = selectInput(
          ns("model"), "Model",
          choices = c(
            "Claude Haiku 4.5"  = "claude-haiku-4-5-20251001",
            "Claude Sonnet 4.5" = "claude-sonnet-4-5-20251001",
            "Claude Opus 4.5"   = "claude-opus-4-5"
          )
        ),
        "Google Gemini" = selectInput(
          ns("model"), "Model",
          choices = c(
            "Gemini 1.5 Flash" = "gemini-1.5-flash",
            "Gemini 2.0 Flash" = "gemini-2.0-flash",
            "Gemini 1.5 Pro"   = "gemini-1.5-pro"
          )
        ),
        "Ollama (local)" = textInput(
          ns("model"), "Model name",
          value = "llama3.2",
          placeholder = "e.g. llama3.2, mistral, phi4"
        )
      )
    })

    # ── Reactive store for dynamically rendered plots/tables ─────────────────
    plot_store <- reactiveValues()

    # ── Session-level QF store — tools can write modified QF here ────────────
    qf_session <- reactiveVal(NULL)

    # ── Stored experiment context ─────────────────────────────────────────────
    submitted_context <- reactiveVal("")

    # ── Show experiment context modal on Connect ──────────────────────────────
    show_context_modal <- function() {
      showModal(modalDialog(
        title = tagList(icon("robot"), " Describe Your Experiment"),
        tags$p(
          class = "text-muted mb-3",
          "Providing context helps the AI give more relevant, actionable answers.",
          " Include your research question, sample types, treatment groups, and experimental goals.",
          " This step is optional — leave blank to connect without context."
        ),
        textAreaInput(
          ns("experiment_context"),
          label       = NULL,
          value       = submitted_context(),
          placeholder = paste0(
            "e.g. Mouse gut microbiome study comparing germ-free vs. conventionally colonized ",
            "mice at baseline and 4 weeks. Goal is to identify differentially abundant proteins ",
            "and enriched pathways between treatment groups."
          ),
          rows  = 5,
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("connect_confirm"), "Connect", class = "btn-primary")
        ),
        size = "l",
        easyClose = TRUE
      ))
    }

    observeEvent(input$connect, { show_context_modal() })

    # ── Chat object — initialized on Connect confirm ──────────────────────────
    chat_obj <- eventReactive(input$connect_confirm, {
      removeModal()
      submitted_context(input$experiment_context)
      req(input$provider, input$model, conduit_obj())

      sys_prompt <- build_ai_system_prompt(conduit_obj(), final_qf(), processed_assay(),
                                            experiment_context = input$experiment_context)

      api_key_val <- input$api_key
      model_val   <- input$model

      chat <- tryCatch({
        switch(input$provider,
          "OpenAI" = ellmer::chat_openai(
            system_prompt = sys_prompt,
            model         = model_val,
            credentials   = function() api_key_val
          ),
          "Anthropic" = ellmer::chat_anthropic(
            system_prompt = sys_prompt,
            model         = model_val,
            credentials   = function() api_key_val
          ),
          "Google Gemini" = ellmer::chat_google_gemini(
            system_prompt = sys_prompt,
            model         = model_val,
            credentials   = function() api_key_val
          ),
          "Ollama (local)" = ellmer::chat_ollama(
            system_prompt = sys_prompt,
            model         = model_val
          )
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Connection Error",
          text  = conditionMessage(e),
          type  = "error",
          html  = FALSE
        )
        NULL
      })

      if (!is.null(chat)) {
        register_conduit_tools(
          chat                = chat,
          conduit_obj         = conduit_obj,
          final_qf            = final_qf,
          qf_session          = qf_session,
          processed_assay     = processed_assay,
          final_colData_names = final_colData_names,
          final_rowData_names = final_rowData_names,
          plot_store          = plot_store,
          output              = output,
          session             = session,
          ns                  = ns,
          chat_id             = "chat"
        )
      }
      chat
    })

    # ── Context display ───────────────────────────────────────────────────────
    output$context_display <- renderUI({
      ctx <- submitted_context()
      if (!nzchar(trimws(ctx))) return(NULL)
      tags$div(
        class = "mt-2",
        tags$label("Experiment Description", class = "form-label",
                   style = "font-size: 0.78em; font-weight: 600; color: #888;
                            text-transform: uppercase; letter-spacing: 0.05em;"),
        tags$textarea(
          class    = "form-control",
          readonly = NA,
          rows     = "3",
          style    = "font-size: 0.82em; resize: none; background: #f8f9fb;
                      color: #555; border-color: rgba(21,19,30,0.12);",
          ctx
        ),
        tags$p(
          class = "text-muted mt-1",
          style = "font-size: 0.75em;",
          icon("circle-info", style = "font-size: 0.9em;"),
          " Reconnect to update the experiment context."
        )
      )
    })

    # ── Connection status indicator ───────────────────────────────────────────
    output$connection_status <- renderUI({
      input$connect_confirm  # depend on connect confirm
      chat <- chat_obj()
      if (!is.null(chat)) {
        tags$div(
          style = "display:flex; align-items:center; gap:6px; margin-top:8px;",
          tags$span(style = "width:10px; height:10px; border-radius:50%; background:#28a745; display:inline-block; flex-shrink:0;"),
          tags$span("Connected", style = "color:#28a745; font-size:0.85em;")
        )
      }
    })

    # ── Clear chat history ────────────────────────────────────────────────────
    observeEvent(input$clear_chat, {
      req(chat_obj())
      chat_obj()$set_turns(list())
      shinychat::chat_clear("chat", session = session)
    })

    # ── Handle user messages (streaming) ─────────────────────────────────────
    observeEvent(input$chat_user_input, {
      req(chat_obj(), input$chat_user_input)
      tryCatch({
        stream <- chat_obj()$stream_async(input$chat_user_input)
        shinychat::chat_append("chat", stream, session = session)
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("401|Unauthorized|authentication", msg, ignore.case = TRUE)) {
          shinyalert::shinyalert(
            title = "Authentication Error",
            text  = "Invalid or expired API key. Please update your key and reconnect.",
            type  = "error"
          )
        } else {
          shinyalert::shinyalert(
            title = "Error",
            text  = msg,
            type  = "error"
          )
        }
      })
    })
  })
}
