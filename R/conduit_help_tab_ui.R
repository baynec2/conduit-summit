conduit_help_tab_ui <- function() {

  ecosystem_card <- function(icon_name, name, desc, url) {
    bslib::card(
      class = "conduit-ecosystem-card",
      bslib::card_body(
        div(class = "conduit-ecosystem-icon", icon(icon_name)),
        tags$h6(name, class = "fw-bold mb-1"),
        tags$p(desc, class = "text-muted small flex-grow-1 mb-2"),
        tags$a(
          href = url, target = "_blank",
          tagList(icon("github"), " View on GitHub"),
          class = "btn btn-outline-primary btn-sm"
        )
      )
    )
  }

  tagList(
    # ── Hero ─────────────────────────────────────────────────────────────────
    div(
      class = "conduit-help-hero",
      h2("Help & Documentation"),
      p("Everything you need to get started with the Conduit metaproteomics platform.")
    ),

    # ── Quick Start ───────────────────────────────────────────────────────────
    bslib::card(
      class = "card-light mb-3",
      bslib::card_header("Quick Start"),
      bslib::card_body(
        div(
          class = "conduit-quickstart-step",
          div(class = "conduit-quickstart-num", "1"),
          div(
            tags$strong("Run Conduit-Ascent"),
            tags$p(
              class = "mb-1 text-muted small",
              "Process your raw mass-spec data through the Snakemake workflow to produce a structured ",
              tags$code(".rds"), " output file."
            ),
            tags$a(
              href = "https://github.com/baynec2/conduit-ascent", target = "_blank",
              tagList(icon("arrow-up-right-from-square"), " Conduit-Ascent setup guide"),
              class = "btn btn-outline-primary btn-sm"
            )
          )
        ),
        div(
          class = "conduit-quickstart-step",
          div(class = "conduit-quickstart-num", "2"),
          div(
            tags$strong("Upload your file"),
            tags$p(
              class = "mb-0 text-muted small",
              "Drag-and-drop or browse for your ", tags$code(".rds"), " file on the Home tab.",
              " All analysis tabs unlock automatically."
            )
          )
        ),
        div(
          class = "conduit-quickstart-step",
          div(class = "conduit-quickstart-num", "3"),
          div(
            tags$strong("Explore your data"),
            tags$p(
              class = "mb-0 text-muted small",
              "Work through the tabs in order: Database → DIA-NN QC → Filter → Analysis → Enrichment/Pathway."
            )
          )
        )
      )
    ),

    # ── AI Assistant ──────────────────────────────────────────────────────────
    bslib::card(
      class = "card-light mb-3",
      bslib::card_header(tagList(icon("robot"), " AI Assistant")),
      bslib::card_body(
        p(
          "The AI tab lets you explore your data, generate plots, run differential expression,",
          " and interpret results using plain-language questions.",
          " It connects to the AI provider of your choice using your own API key —",
          " your key is used only within your browser session and is never stored."
        ),
        tags$hr(class = "my-3"),
        h6("Getting an API Key", class = "fw-bold mb-3"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          # Anthropic
          div(
            class = "conduit-quickstart-step",
            div(
              tags$strong(icon("comment-dots"), " Anthropic (Claude)"),
              tags$p(
                class = "mb-2 text-muted small",
                "Sign in at ", tags$strong("console.anthropic.com"),
                ", go to ", tags$em("API Keys"), ", and create a new key.",
                " Claude models are recommended for best results with this app."
              ),
              tags$a(
                href = "https://console.anthropic.com/settings/keys", target = "_blank",
                tagList(icon("arrow-up-right-from-square"), " Anthropic Console"),
                class = "btn btn-outline-primary btn-sm"
              )
            )
          ),
          # OpenAI
          div(
            class = "conduit-quickstart-step",
            div(
              tags$strong(icon("robot"), " OpenAI (GPT)"),
              tags$p(
                class = "mb-2 text-muted small",
                "Sign in at ", tags$strong("platform.openai.com"),
                ", open the ", tags$em("API Keys"), " section under your profile,",
                " and generate a new secret key."
              ),
              tags$a(
                href = "https://platform.openai.com/api-keys", target = "_blank",
                tagList(icon("arrow-up-right-from-square"), " OpenAI Platform"),
                class = "btn btn-outline-primary btn-sm"
              )
            )
          ),
          # Google Gemini
          div(
            class = "conduit-quickstart-step",
            div(
              tags$strong(icon("gem"), " Google Gemini"),
              tags$p(
                class = "mb-2 text-muted small",
                "Visit ", tags$strong("aistudio.google.com"),
                " and click ", tags$em("Get API Key"), " in the left sidebar.",
                " A free tier is available."
              ),
              tags$a(
                href = "https://aistudio.google.com/app/apikey", target = "_blank",
                tagList(icon("arrow-up-right-from-square"), " Google AI Studio"),
                class = "btn btn-outline-primary btn-sm"
              )
            )
          ),
          # Ollama
          div(
            class = "conduit-quickstart-step",
            div(
              tags$strong(icon("server"), " Ollama (Local)"),
              tags$p(
                class = "mb-2 text-muted small",
                "No API key required. Install Ollama locally from ",
                tags$strong("ollama.com"), ", pull a model (e.g. ",
                tags$code("ollama pull llama3.2"), "), and make sure the server is running."
              ),
              tags$a(
                href = "https://ollama.com", target = "_blank",
                tagList(icon("arrow-up-right-from-square"), " Ollama"),
                class = "btn btn-outline-primary btn-sm"
              )
            )
          )
        )
      )
    ),

    # ── The Conduit Ecosystem ─────────────────────────────────────────────────
    h6("The Conduit Ecosystem", class = "text-muted text-uppercase fw-bold mb-2 mt-1",
       style = "letter-spacing: 0.06em; font-size: 0.75rem;"),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      ecosystem_card(
        "terminal", "Conduit-Ascent",
        "A Snakemake command-line workflow for scalable, reproducible metaproteomics processing. Identifies peptides, resolves taxonomy, and produces structured outputs.",
        "https://github.com/baynec2/conduit-ascent"
      ),
      ecosystem_card(
        "chart-line", "Conduit-Summit",
        "This app. A visual interface for exploring Conduit-Ascent results — plots, statistics, enrichment, and pathway analysis — no coding required.",
        "https://github.com/baynec2/conduit-summit"
      ),
      ecosystem_card(
        "cube", "ConduitR",
        "The R package that powers both tools. Provides utility functions, visualizations, and helper methods for advanced users building on the framework.",
        "https://github.com/baynec2/conduitR"
      )
    ),

    # ── FAQ ───────────────────────────────────────────────────────────────────
    h6("Frequently Asked Questions", class = "text-muted text-uppercase fw-bold mb-2 mt-4",
       style = "letter-spacing: 0.06em; font-size: 0.75rem;"),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = "Why three separate tools?",
        icon  = icon("circle-question"),
        p(
          "Each tool has a distinct job.",
          tags$strong("Conduit-Ascent"), " handles scalable, reproducible computation at the command line.",
          tags$strong(" Conduit-Summit"), " makes the results interactive and interpretable — no code needed.",
          tags$strong(" ConduitR"), " is the glue: an R package supporting internal logic and enabling power users to extend the framework."
        )
      ),
      bslib::accordion_panel(
        title = "What input files does Conduit-Summit accept?",
        icon  = icon("file"),
        p(
          "For full functionality, upload an ", tags$code(".rds"), " file produced by the Conduit-Ascent workflow.",
          " This contains all raw data, metadata, taxonomy mappings, and metrics in a single structured object."
        ),
        p(
          class = "mb-0 text-muted small",
          "Standard proteomics ", tags$code(".parquet"), " files (e.g. from DIA-NN or FragPipe) are also accepted,",
          " though some advanced features will be limited."
        )
      ),
      bslib::accordion_panel(
        title = "How do I get the Conduit .rds file?",
        icon  = icon("box-archive"),
        p(
          "You need to run the Conduit-Ascent workflow, which currently requires command-line access.",
          " We provide detailed setup instructions to make this as straightforward as possible."
        ),
        p(class = "mb-0",
          tags$em("Conduit-Basecamp"), " — a graphical interface for Conduit-Ascent — is in development.",
          " For now:"
        ),
        tags$a(
          href = "https://github.com/baynec2/conduit-ascent", target = "_blank",
          tagList(icon("arrow-up-right-from-square"), " View setup instructions on GitHub"),
          class = "btn btn-outline-primary btn-sm mt-2"
        )
      ),
      bslib::accordion_panel(
        title = "Is my data sent to an AI company?",
        icon  = icon("shield-halved"),
        p(
          "Only if you choose a cloud provider (Anthropic, OpenAI, or Google Gemini).",
          " In that case, your messages and a summary of your dataset structure are sent to that provider's API",
          " — the same way any API call works. Your API key is held only in your browser session",
          " and is never stored by Conduit-Summit."
        ),
        p(class = "mb-0",
          "If you prefer to keep your data entirely local, use the ",
          tags$strong("Ollama"), " option, which runs a model on your own machine with no external calls."
        )
      ),
      bslib::accordion_panel(
        title = "Do I need R programming experience?",
        icon  = icon("code"),
        p(class = "mb-0",
          "No. Conduit-Summit is designed to be fully usable without writing any code.",
          " All analysis, filtering, and visualisation is controlled through point-and-click interfaces."
        )
      ),
      bslib::accordion_panel(
        title = "What does each analysis tab do?",
        icon  = icon("table-list"),
        tags$ul(
          class = "mb-0",
          tags$li(tags$strong("Database"), " — overview of your dataset: sample counts, species coverage, taxonomic tree."),
          tags$li(tags$strong("DIA-NN QC"), " — quality metrics from the database search."),
          tags$li(tags$strong("View Metadata"), " — explore sample annotations and variable distributions."),
          tags$li(tags$strong("Filter Data"), " — subset samples and features; all downstream tabs update automatically."),
          tags$li(tags$strong("Analysis"), " — QC plots, PCA, heatmaps, differential expression (LIMMA), classification."),
          tags$li(tags$strong("Enrichment"), " — GO/KEGG over-representation or gene-set enrichment analysis."),
          tags$li(tags$strong("Pathway"), " — visualise KEGG pathway maps overlaid with your statistics."),
          tags$li(tags$strong("Traverse"), " — follow a feature across assay hierarchies (precursor → peptide → protein).")
        )
      )
    ),

    # ── Report a Bug ─────────────────────────────────────────────────────────
    div(
      class = "mt-4 p-3 rounded",
      style = "background: rgba(243,178,75,0.1); border-left: 4px solid #d4915c;",
      div(
        class = "d-flex align-items-center justify-content-between gap-3",
        div(
          tags$strong(icon("bug"), " Found a bug or have a feature request?"),
          tags$p(
            class = "mb-0 text-muted small",
            "Open an issue on GitHub — include your R session info and a reproducible example if possible."
          )
        ),
        tags$a(
          href = "https://github.com/baynec2/conduit-summit/issues", target = "_blank",
          tagList(icon("github"), " Open an Issue"),
          class = "btn btn-warning btn-sm flex-shrink-0"
        )
      )
    )
  )
}
