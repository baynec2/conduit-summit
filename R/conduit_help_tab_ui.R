conduit_help_tab_ui = function(){
  tabItem(
    tabName = "help",

    h2("Help & Documentation"),
    p("Welcome to the Conduit-GUI Help tab. Below you'll find answers to common questions and a link to the full documentation."),

    br(),

    h3("Frequently Asked Questions (FAQ)"),

    tags$ul(
      tags$li(
        tags$b("Q: What is the difference between Conduit, Conduit-GUI, and ConduitR?"),
        br(),
        "A: Conduit is a Snakemake workflow that handles all the heavy computational tasks involved in (meta)proteomics.
        It defines the experimental search space, identifies present peptides and their abundances, and resolves taxonomic and functional information.",
        br(),
        "Conduit-GUI is a visual interface for interpreting those results—it generates plots, runs statistical tests, and helps users explore their data.",
        br(),
        "ConduitR is an R package that powers both the workflow and GUI with utility functions, visualizations, and helper tools."
      ), br(),

      tags$li(
        tags$b("Q: Why do you need three tools?"),
        br(),
        "A: Each tool serves a specific purpose. Conduit performs scalable, reproducible analysis at the command line.
        Conduit-GUI makes the results interactive and interpretable. ConduitR acts as the glue between them—supporting internal functionality
        and enabling advanced users to build on the framework."
      ), br(),

      tags$li(
        tags$b("Q: What kind of input files does Conduit-GUI accept?"),
        br(),
        "A: For full functionality, Conduit-GUI uses an ",
        tags$code(".rds"),
        " file produced by the Conduit workflow. This file contains all data and metadata in a structured format.",
        br(),
        "Alternatively, for standard proteomics workflows, you can upload a data matrix and annotation file directly
        (e.g., output from DIA-NN), though some features may be limited."
      ), br(),

      tags$li(
        tags$b("Q: How do I get the Conduit .rds file?"),
        br(),
        "A: You need to run the Conduit workflow, which currently requires command-line usage.",
        br(),
        "Don’t worry—we provide detailed instructions to help make this as easy as possible:",
        tags$a(href = "https://github.com/baynec2/conduit",
               "View setup instructions on GitHub",
               target = "_blank", style = "color: #337ab7;")
      ), br(),

      tags$li(
        tags$b("Q: Can I use Conduit-GUI without R programming experience?"),
        br(),
        "A: Yes! The interface is designed to be fully usable without writing any code."
      ), br(),

      tags$li(
        tags$b("Q: Where can I report bugs or suggest features?"),
        br(),
        "A: Please open an issue or feature request on GitHub:",
        tags$a(href = "https://github.com/baynec2/conduit-GUI/issues",
               "Submit an issue or feature request",
               target = "_blank", style = "color: #337ab7;")
      )
    ),

    br(),

    h3("Full Documentation"),
    tags$a(
      href = "https://github.com/baynec2/conduit-GUI/wiki",
      "📖 Visit the Conduit-GUI Wiki",
      target = "_blank",
      style = "font-size: 16px; color: #337ab7;"
    )
  )
}
