conduit_about_tab_ui <- function() {

  step_card <- function(id, number, icon_name, title, desc, btn_id, btn_label) {
    div(
      id    = id,
      class = "workflow-step",
      bslib::card(
        class = "text-center h-100",
        bslib::card_body(
          class = "d-flex flex-column align-items-center gap-2 py-3",
          div(class = "workflow-step-number", number),
          div(icon(icon_name), class = "conduit-feature-icon mb-0"),
          h6(title, class = "mb-1 mt-1"),
          p(desc, class = "text-muted small flex-grow-1 mb-2"),
          actionButton(
            btn_id,
            tagList(btn_label, icon("arrow-right")),
            class = "btn-outline-primary btn-sm w-100"
          )
        )
      )
    )
  }

  tagList(
    # ── Hero banner ──────────────────────────────────────────────────────────
    div(
      class = "conduit-hero text-center",
      tags$img(
        src   = "conduit-summit.png",
        style = "max-width: 240px; margin-bottom: 1.75rem;"
      ),
      h1("Conduit-Summit", class = "conduit-hero-title"),
      p(
        "A full-featured visual interface for metaproteomics data analysis.",
        class = "conduit-hero-subtitle"
      ),
      div(
        class = "conduit-hero-upload mt-3",
        p(
          "Upload the ",
          tags$code(".rds", style = "color: #f3b24b; background: rgba(243,178,75,0.15); border: none;"),
          " file produced by ",
          actionLink("goto_help_from_upload", "conduit-ascent",
                     style = "color: #f3b24b; text-decoration: underline;"),
          ".",
          class = "small mb-2"
        ),
        fileInput(
          "conduit_rds", NULL, accept = ".rds",
          buttonLabel = tagList(icon("folder-open"), " Browse"),
          placeholder = "No file selected"
        )
      ),
    ),

    # ── Workflow steps ───────────────────────────────────────────────────────
    div(
      class = "mt-4",
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Workflow",
          icon  = icon("list-ol"),
          bslib::layout_columns(
            col_widths = NA,
            step_card(
              "step_1", "1", "cloud-upload-alt", "File Upload",
              "Upload your Conduit-Ascent .rds file above to unlock all features.",
              "step_goto_file_upload", "File Upload"
            ),
            step_card(
              "step_2", "2", "database", "Database",
              "Explore taxonomic coverage, protein groups detected, and per-species summaries.",
              "step_goto_database", "Database"
            ),
            step_card(
              "step_3", "3", "check-circle", "DIA-NN QC",
              "Review DIA-NN quality metrics and search summaries.",
              "step_goto_diann_qc", "DIA-NN QC"
            ),
            step_card(
              "step_4", "4", "id-card", "View Metadata",
              "Inspect and verify sample metadata and experimental design.",
              "step_goto_metadata", "Metadata"
            ),
            step_card(
              "step_5", "5", "filter", "Filter Data",
              "Filter samples, normalize, impute, and transform your data.",
              "step_goto_filter", "Filter Data"
            ),
            step_card(
              "step_6", "6", "table", "View Assay",
              "Inspect the processed quantitative matrix before running analyses.",
              "step_goto_view_assay", "View Assay"
            ),
            step_card(
              "step_7", "7", "chart-line", "Analysis",
              "PCA, heatmaps, differential expression, GO/KEGG enrichment, and pathway maps.",
              "step_goto_analysis", "Analysis"
            ),
            step_card(
              "step_8", "8", "sitemap", "Traverse",
              "Navigate connections across proteins, taxa, and pathways.",
              "step_goto_traverse", "Traverse"
            )
          )
        )
      )
    )
  )
}
