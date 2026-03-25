# ── Helper: decode processing info per assay ──────────────────────────────────
# Decodes the method directly from the assay name, which now encodes it:
#   {base}_log{N}_{impute_method}_{norm_method}  (three-part: fully processed)
#   {base}_log{N}_{impute_method}                (two-part: imputed, no norm)
#   {base}_log{N}                                (one-part: log only)
assay_processing_info <- function(qf) {
  assay_names <- names(qf)

  lines <- lapply(assay_names, function(nm) {
    # Three-part: _log{N}_{imp}_{norm}
    m3 <- regmatches(nm, regexpr("_log([0-9]+)_([^_]+)_([^_]+)$", nm, perl = TRUE))
    if (length(m3) == 1 && nzchar(m3)) {
      parts <- strsplit(sub("^_log", "", m3), "_")[[1]]
      return(paste0("  - `", nm, "`: log", parts[1], "-transformed, ",
                    "imputed (", parts[2], "), normalized (", parts[3], ") — ready for limma"))
    }
    # Two-part: _log{N}_{imp}
    m2 <- regmatches(nm, regexpr("_log([0-9]+)_([^_]+)$", nm, perl = TRUE))
    if (length(m2) == 1 && nzchar(m2)) {
      parts <- strsplit(sub("^_log", "", m2), "_")[[1]]
      return(paste0("  - `", nm, "`: log", parts[1], "-transformed, imputed (", parts[2], ")"))
    }
    # One-part: _log{N}
    m1 <- regmatches(nm, regexpr("_log([0-9]+)$", nm, perl = TRUE))
    if (length(m1) == 1 && nzchar(m1)) {
      base <- sub("^_log", "", m1)
      return(paste0("  - `", nm, "`: log", base, "-transformed only"))
    }
    NULL
  })

  lines <- Filter(Negate(is.null), lines)
  if (length(lines) == 0) return("")
  paste0("Assay processing applied:\n", paste(lines, collapse = "\n"))
}

# ── Helper: assay name → use-case description ─────────────────────────────────
get_assay_descriptions <- function(assay_names) {
  taxonomic_ranks <- c("domain", "phylum", "class", "order", "family", "genus", "species")
  functional_types <- c("go", "kegg", "kegg_orthology", "pfam", "uniprot_pfam", "eggnog")

  desc <- vapply(assay_names, function(nm) {
    nm_lower <- tolower(nm)
    if (grepl("_log[0-9]+_[^_]+_[^_]+$", nm_lower)) {
      # Three-part suffix: _log{N}_{impute}_{norm} — fully processed
      m <- regmatches(nm, regexpr("_log([0-9]+)_([^_]+)_([^_]+)$", nm, perl = TRUE))
      parts <- strsplit(sub("^_log", "", m), "_")[[1]]
      paste0("processed (log", parts[1], ", imputed: ", parts[2], ", normalized: ", parts[3], ") — ready for limma DE")
    } else if (grepl("_log[0-9]+_[^_]+$", nm_lower)) {
      # Two-part suffix: _log{N}_{impute} — imputed, not yet normalized
      m <- regmatches(nm, regexpr("_log([0-9]+)_([^_]+)$", nm, perl = TRUE))
      parts <- strsplit(sub("^_log", "", m), "_")[[1]]
      paste0("log", parts[1], "-transformed, imputed (", parts[2], ") — not yet normalized")
    } else if (grepl("_log[0-9]+$", nm_lower)) {
      "log-transformed only — intermediate step before imputation"
    } else if (nm_lower == "precursors") {
      "raw precursor-level intensities — lowest level of summarization"
    } else if (nm_lower == "peptides") {
      "peptide-level intensities — aggregated from precursors"
    } else if (nm_lower == "protein_groups") {
      "protein group intensities — use for DE analysis and PCA"
    } else if (nm_lower %in% taxonomic_ranks) {
      paste0("taxonomic rank (", nm, ") — use for relative abundance and community composition plots")
    } else if (nm_lower %in% functional_types) {
      paste0("functional grouping (", nm, ") — use for pathway/functional analysis")
    } else {
      "custom assay"
    }
  }, character(1))

  paste(paste0("  - `", names(desc), "`: ", desc), collapse = "\n")
}

# ── Helper: colData column levels/range summary ────────────────────────────────
coldata_levels_summary <- function(cd) {
  cols <- names(cd)
  if (length(cols) == 0) return("")

  lines <- vapply(cols, function(col) {
    vals <- cd[[col]]
    if (is.numeric(vals)) {
      paste0("  - ", col, " (numeric): range ", round(min(vals, na.rm = TRUE), 2),
             " – ", round(max(vals, na.rm = TRUE), 2))
    } else {
      lvls <- unique(as.character(vals))
      lvls <- sort(lvls)
      if (length(lvls) > 8) {
        paste0("  - ", col, " (categorical, ", length(lvls), " levels): ",
               paste(head(lvls, 8), collapse = ", "), ", ...")
      } else {
        paste0("  - ", col, " (categorical): ", paste(lvls, collapse = ", "))
      }
    }
  }, character(1))

  paste0(
    "Sample metadata column details:\n",
    paste(lines, collapse = "\n")
  )
}

# ── Helper: backtick-escape dotted term names in a contrast string ─────────────
# limma::makeContrasts parses contrast strings as R expressions.
# Terms produced by make.names() that contain dots must be backtick-quoted
# to be treated as identifiers (e.g. `groupA.B` - `groupC`).
escape_contrast_terms <- function(contrast, terms) {
  needs_escape <- terms[grepl("\\.", terms) | grepl("^[0-9]", terms)]
  for (term in needs_escape) {
    # Only escape if not already backtick-wrapped
    if (!grepl(paste0("`", term, "`"), contrast, fixed = TRUE)) {
      contrast <- gsub(term, paste0("`", term, "`"), contrast, fixed = TRUE)
    }
  }
  contrast
}

# ── Helper: contrast terms from find_possible_contrast_terms ──────────────────
contrast_terms_summary <- function(qf, assay_name) {
  cd <- tryCatch(
    SummarizedExperiment::colData(qf[[assay_name]]),
    error = function(e) NULL
  )
  if (is.null(cd)) return("")

  cols <- names(cd)
  usable_cols <- Filter(
    function(col) {
      vals <- cd[[col]]
      n <- length(unique(vals))
      is.numeric(vals) || (n > 1 && n <= 10)
    },
    cols
  )

  if (length(usable_cols) == 0) return("")

  lines <- lapply(usable_cols, function(col) {
    vals <- cd[[col]]
    continuous <- is.numeric(vals)
    terms <- tryCatch(
      conduitR::find_possible_contrast_terms(
        qf, assay_name, as.formula(paste0("~", col))
      ),
      error = function(e) NULL
    )
    if (is.null(terms)) return(NULL)
    label <- if (continuous) " (continuous)" else ""
    paste0("  ~", col, label, " -> ", paste(terms, collapse = ", "))
  })
  lines <- Filter(Negate(is.null), lines)
  if (length(lines) == 0) return("")
  paste0(
    "Exact limma design matrix terms by variable (use these to build contrasts):\n",
    paste(lines, collapse = "\n"), "\n",
    "Note: for continuous variables the contrast is just the term name itself ",
    "(e.g., contrast = 'age' tests the effect of a 1-unit increase in age; ",
    "no subtraction needed)."
  )
}

# ── System prompt ──────────────────────────────────────────────────────────────
build_ai_system_prompt <- function(conduit, qf, assay, experiment_context = NULL) {
  qf_slot <- tryCatch(slot(conduit, "QFeatures"), error = function(e) NULL)

  if (is.null(qf_slot)) {
    return(paste0(
      "You are a proteomics data analysis assistant embedded in conduit-summit. ",
      "The user has loaded a conduit proteomics dataset."
    ))
  }

  assay_names  <- names(qf_slot)
  first_assay  <- qf_slot[[assay_names[1]]]
  n_samples    <- ncol(first_assay)
  n_features   <- nrow(first_assay)
  cd           <- SummarizedExperiment::colData(qf_slot)
  coldata_cols <- names(cd)

  taxonomy    <- tryCatch(slot(conduit, "taxonomy"),    error = function(e) NULL)
  annotations <- tryCatch(slot(conduit, "annotations"), error = function(e) NULL)
  n_taxa      <- if (!is.null(taxonomy) && nrow(taxonomy) > 0) nrow(taxonomy) else 0
  annot_types <- if (!is.null(annotations) && nrow(annotations) > 0) {
    unique(annotations$annotation_type)
  } else character(0)

  # colData levels
  coldata_detail <- tryCatch(coldata_levels_summary(cd), error = function(e) "")

  # rowData columns for the active assay (and first assay if different)
  rowdata_info <- tryCatch({
    active_rd <- names(SummarizedExperiment::rowData(qf[[assay]]))[
      names(SummarizedExperiment::rowData(qf[[assay]])) != ""
    ]
    lines <- paste0("  - `", assay, "` rowData columns: ",
                    paste(active_rd, collapse = ", "))
    if (assay_names[1] != assay) {
      first_rd <- names(SummarizedExperiment::rowData(qf[[assay_names[1]]]))
      first_rd <- first_rd[first_rd != ""]
      lines <- c(
        paste0("  - `", assay_names[1], "` rowData columns: ", paste(first_rd, collapse = ", ")),
        lines
      )
    }
    paste0("Feature metadata (rowData) columns:\n", paste(lines, collapse = "\n"))
  }, error = function(e) "")

  # Annotation completeness
  annot_coverage <- if (!is.null(annotations) && nrow(annotations) > 0 &&
                        "protein_id" %in% names(annotations) &&
                        "annotation_type" %in% names(annotations)) {
    tryCatch({
      counts <- tapply(
        annotations$protein_id,
        annotations$annotation_type,
        function(x) length(unique(x))
      )
      paste0(
        "Annotation coverage (unique proteins annotated):\n",
        paste0("  - ", names(counts), ": ", counts, " proteins", collapse = "\n")
      )
    }, error = function(e) "")
  } else ""

  # Contrast terms
  contrast_info <- tryCatch(
    contrast_terms_summary(qf, assay),
    error = function(e) ""
  )

  paste0(
    "You are an expert proteomics data analyst embedded in conduit-summit, a Shiny app ",
    "for metaproteomics analysis. Your role is to help users explore, analyze, and interpret ",
    "their data. Respond in markdown. Separate your interpretation from tool outputs.\n\n",

    if (!is.null(experiment_context) && nzchar(trimws(experiment_context)))
      paste0("## Experiment Context\n", trimws(experiment_context), "\n\n")
    else "",

    "## Loaded Dataset\n",
    "- Assays available: ", paste(assay_names, collapse = ", "), "\n",
    "- Active assay: ", assay, "\n",
    "- Samples: ", n_samples, "\n",
    "- Features (", assay, "): ", n_features, "\n",
    "- Sample metadata columns: ", paste(coldata_cols, collapse = ", "), "\n",
    if (n_taxa > 0) paste0("- Detected taxa: ", n_taxa, " organisms\n") else "",
    if (length(annot_types) > 0) paste0("- Annotation types: ", paste(annot_types, collapse = ", "), "\n") else "",
    "\n",
    if (nzchar(coldata_detail)) paste0(coldata_detail, "\n\n") else "",
    if (nzchar(rowdata_info)) paste0(rowdata_info, "\n\n") else "",
    if (nzchar(annot_coverage)) paste0(annot_coverage, "\n\n") else "",
    if (nzchar(contrast_info)) paste0(contrast_info, "\n\n") else "",

    "### Assay Selection\n",
    "The user's active assay is `", assay, "` but you should select whichever assay best answers ",
    "the user's question. Available assays and their appropriate use cases:\n\n",
    get_assay_descriptions(assay_names),
    "\n\n",
    "When switching assays, tell the user which assay you selected and why before proceeding.\n\n",

    "## conduit Object Structure\n",
    "A conduit object is an S4 class containing all data needed for metaproteomics analysis.\n\n",

    "**`@metrics`** — QC tibbles:\n",
    "- `diann_stats`: per-sample summaries (proteins detected, MS1 signal, etc). ",
    "Use when the user asks about data quality or run success.\n",
    "- `protein_coverage_[rank]`: proteins in database vs. detected, with percent coverage. ",
    "Use to assess whether the search database was appropriate for the sample.\n\n",

    "**`@database`** — tibble of all proteins in the search database with their source organism. ",
    "Represents what was searchable, not what was detected.\n\n",

    "**`@taxonomy`** — full taxonomic lineage of organisms in the database (domain through species). ",
    "Use for taxon-level lookups and to understand database composition. ",
    "Distinct from `@database`: taxonomy describes the organismal tree; database describes the proteins.\n\n",

    "**`@annotations`** — functional annotations per protein: term type, term ID (e.g. GO:0016020), ",
    "and description (e.g. 'membrane'). Self-contained — no external annotation tools needed.\n\n",

    "**`@provenance`** — workflow metadata from conduit-ascent (processing parameters, versions, etc). ",
    "May be NULL for older datasets. Surface this when the user asks how their data was processed.\n\n",

    "**`@QFeatures`** — all quantitative data. `colData` holds sample metadata; `rowData` holds ",
    "feature metadata. Assays represent data summarized at increasing levels of abstraction:\n\n",
    "  precursors → peptides → protein_groups → [taxonomic rank: domain through species]\n",
    "                                          → [functional grouping: go, uniprot_pfam, etc.]\n\n",
    "Each level is created by summing values from the level below. ",
    "Taxonomic and functional assays have high missingness by design — not all organisms or ",
    "functions are detected in every sample.\n\n",

    "## Normalization & Imputation\n",
    "Choose based on the user's analytical goal and the active assay. Always state the method ",
    "used and its key assumption in your response.\n\n",

    "### Missingness in DIA Metaproteomics\n",
    "This dataset was generated by DIA and processed by DIA-NN. Missingness is predominantly ",
    "**MNAR** for three reasons: DIA interrogates every precursor each cycle so a missing value ",
    "means signal was genuinely below detection; missing values at taxonomic/functional assay ",
    "levels often reflect true biological absence; and DIA-NN FDR filtering removes borderline ",
    "detections entirely. A small MCAR component exists near the detection limit but is minor ",
    "relative to total missingness.\n\n",

    "**Default to MNAR-appropriate imputation (`MinDet` or `MinProb`). If a user requests a MCAR ",
    "method without justification, flag that DIA metaproteomics missingness is predominantly MNAR ",
    "and confirm they want to proceed.**\n\n",

    "### Defaults\n",
    "- **Imputation:** `MinDet` (deterministic, conservative) or `MinProb` (adds Gaussian noise; ",
    "preferable for limma). Use `QRILC` if the user wants to model the censoring mechanism explicitly.\n",
    "- **Normalization:** None — DIA-NN's internal normalization is retained. For taxonomic assays, ",
    "use `sum` for relative abundance. For protein/peptide assays, use `vsn` for differential testing.\n\n",

    "If the user requests something outside these defaults, apply judgment and note the tradeoff. ",
    "If the goal is unclear, apply `MinDet` with no additional normalization and offer to adjust.\n\n",

    "## Tools\n",
    "When a tool returns empty or unexpected results, report this to the user before proceeding.\n\n",

    "**Orientation**\n",
    "- `get_data_summary()` — call first whenever you need to orient yourself to the dataset structure.\n\n",

    "**Sample & feature metadata**\n",
    "- `get_col_data()` — retrieve sample-level metadata (groups, covariates, batch). ",
    "Use before designing a model formula.\n",
    "- `get_row_data(assay_name)` — retrieve feature-level metadata (protein IDs, gene names, annotations). ",
    "Use to look up specific features without pulling abundance values.\n",
    "- `get_taxonomy()` — retrieve organism-level taxonomy. Use when the user asks about which ",
    "taxa are present or to set up abundance plots.\n",
    "- `get_metrics(metric_name)` — call without arguments to list available metrics tables, then pass ",
    "a name (e.g. 'diann_stats') to display the full table. Use when the user asks about run quality, ",
    "detection rates, or protein coverage.\n",
    "- `get_annotations()` — retrieve functional annotations (GO, KEGG, etc). ",
    "Use before pathway-level interpretation.\n\n",

    "**Contrast validation**\n",
    "- `get_valid_contrasts(assay_name, formula)` — returns all valid, ready-to-use contrast strings for a given formula. ",
    "ALWAYS call this first. Copy one of the returned contrast strings exactly into `run_limma` or `plot_volcano_tool` — ",
    "never construct contrast strings manually.\n\n",

    "**Filtering**\n",
    "- `filter_data(sample_column, sample_values, feature_column, feature_values)` — filter samples or features. ",
    "Filters accumulate: call once to filter by timepoint, again to filter by group, etc. ",
    "Pass `clear=TRUE` to reset. Use when the user wants to focus on a subset (e.g. endpoint samples only). ",
    "Values: comma-separated for categorical ('endpoint,baseline'), 'min:max' for numeric ('0:10').\n\n",

    "**Preprocessing**\n",
    "- `prepare_assay(assay_name, impute_method, norm_method)` — applies log2 transform, imputation, and ",
    "normalization to create a processed assay ready for limma. Call this if no processed assay (e.g. `*_log2_MinDet_none`) exists ",
    "for the target assay. The new assays persist for the rest of the session.\n\n",

    "**Abundance values**\n",
    "- `tidy_data(assay_name, n_rows)` — retrieve actual abundance values in long format. ",
    "Use when the user asks about expression levels or trends for particular proteins or organisms.\n\n",

    "**Analyses**\n",
    "- `run_limma(assay_name, formula, contrast)` — differential expression. ",
    "Requires a processed assay. Workflow: call `get_valid_contrasts(assay_name, formula)` first, ",
    "then copy one of the returned contrast strings exactly as the `contrast` argument.\n",
    "- `run_ora(assay_name, formula, contrast, annotation_type, direction)` — over-representation analysis. ",
    "Tests which annotation terms (e.g. GO, KEGG, species) are over-represented among significant DE features. ",
    "Use get_row_data() to confirm which annotation columns are available.\n",
    "- `run_gsea(assay_name, formula, contrast, annotation_type)` — gene set enrichment analysis. ",
    "Ranks all features by logFC and tests for directional enrichment of annotation terms. ",
    "Preferable to ORA when you want to use the full ranked list rather than a significance cutoff.\n",
    "- `run_classification(assay_name, outcome, model_type)` — ML-based prediction. Use when the user asks about ",
    "predictive modeling or feature importance.\n\n",

    "**QC Plots**\n",
    "- `plot_taxa_tree(layout, type)` — heat tree of taxonomic detection coverage (same as Database tab). ",
    "Node size/color = % proteins detected per taxon. Use to explore which organisms are well-covered.\n",
    "- `plot_diann_metric(column_name)` — line/point plot of any DIA-NN sample-level metric (MS1.Signal, RT.Mean, etc.) across all files. ",
    "Call without arguments to list available columns. Use to detect failed injections or run-order effects.\n",
    "- `plot_features_per_sample(assay_name)` — bar chart of features detected per sample. Flag outlier low-count samples.\n",
    "- `plot_missing_values(assay_name, col_color_variables, row_color_variables)` — missing value heatmap. ",
    "Use a log-transformed (pre-imputation) assay (e.g. 'protein_groups_log2').\n",
    "- `plot_sample_correlation(assay_name, col_color_variables)` — sample-to-sample correlation heatmap. ",
    "Use to spot outlier samples and confirm biological groupings.\n",
    "- `plot_intensity_distribution(assay_name)` — histogram of per-feature detection frequency across samples.\n\n",

    "**Analytical Plots**\n",
    "- `run_pca(assay_name, color_by, shape_by)` — PCA of an assay. Use to explore sample clustering and batch effects.\n",
    "- `plot_volcano_tool(assay_name, formula, contrast, pval_threshold)` — volcano plot. ",
    "Same workflow as run_limma: call get_valid_contrasts first, copy a contrast string, pass it here.\n",
    "- `plot_selected_features(assay_name, feature_ids, x_axis, color_by, facet_formula)` — plot abundance of specific features across samples. ",
    "Use after run_limma to visualize individual significant hits.\n",
    "- `plot_ora_tool(assay_name, formula, contrast, annotation_type, direction, plot_type)` — barplot or dotplot of ORA enriched terms.\n",
    "- `plot_gsea_tool(assay_name, formula, contrast, annotation_type, plot_type)` — dotplot or ridgeplot of GSEA enriched terms. Use ridgeplot to show directionality.\n",
    "- `plot_heatmap_tool(assay_name, col_color_variables)` — heatmap of expression values across samples.\n",
    "- `plot_relative_abundance_tool(assay_name, facet_by)` — taxon-level relative abundance. ",
    "Use when the user asks about community composition.\n\n",
    "For all plots: render first, then interpret below. Comment on scientifically noteworthy patterns — ",
    "don't just describe axes.\n\n",

    "## Guidelines\n",
    "- Use proteomics and metaproteomics terminology precisely.\n",
    "- Stay scoped to the loaded dataset. Politely redirect off-topic questions.\n",
    "- Be concise. Prioritize actionable insight over exhaustive explanation.\n"
  )
}

# ── Tool registration ──────────────────────────────────────────────────────────
register_conduit_tools <- function(chat, conduit_obj, final_qf, qf_session,
                                    processed_assay, final_colData_names,
                                    final_rowData_names, plot_store, output,
                                    session, ns, chat_id) {

  # Accessor: returns tool-modified QF if available, else falls back to reactive
  get_qf <- function() {
    override <- qf_session()
    if (!is.null(override)) override else final_qf()
  }

  # Helper: inject a ggplot into the chat
  inject_plot <- function(p, height = "400px") {
    plot_id <- paste0("ai_plot_", sample.int(1e6, 1))
    local({
      pid <- plot_id
      output[[pid]] <- shiny::renderPlot({ plot_store[[pid]] })
    })
    plot_store[[plot_id]] <- p
    shinychat::chat_append(
      chat_id,
      shiny::tagList(shiny::plotOutput(ns(plot_id), height = height)),
      session = session
    )
    invisible(NULL)
  }

  # Helper: inject a data frame as a DT table into the chat
  inject_table <- function(df, n_rows = 50) {
    tbl_id <- paste0("ai_table_", sample.int(1e6, 1))
    local({
      tid <- tbl_id
      output[[tid]] <- DT::renderDT({
        DT::datatable(
          head(plot_store[[tid]], n_rows),
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE
        )
      })
    })
    plot_store[[tbl_id]] <- df
    shinychat::chat_append(
      chat_id,
      shiny::tagList(DT::DTOutput(ns(tbl_id))),
      session = session
    )
    invisible(NULL)
  }

  # ── Tool: get_data_summary ────────────────────────────────────────────────
  get_data_summary_fn <- function() {
    conduit  <- conduit_obj()
    qf_slot  <- tryCatch(slot(conduit, "QFeatures"), error = function(e) NULL)
    if (is.null(qf_slot)) return("No QFeatures object found in conduit.")

    assay_names  <- names(qf_slot)
    first_assay  <- qf_slot[[assay_names[1]]]
    n_samples    <- ncol(first_assay)
    sample_names <- colnames(first_assay)
    n_features   <- nrow(first_assay)
    coldata_cols <- names(SummarizedExperiment::colData(qf_slot))

    taxonomy    <- tryCatch(slot(conduit, "taxonomy"),    error = function(e) NULL)
    annotations <- tryCatch(slot(conduit, "annotations"), error = function(e) NULL)
    metrics     <- tryCatch(slot(conduit, "metrics"),     error = function(e) NULL)
    n_taxa      <- if (!is.null(taxonomy)    && nrow(taxonomy) > 0) nrow(taxonomy) else 0
    annot_types <- if (!is.null(annotations) && nrow(annotations) > 0) unique(annotations$annotation_type) else character(0)
    metric_keys <- if (!is.null(metrics)     && length(metrics) > 0) names(metrics) else character(0)

    contrast_info <- tryCatch(
      contrast_terms_summary(qf_slot, processed_assay()),
      error = function(e) ""
    )

    processing_info <- tryCatch(assay_processing_info(get_qf()), error = function(e) "")

    paste0(
      "Dataset summary:\n",
      "- Assays: ", paste(assay_names, collapse = ", "), "\n",
      "- Current processed assay: ", processed_assay(), "\n",
      "- Samples (n=", n_samples, "): ",
      paste(head(sample_names, 8), collapse = ", "),
      if (n_samples > 8) paste0(" ... and ", n_samples - 8, " more") else "", "\n",
      "- Features in '", assay_names[1], "': ", n_features, "\n",
      "- Sample metadata (colData) columns: ", paste(coldata_cols, collapse = ", "), "\n",
      if (n_taxa > 0) paste0("- Detected taxa: ", n_taxa, " organisms\n") else "- Detected taxa: none\n",
      if (length(annot_types) > 0) paste0("- Annotation types: ", paste(annot_types, collapse = ", "), "\n") else "",
      if (length(metric_keys) > 0) paste0("- Metrics available: ", paste(metric_keys, collapse = ", "), "\n") else "",
      if (nzchar(processing_info)) paste0(processing_info, "\n") else "",
      if (nzchar(contrast_info)) paste0(contrast_info, "\n") else ""
    )
  }

  chat$register_tool(ellmer::tool(
    get_data_summary_fn,
    description = "Get a comprehensive summary of the loaded conduit proteomics dataset: assay names, sample count, feature count, sample metadata columns, taxonomy, and annotations. Call this first to orient yourself about the data.",
    arguments   = list()
  ))

  # ── Tool: get_col_data ────────────────────────────────────────────────────
  get_col_data_fn <- function() {
    qf <- get_qf()
    cd <- as.data.frame(SummarizedExperiment::colData(qf))
    inject_table(cd, n_rows = nrow(cd))
    paste0(
      "Sample metadata (colData) rendered above.\n",
      "Columns: ", paste(names(cd), collapse = ", "), "\n",
      "Samples: ", nrow(cd)
    )
  }

  chat$register_tool(ellmer::tool(
    get_col_data_fn,
    description = "Retrieve and display the sample metadata table (colData) showing all sample-level variables such as group, batch, treatment, etc. Use this before running limma to confirm group variable names and factor levels.",
    arguments   = list()
  ))

  # ── Tool: get_row_data ────────────────────────────────────────────────────
  get_row_data_fn <- function(assay_name) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    rd <- as.data.frame(SummarizedExperiment::rowData(qf[[assay_name]]))
    inject_table(rd, n_rows = min(100, nrow(rd)))
    paste0(
      "Feature metadata (rowData) for assay '", assay_name, "' rendered above.\n",
      "Columns: ", paste(names(rd), collapse = ", "), "\n",
      "Features: ", nrow(rd)
    )
  }

  chat$register_tool(ellmer::tool(
    get_row_data_fn,
    description = "Retrieve and display feature metadata (rowData) for a specific assay, showing protein/peptide identifiers and associated annotations.",
    arguments   = list(
      assay_name = ellmer::type_string("The name of the assay (e.g., 'protein_groups')")
    )
  ))

  # ── Tool: get_taxonomy ────────────────────────────────────────────────────
  get_taxonomy_fn <- function() {
    conduit <- conduit_obj()
    tax     <- tryCatch(slot(conduit, "taxonomy"), error = function(e) NULL)
    if (is.null(tax) || nrow(tax) == 0) return("No taxonomy data found in this conduit object.")
    inject_table(tax, n_rows = nrow(tax))
    paste0(
      "Taxonomy table rendered above.\n",
      "Detected organisms: ", nrow(tax), "\n",
      "Columns: ", paste(names(tax), collapse = ", ")
    )
  }

  chat$register_tool(ellmer::tool(
    get_taxonomy_fn,
    description = "Retrieve and display the detected taxonomy table showing all organisms identified in the experiment, including NCBI taxonomy IDs and full taxonomic lineage (domain through species).",
    arguments   = list()
  ))

  # ── Tool: get_metrics ─────────────────────────────────────────────────────
  get_metrics_fn <- function(metric_name = NULL) {
    conduit <- conduit_obj()
    m       <- tryCatch(slot(conduit, "metrics"), error = function(e) NULL)
    if (is.null(m) || length(m) == 0) return("No metrics found in this conduit object.")

    if (!is.null(metric_name) && nzchar(metric_name)) {
      if (!metric_name %in% names(m)) {
        return(paste0("Metric '", metric_name, "' not found. Available: ", paste(names(m), collapse = ", ")))
      }
      tbl <- as.data.frame(m[[metric_name]])
      inject_table(tbl, n_rows = nrow(tbl))
      return(paste0("Metrics table '", metric_name, "' rendered above.\n",
                    "Rows: ", nrow(tbl), ", Columns: ", paste(names(tbl), collapse = ", ")))
    }

    lines <- lapply(names(m), function(nm) {
      tbl <- m[[nm]]
      paste0("  - ", nm, ": ", nrow(tbl), " rows, columns: ", paste(names(tbl), collapse = ", "))
    })
    paste0("Metrics slot contents (pass metric_name to view a table):\n", paste(lines, collapse = "\n"))
  }

  chat$register_tool(ellmer::tool(
    get_metrics_fn,
    description = "Get a summary of the metrics slot, or display a specific metrics table. Pass metric_name (e.g. 'diann_stats') to view the full table. Call without arguments first to see what metrics are available.",
    arguments   = list(
      metric_name = ellmer::type_string(
        "Name of the metrics table to display (e.g. 'diann_stats'). Omit to list all available tables.",
        required = FALSE
      )
    )
  ))

  # ── Tool: get_annotations ─────────────────────────────────────────────────
  get_annotations_fn <- function(annotation_type = NULL) {
    conduit <- conduit_obj()
    ann     <- tryCatch(slot(conduit, "annotations"), error = function(e) NULL)
    if (is.null(ann) || nrow(ann) == 0) return("No annotation data found in this conduit object.")

    all_types <- unique(ann$annotation_type)
    if (!is.null(annotation_type) && nzchar(annotation_type)) {
      ann <- ann[ann$annotation_type == annotation_type, ]
    }

    inject_table(ann, n_rows = 50)
    paste0(
      "Annotations rendered above",
      if (!is.null(annotation_type) && nzchar(annotation_type)) paste0(" (filtered to '", annotation_type, "')") else "",
      ".\n",
      "Rows shown: ", min(50, nrow(ann)), " of ", nrow(ann), "\n",
      "Available annotation types: ", paste(all_types, collapse = ", ")
    )
  }

  chat$register_tool(ellmer::tool(
    get_annotations_fn,
    description = "Retrieve functional annotations for proteins in the dataset (GO terms, KEGG pathways, Pfam domains, EggNOG, etc.).",
    arguments   = list(
      annotation_type = ellmer::type_string(
        "Filter by type: 'go', 'kegg', 'pfam', 'eggnog', or empty string for all",
        required = FALSE
      )
    )
  ))

  # ── Tool: tidy_data ───────────────────────────────────────────────────────
  tidy_data_fn <- function(assay_name, n_rows = 50L) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    tidy <- tryCatch(
      conduitR::tidy_conduit(qf, assay_name),
      error = function(e) NULL
    )
    if (is.null(tidy)) return(paste0("Could not convert assay '", assay_name, "' to tidy format."))

    inject_table(tidy, n_rows = min(n_rows, nrow(tidy)))
    paste0(
      "Tidy long-format data for '", assay_name, "' rendered above.\n",
      "Total rows: ", nrow(tidy), " (showing first ", min(n_rows, nrow(tidy)), ")\n",
      "Columns: ", paste(names(tidy), collapse = ", ")
    )
  }

  chat$register_tool(ellmer::tool(
    tidy_data_fn,
    description = "Convert an assay to a tidy long-format tibble joining abundance values with sample metadata and feature annotations. Useful for inspecting raw data values.",
    arguments   = list(
      assay_name = ellmer::type_string("The assay name to convert (use get_data_summary to see available assays)"),
      n_rows     = ellmer::type_integer("Number of rows to display, default 50", required = FALSE)
    )
  ))

  # ── Tool: filter_data ────────────────────────────────────────────────────
  filter_data_fn <- function(sample_column = NULL, sample_values = NULL,
                             feature_column = NULL, feature_values = NULL,
                             clear = FALSE) {
    if (isTRUE(clear)) {
      qf_session(NULL)
      return(paste0(
        "Filters cleared. Dataset reset to full unprocessed state.\n",
        "Note: any assays prepared with prepare_assay() have also been cleared and will need to be re-run."
      ))
    }

    qf <- get_qf()

    # ── Sample filter ────────────────────────────────────────────────────────
    if (!is.null(sample_column) && nzchar(sample_column)) {
      cd <- SummarizedExperiment::colData(qf)
      if (!sample_column %in% names(cd)) {
        return(paste0("Sample metadata column '", sample_column, "' not found. ",
                      "Available: ", paste(names(cd), collapse = ", ")))
      }
      col_vals <- cd[[sample_column]]
      n_before  <- ncol(qf)

      if (is.numeric(col_vals) && !is.null(sample_values) && grepl(":", sample_values, fixed = TRUE)) {
        # Numeric range: "min:max"
        parts <- as.numeric(trimws(strsplit(sample_values, ":", fixed = TRUE)[[1]]))
        keep  <- is.na(col_vals) | (col_vals >= parts[1] & col_vals <= parts[2])
      } else if (!is.null(sample_values) && nzchar(sample_values)) {
        # Categorical: comma-separated values
        vals <- trimws(strsplit(sample_values, ",")[[1]])
        keep <- is.na(col_vals) | as.character(col_vals) %in% vals
      } else {
        keep <- rep(TRUE, ncol(qf))
      }

      if (!any(keep)) {
        return(paste0("No samples remain after filtering '", sample_column,
                      "' to '", sample_values, "'. Filter not applied."))
      }
      qf <- qf[, keep]
    }

    # ── Feature filter ───────────────────────────────────────────────────────
    feature_report <- character(0)
    if (!is.null(feature_column) && nzchar(feature_column)) {
      for (assay_nm in names(qf)) {
        rd <- SummarizedExperiment::rowData(qf[[assay_nm]])
        if (!feature_column %in% names(rd)) next
        col_vals  <- rd[[feature_column]]
        n_before  <- nrow(qf[[assay_nm]])

        if (is.numeric(col_vals) && !is.null(feature_values) && grepl(":", feature_values, fixed = TRUE)) {
          parts <- as.numeric(trimws(strsplit(feature_values, ":", fixed = TRUE)[[1]]))
          keep  <- is.na(col_vals) | (col_vals >= parts[1] & col_vals <= parts[2])
        } else if (!is.null(feature_values) && nzchar(feature_values)) {
          vals <- trimws(strsplit(feature_values, ",")[[1]])
          keep <- is.na(col_vals) | as.character(col_vals) %in% vals
        } else {
          next
        }

        if (any(keep)) {
          qf[[assay_nm]] <- qf[[assay_nm]][keep, ]
          feature_report <- c(feature_report,
            paste0("  - `", assay_nm, "`: ", n_before, " → ", sum(keep), " features"))
        }
      }
    }

    qf_session(qf)

    n_samples <- ncol(qf)
    lines <- c()
    if (!is.null(sample_column) && nzchar(sample_column)) {
      lines <- c(lines, paste0("Samples retained: ", n_samples,
                               " (filtered '", sample_column, "' = '", sample_values, "')"))
    }
    if (length(feature_report) > 0) {
      lines <- c(lines, paste0("Feature counts after filter ('", feature_column, "' = '", feature_values, "'):"),
                 feature_report)
    }
    paste0(
      "Filter applied. Session dataset updated.\n",
      paste(lines, collapse = "\n"), "\n\n",
      "Call filter_data(clear=TRUE) to reset. All subsequent tools will use the filtered dataset."
    )
  }

  chat$register_tool(ellmer::tool(
    filter_data_fn,
    description = paste0(
      "Filter the dataset by sample metadata (colData) or feature metadata (rowData). ",
      "Filters accumulate — call multiple times to add conditions. ",
      "Use clear=TRUE to reset to the full dataset. ",
      "Use get_col_data() or get_row_data() first to see available columns and values."
    ),
    arguments = list(
      sample_column  = ellmer::type_string(
        "colData column name to filter samples by (e.g. 'timepoint')",
        required = FALSE
      ),
      sample_values  = ellmer::type_string(
        "Values to keep: comma-separated for categorical (e.g. 'endpoint') or 'min:max' for numeric (e.g. '0:10')",
        required = FALSE
      ),
      feature_column = ellmer::type_string(
        "rowData column name to filter features by (e.g. 'species')",
        required = FALSE
      ),
      feature_values = ellmer::type_string(
        "Values to keep: comma-separated for categorical or 'min:max' for numeric",
        required = FALSE
      ),
      clear = ellmer::type_boolean(
        "Set TRUE to clear all AI-applied filters and reset to the full dataset",
        required = FALSE
      )
    )
  ))

  # ── Tool: prepare_assay ───────────────────────────────────────────────────
  prepare_assay_fn <- function(assay_name, impute_method = "MinDet", norm_method = "none") {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }

    log_name    <- paste0(assay_name, "_log2")
    impute_name <- paste0(log_name, "_", impute_method)
    norm_name   <- paste0(impute_name, "_", norm_method)

    if (norm_name %in% available) {
      return(paste0(
        "Processed assay '", norm_name, "' already exists. ",
        "Use it directly for limma or PCA."
      ))
    }

    qf_new <- tryCatch(
      conduitR::add_log_imputed_norm_assay(
        qf,
        assay          = assay_name,
        base           = 2,
        impute_method  = impute_method,
        norm_method    = norm_method
      ),
      error = function(e) list(error = conditionMessage(e))
    )

    if (is.list(qf_new) && !is.null(qf_new$error)) {
      return(paste0("Error preparing assay: ", qf_new$error))
    }

    qf_session(qf_new)

    paste0(
      "Preprocessing complete for '", assay_name, "'.\n",
      "New assays created:\n",
      "  - `", log_name, "`: log2-transformed\n",
      "  - `", impute_name, "`: log2 + imputed (method: ", impute_method, ")\n",
      "  - `", norm_name, "`: log2 + imputed + normalized (method: ", norm_method, ") — ready for limma\n\n",
      "Use `", norm_name, "` as the assay_name in run_limma, plot_volcano_tool, run_pca, or plot_heatmap_tool."
    )
  }

  chat$register_tool(ellmer::tool(
    prepare_assay_fn,
    description = paste0(
      "Apply log2 transformation, imputation, and optional normalization to an assay, creating three new derived assays. ",
      "Call this before run_limma or plot_volcano_tool if no processed assay (e.g. 'protein_groups_log2_MinDet_none') exists. ",
      "The processed assays persist for the rest of the session."
    ),
    arguments = list(
      assay_name    = ellmer::type_string("The base assay to process (e.g., 'protein_groups')"),
      impute_method = ellmer::type_string(
        "Imputation method: 'MinDet' (default, MNAR-appropriate), 'MinProb' (MNAR with noise), 'QRILC', or 'min'",
        required = FALSE
      ),
      norm_method   = ellmer::type_string(
        "Normalization method: 'none' (default, retains DIA-NN normalization), 'vsn', 'quantiles', or 'sum'",
        required = FALSE
      )
    )
  ))

  # ── Tool: run_pca ─────────────────────────────────────────────────────────
  run_pca_fn <- function(assay_name, color_by = NULL, shape_by = NULL) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    p <- tryCatch(
      conduitR::plot_biplot(
        qf,
        assay_name = assay_name,
        color      = color_by,
        shape      = shape_by
      ),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate PCA biplot for '", assay_name, "'. Ensure sufficient samples and no all-NA columns."))
    inject_plot(p)
    paste0(
      "PCA biplot for '", assay_name, "' rendered above",
      if (!is.null(color_by) && nzchar(color_by)) paste0(", colored by '", color_by, "'") else "",
      if (!is.null(shape_by) && nzchar(shape_by)) paste0(", shaped by '", shape_by, "'") else "",
      "."
    )
  }

  chat$register_tool(ellmer::tool(
    run_pca_fn,
    description = "Generate and display a PCA biplot for a given assay. Points represent samples; optionally color and shape them by sample metadata variables.",
    arguments   = list(
      assay_name = ellmer::type_string("The processed assay name to use for PCA"),
      color_by   = ellmer::type_string("colData column to color points by (optional)", required = FALSE),
      shape_by   = ellmer::type_string("colData column to shape points by (optional)", required = FALSE)
    )
  ))

  # ── Tool: get_valid_contrasts ─────────────────────────────────────────────
  get_valid_contrasts_fn <- function(assay_name, formula) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    terms <- tryCatch(
      conduitR::find_possible_contrast_terms(qf, assay_name, as.formula(formula)),
      error = function(e) NULL
    )
    if (is.null(terms)) {
      return(paste0("Could not compute contrast terms for formula '", formula, "'. ",
                    "Check that the formula variables exist in sample metadata."))
    }
    non_intercept <- terms[terms != "(Intercept)" & terms != "X.Intercept."]

    # Backtick-quote terms that need it (dots or leading digits from make.names())
    quote_term <- function(t) if (grepl("\\.", t) || grepl("^[0-9]", t)) paste0("`", t, "`") else t
    quoted <- vapply(non_intercept, quote_term, character(1))

    # Generate all pairwise contrast strings, ready to copy into run_limma/plot_volcano
    contrast_lines <- if (length(quoted) >= 2) {
      pairs <- combn(seq_along(quoted), 2, simplify = FALSE)
      vapply(pairs, function(idx) {
        paste0("  '", quoted[idx[2]], " - ", quoted[idx[1]], "'")
      }, character(1))
    } else if (length(quoted) == 1) {
      paste0("  '", quoted[1], "'  (continuous variable — tests the slope)")
    } else {
      character(0)
    }

    terms_display <- paste(quoted, collapse = ", ")

    paste0(
      "Design matrix terms for formula '", formula, "' on assay '", assay_name, "':\n",
      terms_display, "\n\n",
      "Ready-to-use contrast strings (copy one exactly into run_limma or plot_volcano_tool):\n",
      if (length(contrast_lines) > 0) paste(contrast_lines, collapse = "\n") else "(none)",
      "\n\nNote: '(Intercept)' is the baseline — not used directly in contrasts."
    )
  }

  chat$register_tool(ellmer::tool(
    get_valid_contrasts_fn,
    description = paste0(
      "Get all valid contrast strings for a given formula and assay. ",
      "ALWAYS call this before run_limma or plot_volcano_tool. ",
      "Returns ready-to-use contrast strings — copy one exactly into run_limma or plot_volcano_tool. ",
      "Never guess contrast strings — R's make.names() sanitization produces unpredictable term names."
    ),
    arguments = list(
      assay_name = ellmer::type_string("The assay name to compute terms for"),
      formula    = ellmer::type_string("The R formula string (e.g., '~microbiome_treatment' or '~group + batch')")
    )
  ))

  # ── Tool: run_limma ───────────────────────────────────────────────────────
  run_limma_fn <- function(assay_name, formula, contrast) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    design_terms <- tryCatch(
      conduitR::find_possible_contrast_terms(qf, assay_name, as.formula(formula)),
      error = function(e) character(0)
    )
    contrast <- escape_contrast_terms(contrast, design_terms)
    result <- tryCatch(
      conduitR::perform_limma_analysis(
        qf, assay_name = assay_name,
        formula = as.formula(formula), contrast = contrast
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      non_int <- design_terms[design_terms != "(Intercept)" & design_terms != "X.Intercept."]
      return(paste0(
        "limma error: ", result$error, "\n",
        "Call get_valid_contrasts('", assay_name, "', '", formula, "') to see ready-to-use contrast strings."
      ))
    }

    top    <- result$top_table
    n_sig  <- sum(top$adj.P.Val < 0.05, na.rm = TRUE)
    n_up   <- sum(top$adj.P.Val < 0.05 & top$logFC > 0, na.rm = TRUE)
    n_down <- sum(top$adj.P.Val < 0.05 & top$logFC < 0, na.rm = TRUE)

    inject_table(top, n_rows = 20)
    paste0(
      "Differential expression results for '", assay_name, "' rendered above.\n",
      "Formula: ", formula, ", Contrast: ", contrast, "\n",
      "Significant (adj.p < 0.05): ", n_sig, " of ", nrow(top),
      " (", n_up, " up, ", n_down, " down)"
    )
  }

  chat$register_tool(ellmer::tool(
    run_limma_fn,
    description = paste0(
      "Run limma differential expression analysis on a processed assay. Returns a top table with logFC, t-statistics, p-values, and BH-adjusted p-values. ",
      "Workflow: (1) call get_valid_contrasts(assay_name, formula) to get ready-to-use contrast strings, ",
      "(2) copy one of those contrast strings exactly as the contrast argument here. ",
      "Use a processed assay (call prepare_assay first if none exists)."
    ),
    arguments   = list(
      assay_name = ellmer::type_string("The processed assay name (e.g., 'protein_groups_log2_MinDet_none')"),
      formula    = ellmer::type_string("R formula string (e.g., '~microbiome_treatment' or '~group + batch')"),
      contrast   = ellmer::type_string("A contrast string copied exactly from get_valid_contrasts() output")
    )
  ))

  # ── Tool: plot_volcano_tool ───────────────────────────────────────────────
  plot_volcano_fn <- function(assay_name, formula, contrast,
                              color_by = NULL, facet_by = NULL,
                              pval_threshold = 0.05) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    design_terms <- tryCatch(
      conduitR::find_possible_contrast_terms(qf, assay_name, as.formula(formula)),
      error = function(e) character(0)
    )
    contrast <- escape_contrast_terms(contrast, design_terms)
    result <- tryCatch(
      conduitR::perform_limma_analysis(
        qf, assay_name = assay_name,
        formula = as.formula(formula), contrast = contrast
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      return(paste0(
        "limma error: ", result$error, "\n",
        "Call get_valid_contrasts('", assay_name, "', '", formula,
        "') to see ready-to-use contrast strings."
      ))
    }

    facet_formula <- if (!is.null(facet_by) && nzchar(facet_by)) as.formula(paste0("~", facet_by)) else NULL

    p <- tryCatch(
      conduitR::plot_volcano(
        result$top_table,
        color_by       = if (!is.null(color_by) && nzchar(color_by)) color_by else NULL,
        facet_formula  = facet_formula,
        pval_threshold = pval_threshold
      ),
      error = function(e) NULL
    )
    if (is.null(p)) return("Could not generate volcano plot from limma results.")
    inject_plot(p, height = "450px")

    top   <- result$top_table
    n_sig <- sum(top$adj.P.Val < 0.05, na.rm = TRUE)
    paste0(
      "Volcano plot for contrast '", contrast, "' rendered above.\n",
      "Significant features (adj.p < ", pval_threshold, "): ", n_sig, " of ", nrow(top)
    )
  }

  chat$register_tool(ellmer::tool(
    plot_volcano_fn,
    description = paste0(
      "Run limma differential expression and display a volcano plot (logFC vs -log10 adjusted p-value). ",
      "Workflow: (1) call get_valid_contrasts(assay_name, formula) to get ready-to-use contrast strings, ",
      "(2) copy one of those contrast strings exactly as the contrast argument here."
    ),
    arguments   = list(
      assay_name     = ellmer::type_string("The processed assay name"),
      formula        = ellmer::type_string("R formula string (e.g., '~microbiome_treatment')"),
      contrast       = ellmer::type_string("A contrast string copied exactly from get_valid_contrasts() output"),
      color_by       = ellmer::type_string("rowData column to color points by (e.g. 'species' or 'gene_name')", required = FALSE),
      facet_by       = ellmer::type_string("rowData column to facet the plot by (e.g. 'species')", required = FALSE),
      pval_threshold = ellmer::type_number("Adjusted p-value significance threshold, default 0.05", required = FALSE)
    )
  ))

  # ── Tool: run_ora ─────────────────────────────────────────────────────────
  run_ora_fn <- function(assay_name, formula, contrast, annotation_type = "species",
                         direction = "up", adj_pval_threshold = 0.05, logFC_threshold = 1) {
    qf      <- get_qf()
    conduit <- conduit_obj()

    if (!assay_name %in% names(qf)) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(names(qf), collapse = ", ")))
    }
    if (!direction %in% c("up", "down")) {
      return("direction must be 'up' or 'down'.")
    }

    design_terms <- tryCatch(
      conduitR::find_possible_contrast_terms(qf, assay_name, as.formula(formula)),
      error = function(e) character(0)
    )
    contrast <- escape_contrast_terms(contrast, design_terms)

    result <- tryCatch(
      conduitR::perform_limma_analysis(
        qf, assay_name = assay_name,
        formula = as.formula(formula), contrast = contrast
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      return(paste0("limma error: ", result$error, "\n",
                    "Call get_valid_contrasts('", assay_name, "', '", formula, "') to get valid contrasts."))
    }

    top <- result$top_table
    if (!annotation_type %in% names(top)) {
      avail <- names(top)[!names(top) %in% c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val",
                                              "neg_log10.adj.P.Val", "B", "id")]
      return(paste0("Annotation column '", annotation_type, "' not found in limma results. ",
                    "Available annotation columns: ", paste(avail, collapse = ", ")))
    }

    lfc_thresh <- if (direction == "down") -abs(logFC_threshold) else abs(logFC_threshold)

    ora <- tryCatch(
      conduitR::perform_ora(
        limma_stats       = top,
        direction         = direction,
        conduit           = conduit,
        annotation_type   = annotation_type,
        adj_pval_threshold = adj_pval_threshold,
        logFC_threshold   = lfc_thresh
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (is.list(ora) && !is.null(ora$error)) {
      return(paste0("ORA error: ", ora$error))
    }

    ora_df <- tryCatch(as.data.frame(ora), error = function(e) NULL)
    if (is.null(ora_df) || nrow(ora_df) == 0) {
      return(paste0("ORA complete but no significant terms found (adj.p < 0.05) for ",
                    direction, "-regulated '", annotation_type, "' features."))
    }

    inject_table(ora_df, n_rows = min(50, nrow(ora_df)))
    paste0(
      "ORA results for ", direction, "-regulated features rendered above.\n",
      "Annotation type: ", annotation_type, ", Contrast: ", contrast, "\n",
      "Significant terms: ", nrow(ora_df)
    )
  }

  chat$register_tool(ellmer::tool(
    run_ora_fn,
    description = paste0(
      "Run over-representation analysis (ORA) on limma differential expression results. ",
      "Runs limma internally then tests which annotation terms are over-represented among ",
      "significantly up- or down-regulated features. ",
      "Use get_row_data() to see which annotation columns are available (e.g. 'species', 'go', 'kegg')."
    ),
    arguments = list(
      assay_name        = ellmer::type_string("The processed assay name"),
      formula           = ellmer::type_string("R formula string (e.g., '~microbiome_treatment')"),
      contrast          = ellmer::type_string("Contrast string from get_valid_contrasts()"),
      annotation_type   = ellmer::type_string("rowData column to use as terms (e.g. 'species', 'go', 'kegg')", required = FALSE),
      direction         = ellmer::type_string("'up' or 'down' — which DE direction to test", required = FALSE),
      adj_pval_threshold = ellmer::type_number("Significance threshold for DE features, default 0.05", required = FALSE),
      logFC_threshold   = ellmer::type_number("Minimum absolute logFC for DE features, default 1", required = FALSE)
    )
  ))

  # ── Tool: run_gsea ────────────────────────────────────────────────────────
  run_gsea_fn <- function(assay_name, formula, contrast,
                          annotation_type = "go", ranking_column = "logFC") {
    qf      <- get_qf()
    conduit <- conduit_obj()

    if (!assay_name %in% names(qf)) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(names(qf), collapse = ", ")))
    }

    design_terms <- tryCatch(
      conduitR::find_possible_contrast_terms(qf, assay_name, as.formula(formula)),
      error = function(e) character(0)
    )
    contrast <- escape_contrast_terms(contrast, design_terms)

    result <- tryCatch(
      conduitR::perform_limma_analysis(
        qf, assay_name = assay_name,
        formula = as.formula(formula), contrast = contrast
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      return(paste0("limma error: ", result$error, "\n",
                    "Call get_valid_contrasts('", assay_name, "', '", formula, "') to get valid contrasts."))
    }

    top <- result$top_table
    if (!annotation_type %in% names(top)) {
      avail <- names(top)[!names(top) %in% c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val",
                                              "neg_log10.adj.P.Val", "B", "id")]
      return(paste0("Annotation column '", annotation_type, "' not found in limma results. ",
                    "Available annotation columns: ", paste(avail, collapse = ", ")))
    }

    gsea <- tryCatch(
      conduitR::perform_gsea(
        limma_stats    = top,
        conduit        = conduit,
        annotation_type = annotation_type,
        ranking_column  = ranking_column
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (is.list(gsea) && !is.null(gsea$error)) {
      return(paste0("GSEA error: ", gsea$error))
    }

    gsea_df <- tryCatch(as.data.frame(gsea), error = function(e) NULL)
    if (is.null(gsea_df) || nrow(gsea_df) == 0) {
      return(paste0("GSEA complete but no significant terms found (p.adjust < 0.05) for '",
                    annotation_type, "'."))
    }

    inject_table(gsea_df, n_rows = min(50, nrow(gsea_df)))
    paste0(
      "GSEA results rendered above.\n",
      "Annotation type: ", annotation_type, ", Ranked by: ", ranking_column, "\n",
      "Contrast: ", contrast, "\n",
      "Significant terms: ", nrow(gsea_df)
    )
  }

  chat$register_tool(ellmer::tool(
    run_gsea_fn,
    description = paste0(
      "Run gene set enrichment analysis (GSEA) using ranked limma results. ",
      "Runs limma internally, ranks features by the ranking column, then tests for enrichment of annotation terms. ",
      "Does not require a significance cutoff — uses the full ranked list. ",
      "Use get_row_data() to see which annotation columns are available (e.g. 'go', 'kegg', 'species')."
    ),
    arguments = list(
      assay_name      = ellmer::type_string("The processed assay name"),
      formula         = ellmer::type_string("R formula string (e.g., '~microbiome_treatment')"),
      contrast        = ellmer::type_string("Contrast string from get_valid_contrasts()"),
      annotation_type = ellmer::type_string("rowData column to use as terms (e.g. 'go', 'kegg', 'species')", required = FALSE),
      ranking_column  = ellmer::type_string("Column to rank features by, default 'logFC'", required = FALSE)
    )
  ))

  # ── Tool: plot_ora_tool ───────────────────────────────────────────────────
  plot_ora_fn <- function(assay_name, formula, contrast,
                          annotation_type = "species", direction = "up",
                          adj_pval_threshold = 0.05, logFC_threshold = 1,
                          plot_type = "dotplot", show_category = 20L) {
    qf      <- get_qf()
    conduit <- conduit_obj()

    if (!assay_name %in% names(qf)) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(names(qf), collapse = ", ")))
    }
    if (!direction %in% c("up", "down")) return("direction must be 'up' or 'down'.")
    if (!plot_type %in% c("barplot", "dotplot")) return("plot_type must be 'barplot' or 'dotplot'.")

    design_terms <- tryCatch(
      conduitR::find_possible_contrast_terms(qf, assay_name, as.formula(formula)),
      error = function(e) character(0)
    )
    contrast <- escape_contrast_terms(contrast, design_terms)

    result <- tryCatch(
      conduitR::perform_limma_analysis(
        qf, assay_name = assay_name,
        formula = as.formula(formula), contrast = contrast
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      return(paste0("limma error: ", result$error))
    }

    top <- result$top_table
    if (!annotation_type %in% names(top)) {
      avail <- names(top)[!names(top) %in% c("logFC","AveExpr","t","P.Value","adj.P.Val",
                                              "neg_log10.adj.P.Val","B","id")]
      return(paste0("Annotation column '", annotation_type, "' not found. Available: ",
                    paste(avail, collapse = ", ")))
    }

    lfc_thresh <- if (direction == "down") -abs(logFC_threshold) else abs(logFC_threshold)
    ora <- tryCatch(
      conduitR::perform_ora(
        limma_stats        = top,
        direction          = direction,
        conduit            = conduit,
        annotation_type    = annotation_type,
        adj_pval_threshold = adj_pval_threshold,
        logFC_threshold    = lfc_thresh
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (is.list(ora) && !is.null(ora$error)) return(paste0("ORA error: ", ora$error))
    if (is.null(ora) || nrow(as.data.frame(ora)) == 0) {
      return(paste0("ORA complete but no significant terms found for ",
                    direction, "-regulated '", annotation_type, "' features."))
    }

    p <- tryCatch(
      if (plot_type == "barplot") enrichplot::barplot(ora, showCategory = show_category)
      else                        enrichplot::dotplot(ora, showCategory = show_category),
      error = function(e) NULL
    )
    if (is.null(p)) return("Could not generate ORA plot.")
    inject_plot(p, height = "500px")

    paste0(
      tools::toTitleCase(plot_type), " of ORA results (", direction, "-regulated '",
      annotation_type, "') rendered above.\n",
      "Showing top ", show_category, " terms. Contrast: ", contrast
    )
  }

  chat$register_tool(ellmer::tool(
    plot_ora_fn,
    description = paste0(
      "Run ORA and display a barplot or dotplot of enriched annotation terms. ",
      "Runs limma + ORA internally. Use after run_ora to visualize the same results, ",
      "or call directly. Use get_row_data() to confirm available annotation columns."
    ),
    arguments = list(
      assay_name         = ellmer::type_string("The processed assay name"),
      formula            = ellmer::type_string("R formula string (e.g., '~microbiome_treatment')"),
      contrast           = ellmer::type_string("Contrast string from get_valid_contrasts()"),
      annotation_type    = ellmer::type_string("rowData column for terms (e.g. 'species', 'go', 'kegg')", required = FALSE),
      direction          = ellmer::type_string("'up' or 'down'", required = FALSE),
      adj_pval_threshold = ellmer::type_number("DE significance threshold, default 0.05", required = FALSE),
      logFC_threshold    = ellmer::type_number("Minimum absolute logFC, default 1", required = FALSE),
      plot_type          = ellmer::type_string("'dotplot' (default) or 'barplot'", required = FALSE),
      show_category      = ellmer::type_integer("Number of top terms to show, default 20", required = FALSE)
    )
  ))

  # ── Tool: plot_gsea_tool ──────────────────────────────────────────────────
  plot_gsea_fn <- function(assay_name, formula, contrast,
                           annotation_type = "go", ranking_column = "logFC",
                           plot_type = "dotplot", show_category = 20L) {
    qf      <- get_qf()
    conduit <- conduit_obj()

    if (!assay_name %in% names(qf)) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(names(qf), collapse = ", ")))
    }
    if (!plot_type %in% c("dotplot", "ridgeplot")) return("plot_type must be 'dotplot' or 'ridgeplot'.")

    design_terms <- tryCatch(
      conduitR::find_possible_contrast_terms(qf, assay_name, as.formula(formula)),
      error = function(e) character(0)
    )
    contrast <- escape_contrast_terms(contrast, design_terms)

    result <- tryCatch(
      conduitR::perform_limma_analysis(
        qf, assay_name = assay_name,
        formula = as.formula(formula), contrast = contrast
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      return(paste0("limma error: ", result$error))
    }

    top <- result$top_table
    if (!annotation_type %in% names(top)) {
      avail <- names(top)[!names(top) %in% c("logFC","AveExpr","t","P.Value","adj.P.Val",
                                              "neg_log10.adj.P.Val","B","id")]
      return(paste0("Annotation column '", annotation_type, "' not found. Available: ",
                    paste(avail, collapse = ", ")))
    }

    gsea <- tryCatch(
      conduitR::perform_gsea(
        limma_stats     = top,
        conduit         = conduit,
        annotation_type = annotation_type,
        ranking_column  = ranking_column
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (is.list(gsea) && !is.null(gsea$error)) return(paste0("GSEA error: ", gsea$error))
    if (is.null(gsea) || nrow(as.data.frame(gsea)) == 0) {
      return(paste0("GSEA complete but no significant terms found for '", annotation_type, "'."))
    }

    p <- tryCatch(
      if (plot_type == "ridgeplot") enrichplot::ridgeplot(gsea, showCategory = show_category)
      else                          enrichplot::dotplot(gsea,  showCategory = show_category),
      error = function(e) NULL
    )
    if (is.null(p)) return("Could not generate GSEA plot.")
    inject_plot(p, height = "500px")

    paste0(
      tools::toTitleCase(plot_type), " of GSEA results ('", annotation_type,
      "', ranked by ", ranking_column, ") rendered above.\n",
      "Showing top ", show_category, " terms. Contrast: ", contrast
    )
  }

  chat$register_tool(ellmer::tool(
    plot_gsea_fn,
    description = paste0(
      "Run GSEA and display a dotplot or ridgeplot of enriched annotation terms. ",
      "Runs limma + GSEA internally. Ridgeplot shows the distribution of ranks for each term — ",
      "use it when you want to visualize the directionality of enrichment."
    ),
    arguments = list(
      assay_name      = ellmer::type_string("The processed assay name"),
      formula         = ellmer::type_string("R formula string (e.g., '~microbiome_treatment')"),
      contrast        = ellmer::type_string("Contrast string from get_valid_contrasts()"),
      annotation_type = ellmer::type_string("rowData column for terms (e.g. 'go', 'kegg', 'species')", required = FALSE),
      ranking_column  = ellmer::type_string("Column to rank features by, default 'logFC'", required = FALSE),
      plot_type       = ellmer::type_string("'dotplot' (default) or 'ridgeplot'", required = FALSE),
      show_category   = ellmer::type_integer("Number of top terms to show, default 20", required = FALSE)
    )
  ))

  # ── Tool: plot_heatmap_tool ───────────────────────────────────────────────
  plot_heatmap_fn <- function(assay_name, col_color_variables = NULL) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    col_vars <- if (!is.null(col_color_variables) && nzchar(col_color_variables)) {
      trimws(strsplit(col_color_variables, ",")[[1]])
    } else NULL

    p <- tryCatch(
      conduitR::plot_heatmap(qf, assay_name = assay_name, col_color_variables = col_vars),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate heatmap for '", assay_name, "'."))
    inject_plot(p, height = "500px")
    paste0("Heatmap for '", assay_name, "' rendered above",
      if (!is.null(col_vars)) paste0(", annotated with: ", paste(col_vars, collapse = ", ")) else "", ".")
  }

  chat$register_tool(ellmer::tool(
    plot_heatmap_fn,
    description = "Generate and display a clustered heatmap of the expression matrix. Optionally annotate samples (columns) with metadata variables.",
    arguments   = list(
      assay_name           = ellmer::type_string("The processed assay name"),
      col_color_variables  = ellmer::type_string("Comma-separated colData column names for column annotation bars (optional)", required = FALSE)
    )
  ))

  # ── Tool: plot_relative_abundance_tool ────────────────────────────────────
  plot_rel_abundance_fn <- function(assay_name, facet_by = NULL) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    facet_formula <- if (!is.null(facet_by) && nzchar(facet_by)) as.formula(paste0("~", facet_by)) else NULL

    p <- tryCatch(
      conduitR::plot_relative_abundance(qf, assay_name = assay_name, facet_formula = facet_formula),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate relative abundance plot for '", assay_name, "'."))
    inject_plot(p, height = "400px")
    paste0("Relative abundance plot for '", assay_name, "' rendered above",
      if (!is.null(facet_by) && nzchar(facet_by)) paste0(", faceted by '", facet_by, "'") else "", ".")
  }

  chat$register_tool(ellmer::tool(
    plot_rel_abundance_fn,
    description = "Generate and display a stacked bar chart of relative abundances. Best used with taxonomic-level assays (e.g., genus, species, phylum).",
    arguments   = list(
      assay_name = ellmer::type_string("The relative abundance assay name (a taxonomic rank like 'genus')"),
      facet_by   = ellmer::type_string("colData column name to facet the plot by (optional)", required = FALSE)
    )
  ))

  # ── Tool: plot_taxa_tree ──────────────────────────────────────────────────
  plot_taxa_tree_fn <- function(layout = "reingold-tilford",
                                type   = "multiple_proteins_in_group") {
    conduit <- conduit_obj()
    tax     <- tryCatch(slot(conduit, "taxonomy"), error = function(e) NULL)
    if (is.null(tax) || nrow(tax) == 0) {
      return("No taxonomy data found in this conduit object. The taxa tree requires a conduit with taxonomic annotations.")
    }

    valid_types <- c("multiple_proteins_in_group", "any_protein_in_group")
    if (!type %in% valid_types) {
      return(paste0("Invalid type '", type, "'. Choose from: ", paste(valid_types, collapse = ", ")))
    }

    p <- tryCatch(
      conduitR::plot_percent_detected_taxa_tree(conduit, type = type, layout = layout),
      error = function(e) NULL
    )
    if (is.null(p)) return("Could not generate taxonomic tree. Check that taxonomy data is populated.")
    inject_plot(p, height = "600px")
    paste0(
      "Taxonomic tree rendered above (layout: ", layout, ", type: ", type, ").\n",
      "Node size and color represent the percentage of proteins detected at each taxonomic level. ",
      "Larger/brighter nodes indicate higher detection coverage within that taxon."
    )
  }

  chat$register_tool(ellmer::tool(
    plot_taxa_tree_fn,
    description = paste0(
      "Generate a heat tree visualization of the taxonomic distribution of detected proteins. ",
      "Node size and color reflect detection rates at each taxonomic level. ",
      "Same plot as shown in the Database tab of the app."
    ),
    arguments = list(
      layout = ellmer::type_string(
        paste0("Tree layout algorithm. Options: 'reingold-tilford' (default), 'davidson-harel', ",
               "'fruchterman-reingold', 'kamada-kawai', 'gem', 'graphopt', 'mds', 'large-graph', 'drl', 'automatic'"),
        required = FALSE
      ),
      type = ellmer::type_string(
        paste0("Detection rate calculation method: ",
               "'multiple_proteins_in_group' (proteins in multiple samples per group, default) or ",
               "'any_protein_in_group' (any protein detected in a group)"),
        required = FALSE
      )
    )
  ))

  # ── Tool: plot_diann_metric ───────────────────────────────────────────────
  plot_diann_metric_fn <- function(column_name = NULL) {
    conduit <- conduit_obj()
    m       <- tryCatch(slot(conduit, "metrics"), error = function(e) NULL)
    if (is.null(m) || !"diann_stats" %in% names(m)) {
      return("No 'diann_stats' table found in this conduit object.")
    }
    diann <- m[["diann_stats"]]

    if (is.null(column_name) || !nzchar(column_name)) {
      numeric_cols <- names(diann)[vapply(diann, is.numeric, logical(1))]
      return(paste0(
        "Available numeric columns in diann_stats (pass one as column_name):\n",
        paste(numeric_cols, collapse = ", ")
      ))
    }

    if (!column_name %in% names(diann)) {
      numeric_cols <- names(diann)[vapply(diann, is.numeric, logical(1))]
      return(paste0("Column '", column_name, "' not found in diann_stats.\n",
                    "Available numeric columns: ", paste(numeric_cols, collapse = ", ")))
    }

    p <- tryCatch(
      conduitR::plot_conduit_metric_diann(conduit, column_name = column_name),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate DIA-NN metric plot for column '", column_name, "'."))
    inject_plot(p, height = "400px")
    paste0("DIA-NN metric plot for '", column_name, "' rendered above. ",
           "Each point is one sample/file; the line connects them in run order. ",
           "Look for outliers or trends that may indicate injection order effects or run failures.")
  }

  chat$register_tool(ellmer::tool(
    plot_diann_metric_fn,
    description = paste0(
      "Plot a sample-level DIA-NN metric (e.g. MS1.Signal, RT.Mean, Proteins.Identified) across all samples/files. ",
      "Call without arguments to list available columns. ",
      "Use to assess run quality, spot failed injections, or check for injection-order effects."
    ),
    arguments = list(
      column_name = ellmer::type_string(
        "Column from diann_stats to plot (e.g. 'MS1.Signal', 'RT.Mean'). Omit to list available columns.",
        required = FALSE
      )
    )
  ))

  # ── Tool: plot_features_per_sample ───────────────────────────────────────
  plot_features_per_sample_fn <- function(assay_name) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    p <- tryCatch(
      conduitR::plot_features_per_sample(qf, assay = assay_name),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate features-per-sample plot for '", assay_name, "'."))
    inject_plot(p, height = "400px")
    paste0("Features per sample bar chart for '", assay_name, "' rendered above. ",
           "Use this to spot samples with unusually low detection counts.")
  }

  chat$register_tool(ellmer::tool(
    plot_features_per_sample_fn,
    description = "Plot the number of detected features (proteins, peptides, etc.) per sample as a bar chart. Useful for QC — low-count samples may be failed runs.",
    arguments   = list(
      assay_name = ellmer::type_string("The assay to plot feature counts for (e.g. 'protein_groups')")
    )
  ))

  # ── Tool: plot_missing_values ─────────────────────────────────────────────
  plot_missing_values_fn <- function(assay_name, col_color_variables = NULL,
                                     row_color_variables = NULL) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    col_vars <- if (!is.null(col_color_variables) && nzchar(col_color_variables))
      trimws(strsplit(col_color_variables, ",")[[1]]) else NULL
    row_vars <- if (!is.null(row_color_variables) && nzchar(row_color_variables))
      trimws(strsplit(row_color_variables, ",")[[1]]) else NULL

    p <- tryCatch(
      conduitR::plot_missing_val_heatmap(
        qf,
        assay_name          = assay_name,
        col_color_variables = col_vars,
        row_color_variables = row_vars
      ),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate missing value heatmap for '", assay_name, "'. ",
                                   "Note: use a log-transformed assay (e.g. 'protein_groups_log2') not an imputed one."))
    inject_plot(p, height = "500px")
    paste0("Missing value heatmap for '", assay_name, "' rendered above. ",
           "Black cells = missing; white cells = detected.")
  }

  chat$register_tool(ellmer::tool(
    plot_missing_values_fn,
    description = "Generate a missing value heatmap showing which features are absent in which samples. Use a log-transformed (pre-imputation) assay (e.g. 'protein_groups_log2') so NAs are still present.",
    arguments   = list(
      assay_name          = ellmer::type_string("The log-transformed assay name (pre-imputation, e.g. 'protein_groups_log2')"),
      col_color_variables = ellmer::type_string("Comma-separated colData columns to annotate samples (optional)", required = FALSE),
      row_color_variables = ellmer::type_string("Comma-separated rowData columns to annotate features (optional)", required = FALSE)
    )
  ))

  # ── Tool: plot_sample_correlation ─────────────────────────────────────────
  plot_sample_correlation_fn <- function(assay_name, col_color_variables = NULL) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    col_vars <- if (!is.null(col_color_variables) && nzchar(col_color_variables))
      trimws(strsplit(col_color_variables, ",")[[1]]) else NULL

    p <- tryCatch(
      conduitR::plot_sample_cor_heatmap(
        qf,
        assay_name                  = assay_name,
        sample_annotation_variables = col_vars
      ),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate sample correlation heatmap for '", assay_name, "'."))
    inject_plot(p, height = "500px")
    paste0("Sample correlation heatmap for '", assay_name, "' rendered above. ",
           "Samples that cluster together have similar proteome profiles.")
  }

  chat$register_tool(ellmer::tool(
    plot_sample_correlation_fn,
    description = "Generate a sample-to-sample correlation heatmap. High within-group and low between-group correlation indicates good data quality and biological separation.",
    arguments   = list(
      assay_name          = ellmer::type_string("The processed assay name"),
      col_color_variables = ellmer::type_string("Comma-separated colData columns for sample annotation bars (optional)", required = FALSE)
    )
  ))

  # ── Tool: plot_intensity_distribution ────────────────────────────────────
  plot_intensity_distribution_fn <- function(assay_name) {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    se  <- qf[[assay_name]]
    mat <- SummarizedExperiment::assay(se)
    n_detected <- rowSums(!is.na(mat))
    df <- data.frame(n_detected = n_detected)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = n_detected)) +
      ggplot2::geom_histogram(binwidth = 1, fill = "#3B528BFF", color = "white") +
      ggplot2::labs(
        x     = "Number of Samples Feature Is Detected In",
        y     = "Number of Features",
        title = paste("Detection Frequency —", assay_name)
      ) +
      ggplot2::theme_minimal()
    inject_plot(p, height = "350px")
    paste0("Detection frequency histogram for '", assay_name, "' rendered above. ",
           "Features detected in few samples may represent rare taxa or borderline detections.")
  }

  chat$register_tool(ellmer::tool(
    plot_intensity_distribution_fn,
    description = "Plot a histogram showing how many samples each feature is detected in. Reveals the overall detection frequency distribution — useful for deciding missingness thresholds.",
    arguments   = list(
      assay_name = ellmer::type_string("The assay to plot detection frequency for (e.g. 'protein_groups')")
    )
  ))

  # ── Tool: plot_selected_features ─────────────────────────────────────────
  plot_selected_features_fn <- function(assay_name, feature_ids, x_axis,
                                         color_by = NULL, shape_by = NULL,
                                         facet_formula = "~ id") {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    ids <- trimws(strsplit(feature_ids, ",")[[1]])

    p <- tryCatch(
      conduitR::plot_selected_features(
        qf,
        assay_name    = assay_name,
        features      = ids,
        x_axis        = x_axis,
        color_by      = if (!is.null(color_by)  && nzchar(color_by))  color_by  else NULL,
        shape         = if (!is.null(shape_by)   && nzchar(shape_by))  shape_by  else NULL,
        facet_formula = tryCatch(as.formula(facet_formula), error = function(e) as.formula("~ id"))
      ),
      error = function(e) NULL
    )
    if (is.null(p)) return(paste0("Could not generate selected features plot. Check that the feature IDs exist in '",
                                   assay_name, "' rowData."))
    inject_plot(p, height = "450px")
    paste0("Selected features plot for ", length(ids), " feature(s) rendered above. ",
           "X-axis: ", x_axis,
           if (!is.null(color_by) && nzchar(color_by)) paste0(", colored by: ", color_by) else "", ".")
  }

  chat$register_tool(ellmer::tool(
    plot_selected_features_fn,
    description = "Plot abundance values for one or more specific features (proteins/peptides) across samples. Use after run_limma to visualize individual significant hits. Feature IDs must match row names in the assay.",
    arguments   = list(
      assay_name    = ellmer::type_string("The assay name containing the features"),
      feature_ids   = ellmer::type_string("Comma-separated feature IDs to plot (e.g. 'P12345,P67890' or a single ID)"),
      x_axis        = ellmer::type_string("colData column to use as the x-axis (e.g. 'treatment', 'timepoint')"),
      color_by      = ellmer::type_string("colData or rowData column to color points by (optional)", required = FALSE),
      shape_by      = ellmer::type_string("colData or rowData column to shape points by (optional)", required = FALSE),
      facet_formula = ellmer::type_string("Facet formula string, default '~ id' to show one panel per feature", required = FALSE)
    )
  ))

  # ── Tool: run_classification ──────────────────────────────────────────────
  run_classification_fn <- function(assay_name, outcome, model_type = "random_forest") {
    qf        <- get_qf()
    available <- names(qf)
    if (!assay_name %in% available) {
      return(paste0("Assay '", assay_name, "' not found. Available: ", paste(available, collapse = ", ")))
    }
    valid_models <- c("lasso_regression", "random_forest", "xgboost")
    if (!model_type %in% valid_models) {
      return(paste0("Invalid model_type '", model_type, "'. Choose from: ", paste(valid_models, collapse = ", ")))
    }

    result <- tryCatch(
      conduitR::predict_classification(
        qf,
        assay_name = assay_name,
        outcome    = outcome,
        model_type = model_type
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) return(paste0("Classification error: ", result$error))

    p_roc <- tryCatch(conduitR::plot_roc(result),              error = function(e) NULL)
    p_imp <- tryCatch(conduitR::plot_feature_importance(result), error = function(e) NULL)

    if (!is.null(p_roc)) inject_plot(p_roc, height = "350px")
    if (!is.null(p_imp)) inject_plot(p_imp, height = "350px")

    metrics_str <- tryCatch({
      m <- result$metrics
      paste(utils::capture.output(print(m)), collapse = "\n")
    }, error = function(e) "Metrics not available")

    paste0(
      "Classification model (", model_type, ") predicting '", outcome, "' complete.\n",
      "ROC curve and feature importance plots rendered above.\n",
      "Model metrics:\n", metrics_str
    )
  }

  chat$register_tool(ellmer::tool(
    run_classification_fn,
    description = "Train and evaluate a classification model (lasso regression, random forest, or XGBoost) to predict a binary/categorical outcome from proteomics features. Displays ROC curve and top feature importance.",
    arguments   = list(
      assay_name = ellmer::type_string("The processed assay to use as input features"),
      outcome    = ellmer::type_string("The colData column to predict (must be binary/categorical)"),
      model_type = ellmer::type_enum(
        c("lasso_regression", "random_forest", "xgboost"),
        "Model type, default 'random_forest'",
        required = FALSE
      )
    )
  ))
}
