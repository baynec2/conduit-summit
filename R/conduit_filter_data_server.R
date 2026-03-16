conduit_filter_data_server <- function(id, qf, colData, selected_assay) {
  moduleServer(id, function(input, output, session) {

    sample_vars <- reactive({
      req(colData())
      colnames(colData())
    })

    feature_vars <- reactive({
      req(qf(), selected_assay())
      colnames(SummarizedExperiment::rowData(qf()[[selected_assay()]]))
    })

    # ── Clear-all tracking ────────────────────────────────────────────────────
    cleared <- reactiveVal(0)
    observeEvent(input$clear_all_filters, { cleared(cleared() + 1) })

    # ── Helper: build a single filter control based on column type ────────────
    make_filter_ui <- function(ns_id, label, col) {
      if (is.list(col)) return(NULL)
      if (is.numeric(col)) {
        rng <- range(col, na.rm = TRUE)
        if (rng[1] == rng[2]) return(NULL)
        step <- signif((rng[2] - rng[1]) / 100, 2)
        shiny::sliderInput(
          ns_id, label = label,
          min = rng[1], max = rng[2], value = rng, step = step
        )
      } else {
        vals <- sort(unique(as.character(col[!is.na(col)])))
        if (length(vals) == 0 || length(vals) >= 50) return(NULL)
        shinyWidgets::pickerInput(
          ns_id, label = label,
          choices  = vals,
          selected = vals,
          multiple = TRUE,
          options  = shinyWidgets::pickerOptions(
            actionsBox = TRUE, liveSearch = TRUE,
            size = 8, container = "body"
          )
        )
      }
    }

    # ── Dynamic filter UIs ────────────────────────────────────────────────────
    output$sample_filters <- renderUI({
      req(sample_vars(), colData())
      cleared()
      lapply(sample_vars(), function(var) {
        make_filter_ui(session$ns(paste0("sample_", var)), var, colData()[[var]])
      })
    })

    output$feature_filters <- renderUI({
      req(qf(), selected_assay())
      if (!(selected_assay() %in% names(qf()))) return(NULL)
      cleared()

      rd <- SummarizedExperiment::rowData(qf()[[selected_assay()]])
      lapply(feature_vars(), function(var) {
        make_filter_ui(session$ns(paste0("feature_", var)), var, rd[[var]])
      })
    })

    # ── Filtered data (fires on Apply or Clear, not on every input change) ────
    filtered_qf <- reactive({
      req(qf(), colData(), selected_assay(), sample_vars(), feature_vars())

      qf_filtered <- qf()

      for (var in sample_vars()) {
        sel <- isolate(input[[paste0("sample_", var)]])
        if (is.null(sel) || length(sel) == 0) next
        col <- SummarizedExperiment::colData(qf_filtered)[[var]]
        if (is.numeric(col)) {
          keep_samples <- is.na(col) | (col >= sel[1] & col <= sel[2])
        } else {
          keep_samples <- is.na(col) | col %in% sel
        }
        if (!any(keep_samples)) return(NULL)
        qf_filtered <- qf_filtered[, keep_samples]
      }

      if (!(selected_assay() %in% names(qf_filtered))) return(NULL)

      se <- qf_filtered[[selected_assay()]]
      if (is.null(se) || nrow(se) == 0) return(NULL)

      for (var in feature_vars()) {
        sel <- isolate(input[[paste0("feature_", var)]])
        if (is.null(sel) || length(sel) == 0) next
        rd_vals <- SummarizedExperiment::rowData(se)[[var]]
        if (is.numeric(rd_vals)) {
          keep_features <- is.na(rd_vals) | (rd_vals >= sel[1] & rd_vals <= sel[2])
        } else {
          keep_features <- is.na(rd_vals) | rd_vals %in% sel
        }
        if (!any(keep_features)) return(NULL)
        se <- se[keep_features, ]
      }

      qf_filtered[[selected_assay()]] <- se
      qf_filtered
    }) |> bindEvent(input$apply_filters, input$clear_all_filters,
                    qf(), selected_assay(),
                    ignoreNULL = FALSE, ignoreInit = FALSE)

    # ── Retained count outputs ────────────────────────────────────────────────
    output$n_samples_retained <- renderText({
      req(colData())
      n_total <- nrow(colData())
      n_kept  <- if (!is.null(filtered_qf())) nrow(SummarizedExperiment::colData(filtered_qf())) else n_total
      paste0(n_kept, " / ", n_total)
    })

    output$n_features_retained <- renderText({
      req(qf(), selected_assay())
      n_total <- nrow(qf()[[selected_assay()]])
      n_kept  <- if (!is.null(filtered_qf()) && selected_assay() %in% names(filtered_qf())) {
        nrow(filtered_qf()[[selected_assay()]])
      } else 0
      paste0(n_kept, " / ", n_total)
    })

    # ── Helpers: which vars have a renderable filter control ─────────────────
    filterable_sample_vars <- reactive({
      req(colData(), sample_vars())
      Filter(function(var) {
        col <- colData()[[var]]
        if (is.list(col)) return(FALSE)
        if (is.numeric(col)) { rng <- range(col, na.rm = TRUE); rng[1] != rng[2] }
        else { n <- length(unique(col[!is.na(col)])); n > 0 && n < 50 }
      }, sample_vars())
    })

    filterable_feature_vars <- reactive({
      req(qf(), selected_assay(), feature_vars())
      rd <- SummarizedExperiment::rowData(qf()[[selected_assay()]])
      Filter(function(var) {
        col <- rd[[var]]
        if (is.list(col)) return(FALSE)
        if (is.numeric(col)) { rng <- range(col, na.rm = TRUE); rng[1] != rng[2] }
        else { n <- length(unique(col[!is.na(col)])); n > 0 && n < 50 }
      }, feature_vars())
    })

    # ── Track which vars have non-default filters (set at Apply time) ─────────
    active_filter_vars <- reactiveVal(list(sample = character(0), feature = character(0)))

    observeEvent(input$apply_filters, {
      req(colData(), filterable_sample_vars())

      svars <- Filter(function(var) {
        col <- colData()[[var]]
        sel <- input[[paste0("sample_", var)]]
        if (is.null(sel)) return(FALSE)
        if (is.numeric(col)) {
          rng <- range(col, na.rm = TRUE)
          isTRUE(sel[1] > rng[1]) || isTRUE(sel[2] < rng[2])
        } else {
          all_vals <- sort(unique(as.character(col[!is.na(col)])))
          length(sel) < length(all_vals)
        }
      }, filterable_sample_vars())

      fvars <- if (!is.null(qf()) && selected_assay() %in% names(qf())) {
        rd <- SummarizedExperiment::rowData(qf()[[selected_assay()]])
        Filter(function(var) {
          col <- rd[[var]]
          sel <- input[[paste0("feature_", var)]]
          if (is.null(sel)) return(FALSE)
          if (is.numeric(col)) {
            rng <- range(col, na.rm = TRUE)
            isTRUE(sel[1] > rng[1]) || isTRUE(sel[2] < rng[2])
          } else {
            all_vals <- sort(unique(as.character(col[!is.na(col)])))
            length(sel) < length(all_vals)
          }
        }, filterable_feature_vars())
      } else character(0)

      active_filter_vars(list(sample = svars, feature = fvars))
    })

    observeEvent(input$clear_all_filters, {
      active_filter_vars(list(sample = character(0), feature = character(0)))
    })

    # ── Filter impact charts ──────────────────────────────────────────────────
    output$filter_impact_ui <- renderUI({
      afv   <- active_filter_vars()
      svars <- afv$sample
      fvars <- afv$feature

      if (length(svars) == 0 && length(fvars) == 0) {
        return(tags$p(
          "No active filters. Set filters and click Apply to see impact.",
          class = "text-muted fst-italic small p-2"
        ))
      }

      make_section <- function(vars, prefix, title) {
        if (length(vars) == 0) return(NULL)
        plots <- lapply(vars, function(v) {
          plotly::plotlyOutput(session$ns(paste0(prefix, v)), height = "200px")
        })
        col_w <- if (length(vars) == 1) c(12) else rep(6, length(vars))
        tagList(
          tags$p(title, class = "fw-semibold text-muted small mb-2 mt-3"),
          do.call(bslib::layout_columns, c(list(col_widths = col_w), plots))
        )
      }

      tagList(
        make_section(svars, "sc_", "Sample Variables"),
        make_section(fvars, "fc_", "Feature Variables")
      )
    })

    # ── Build one impact chart for a single variable ──────────────────────────
    make_impact_plot <- function(col, retained_mask) {
      status <- factor(
        ifelse(retained_mask, "Retained", "Filtered"),
        levels = c("Filtered", "Retained")
      )
      df <- data.frame(value = col, status = status)
      df <- df[!is.na(df$value), ]

      fill_vals <- c("Retained" = "#15131e", "Filtered" = "#d0d3de")

      if (is.numeric(col)) {
        p <- ggplot2::ggplot(df, ggplot2::aes(x = value, fill = status)) +
          ggplot2::geom_histogram(bins = 25, position = "stack", colour = NA) +
          ggplot2::scale_fill_manual(values = fill_vals) +
          ggplot2::labs(x = NULL, y = "Count", fill = NULL) +
          theme_conduit()
      } else {
        df$value <- as.character(df$value)
        p <- ggplot2::ggplot(df, ggplot2::aes(y = value, fill = status)) +
          ggplot2::geom_bar(position = "stack") +
          ggplot2::scale_fill_manual(values = fill_vals) +
          ggplot2::labs(x = "Count", y = NULL, fill = NULL) +
          theme_conduit()
      }
      plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
        plotly::layout(legend = list(orientation = "h", x = 0, y = -0.2))
    }

    # ── Dynamic renderPlotly for each active sample variable ──────────────────
    observe({
      req(filtered_qf(), colData())
      svars <- active_filter_vars()$sample
      if (length(svars) == 0) return()

      all_cd        <- colData()
      ret_names     <- rownames(SummarizedExperiment::colData(filtered_qf()))
      retained_mask <- rownames(all_cd) %in% ret_names

      lapply(svars, function(v) {
        local({
          var <- v
          output[[paste0("sc_", var)]] <- plotly::renderPlotly({
            make_impact_plot(all_cd[[var]], retained_mask)
          })
        })
      })
    })

    # ── Dynamic renderPlotly for each active feature variable ─────────────────
    observe({
      req(filtered_qf(), qf(), selected_assay())
      fvars <- active_filter_vars()$feature
      if (length(fvars) == 0) return()

      full_rd       <- SummarizedExperiment::rowData(qf()[[selected_assay()]])
      ret_rd        <- SummarizedExperiment::rowData(filtered_qf()[[selected_assay()]])
      retained_mask <- rownames(full_rd) %in% rownames(ret_rd)

      lapply(fvars, function(v) {
        local({
          var <- v
          output[[paste0("fc_", var)]] <- plotly::renderPlotly({
            make_impact_plot(full_rd[[var]], retained_mask)
          })
        })
      })
    })

    return(filtered_qf)
  })
}
