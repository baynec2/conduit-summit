# ── bslib / Bootstrap 5 app theme ────────────────────────────────────────────
create_conduit_theme <- function() {
  bslib::bs_theme(
    version  = 5,
    bg       = "#f0f2f5",
    fg       = "#1a1a2e",
    primary  = "#15131e",
    warning  = "#f3b24b",
    "navbar-bg"            = "#000000",
    "navbar-color"         = "#ffffff",
    "navbar-brand-color"   = "#ffffff",
    "sidebar-bg"           = "#15131e",
    "sidebar-fg"           = "#ffffff",
    "sidebar-border-color" = "#2a2840",
    "card-bg"              = "#ffffff",
    "card-border-color"    = "rgba(21, 19, 30, 0.1)",
    "card-cap-bg"          = "#000000",
    "card-cap-color"       = "#ffffff",
    "card-border-radius"   = "0.6rem"
  ) |>
    bslib::bs_add_rules("
    /* ── Top navbar tab links ───────────────────────────── */
    .navbar .navbar-nav .nav-link {
      color: rgba(255, 255, 255, 0.72) !important;
      font-size: 0.875rem;
      padding: 0.5rem 0.8rem;
      border-radius: 0.35rem;
      transition: color 0.15s ease, background-color 0.15s ease;
    }
    .navbar .navbar-nav .nav-link:hover {
      color: #f3b24b !important;
      background-color: rgba(255, 255, 255, 0.06);
    }
    .navbar .navbar-nav .nav-link.active {
      color: #f3b24b !important;
      font-weight: 600;
      background-color: rgba(243, 178, 75, 0.12);
    }

    /* ── Cards: soft shadow + white body ────────────────── */
    .card {
      box-shadow: 0 1px 3px rgba(21, 19, 30, 0.06),
                  0 4px 10px rgba(21, 19, 30, 0.04);
      border: 1px solid rgba(21, 19, 30, 0.08) !important;
      transition: box-shadow 0.18s ease;
    }
    .card:hover {
      box-shadow: 0 2px 8px rgba(21, 19, 30, 0.09),
                  0 6px 16px rgba(21, 19, 30, 0.06);
    }
    .card > .card-header {
      background-color: #000000 !important;
      color: #ffffff !important;
      font-weight: 600;
      font-size: 0.82rem;
      letter-spacing: 0.04em;
      text-transform: uppercase;
      border-bottom: 3px solid #f3b24b !important;
      border-radius: 0.55rem 0.55rem 0 0 !important;
      padding: 0.6rem 1rem;
    }
    .card > .card-footer {
      background: #fafafa;
      border-top: 1px solid rgba(21, 19, 30, 0.06);
      padding: 0.5rem 1rem;
    }

    /* ── Light card variant (plot containers) ────────────── */
    .card.card-light > .card-header {
      background-color: #ffffff !important;
      color: #1a1a2e !important;
      border-bottom: 2px solid #f3b24b !important;
      font-size: 0.78rem;
      font-weight: 700;
      letter-spacing: 0.05em;
      text-transform: uppercase;
    }
    .card.card-light {
      border-top: 3px solid #f3b24b !important;
    }

    /* ── Borderless card variant ─────────────────────────── */
    .card.card-borderless {
      border: none !important;
      box-shadow: none !important;
      background: transparent !important;
    }
    .card.card-borderless > .card-header {
      background: transparent !important;
      color: #1a1a2e !important;
      border-bottom: 1px solid rgba(21, 19, 30, 0.1) !important;
      text-transform: none;
      letter-spacing: 0;
    }

    /* ── navset_card_tab: tab pills above content ────────── */
    /* Target both class orderings bslib may render */
    .card > .card-header.bslib-navs-top,
    .bslib-card > .bslib-navs-top {
      background-color: #000000 !important;
      border-bottom: 3px solid #f3b24b !important;
      padding: 0.3rem 0.75rem 0 !important;
      text-transform: none;
      letter-spacing: 0;
    }
    /* Broad selector catches all nav-links inside any dark card header */
    .card > .card-header .nav-link,
    .card > .card-header.bslib-navs-top .nav-link,
    .bslib-card > .bslib-navs-top .nav-link {
      color: rgba(255,255,255,0.72) !important;
      --bs-nav-link-color: rgba(255,255,255,0.72);
      --bs-nav-tabs-link-color: rgba(255,255,255,0.72);
      font-size: 0.85rem;
      padding: 0.45rem 0.9rem;
      border-radius: 0.35rem 0.35rem 0 0;
      border: none;
    }
    .card > .card-header .nav-link:hover,
    .card > .card-header.bslib-navs-top .nav-link:hover {
      color: #ffffff !important;
      background: rgba(243,178,75,0.15);
    }
    .card > .card-header .nav-link.active,
    .card > .card-header.bslib-navs-top .nav-link.active,
    .bslib-card > .bslib-navs-top .nav-link.active {
      background: #f3b24b !important;
      color: #15131e !important;
      font-weight: 600;
    }
    /* tab-content only: reset Bootstrap dark-mode bleedthrough from dark header.
       Scoped to .tab-content so value boxes and regular cards are unaffected. */
    .tab-content {
      --bs-body-color: #1a1a2e;
      --bs-body-bg: #ffffff;
      color: #1a1a2e;
    }

    /* ── navset_underline (QC sub-tabs) ─────────────────── */
    .nav-underline .nav-link {
      color: #15131e;
      font-size: 0.85rem;
      padding: 0.4rem 0.8rem;
    }
    .nav-underline .nav-link.active {
      color: #15131e !important;
      border-bottom-color: #f3b24b !important;
      font-weight: 600;
    }
    .nav-underline .nav-link:hover {
      color: #15131e;
      border-bottom-color: rgba(243,178,75,0.5) !important;
    }

    /* ── Value boxes ─────────────────────────────────────── */
    .bslib-value-box {
      border-radius: 0.6rem !important;
      border: 1px solid rgba(21, 19, 30, 0.08) !important;
      box-shadow: 0 1px 3px rgba(21, 19, 30, 0.06),
                  0 4px 10px rgba(21, 19, 30, 0.04) !important;
    }

    /* ── Card body padding ───────────────────────────────── */
    .card-body { padding: 1rem 1.25rem; }

    /* ── navset_card_tab: force light text in tab content ── */
    /* bslib propagates a dark color scheme from the dark card header into
       the tab-content body, making all form text white on white.
       Explicitly reset everything inside .tab-content to dark. */
    .tab-content {
      color: #1a1a2e;
    }
    .tab-content label,
    .tab-content .form-label,
    .tab-content .control-label,
    .tab-content .shiny-input-label {
      color: #1a1a2e !important;
    }
    .tab-content .form-control,
    .tab-content .form-select,
    .tab-content input[type='text'],
    .tab-content input[type='number'] {
      color: #1a1a2e !important;
      background-color: #ffffff !important;
    }
    .tab-content .selectize-input,
    .tab-content .selectize-dropdown {
      color: #1a1a2e !important;
      background-color: #ffffff !important;
    }
    .tab-content .accordion-button {
      color: #1a1a2e !important;
      background-color: #f8f9fb !important;
    }
    .tab-content .accordion-body {
      color: #1a1a2e !important;
    }

    /* ── In-tab control sidebars ─────────────────────────── */
    /* sidebar-fg:#fff (for the dark nav sidebar) bleeds into layout_sidebar
       panels — explicitly reset all text to dark here */
    .bslib-sidebar-layout > .sidebar {
      color: #1a1a2e;
      font-size: 0.875rem;
    }
    .bslib-sidebar-layout > .sidebar .form-label,
    .bslib-sidebar-layout > .sidebar label,
    .bslib-sidebar-layout > .sidebar .control-label {
      color: #444455;
      font-size: 0.82rem;
      font-weight: 600;
      margin-bottom: 0.2rem;
    }
    .bslib-sidebar-layout > .sidebar p,
    .bslib-sidebar-layout > .sidebar small,
    .bslib-sidebar-layout > .sidebar .text-muted {
      color: #6c757d !important;
    }
    .bslib-sidebar-layout > .sidebar .form-control,
    .bslib-sidebar-layout > .sidebar .form-select,
    .bslib-sidebar-layout > .sidebar .selectize-input {
      color: #1a1a2e;
      background-color: #ffffff;
    }
    .bslib-sidebar-layout > .sidebar hr {
      border-color: rgba(21, 19, 30, 0.12);
      margin: 0.75rem 0;
    }
    .bslib-sidebar-layout > .sidebar .sidebar-title {
      color: #888899;
      font-size: 0.72rem;
      font-weight: 700;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      padding-bottom: 0.5rem;
      border-bottom: 2px solid #f3b24b;
      margin-bottom: 1rem;
    }
    /* Accordion items within sidebars */
    .bslib-sidebar-layout > .sidebar .accordion-button {
      color: #1a1a2e;
      background-color: #f8f9fb;
    }
    .bslib-sidebar-layout > .sidebar .accordion-body {
      color: #1a1a2e;
    }
    /* Sidebar toggle button */
    .bslib-sidebar-layout .collapse-toggle {
      color: #15131e !important;
      opacity: 0.5;
    }
    .bslib-sidebar-layout .collapse-toggle:hover {
      opacity: 1;
    }

    /* ── Accordion (used in sidebars) ────────────────────── */
    .accordion-button {
      font-size: 0.82rem;
      font-weight: 600;
      padding: 0.55rem 0.75rem;
    }
    .accordion-button:not(.collapsed) {
      color: #15131e;
      background-color: rgba(243, 178, 75, 0.12);
      box-shadow: none;
    }
    .accordion-button::after {
      filter: none;
    }
    .accordion-item {
      border-color: rgba(21, 19, 30, 0.1);
    }
    .accordion-body {
      padding: 0.5rem 0.75rem;
    }

    /* ── About page hero ─────────────────────────────────── */
    .conduit-hero {
      background: #000000;
      border-radius: 1rem;
      color: #fff;
      padding: 3.5rem 2rem;
      margin-bottom: 0;
      position: relative;
      overflow: hidden;
    }
    .conduit-hero-canvas {
      position: absolute;
      inset: 0;
      width: 100%;
      height: 100%;
      pointer-events: none;
      z-index: 0;
    }
    .conduit-hero-grid {
      position: absolute;
      inset: 0;
      pointer-events: none;
      z-index: 1;
      background-image:
        linear-gradient(rgba(255,255,255,0.08) 1px, transparent 1px),
        linear-gradient(90deg, rgba(255,255,255,0.08) 1px, transparent 1px);
      background-size: 80px 80px;
      -webkit-mask-image: radial-gradient(ellipse 90% 80% at 50% 50%, black 40%, transparent 85%);
      mask-image: radial-gradient(ellipse 90% 80% at 50% 50%, black 40%, transparent 85%);
    }
    .conduit-hero-content {
      position: relative;
      z-index: 2;
    }
    .conduit-hero-title {
      color: #f3b24b;
      font-weight: 800;
      font-size: 2.4rem;
      margin-bottom: 0.75rem;
    }
    .conduit-hero-subtitle {
      color: rgba(255, 255, 255, 0.78);
      font-size: 1.05rem;
      max-width: 460px;
      margin: 0 auto 1.25rem;
    }
    .conduit-feature-icon {
      font-size: 1.8rem;
      color: #f3b24b;
      margin-bottom: 0.6rem;
    }
    /* Hero upload widget */
    .conduit-hero-upload {
      max-width: 420px;
      margin: 0 auto;
    }
    .conduit-hero-upload .form-control {
      background: rgba(255,255,255,0.08);
      border-color: rgba(255,255,255,0.25);
      color: rgba(255,255,255,0.85);
    }
    .conduit-hero-upload .form-control::placeholder {
      color: rgba(255,255,255,0.45);
    }
    .conduit-hero-upload .btn {
      background: #f3b24b;
      border-color: #f3b24b;
      color: #15131e;
      font-weight: 600;
    }
    .conduit-hero-upload .btn:hover {
      background: #e0a040;
      border-color: #e0a040;
    }
    /* Success banner inside hero */
    .conduit-hero .alert-success {
      max-width: 500px;
      margin: 0.75rem auto 0;
    }

    /* ── Workflow steps ──────────────────────────────────── */
    .workflow-step-number {
      font-size: 2rem;
      font-weight: 800;
      color: #f3b24b;
      line-height: 1;
    }
    .workflow-step.workflow-locked {
      opacity: 0.35;
      pointer-events: none;
    }
    .workflow-step-card {
      border-top: 3px solid #f3b24b !important;
      border-left: none !important;
      border-right: none !important;
      border-bottom: none !important;
      transition: box-shadow 0.2s ease, transform 0.2s ease !important;
    }
    .workflow-step-card:hover {
      box-shadow: 0 6px 24px rgba(21, 19, 30, 0.12), 0 2px 8px rgba(21, 19, 30, 0.08) !important;
      transform: translateY(-3px);
    }
    .workflow-step-card .workflow-icon-circle {
      width: 52px;
      height: 52px;
      border-radius: 50%;
      background: rgba(243, 178, 75, 0.12);
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 1.4rem;
      color: #f3b24b;
      margin: 0 auto 0.5rem;
      transition: background 0.18s ease;
    }
    .workflow-step-card:hover .workflow-icon-circle {
      background: rgba(243, 178, 75, 0.22);
    }
    .workflow-step-card .workflow-step-number {
      font-size: 0.7rem;
      font-weight: 700;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      color: #f3b24b;
      line-height: 1;
    }
    .workflow-step-card h6 {
      font-weight: 700;
      font-size: 0.9rem;
      color: #1a1a2e;
      margin-bottom: 0.4rem !important;
    }

    /* ── Help page ───────────────────────────────────────── */
    .conduit-help-hero {
      background: #000000;
      border-radius: 0.75rem;
      color: #fff;
      padding: 2.25rem 2rem;
      margin-bottom: 1.5rem;
      text-align: center;
      position: relative;
      overflow: hidden;
    }
    .conduit-help-hero h2 {
      color: #f3b24b;
      font-weight: 800;
      margin-bottom: 0.4rem;
    }
    .conduit-help-hero p {
      color: rgba(255,255,255,0.75);
      margin-bottom: 0;
      font-size: 0.95rem;
    }
    .conduit-ecosystem-card {
      border-top: 4px solid #f3b24b !important;
      text-align: center;
      height: 100%;
    }
    .conduit-ecosystem-card .card-body {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 0.4rem;
    }
    .conduit-ecosystem-icon {
      width: 48px;
      height: 48px;
      border-radius: 50%;
      background: rgba(243,178,75,0.15);
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 1.4rem;
      color: #f3b24b;
      margin-bottom: 0.25rem;
    }
    .conduit-quickstart-step {
      display: flex;
      align-items: flex-start;
      gap: 1rem;
      padding: 0.9rem 1rem;
      border-radius: 0.5rem;
      background: #f8f9fb;
      border-left: 4px solid #f3b24b;
      margin-bottom: 0.6rem;
    }
    .conduit-quickstart-num {
      font-size: 1.5rem;
      font-weight: 800;
      color: #f3b24b;
      line-height: 1.2;
      flex-shrink: 0;
    }

    /* ── Page footer ────────────────────────────────────── */
    .conduit-footer {
      border-top: 1px solid rgba(21, 19, 30, 0.1);
      padding: 0.65rem 1.5rem;
      font-size: 0.8rem;
      color: #6c757d;
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-top: 1.5rem;
    }

    /* ── DataTables styling ──────────────────────────────── */
    table.dataTable thead th,
    table.dataTable thead td {
      background-color: #f8f9fb;
      color: #1a1a2e;
      font-size: 0.76rem;
      font-weight: 700;
      letter-spacing: 0.05em;
      text-transform: uppercase;
      border-bottom: 2px solid #f3b24b !important;
      border-top: none;
    }
    table.dataTable thead th:hover {
      background-color: rgba(243, 178, 75, 0.12);
    }
    table.dataTable tbody tr {
      font-size: 0.875rem;
    }
    table.dataTable tbody tr:hover > * {
      background-color: rgba(243, 178, 75, 0.07) !important;
      color: #1a1a2e;
    }
    table.dataTable.table-sm > tbody > tr > td {
      padding: 0.35rem 0.6rem;
    }
    /* Search box */
    .dataTables_filter input {
      border: 1px solid rgba(21, 19, 30, 0.18);
      border-radius: 0.35rem;
      padding: 0.3rem 0.65rem;
      font-size: 0.85rem;
      outline: none;
      transition: border-color 0.15s ease, box-shadow 0.15s ease;
    }
    .dataTables_filter input:focus {
      border-color: #f3b24b;
      box-shadow: 0 0 0 3px rgba(243, 178, 75, 0.18);
    }
    .dataTables_filter label {
      font-size: 0 !important;
    }
    .dataTables_filter label input {
      font-size: 0.85rem !important;
      margin-left: 0 !important;
    }
    /* Info + pagination */
    .dataTables_info {
      font-size: 0.8rem;
      color: #888899;
    }
    .dataTables_paginate .paginate_button {
      border-radius: 0.3rem !important;
      font-size: 0.8rem !important;
      padding: 0.2rem 0.55rem !important;
      border: 1px solid transparent !important;
      transition: background-color 0.15s ease !important;
    }
    .dataTables_paginate .paginate_button.current,
    .dataTables_paginate .paginate_button.current:hover {
      background: #f3b24b !important;
      border-color: #f3b24b !important;
      color: #15131e !important;
      font-weight: 700;
    }
    .dataTables_paginate .paginate_button:hover {
      background: rgba(243, 178, 75, 0.15) !important;
      border-color: rgba(243, 178, 75, 0.3) !important;
      color: #15131e !important;
    }
    .dataTables_paginate .paginate_button.disabled,
    .dataTables_paginate .paginate_button.disabled:hover {
      color: #aaaaaa !important;
      background: transparent !important;
    }

    /* ── shinyalert z-index above bslib sidebar ─────────── */
    .swal2-container { z-index: 99999 !important; }
  ")
}


# ── ggplot2 plot theme ────────────────────────────────────────────────────────
theme_conduit <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(fill = "#ffffff", colour = NA),
      plot.background    = ggplot2::element_rect(fill = "#ffffff", colour = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#e8eaed", linewidth = 0.45),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.line    = ggplot2::element_line(colour = "#c5c9d6", linewidth = 0.5),
      axis.ticks   = ggplot2::element_line(colour = "#c5c9d6", linewidth = 0.4),
      axis.text    = ggplot2::element_text(size = ggplot2::rel(0.88), colour = "#444455"),
      axis.title   = ggplot2::element_text(size = ggplot2::rel(0.93), colour = "#1a1a2e"),
      strip.background = ggplot2::element_rect(fill = "#f3b24b", colour = NA),
      strip.text       = ggplot2::element_text(
        colour = "#15131e", face = "bold", size = ggplot2::rel(0.82)
      ),
      legend.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
      legend.key        = ggplot2::element_rect(fill = "#ffffff", colour = NA),
      legend.title      = ggplot2::element_text(size = ggplot2::rel(0.85), face = "bold"),
      legend.text       = ggplot2::element_text(size = ggplot2::rel(0.82)),
      plot.title    = ggplot2::element_text(
        size = ggplot2::rel(1.1), face = "bold", colour = "#15131e",
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = "#6c757d", size = ggplot2::rel(0.9),
        margin = ggplot2::margin(b = 8)
      )
    )
}


# ── Plot color palette ────────────────────────────────────────────────────────
set_color_palette <- function(palette_name = "viridis") {
  if (palette_name %in% c("viridis", "plasma", "magma", "cividis")) {
    options(ggplot2.continuous.fill = palette_name)
    ggplot2::scale_color_continuous <- function(...) ggplot2::scale_color_viridis(discrete = FALSE, option = palette_name, ...)
    ggplot2::scale_fill_continuous  <- function(...) ggplot2::scale_fill_viridis(discrete = FALSE, option = palette_name, ...)
    ggplot2::scale_color_discrete   <- function(...) ggplot2::scale_color_viridis_d(option = palette_name, ...)
    ggplot2::scale_fill_discrete    <- function(...) ggplot2::scale_fill_viridis_d(option = palette_name, ...)
  } else {
    stop("Error: Invalid palette name selected. Choose from 'viridis', 'plasma', 'magma', or 'cividis'.")
  }
}

set_plot_theme <- function(theme_name = "theme_conduit") {
  `%!in%` <- Negate(`%in%`)

  supported_themes <- c(
    "theme_conduit",
    "ggprism",
    "theme_classic",
    "theme_bw",
    "theme_dark",
    "theme_void",
    "theme_light",
    "theme_minimal"
  )

  if (theme_name %!in% supported_themes) {
    stop("Error: Select a supported plot theme")
  }

  if (theme_name == "theme_conduit") {
    ggplot2::theme_set(theme_conduit())
  } else if (theme_name == "ggprism") {
    ggplot2::theme_set(ggprism::theme_prism())
  } else {
    ggplot2::theme_set(get(theme_name, envir = asNamespace("ggplot2"))())
  }
}
