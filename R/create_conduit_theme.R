# Create the conduit theme!
create_conduit_theme = function(){
  # f3b24bff = yellow, #008080ff = light blue, #15131efe = dark blue
  conduit_theme <- create_theme(
    adminlte_color(
      light_blue = "#15131efe"
    ),
    adminlte_sidebar(
      width = "200px",
      dark_bg = "#15131efe",
      dark_hover_bg = "#f3b24bff",
      dark_color = "#FFF"
    ),
    adminlte_global(
      content_bg = "#FFF",
      box_bg = "#D8DEE9",
      info_box_bg = "#D8DEE9"
    )
  )
}


# Plot color pallete

# Function to set color palette globally
set_color_palette <- function(palette_name = "viridis") {
  # Check if the selected palette is a valid viridis palette
  if (palette_name %in% c("viridis", "plasma", "magma", "cividis")) {
    # Set options for ggplot2 to use the selected color palette globally for continuous and discrete colors
    options(ggplot2.continuous.fill = palette_name)

    # Set default scale functions for ggplot2 (both continuous and discrete)
    ggplot2::scale_color_continuous <- function(...) ggplot2::scale_color_viridis(discrete = FALSE, option = palette_name, ...)
    ggplot2::scale_fill_continuous <- function(...) ggplot2::scale_fill_viridis(discrete = FALSE, option = palette_name, ...)
    ggplot2::scale_color_discrete <- function(...) ggplot2::scale_color_viridis_d(option = palette_name, ...)
    ggplot2::scale_fill_discrete <- function(...) ggplot2::scale_fill_viridis_d(option = palette_name, ...)
  } else {
    stop("Error: Invalid palette name selected. Choose from 'viridis', 'plasma', 'magma', or 'cividis'.")
  }
}

set_plot_theme <- function(theme_name = "theme_minimal") {
  `%!in%` <- Negate(`%in%`)

  supported_themes <- c(
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

  if (theme_name == "ggprism") {
    ggplot2::theme_set(ggprism::theme_prism())
  } else {
    ggplot2::theme_set(get(theme_name, envir = asNamespace("ggplot2"))())
  }
}
