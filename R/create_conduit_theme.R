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
