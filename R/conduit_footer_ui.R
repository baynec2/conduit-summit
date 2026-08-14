conduit_footer_ui <- function() {
  tags$footer(
    class = "conduit-footer",
    tags$a(
      href = "https://github.com",
      target = "_blank",
      icon("github"),
      style = "color: #6f6552;"
    ),
    "Developed by Charlie Bayne in the Gonzalez Lab at the University of California San Diego"
  )
}
