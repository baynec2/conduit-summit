conduit_footer_ui <- function() {
  dashboardFooter(
    right = "Developed by Charlie Bayne in the Gonzalez Lab at the University of California San Diego",
    left = tagList(
      socialButton(href = "https://github.com", icon = icon("github"))
    )
  )
}
