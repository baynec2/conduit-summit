conduit_about_tab_ui = function(){
  tabItem(
    "about",
    fluidRow(
      # Column for the flip box and information box stacked vertically
      column(
        width = 8, offset = 2, # Adjust the column width and offset as needed
        # Custom styles to divide the height equally between logo and info box
        tags$style(
          "
        .logo-box {
          height: 50vh;  /* 50% of the screen height for the logo */
          display: flex;
          justify-content: center;
          align-items: center;
        }
        "
        ),
        # Logo section with flip box
        div(
          class = "logo-box", # Apply custom class to the logo section
          flipBox(
            id = "about_flipbox",
            front = div(
              tags$img(
                src = "conduit-summit.png",
                width = "150%", # Adjusted to be responsive with respect to the container
                height = "auto" # Adjust the width to 50% of the container width
              )
            ),
            back = div(
              p("Will add something here")
            )
          )
        ),
        # Information box section
        div(
          class = "info-box", # Apply custom class to the info section
          box(
            title = "About Conduit-Summit",
            width = NULL, # Default width
            solidHeader = TRUE,
            status = "primary", # You can change the status to other options like "warning"
            p(strong("Conduit-Summit is a tool designed to help analyze metaproteomics data.")),
            br(), # Adds space for visual separation
            p("It is a full-featured suite of tools that allows you to:"),
            tags$ul(
              tags$li("Perform QC of your data"),
              tags$li("Assess the Taxonomic Composition in your experiment"),
              tags$li("Transform, Normalize, and Impute data"),
              tags$li("Perform statistical analyses"),
              tags$li("Prepare publication-quality plots"),
              tags$li("Traverse relationships across data aggregation levels"),
              tags$li("Assess Biomarkers"),
              tags$li("and more!")
            ),
            br(), # Adds space for visual separation
            p(
              "It operates on the data output from the ",
              a("snakemake metaproteomics workflow named Conduit-Ascent", href = "https://github.com/baynec2/conduit-ascent")
            ),
            p("Simpily put: "),
            tags$ul(
              tags$li("Conduit-Ascent tackles the heavy lifting on the climb"),
              tags$li(strong("Conduit-Summit lets you take in the view from the top!"))
            )
          )
        )
      )
    )
  )
}
