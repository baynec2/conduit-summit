# Project Guidelines

## Project Stack

This project uses R/Shiny with bslib, renv for dependency management, and Docker for deployment. Key packages: dplyr, AnnotationDbi, matrixStats, plotly, shinytest2. Watch for namespace conflicts between dplyr::select and AnnotationDbi::select.

## Shiny Development

When fixing Shiny modules, ALWAYS check for missing ns() namespace qualification on input IDs in renderUI/dynamicUI. This is the #1 recurring bug source.

## UI/Styling Guidelines

After making UI/CSS changes, verify text contrast and readability before committing. Dark text on dark backgrounds has been a recurring issue.

When making iterative UI/design changes (sizing, colors, layouts), make SMALL incremental changes and confirm with the user before proceeding. Do not make aggressive changes that may need reverting.

## Git Workflow

When committing changes, always run `git status` first and stage ALL modified files including generated files like renv/activate.R. Do not assume files are staged.
