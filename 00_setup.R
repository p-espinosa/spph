# Run once BEFORE Wednesday 25 November 2026.
# Needs R >= 4.1. Takes a few minutes the first time.
packages <- c("ggplot2", "dplyr", "readr",   # charts and data
              "shiny", "bslib",              # Shiny apps and dashboards
              "plotly",                      # the value under the mouse
              "svglite")                     # export to SVG
install.packages(packages)

# Check: this should print TRUE seven times
sapply(packages, requireNamespace, quietly = TRUE)

# No R on your laptop, or the install fails? Use https://posit.cloud (free account):
# New Project -> upload this folder (as a .zip) -> run this file there.
