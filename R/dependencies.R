packages <- c(
  "shiny", "shinydashboard", "shinydashboardPlus", "DT", "readxl", "writexl",
  "ggplot2", "scales", "DBI", "RSQLite", "sf", "dplyr", "tidyr", "janitor",
  "httr", "jsonlite", "leaflet", "rmarkdown", "knitr", "rmapshaper",
  "openxlsx", "plotly", "kableExtra", "flextable", "showtext", "sysfonts",
  "patchwork", "viridis", "shinycssloaders", "gridExtra", "officer", "base64enc"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}


