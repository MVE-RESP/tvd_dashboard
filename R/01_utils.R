# ================================================================
# 01_utils.R — Fonctions transversales, DB, helpers
# ================================================================
# --- Configuration base de données ---
DB_PATH <- file.path("data", "tvd_database.sqlite")

# Créer le dossier si nécessaire
if (!dir.exists(dirname(DB_PATH))) {
  dir.create(dirname(DB_PATH), recursive = TRUE)
}

connect_tvd_db <- function(db_path = DB_PATH) {
  stopifnot(is.character(db_path), length(db_path) == 1)
  DBI::dbConnect(RSQLite::SQLite(), db_path)
}

# ---- Connexion SQLite résiliente ----
#connect_tvd_db <- function() {
#  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
#  tryCatch(DBI::dbGetQuery(con, "PRAGMA busy_timeout = 30000"),
#           error = function(e) { DBI::dbDisconnect(con); stop(e) })
#  con
#}

with_db <- function(expr) {
  con <- connect_tvd_db(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  eval(substitute(expr), envir = environment())
}

# ---- Helpers texte/nommage ----
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

safe_table_name <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- iconv(x, "", "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- "table_importee"
  if (grepl("^[0-9]", x)) x <- paste0("tbl_", x)
  make.unique(x, sep = "_")
}

normalize_text <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- iconv(x, "", "ASCII//TRANSLIT"); x[is.na(x)] <- ""
  x <- tolower(trimws(x))
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}

normalize_country_key <- function(x) {
  x <- iconv(toupper(trimws(as.character(x))), from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  key <- gsub("[^A-Z0-9]", "", x)
  # Canonicalize common GNARF spellings to ISO3 keys so country names,
  # official names and ISO3 codes all reach the same country reference row.
  country_aliases <- c(
    CABOVERDE = "CPV", CAPEVERDE = "CPV", REPUBLICOFCABOVERDE = "CPV",
    COTEDIVOIRE = "CIV", IVORYCOAST = "CIV", REPUBLICOFCOTEDIVOIRE = "CIV",
    UNITEDREPUBLICOFTANZANIA = "TZA"
  )
  alias_match <- match(key, names(country_aliases))
  is_alias <- !is.na(alias_match)
  key[is_alias] <- unname(country_aliases[alias_match[is_alias]])
  key
}

read_excel_safe <- function(path, sheet = 1, skip = 0) {
  readxl::read_excel(path, sheet = sheet, skip = skip) |> janitor::clean_names()
}

required_columns <- function(df, cols) all(cols %in% names(df))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

# Shared helpers used by both the import and processing modules.
est_fichier_referentiel_pays <- function(x) {
  normalized <- iconv(tolower(basename(x)), from = "", to = "ASCII//TRANSLIT")
  normalized <- gsub("[^a-z0-9]", "", normalized)
  grepl("countrylist|localisation|location|stafflist", normalized)
}

est_feuille_referentiel_pays <- function(x) {
  normalized <- iconv(tolower(as.character(x)), from = "", to = "ASCII//TRANSLIT")
  normalized <- gsub("[^a-z0-9]", "", normalized)
  normalized %in% c("countrylist", "localisation", "location", "stafflist")
}

refresh_analysis_tables <- function(session) {
  con <- connect_tvd_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tables <- DBI::dbListTables(con)
  preferred <- c(intersect("final_analysis_database", tables),
                 setdiff(tables, "final_analysis_database"))
  shiny::updateSelectInput(
    session, "analysis_table", choices = preferred,
    selected = if (length(preferred)) preferred[1] else character(0)
  )
  invisible(preferred)
}

normalize_gnarf_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- tolower(trimws(x))
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}

# ---- Exports génériques réutilisés par tous les produits ----
export_plot_handler <- function(plot_fun, ext,
                                w = 12, h = 7, dpi = 150) {
  shiny::downloadHandler(
    filename = function() paste0("produit_", Sys.Date(), ".", ext),
    content  = function(file) {
      ggplot2::ggsave(file, plot = plot_fun(), device = ext,
                      width = w, height = h, dpi = dpi, bg = "white")
    }
  )
}

export_table_xlsx <- function(data_fun, sheet_name = "Donnees") {
  shiny::downloadHandler(
    filename = function() paste0(sheet_name, "_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(stats::setNames(list(data_fun()), sheet_name), file)
  )
}

export_table_pdf <- function(data_fun, title = "Tableau") {
  shiny::downloadHandler(
    filename = function() paste0("tableau_", Sys.Date(), ".pdf"),
    content  = function(file) {
      df <- utils::head(data_fun(), 60)
      grDevices::pdf(file, width = 12, height = 8, title = title)
      gridExtra::grid.table(df); grDevices::dev.off()
    }
  )
}

# ---- Palette style UN/OMS ----
un_palette <- list(
  blue   = "#009EDB",  # UN primary
  navy   = "#17365D",
  red    = "#C8102E",  # WHO red
  green  = "#4C9F38",  # SDG 3
  orange = "#FD9A24",
  yellow = "#FDB714",
  grey   = "#55606E",
  light  = "#F2F2F2"
)

un_endemic_colors <- c(
  "Yes, country is endemic (report required)" = "#C8102E",
  "No, country is not endemic and will not report for this disease" = "#BDC3C7",
  "No, country is known to be endemic but no surveillance system exists or data are not available" = "#FD9A24",
  "Yes, country is not endemic but reports for surveillance/zero reporting" = "#4C9F38",
  "Never endemic" = "#ECF0F1",
  "not applicable" = "#FFFFFF"
)

theme_un <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", color = un_palette$navy, size = base_size + 4),
      plot.subtitle = ggplot2::element_text(color = un_palette$grey, size = base_size),
      axis.title    = ggplot2::element_text(color = un_palette$navy),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}
