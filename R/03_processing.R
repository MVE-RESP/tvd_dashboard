# ================================================================
# 03_processing.R — ETL : analytique → finale → fusionnée
# ================================================================

# ---------- UI ----------

ui_tab_sqlite <- function() {
  tabItem(
    tabName = "sqlite",
    h4("Datawarehouse SQLite"),
    box(
      title = "Importer des fichiers Excel", status = "primary",
      solidHeader = FALSE, width = 12,
      p("Sélectionnez les fichiers du dossier input/upload à charger dans la base SQLite.",
        class = "text-muted"),
      fluidRow(
        column(6, actionButton("refresh_files", "Actualiser la liste",
                               icon = icon("sync"), class = "btn-default")),
        column(6, uiOutput("sqlite_selection_summary"))
      ),
      tags$hr(),
      uiOutput("sqlite_files_ui"),
      div(class = "well", icon("info-circle"),
          "La synchronisation remplace les tables portant le même nom."),
      actionButton("sync_sqlite", "Synchroniser les fichiers sélectionnés",
                   icon = icon("database"), class = "btn-primary btn-lg"),
      actionButton("build_analysis_db", "Construire / actualiser la base analytique",
                   icon = icon("chart-line"), class = "btn-success btn-lg"),
      uiOutput("download_analysis_db_ui"),
      actionButton("build_final_analysis_db", "Créer / actualiser la base finale pour les analyses",
                   icon = icon("database"), class = "btn-warning btn-lg"),
      uiOutput("download_final_database_ui"),
      br(), br(), uiOutput("sqlite_log_ui"),
      verbatimTextOutput("analysis_db_status")
    ),
    box(title = "Tables présentes dans SQLite", status = "info",
        solidHeader = FALSE, width = 12, DT::DTOutput("sqlite_tables"))
  )
}

ui_tab_fusion <- function() {
  tabItem(
    tabName = "fusion",
    h4("Base NTD Fusionnée"),
    box(
      title = "Génération de la base finale pour les analyses",
      status = "primary", width = 12,
      p("Fusionne final_analysis_database, country_list et indicator en une table unique ntd_fusionnee."),
      p(tags$em("Contrôles qualité automatiques : existence des sources, intégrité des clés, détection des pays/indicateurs manquants et des doublons.")),
      actionButton("build_fusion", "Générer / actualiser la base fusionnée",
                   icon = icon("database"), class = "btn-success btn-lg"),
      downloadButton("download_fusion_excel", "Télécharger (Excel 2 feuilles)",
                     class = "btn-primary"),
      hr(), h4("Journal de traitement et contrôles qualité"),
      verbatimTextOutput("fusion_status"), DT::DTOutput("fusion_journal"),
      hr(), h4("Aperçu des données fusionnées"),
      DT::DTOutput("fusion_preview")
    )
  )
}

# ---------- Fonctions ETL (pures) ----------

deduplicate_autres_bases <- function(data) {
  if (!is.data.frame(data) || nrow(data) < 2L) return(data)
  key_columns <- intersect(
    c("dictionary_indicator_code", "period", "organisation_unit",
      "ntd_age_groups_gnarf", "ntd_sex_with_unknown", "ntd_dimession"),
    names(data)
  )
  if (!length(key_columns)) return(data)
  key_parts <- lapply(key_columns, function(column) {
    value <- tolower(trimws(as.character(data[[column]])))
    value[is.na(value)] <- ""
    if (column == "dictionary_indicator_code") value <- toupper(value)
    if (column == "period") {
      numeric_period <- suppressWarnings(as.numeric(value))
      valid_period <- !is.na(numeric_period)
      value[valid_period] <- format(numeric_period[valid_period],
                                    scientific = FALSE, trim = TRUE)
    }
    value
  })
  key <- do.call(paste, c(key_parts, sep = "\u001f"))
  data[!duplicated(key, fromLast = TRUE), , drop = FALSE]
}

rebuild_epidemiological_analysis <- function(con) {
  tables <- DBI::dbListTables(con)
  source_frames <- list()
  
  get_text <- function(data, candidates, default = NA_character_) {
    column <- intersect(candidates, names(data))
    if (length(column)) as.character(data[[column[1]]]) else rep(default, nrow(data))
  }

  standardize <- function(data, source) {
    if (!is.data.frame(data) || nrow(data) == 0L) return(NULL)
    source_key <- tolower(trimws(source))
    code <- get_text(data, c("dictionary_indicator_code", "indicator_code", "gho_indicator_code"))
    indicator_name <- get_text(data, c("dictionary_indicator_name", "indicator_name", "gnarf_indicator_name", "indicator_original", "data"))
    raw_value <- get_text(data, c("value", "value_numeric", "numeric_value", "endemicity_status_code"))
    explicit_value <- get_text(data, c("value_numeric", "numeric_value"))
    numeric_value <- suppressWarnings(as.numeric(gsub(",", ".", raw_value, fixed = TRUE)))
    preferred <- suppressWarnings(as.numeric(gsub(",", ".", explicit_value, fixed = TRUE)))
    numeric_value[!is.na(preferred)] <- preferred[!is.na(preferred)]

    is_other_source <- source_key == "autres_bases_excel"
    is_endemicity <- is_other_source & (
      grepl("^GNAR-", code, ignore.case = TRUE) |
        grepl("endemicity|endemicite|endémicité", indicator_name, ignore.case = TRUE)
    )
    is_endemicity[is.na(is_endemicity)] <- FALSE
    if (any(is_endemicity)) {
      status_text <- tolower(trimws(raw_value))
      status_text[is.na(status_text)] <- ""
      status_code <- suppressWarnings(as.numeric(gsub(",", ".", status_text, fixed = TRUE)))
      text_status <- rep(NA_real_, length(status_text))
      text_status[grepl("never endemic|jamais endemique", status_text)] <- 5
      text_status[grepl("known to be endemic.*(no surveillance|data.*unavailable)|surveillance.*unavailable", status_text)] <- 3
      text_status[grepl("not endemic.*(reports|reporting|zero)|reports?.*surveillance|rapport.*surveillance", status_text)] <- 4
      text_status[grepl("^no,? country is not endemic|^non[- ]endemique$", status_text)] <- 2
      text_status[grepl("^endemic$|^endemique$|yes,? country is endemic", status_text)] <- 1
      valid_status_code <- !is.na(status_code) & status_code %in% 1:5
      status_code[!valid_status_code] <- text_status[!valid_status_code]
      numeric_value[is_endemicity] <- status_code[is_endemicity]
    }

    gnarf_type <- get_text(data, c("gnarf_type"))
    missing_type <- is.na(gnarf_type) | !nzchar(trimws(gnarf_type))
    if (is_other_source) {
      gnarf_type[missing_type & is_endemicity] <- "endemicite"
      gnarf_type[missing_type & !is_endemicity] <- "numerique"
    }
    ref_data <- switch(source_key,
      "gnarf" = "gnarf",
      "gho" = "gho",
      "autres_bases_excel" = "autres_bases_excel",
      source_key
    )

    organisation_unit <- get_text(
      data,
      c("organisation_unit", "organization_unit", "country", "location")
    )
    organisation_unit_code <- get_text(
      data,
      c("organisation_unit_code", "spatial_dim", "country_iso", "iso3")
    )
    missing_unit <- is.na(organisation_unit) |
      !nzchar(trimws(organisation_unit))
    organisation_unit[missing_unit] <- organisation_unit_code[missing_unit]

    data.frame(
      source = source,
      indicator_ref_data = ref_data,
      dictionary_indicator_code = trimws(code),
      dictionary_indicator_name = trimws(indicator_name),
      period = get_text(data, c("period", "year", "time_dim")),
      organisation_unit = organisation_unit,
      ntd_age_groups_gnarf = get_text(data, c("ntd_age_groups_gnarf", "age", "age_group", "agedim")),
      ntd_sex_with_unknown = get_text(data, c("ntd_sex_with_unknown", "sex", "gender", "sexdim")),
      ntd_dimession = get_text(data, c("ntd_dimession", "dimension", "dim1", "dim2")),
      value = numeric_value,
      status_raw = get_text(data, c("endemicity_status_code", "value")),
      gnarf_type = gnarf_type,
      source_file = get_text(data, c("source_file"), default = source),
      source_imported_at = get_text(data, c("source_imported_at", "imported_at", "validation_saved_at")),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  if ("gnarf_data_validated_structured" %in% tables) {
    source_frames$gnarf <- standardize(DBI::dbReadTable(con, "gnarf_data_validated_structured"), "GNARF")
  } else if ("gnarf_master" %in% tables) {
    source_frames$gnarf <- standardize(DBI::dbReadTable(con, "gnarf_master"), "GNARF")
  } else {
    gnarf_tables <- intersect(c("gnarf_numeric", "gnarf_endemicity"), tables)
    if (length(gnarf_tables)) {
      source_frames$gnarf <- standardize(dplyr::bind_rows(lapply(gnarf_tables, function(table) DBI::dbReadTable(con, table))), "GNARF")
    }
  }
  if ("gho_data_validated" %in% tables) {
    source_frames$gho <- standardize(DBI::dbReadTable(con, "gho_data_validated"), "GHO")
  }
  if ("autres_bases_excel" %in% tables) {
    other_data <- DBI::dbGetQuery(
      con,
      'SELECT rowid AS "tvd_import_order", * FROM "autres_bases_excel" ORDER BY rowid'
    )
    if ("tvd_import_order" %in% names(other_data)) other_data$tvd_import_order <- NULL
    other_data <- deduplicate_autres_bases(other_data)
    source_frames$autres_bases <- standardize(other_data, "autres_bases_excel")
  }
  
  source_frames <- Filter(function(x) !is.null(x) && nrow(x) > 0L, source_frames)
  if (!length(source_frames)) stop("Aucune donnée GNARF, GHO ou Autres bases Excel validée n'est disponible dans SQLite.")
  analysis <- dplyr::bind_rows(source_frames)
  
  if ("indicator" %in% tables) {
    dictionary <- DBI::dbReadTable(con, "indicator")
    dict_code_col <- intersect(c("dictionary_indicator_code", "indicator_code"), names(dictionary))
    dict_name_col <- intersect(c("dictionary_indicator_name", "indicator_name"), names(dictionary))
    if (length(dict_code_col)) {
      dict_code <- toupper(trimws(as.character(dictionary[[dict_code_col[1]]])))
      analysis_code <- toupper(trimws(analysis$dictionary_indicator_code))
      valid <- !is.na(dict_code) & nzchar(dict_code)
      keep <- valid & !duplicated(dict_code)
      dictionary <- dictionary[keep, , drop = FALSE]
      dict_code <- dict_code[keep]
      match_index <- match(analysis_code, dict_code)
      analysis$dictionary_match_status <- ifelse(
        is.na(analysis_code) | !nzchar(analysis_code), "Code absent",
        ifelse(is.na(match_index), "Non trouvé dans indicator", "Correspondance trouvée")
      )
      if (length(dict_name_col)) {
        dict_name <- as.character(dictionary[[dict_name_col[1]]])
        missing_name <- is.na(analysis$dictionary_indicator_name) | !nzchar(trimws(analysis$dictionary_indicator_name))
        fill <- missing_name & !is.na(match_index)
        analysis$dictionary_indicator_name[fill] <- dict_name[match_index[fill]]
      }
      excluded <- c("dictionary_indicator_code", "dictionary_indicator_name", "indicator_code", "indicator_name")
      for (column in setdiff(names(dictionary), excluded)) {
        output_column <- paste0("indicator_", make.names(column))
        if (!output_column %in% names(analysis)) analysis[[output_column]] <- dictionary[[column]][match_index]
      }
    } else {
      analysis$dictionary_match_status <- "Colonne de code absente dans indicator"
    }
  } else {
    analysis$dictionary_match_status <- "Table indicator absente"
  }
  
  analysis$analysis_built_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbWriteTable(con, "epidemiological_analysis", as.data.frame(analysis), overwrite = TRUE, row.names = FALSE)
  nrow(analysis)
}

rebuild_final_analysis_database <- function(con) {
  
  # ============================================================
  # 1. Vérification de la table analytique source
  # ============================================================
  
  if (!DBI::dbExistsTable(con, "epidemiological_analysis")) {
    stop(
      paste(
        "La table 'epidemiological_analysis' est introuvable.",
        "Construisez d’abord Base_analytique_NTD",
        "(base analytique source)."
      )
    )
  }
  
  # ============================================================
  # 2. Vérification du référentiel des pays
  # ============================================================
  
  country_file <- file.path(
    "input",
    "dictionnaire",
    "Country_list.xlsx"
  )
  
  if (!file.exists(country_file)) {
    stop(
      paste0(
        "Le fichier '",
        country_file,
        "' est introuvable."
      )
    )
  }
  
  # ============================================================
  # 3. Lecture de la base analytique et du référentiel pays
  # ============================================================
  
  source_data <- as.data.frame(
    DBI::dbReadTable(
      con,
      "epidemiological_analysis"
    ),
    check.names = FALSE
  )
  
  countries <- as.data.frame(
    readxl::read_excel(
      country_file,
      .name_repair = "minimal"
    ),
    check.names = FALSE
  )
  
  # ============================================================
  # 4. Identification des colonnes du référentiel pays
  # ============================================================
  
  country_code_col <- intersect(
    c(
      "cca3",
      "CCA3",
      "ISO3",
      "iso3",
      "iso_3_code",
      "ISO_3_CODE"
    ),
    names(countries)
  )
  
  country_name_col <- intersect(
    c(
      "name",
      "Name",
      "country_name",
      "Country_name",
      "COUNTRY_NAME"
    ),
    names(countries)
  )
  
  if (length(country_code_col) == 0) {
    stop(
      paste(
        "Country_list.xlsx doit contenir une colonne de code pays.",
        "Noms acceptés : cca3, CCA3, ISO3, iso3,",
        "iso_3_code ou ISO_3_CODE."
      )
    )
  }
  
  if (length(country_name_col) == 0) {
    stop(
      paste(
        "Country_list.xlsx doit contenir une colonne de nom de pays.",
        "Noms acceptés : name, Name, country_name,",
        "Country_name ou COUNTRY_NAME."
      )
    )
  }
  
  # ============================================================
  # 5. Vérification des variables de la table source
  # ============================================================
  
  required <- c(
    "indicator_ref_data",
    "indicator_component_name",
    "indicator_category_indicator",
    "indicator_disease_name",
    "dictionary_indicator_code",
    "dictionary_indicator_name",
    "period",
    "organisation_unit",
    "gnarf_type",
    "ntd_age_groups_gnarf",
    "ntd_sex_with_unknown",
    "indicator_performance_indicator",
    "indicator_type_indicator",
    "value"
  )
  
  missing_columns <- setdiff(
    required,
    names(source_data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "La table 'epidemiological_analysis' ne contient pas ",
        "les colonnes requises suivantes : ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  
  # ============================================================
  # 6. Préparation du référentiel des pays
  # ============================================================
  
  countries <- data.frame(
    country_join_code = toupper(
      trimws(
        as.character(
          countries[[country_code_col[1]]]
        )
      )
    ),
    country_iso = toupper(
      trimws(
        as.character(
          countries[[country_code_col[1]]]
        )
      )
    ),
    country_lookup_name = trimws(
      as.character(
        countries[[country_name_col[1]]]
      )
    ),
    stringsAsFactors = FALSE
  )
  
  # Supprimer les codes manquants ou vides
  countries <- countries[
    !is.na(countries$country_iso) &
      nzchar(countries$country_iso),
    ,
    drop = FALSE
  ]
  
  # Supprimer les noms de pays manquants ou vides
  countries <- countries[
    !is.na(countries$country_lookup_name) &
      nzchar(countries$country_lookup_name),
    ,
    drop = FALSE
  ]
  
  # Conserver une seule ligne par code ISO3
  countries <- countries[
    !duplicated(countries$country_iso),
    ,
    drop = FALSE
  ]

  # Créer une clé de rapprochement par code ISO et par nom du pays.
  # Les exports GNARF utilisent souvent le nom dans organisation_unit.
  countries <- dplyr::bind_rows(
    countries |>
      dplyr::transmute(
        country_join_code = normalize_country_key(country_iso),
        country_iso,
        country_lookup_name
      ),
    countries |>
      dplyr::transmute(
        country_join_code = normalize_country_key(country_lookup_name),
        country_iso,
        country_lookup_name
      )
  ) |>
    dplyr::filter(nzchar(country_join_code)) |>
    dplyr::distinct(country_join_code, .keep_all = TRUE)
  
  # ============================================================
  # 7. Préparation de la table analytique
  # ============================================================
  
  data <- source_data[
    ,
    required,
    drop = FALSE
  ]
  
  data$indicator_ref_data <- trimws(
    as.character(data$indicator_ref_data)
  )
  
  data$organisation_unit <- normalize_country_key(data$organisation_unit)
  
  data$gnarf_type <- tolower(
    trimws(
      as.character(data$gnarf_type)
    )
  )
  
  # Remplacer la virgule décimale par un point puis convertir
  data$value <- suppressWarnings(
    as.numeric(
      gsub(
        ",",
        ".",
        trimws(as.character(data$value)),
        fixed = TRUE
      )
    )
  )
  
  # ============================================================
  # 8. Ajout des noms des pays
  # ============================================================
  
  data <- dplyr::left_join(
    data,
    countries,
    by = c(
      "organisation_unit" = "country_join_code"
    )
  )
  
  # Prefer the canonical name from the country reference for every source;
  # organisation_unit is only a fallback when no dictionary match exists.
  data$country_name <- dplyr::if_else(
    !is.na(data$country_lookup_name) &
      nzchar(data$country_lookup_name),
    data$country_lookup_name,
    data$organisation_unit
  )
  
  data <- dplyr::select(
    data,
    -country_lookup_name
  )
  
  data <- dplyr::filter(
    data,
    !is.na(country_name),
    nzchar(trimws(country_name))
  )
  
  # Si country_iso est absent après la jointure, conserver
  # organisation_unit lorsqu’il s’agit déjà d’un code ISO3.
  missing_country_iso <- is.na(data$country_iso) |
    !nzchar(trimws(data$country_iso))
  
  valid_iso3 <- grepl(
    "^[A-Z]{3}$",
    data$organisation_unit
  )
  
  data$country_iso[
    missing_country_iso & valid_iso3
  ] <- data$organisation_unit[
    missing_country_iso & valid_iso3
  ]
  
  # ============================================================
  # 9. Création du statut d’endémicité
  # ============================================================
  
  data$status_endemicite <- dplyr::case_when(
    data$gnarf_type == "endemicite" &
      data$value == 1 ~
      "Yes, country is endemic (report required)",
    
    data$gnarf_type == "endemicite" &
      data$value == 2 ~
      paste(
        "No, country is not endemic and will not report",
        "for this disease"
      ),
    
    data$gnarf_type == "endemicite" &
      data$value == 3 ~
      paste(
        "No, country is known to be endemic but no",
        "surveillance system exists or data are not available"
      ),
    
    data$gnarf_type == "endemicite" &
      data$value == 4 ~
      paste(
        "Yes, country is not endemic but reports for",
        "surveillance/zero reporting"
      ),
    
    data$gnarf_type == "endemicite" &
      data$value == 5 ~
      "Never endemic",
    
    TRUE ~ "not applicable"
  )
  
  # ============================================================
  # 10. Suppression des doublons
  # ============================================================
  
  data <- dplyr::distinct(
    data,
    dplyr::across(-value),
    .keep_all = TRUE
  )
  
  # ============================================================
  # 11. Exclusion des catégories d’âge non analytiques
  # ============================================================
  
  excluded_ages <- c(
    "NTD - Basic Level Age-group (in yrs)",
    "NTD -Detailed Level Age-group (in yrs)",
    "NTD -Other/Age Groups"
  )
  
  data <- dplyr::filter(
    data,
    is.na(ntd_age_groups_gnarf) |
      !ntd_age_groups_gnarf %in% excluded_ages
  )
  
  # ============================================================
  # 12. Variables de regroupement
  # ============================================================
  
  grouping <- c(
    "indicator_ref_data",
    "indicator_component_name",
    "indicator_category_indicator",
    "indicator_disease_name",
    "dictionary_indicator_code",
    "dictionary_indicator_name",
    "period",
    "country_name",
    "country_iso",
    "gnarf_type",
    "indicator_performance_indicator",
    "indicator_type_indicator",
    "ntd_age_groups_gnarf",
    "ntd_sex_with_unknown",
    "status_endemicite"
  )
  
  # ============================================================
  # 13. Agrégation des données
  # ============================================================
  
  grouped <- data |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(grouping)
      )
    ) |>
    dplyr::summarise(
      total_value = if (
        all(is.na(value))
      ) {
        NA_real_
      } else {
        sum(value, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    dplyr::arrange(
      country_name,
      indicator_disease_name,
      indicator_category_indicator,
      period
    )
  
  # ============================================================
  # 14. Création de la base au format long
  # ============================================================
  
  final_long <- grouped |>
    dplyr::mutate(
      source = tolower(
        trimws(
          as.character(indicator_ref_data)
        )
      ),
      organisation_unit = country_name,
      value = total_value
    )
  
  DBI::dbWriteTable(
    con,
    "final_analysis_long",
    as.data.frame(final_long),
    overwrite = TRUE,
    row.names = FALSE
  )
  
  # ============================================================
  # 15. Création de la base au format large
  # ============================================================
  
  final_data <- grouped |>
    tidyr::pivot_wider(
      names_from = c(
        ntd_age_groups_gnarf,
        ntd_sex_with_unknown
      ),
      values_from = total_value,
      values_fn = list(
        total_value = function(x) {
          if (all(is.na(x))) {
            NA_real_
          } else {
            sum(x, na.rm = TRUE)
          }
        }
      ),
      values_fill = 0,
      names_sep = "_"
    ) |>
    janitor::clean_names()
  
  # ============================================================
  # 16. Fonction de lecture sécurisée des colonnes numériques
  # ============================================================
  
  value_col <- function(column) {
    if (column %in% names(final_data)) {
      result <- suppressWarnings(
        as.numeric(final_data[[column]])
      )
      
      result[is.na(result)] <- 0
      
      return(result)
    }
    
    rep(0, nrow(final_data))
  }
  
  # ============================================================
  # 17. Calcul du nombre total de cas
  # ============================================================
  
  female <- value_col(
    "ntd_all_ages_ntd_female"
  )
  
  male <- value_col(
    "ntd_all_ages_ntd_male"
  )
  
  unknown <- value_col(
    "ntd_all_ages_ntd_unknown_both_sexes"
  )

  # GHO renvoie souvent les indicateurs quantitatifs sans dimension d'âge
  # ni de sexe. Le pivot les place alors dans na_na ; les récupérer dans
  # all_cases au lieu de les perdre lors du calcul des totaux par sexe.
  no_dimension <- value_col("na_na")
  is_gho_quantitative <-
    tolower(trimws(as.character(final_data$indicator_ref_data))) == "gho" &
    tolower(trimws(as.character(final_data$indicator_type_indicator))) == "quanti_ind"
  is_gho_quantitative[is.na(is_gho_quantitative)] <- FALSE
  
  # Utiliser le total femmes + hommes lorsqu’il existe.
  # Sinon, utiliser la valeur « Unknown/Both sexes ».
  sex_total <- female + male
  
  final_data$all_cases <- dplyr::case_when(
    sex_total != 0 ~ sex_total,
    unknown != 0 ~ unknown,
    is_gho_quantitative & !is.na(no_dimension) ~ no_dimension,
    TRUE ~ 0
  )

  # La valeur GHO sans dimension a été transférée vers all_cases.
  if ("na_na" %in% names(final_data)) {
    final_data$na_na[is_gho_quantitative] <- NA_real_
  }
  
  final_data$all_cases[
    !is.finite(final_data$all_cases)
  ] <- 0
  
  # ============================================================
  # 18. Enregistrement de la base finale
  # ============================================================
  
  DBI::dbWriteTable(
    con,
    "final_analysis_database",
    as.data.frame(final_data),
    overwrite = TRUE,
    row.names = FALSE
  )
  
  # Retourner le nombre de lignes créées
  return(nrow(final_data))
}

rebuild_ntd_fusionnee <- function(con, export_excel = TRUE) {
  
  journal <- list()
  log_step <- function(etape, statut, detail = "") {
    journal[[length(journal) + 1L]] <<- data.frame(
      horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      etape = etape, statut = statut, detail = detail,
      stringsAsFactors = FALSE
    )
  }
  
  # ---------- 1. Vérification des sources ----------
  tables <- DBI::dbListTables(con)
  for (t in c("final_analysis_database", "country_list", "indicator")) {
    if (!t %in% tables) {
      log_step("Vérification source", "ERREUR", paste("Table absente :", t))
      stop("Table requise absente : ", t)
    }
    log_step("Vérification source", "OK", t)
  }
  
  final_db   <- DBI::dbReadTable(con, "final_analysis_database")
  countries  <- DBI::dbReadTable(con, "country_list")
  indicators <- DBI::dbReadTable(con, "indicator")
  log_step("Lecture tables", "OK",
           sprintf("final=%d | country=%d | indicator=%d",
                   nrow(final_db), nrow(countries), nrow(indicators)))
  
  # ---------- 2. Normalisation des clés ----------
  # Code indicateur
  ind_code_col <- intersect(c("dictionary_indicator_code", "indicator_code"),
                            names(indicators))[1]
  if (is.na(ind_code_col)) stop("Colonne code indicateur introuvable dans 'indicator'.")
  indicators$.k_ind <- toupper(trimws(as.character(indicators[[ind_code_col]])))
  
  fdb_code_col <- intersect(c("dictionary_indicator_code", "indicator_code"),
                            names(final_db))[1]
  final_db$.k_ind <- toupper(trimws(as.character(final_db[[fdb_code_col]])))
  
  # Clé pays (ISO3) — DOIT fonctionner pour toutes les lignes, y compris gnarf
  iso_country_col <- intersect(c("cca3", "ISO3", "iso3", "country_iso"),
                               names(countries))[1]
  if (is.na(iso_country_col)) stop("Colonne ISO3 absente de country_list.")
  countries$.k_iso <- toupper(trimws(as.character(countries[[iso_country_col]])))
  
  iso_fdb_col <- intersect(c("country_iso", "cca3", "ISO3", "iso3"),
                           names(final_db))[1]
  country_name_fdb_col <- intersect(c("country_name", "country", "organisation_unit"),
                                    names(final_db))[1]
  final_db$.k_iso <- if (!is.na(iso_fdb_col)) {
    normalize_country_key(final_db[[iso_fdb_col]])
  } else {
    rep("", nrow(final_db))
  }
  if (!is.na(country_name_fdb_col)) {
    missing_key <- !nzchar(final_db$.k_iso)
    final_db$.k_iso[missing_key] <- normalize_country_key(
      final_db[[country_name_fdb_col]][missing_key]
    )
  }
  
  log_step("Harmonisation clés", "OK",
           "Clés indicateur et ISO3 normalisées (toupper + trim) pour toutes les lignes")
  
  # ---------- 3. Sélection ciblée des champs de 'countries' ----------
  # On conserve country_iso + les 4 variables de pays à propager partout (y compris gnarf)
  country_name_col <- intersect(c("name", "country_name", "official"), names(countries))[1]
  if (is.na(country_name_col)) country_name_col <- NA_character_
  country_name <- if (!is.na(country_name_col)) {
    as.character(countries[[country_name_col]])
  } else rep(NA_character_, nrow(countries))
  country_iso <- as.character(countries[[iso_country_col]])
  country_attributes <- data.frame(
    country_name_ctry = country_name,
    country_iso_ctry = country_iso,
    population = if ("population" %in% names(countries)) countries$population else NA_real_,
    country_class = if ("country_class" %in% names(countries)) countries$country_class else NA_character_,
    subregion = if ("subregion" %in% names(countries)) countries$subregion else NA_character_,
    area = if ("area" %in% names(countries)) countries$area else NA_real_,
    stringsAsFactors = FALSE
  )
  countries_sel <- dplyr::bind_rows(
    dplyr::mutate(country_attributes, .k_iso = normalize_country_key(country_iso)),
    dplyr::mutate(country_attributes, .k_iso = normalize_country_key(country_name))
  ) |>
    dplyr::filter(nzchar(.k_iso)) |>
    dplyr::distinct(.k_iso, .keep_all = TRUE)
  
  # Champs du référentiel 'indicator' (sans écraser dictionary_indicator_name si déjà présente)
  indicators_sel <- indicators |>
    dplyr::select(.k_ind, dplyr::any_of(c(
      "indicator_ref_data", "indicator_component_name",
      "indicator_category_indicator", "indicator_disease_name",
      "indicator_code", "indicator_target",
      "indicator_performance_indicator", "indicator_type_indicator",
      "dictionary_indicator_name"
    ))) |>
    dplyr::distinct(.k_ind, .keep_all = TRUE)
  
  # ---------- 4. Jointures (left joins universelles) ----------
  fusion <- final_db |>
    dplyr::left_join(indicators_sel, by = ".k_ind", suffix = c("", "_ind")) |>
    dplyr::left_join(countries_sel,  by = ".k_iso")
  
  log_step("Jointures", "OK",
           paste(nrow(fusion), "lignes fusionnées (country info propagée à toutes les lignes, incl. gnarf)"))
  
  # ---------- 5. Construction du schéma cible dans l'ordre exact ----------
  g <- function(df, name, default = NA) {
    if (name %in% names(df)) df[[name]] else rep(default, nrow(df))
  }
  
  fusionnee <- data.frame(
    indicator_ref_data                   = g(fusion, "indicator_ref_data"),
    indicator_component_name             = g(fusion, "indicator_component_name"),
    indicator_category_indicator         = g(fusion, "indicator_category_indicator"),
    indicator_disease_name               = g(fusion, "indicator_disease_name"),
    indicator_code                       = dplyr::coalesce(
      as.character(g(fusion, "indicator_code")),
      as.character(g(fusion, "dictionary_indicator_code"))),
    indicator_target                     = g(fusion, "indicator_target"),
    dictionary_indicator_name            = g(fusion, "dictionary_indicator_name"),
    period                               = g(fusion, "period"),
    
    # ----- Données pays propagées à TOUTES les lignes, y compris gnarf -----
    country_name                         = dplyr::coalesce(
      as.character(g(fusion, "country_name")),
      as.character(g(fusion, "country_name_ctry"))),
    country_iso                          = dplyr::coalesce(
      as.character(g(fusion, "country_iso")),
      as.character(g(fusion, "country_iso_ctry"))),
    population                           = suppressWarnings(as.numeric(g(fusion, "population"))),
    country_class                        = g(fusion, "country_class"),
    subregion                            = g(fusion, "subregion"),
    area                                 = suppressWarnings(as.numeric(g(fusion, "area"))),
    # ----------------------------------------------------------------------
    
    gnarf_type                           = g(fusion, "gnarf_type"),
    indicator_performance_indicator      = g(fusion, "indicator_performance_indicator"),
    indicator_type_indicator             = g(fusion, "indicator_type_indicator"),
    status_endemicite                    = g(fusion, "status_endemicite"),
    na_na                                = dplyr::if_else(
      tolower(trimws(as.character(g(fusion, "indicator_ref_data")))) == "gho" &
        tolower(trimws(as.character(g(fusion, "indicator_type_indicator")))) == "quanti_ind",
      NA_real_,
      suppressWarnings(as.numeric(g(fusion, "na_na")))
    ),
    ntd_all_ages_ntd_female              = suppressWarnings(as.numeric(g(fusion, "ntd_all_ages_ntd_female", 0))),
    ntd_all_ages_ntd_male                = suppressWarnings(as.numeric(g(fusion, "ntd_all_ages_ntd_male", 0))),
    ntd_all_ages_ntd_unknown_both_sexes  = suppressWarnings(as.numeric(g(fusion, "ntd_all_ages_ntd_unknown_both_sexes", 0))),
    all_cases                            = suppressWarnings(as.numeric(g(fusion, "all_cases", 0))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # ---------- 6. Contrôles qualité ciblés ----------
  n_gnarf <- sum(tolower(fusionnee$indicator_ref_data) == "gnarf", na.rm = TRUE)
  n_gnarf_sans_pays <- sum(tolower(fusionnee$indicator_ref_data) == "gnarf" &
                             (is.na(fusionnee$country_iso) | !nzchar(fusionnee$country_iso)),
                           na.rm = TRUE)
  log_step("CQ lignes gnarf", "INFO",
           paste(n_gnarf, "lignes gnarf détectées"))
  log_step("CQ gnarf sans ISO pays",
           if (n_gnarf_sans_pays == 0) "OK" else "AVERTISSEMENT",
           paste(n_gnarf_sans_pays, "lignes gnarf sans correspondance pays"))
  
  gnarf_rows <- tolower(as.character(fusionnee$indicator_ref_data)) == "gnarf"
  for (column in c("population", "country_class", "subregion", "area")) {
    values <- fusionnee[[column]]
    missing <- is.na(values) | !nzchar(trimws(as.character(values)))
    n_missing <- sum(gnarf_rows & missing, na.rm = TRUE)
    log_step(
      paste("CQ gnarf sans", column),
      if (n_missing == 0L) "OK" else "AVERTISSEMENT",
      paste(n_missing, "lignes GNARF sans", column)
    )
  }
  
  n_dup <- sum(duplicated(fusionnee))
  log_step("CQ doublons", if (n_dup == 0) "OK" else "INFO",
           paste(n_dup, "doublons détectés"))
  
  # ---------- 7. Écriture SQLite ----------
  DBI::dbWriteTable(con, "ntd_fusionnee", fusionnee,
                    overwrite = TRUE, row.names = FALSE)
  log_step("Écriture SQLite", "OK", "Table ntd_fusionnee (ré)écrite")
  
  journal_df <- do.call(rbind, journal)
  out_path <- NULL
  if (isTRUE(export_excel)) {
    # ---------- 8. Export Excel (2 feuilles) ----------
    dir.create("output", showWarnings = FALSE, recursive = TRUE)
    out_path <- file.path("output", "base_ntd_fusionnee.xlsx")
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "donnees_fusionnees")
    openxlsx::writeData(wb, "donnees_fusionnees", fusionnee, withFilter = TRUE)
    openxlsx::freezePane(wb, "donnees_fusionnees", firstRow = TRUE)
    openxlsx::setColWidths(wb, "donnees_fusionnees",
                           cols = seq_len(ncol(fusionnee)), widths = "auto")
    openxlsx::addWorksheet(wb, "journal_traitement")
    openxlsx::writeData(wb, "journal_traitement", journal_df, withFilter = TRUE)
    openxlsx::setColWidths(wb, "journal_traitement",
                           cols = seq_len(ncol(journal_df)), widths = "auto")
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
    log_step("Export Excel", "OK", out_path)
    journal_df <- do.call(rbind, journal)
  }
  
  list(rows = nrow(fusionnee), journal = journal_df, path = out_path)
}

# ---------- Module serveur ----------

srv_processing <- function(
    input,
    output,
    session,
    rv,
    sqlite_db_refresh
) {
  
  analysis_db_status <- reactiveVal(
    "La base analytique n'a pas encore été construite."
  )

  output$analysis_db_status <- renderText(analysis_db_status())

  refresh_sqlite_files <- function() {
    files <- list.files(
      "input/upload", pattern = "\\.xlsx$", full.names = FALSE,
      ignore.case = TRUE
    )
    files <- files[!est_fichier_referentiel_pays(files)]
    rv$sqlite_files(files)
    updateCheckboxGroupInput(session, "selected_tables",
                             choices = files, selected = character())
  }
  
  fusion_result <- reactiveVal(NULL)
  
  # ------------------------------------------------------------
  # Construction de la base epidemiological_analysis
  # ------------------------------------------------------------
  # observeEvent(input$build_analysis_db, {
  # })

  
  
  output$fusion_status <- renderText({
    res <- fusion_result()
    if (is.null(res)) return("Base non générée. Cliquez sur le bouton de génération.")
    paste0("Fusion réussie : ", res$rows, " lignes\n",
           "Fichier Excel : ", res$path, "\n",
           "Table SQLite : ntd_fusionnee")
  })
  
  output$fusion_journal <- DT::renderDT({
    res <- fusion_result()
    req(res)
    DT::datatable(res$journal, rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatStyle("statut",
                      backgroundColor = DT::styleEqual(
                        c("OK", "AVERTISSEMENT", "ERREUR", "INFO"),
                        c("#d4edda", "#fff3cd", "#f8d7da", "#d1ecf1")
                      ))
  })
  
  output$fusion_preview <- DT::renderDT({
    req(fusion_result())
    con <- connect_tvd_db(); on.exit(DBI::dbDisconnect(con))
    df <- DBI::dbReadTable(con, "ntd_fusionnee")
    DT::datatable(head(df, 500), rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 10))
  })
  
  
  
  # ------------------------------------------------------------
  # Construction de la base final_analysis_database
  # ------------------------------------------------------------
  # observeEvent(input$build_final_analysis_db, {
  # })
  observeEvent(input$build_analysis_db, {
    con <- NULL
    tryCatch({
      con <- connect_tvd_db()
      analysis_rows <- rebuild_epidemiological_analysis(con)
      country_file <- file.path("input", "dictionnaire", "Country_list.xlsx")
      downstream_status <- ""
      if (file.exists(country_file)) {
        final_rows <- rebuild_final_analysis_database(con)
        fusion_build <- rebuild_ntd_fusionnee(con, export_excel = FALSE)
        downstream_status <- paste0(
          "\nBase finale actualisée : ", final_rows,
          " lignes ; ntd_fusionnee actualisée : ", fusion_build$rows,
          " lignes."
        )
      } else {
        downstream_status <- paste0(
          "\nBase finale non reconstruite : fichier ", country_file,
          " introuvable."
        )
      }
      analysis_db_status(paste0(
        "Base analytique actualisée : ", analysis_rows,
        " observations ; sources GNARF, GHO et autres bases.",
        downstream_status
      ))
      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      refresh_analysis_tables(session)
      showNotification("La base analytique SQLite est prête.", type = "message")
    }, error = function(e) {
      analysis_db_status(paste("Échec de construction :", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })
  })
  
  observeEvent(input$build_final_analysis_db, {
    con <- NULL
    tryCatch({
      con <- connect_tvd_db()
      rows <- rebuild_final_analysis_database(con)
      analysis_db_status(paste0(
        "Base finale actualisée : ", rows,
        " lignes dans final_analysis_database, construite à partir de Country_list.xlsx et Base_analytique_NTD."
      ))
      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      refresh_analysis_tables(session)
      showNotification("La base finale est prête pour les analyses et visualisations.", type = "message")
    }, error = function(e) {
      analysis_db_status(paste("Échec de création de la base finale :", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })
  })
  
  
  
  # ------------------------------------------------------------
  # Construction de la base fusionnée ntd_fusionnee
  # + génération du fichier Excel à deux feuilles
  # ------------------------------------------------------------
  # observeEvent(input$build_fusion, {
  # })
  observeEvent(input$build_fusion, {
    con <- NULL
    tryCatch({
      con <- connect_tvd_db()
      withProgress(message = "Fusion en cours...", value = 0.3, {
        res <- rebuild_ntd_fusionnee(con)
        incProgress(0.7)
        fusion_result(res)
      })
      showNotification(
        paste0("Base fusionnée créée : ", res$rows, " lignes. Excel : ", res$path),
        type = "message", duration = 10
      )
      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      refresh_analysis_tables(session)
    }, error = function(e) {
      showNotification(paste("Erreur fusion :", conditionMessage(e)),
                       type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })
  })
  
  # ------------------------------------------------------------
  # Synchronisation SQLite
  # ------------------------------------------------------------
  # observeEvent(input$sync_sqlite, {
  # })
  
  
  observeEvent(input$sync_sqlite, {
    selected_files <- input$selected_tables
    if (is.null(selected_files) || length(selected_files) == 0L) {
      rv$log_sqlite <- "Sélectionnez au moins un fichier avant de synchroniser."
      showNotification(rv$log_sqlite, type = "warning", duration = 8)
      return()
    }
    selected_files <- selected_files[!est_fichier_referentiel_pays(selected_files)]
    if (length(selected_files) == 0L) {
      rv$log_sqlite <- paste(
        "Aucune donnée synchronisée : Country_list, Localisation/Location et Staff_List",
        "sont des référentiels pays et ne sont pas chargés dans SQLite."
      )
      showNotification(rv$log_sqlite, type = "warning", duration = 8)
      return()
    }
    
    con <- NULL
    saved_count <- 0L
    skipped_sheets <- character()
    
    tryCatch({
      con <- connect_tvd_db()
      
      for (f in selected_files) {
        path <- file.path("input/upload", basename(f))
        
        if (!file.exists(path)) {
          stop(paste("Fichier introuvable :", f))
        }
        
        sheets <- readxl::excel_sheets(path)
        data_sheets <- sheets[!est_feuille_referentiel_pays(sheets)]
        skipped_sheets <- c(skipped_sheets, sheets[est_feuille_referentiel_pays(sheets)])
        if (length(data_sheets) == 0L) next
        preferred <- which(tolower(data_sheets) == "dataset")
        selected_sheet <- if (length(preferred)) data_sheets[preferred[1]] else data_sheets[1]
        df <- read_excel_safe(path, sheet = selected_sheet)
        table_name <- safe_table_name(f)
        
        DBI::dbWriteTable(
          con,
          name = table_name,
          value = as.data.frame(df),
          overwrite = TRUE,
          row.names = FALSE
        )
        saved_count <- saved_count + 1L
      }
      
      rv$log_sqlite <- if (saved_count == 0L) {
        "Aucune feuille de données à synchroniser. Les feuilles Country_list, Localisation/Location et Staff_List sont exclues."
      } else {
        paste0(
          "Synchronisation réussie : ", saved_count, " fichier(s).",
          if (length(skipped_sheets)) paste0(
            " Feuilles de référentiel ignorées : ",
            paste(unique(skipped_sheets), collapse = ", "), "."
          ) else ""
        )
      }
      
      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      showNotification("Synchronisation SQLite terminée.")
    }, error = function(e) {
      rv$log_sqlite <- paste("Erreur :", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    }, finally = {
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    })
    
    
    refresh_analysis_tables(session)
  })
  
  
  # ------------------------------------------------------------
  # Actualisation de la liste des fichiers
  # ------------------------------------------------------------
  # observeEvent(input$refresh_files, {
  # })
  observeEvent(input$refresh_files, {
    refresh_sqlite_files()
  })
  # ------------------------------------------------------------
  # Téléchargements
  # ------------------------------------------------------------
  output$download_fusion_excel <- downloadHandler(
    filename = function() paste0("base_ntd_fusionnee_", Sys.Date(), ".xlsx"),
    content = function(file) {
      src <- file.path("output", "base_ntd_fusionnee.xlsx")
      if (!file.exists(src)) stop("Générez d'abord la base fusionnée.")
      file.copy(src, file, overwrite = TRUE)
    }
  )
  output$download_analysis_db <- downloadHandler(
    filename = function() "Base_analytique_NTD.xlsx",
    content = function(file) {
      con <- connect_tvd_db()
      analysis_data <- tryCatch({
        if (!DBI::dbExistsTable(con, "epidemiological_analysis")) {
          stop("Construisez d’abord la base analytique.")
        }
        DBI::dbReadTable(con, "epidemiological_analysis")
      }, finally = {
        DBI::dbDisconnect(con)
      })
      writexl::write_xlsx(
        list(Analyse = as.data.frame(analysis_data)),
        path = file
      )
    }
  )
  output$download_final_database_ui <- renderUI({
    sqlite_db_refresh()
    con <- connect_tvd_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    if (!DBI::dbExistsTable(con, "final_analysis_database")) return(NULL)
    downloadButton(
      "download_final_database",
      "Télécharger la base finale (Excel)",
      icon = icon("download"),
      class = "btn-info"
    )
  })
  
  output$download_final_database <- downloadHandler(
    filename = function() "Base_finale_NTD.xlsx",
    content = function(file) {
      con <- connect_tvd_db()
      final_data <- tryCatch({
        if (!DBI::dbExistsTable(con, "final_analysis_database")) {
          stop("Créez d’abord la base finale.")
        }
        DBI::dbReadTable(con, "final_analysis_database")
      }, finally = {
        DBI::dbDisconnect(con)
      })
      writexl::write_xlsx(list(Base_finale = as.data.frame(final_data)), path = file)
    }
  )
  
  
  # IMPORTANT :
  # Tous les blocs copiés depuis app.R doivent appeler
  # sqlite_db_refresh()
  # après une mise à jour de la base afin de notifier
  # les autres modules.
}
