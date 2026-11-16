# ================================================================
# 02_import_validation.R — Imports Excel/API, validation, mapping
# ================================================================

# Schéma minimal attendu lors de la validation des observations GHO.
gho_canonical_columns <- c(
  "period", "organisation_unit", "organisation_unit_code", "value",
  "dictionary_indicator_code", "dictionary_indicator_name"
)

# Met à niveau les anciennes tables SQLite avant d'y ajouter un nouvel import.
# Les colonnes absentes sont ajoutées sans toucher aux lignes déjà enregistrées.
ensure_gho_table_schema <- function(con, data) {
  table <- "gho_data_validated"
  if (!DBI::dbExistsTable(con, table)) return(invisible(TRUE))

  existing <- DBI::dbListFields(con, table)
  missing <- setdiff(names(data), existing)
  if (!length(missing)) return(invisible(TRUE))

  sql_type <- function(x) {
    if (inherits(x, c("POSIXct", "Date"))) return("TEXT")
    if (is.integer(x) || is.numeric(x)) return("REAL")
    if (is.logical(x)) return("INTEGER")
    "TEXT"
  }
  quote_id <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))

  for (column in missing) {
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ", quote_id(table), " ADD COLUMN ",
        quote_id(column), " ", sql_type(data[[column]])
      )
    )
  }
  invisible(TRUE)
}

# Associe les colonnes retournées par l'API GHO au schéma utilisé par le module.
gho_mapping_table <- function(data) {
  data <- as.data.frame(data, check.names = FALSE)
  source_names <- names(data)
  aliases <- list(
    period = c("time_dim", "time_dimension", "year", "period"),
    organisation_unit = c("country_name"),
    organisation_unit_code = c("spatial_dim"),
    value = c("numeric_value", "value", "obs_value", "observation_value")
  )

  rows <- lapply(names(aliases), function(target) {
    candidates <- aliases[[target]]
    source <- candidates[candidates %in% source_names]
    source <- if (length(source)) source[[1]] else NA_character_
    required <- target %in% c("period", "organisation_unit_code", "value")
    data.frame(
      variable_cible = target,
      variable_source = source,
      statut = if (is.na(source)) {
        if (required) "OBLIGATOIRE : absente" else "OPTIONNELLE : absente"
      } else {
        "Correspondance automatique"
      },
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

# Résout le code choisi dans le catalogue GHO local ou son cache SQLite.
gho_lookup_indicator <- function(code) {
  code <- if (is.null(code)) "" else trimws(as.character(code))
  if (length(code) != 1L || is.na(code) || !nzchar(code)) {
    return(list(matched = FALSE, code = code, name = NA_character_))
  }

  catalogue <- tryCatch({
    con <- connect_tvd_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    if ("gho_catalogue" %in% DBI::dbListTables(con)) {
      DBI::dbReadTable(con, "gho_catalogue")
    } else data.frame()
  }, error = function(e) data.frame())
  local <- tryCatch(load_local_gho(), error = function(e) data.frame())
  catalogue <- dplyr::bind_rows(catalogue, local)
  if (!all(c("indicator_code", "indicator_name") %in% names(catalogue)) ||
      !nrow(catalogue)) {
    return(list(matched = FALSE, code = code, name = NA_character_))
  }

  hit <- which(toupper(trimws(as.character(catalogue$indicator_code))) == toupper(code))
  if (!length(hit)) {
    return(list(matched = FALSE, code = code, name = NA_character_))
  }
  name <- trimws(as.character(catalogue$indicator_name[hit[1]]))
  list(matched = TRUE, code = code,
       name = if (is.na(name) || !nzchar(name)) code else name)
}

# ---------- UI de chaque onglet import ----------
ui_tab_comp_dis <- function() {
  tabItem(
    tabName = "comp_dis",
    h4("Importation Components & Maladies"),
    
    box(
      title = "Modèle Excel",
      status = "primary",
      width = 12,
      downloadButton("download_tpl_comp", "Télécharger le modèle")
    ),
    
    box(
      title = "Importation et validation",
      status = "info",
      width = 12,
      fileInput("file_comp", "Fichier Excel", accept = ".xlsx"),
      actionButton(
        "validate_comp",
        "Valider et sauvegarder",
        class = "btn-success"
      ),
      hr(),
      DTOutput("check_results_comp"),
      uiOutput("download_copies_ui")
    )
  )
}

ui_tab_indicators <- function() {
  tabItem(
    tabName = "indicators",
    h4("Gestion des indicateurs"),
    box(
      title = "Modèle Excel", width = 12,
      downloadButton("download_tpl_ind",
                     "Télécharger le modèle")
    ),
    box(
      title = "Importation", width = 12,
      fileInput("file_ind", "Fichier Excel",
                accept = ".xlsx"),
      actionButton("validate_ind",
                   "Valider et sauvegarder",
                   class = "btn-success"),
      downloadButton(
        "download_indicators_excel",
        "Télécharger la liste des indicateurs (Excel)",
        class = "btn-primary"
      ),
      hr(),
      verbatimTextOutput("log_ind"),
      DTOutput("table_ind")
    )
  )
}

ui_tab_gnarf <- function() {
  tabItem(
    tabName = "gnarf",
    h4("Importation GNARF : données numériques et endémicité"),
    
    box(
      title = "1. Dictionnaire de référence des indicateurs",
      width = 12,
      p("Le mapping GNARF est vérifié avec les codes et les noms présents dans le fichier indicator du dossier input/dictionnaire."),
      verbatimTextOutput("indicator_dictionary_status"),
      actionButton(
        "reload_indicator_dictionary",
        "Recharger le dictionnaire indicator",
        class = "btn-primary"
      )
    ),
    
    box(
      title = "2. Importer les deux fichiers GNARF",
      width = 12,
      p("Fichier numérique : prévalence, incidence, mortalité et prise en charge. Fichier endémicité : statut d'endémicité uniquement. Les deux fichiers peuvent être importés séparément ou ensemble."),
      fluidRow(
        column(
          width = 6,
          fileInput(
            "file_gnarf_numeric",
            "Fichier GNARF numérique (.xls ou .xlsx)",
            accept = c(".xls", ".xlsx")
          ),
          actionButton(
            "import_gnarf_numeric",
            "Importer les données numériques",
            class = "btn-info"
          )
        ),
        column(
          width = 6,
          fileInput(
            "file_gnarf_endemicity",
            "Fichier GNARF endémicité (.xls ou .xlsx)",
            accept = c(".xls", ".xlsx")
          ),
          actionButton(
            "import_gnarf_endemicity",
            "Importer le statut d'endémicité",
            class = "btn-info"
          )
        )
      ),
      actionButton(
        "import_both_gnarf",
        "Importer les deux fichiers et préparer le mapping",
        class = "btn-primary",
        icon = icon("upload")
      ),
      verbatimTextOutput("log_gnarf")
    ),
    
    
    #  DTOutput("table_gnarf_diff"),
    
    #    downloadButton(
    #      "download_gnarf_manual_mapping",
    #      "Télécharger la base à mapper",
    #      class = "btn-warning"
    #    ),
    
    br(),
    # br(),
    
    #    selectInput(
    #      "selected_indicator_code",
    #     "Indicateur du dictionnaire indicator à associer",
    #       choices = NULL
    #      ),
    
    # Étape 3 affichée uniquement si des indicateurs n'ont pas de correspondance automatique.
    uiOutput("gnarf_validation_ui"),
    
    box(
      title = "Validation finale et sauvegarde",
      width = 12,
      p("Les correspondances exactes par code ou par nom sont validées automatiquement. Vérifiez uniquement les indicateurs affichés à l'étape 3 lorsqu'une correspondance est introuvable ou ambiguë."),
      checkboxInput(
        "approve_gnarf_save",
        "Je confirme la validation des données GNARF.",
        value = FALSE
      ),
      actionButton(
        "save_validated_gnarf",
        "Valider et enregistrer les deux tables dans SQLite",
        class = "btn-success"
      ),
      verbatimTextOutput("gnarf_validation_status")
    ),
    
    box(
      title = "4. Fusionner les deux tables en une base GNARF unique",
      width = 12,
      p("Après importation et validation des correspondances, fusionnez la table numérique et la table d'endémicité. Les colonnes propres à chaque fichier sont conservées ; les colonnes absentes d'une table sont remplies par NA."),
      actionButton(
        "merge_gnarf_tables",
        "Fusionner les deux tables GNARF",
        class = "btn-success",
        icon = icon("layer-group")
      ),
      downloadButton(
        "download_gnarf_master",
        "Télécharger la base GNARF fusionnée"
      ),
      verbatimTextOutput("gnarf_merge_status"),
      DTOutput("table_gnarf_master")
    )
  )
}

ui_tab_other_db <- function() {
  tabItem(
    tabName = "other_db",
    
    fluidRow(
      box(
        title = "Modèle Excel standardisé",
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        
        p(
          "Téléchargez le modèle Excel standardisé, renseignez les
        variables requises, puis importez le fichier complété."
        ),
        
        downloadButton(
          outputId = "download_modele_autres_bases",
          label = "Télécharger le modèle Excel",
          class = "btn-primary"
        ),
        
        br(), br(),
        
        fileInput(
          inputId = "import_autres_bases_excel",
          label = "Importer un fichier Excel complété",
          accept = c(".xlsx", ".xls")
        ),
        
        # Vérification des correspondances
        actionButton(
          "verifier_codes_autres_bases",
          "Vérifier les codes indicateurs",
          icon = icon("search"),
          class = "btn-warning"
        ),
        
        br(), br(),
        
        # Résultat de la vérification
        uiOutput("statut_correspondance_autres_bases"),
        
        DT::DTOutput("table_codes_non_correspondants"),
        
        br(),
        
        # Actions disponibles uniquement après validation
        uiOutput("actions_autres_bases_validees"),
        
        
        actionButton(
          inputId = "verifier_autres_bases_excel",
          label = "Vérifier les données",
          icon = icon("check-circle"),
          class = "btn-success"
        ),
        actionButton(
          inputId = "sauvegarder_autres_bases_sqlite",
          label = "Sauvegarder dans SQLite",
          icon = icon("database"),
          class = "btn-primary"
        )
      )
    ),
    
    fluidRow(
      valueBoxOutput("autres_bases_nb_lignes", width = 4),
      valueBoxOutput("autres_bases_nb_indicateurs", width = 4),
      valueBoxOutput("autres_bases_nb_pays", width = 4)
    ),
    
    fluidRow(
      box(
        title = "Aperçu des données importées",
        status = "info",
        solidHeader = FALSE,
        width = 12,
        DT::DTOutput("table_autres_bases_excel")
      )
    ),
    
    fluidRow(
      box(
        title = "Contrôle qualité",
        status = "warning",
        solidHeader = FALSE,
        width = 12,
        DT::DTOutput("table_autres_bases_erreurs")
      )
    )
  )

}

ui_tab_country_loc <- function() {
  tabItem(
    tabName = "country_localisation",
    h4("Référentiels pays et localisation"),
    fluidRow(
      box(
        title = "1. Télécharger le modèle Excel",
        status = "primary",
        solidHeader = FALSE,
        width = 6,
        p("Le modèle contient les trois feuilles attendues : Localisation, Country_list et Staff_List."),
        downloadButton(
          "download_country_localisation_template",
          "Télécharger les modèles Excel",
          class = "btn-primary"
        )
      ),
      box(
        title = "2. Importer et vérifier le classeur",
        status = "info",
        solidHeader = FALSE,
        width = 6,
        fileInput(
          "country_localisation_file",
          "Classeur Excel à traiter",
          accept = ".xlsx"
        ),
        actionButton(
          "process_country_localisation",
          "Séparer les feuilles et vérifier les pays",
          icon = icon("search"),
          class = "btn-success"
        )
      )
    ),
    uiOutput("country_localisation_status"),
    conditionalPanel(
      condition = "output.country_localisation_ready == 'TRUE'",
      fluidRow(
        box(
          title = "Feuilles importées",
          status = "info",
          solidHeader = FALSE,
          width = 12,
          tabsetPanel(
            tabPanel("Localisation", DT::DTOutput("country_preview_localisation")),
            tabPanel("Country_list", DT::DTOutput("country_preview_list")),
            tabPanel("Staff_List", DT::DTOutput("country_preview_staff"))
          )
        )
      ),
      fluidRow(
        box(
          title = "3. Vérifier les correspondances des pays",
          status = "warning",
          solidHeader = FALSE,
          width = 12,
          p("Les noms standards viennent de la feuille Localisation. Vérifiez ou modifiez la proposition pour chaque nom du champ Countries avant d’enregistrer les copies."),
          h4("Noms de Country_list rapprochés par code cca3"),
          DT::DTOutput("country_list_match_review"),
          h4("Noms de pays dans Staff_List — modifiables"),
          DT::DTOutput("country_match_review"),
          br(),
          actionButton(
            "apply_country_corrections",
            "Appliquer les modifications et créer les copies",
            icon = icon("save"),
            class = "btn-primary btn-lg"
          )
        )
      )
    ),
    uiOutput("country_localisation_save_status")
  )
}

ui_tab_gho_api <- function() {
  tabItem(
    tabName = "gho_api",
    h4("Global Health Observatory (GHO)"),
    
    box(
      title = "1. Configuration et importation API",
      status = "primary",
      width = 12,
      textInput(
        inputId = "gho_url",
        label = "URL de base",
        value = "https://ghoapi.azureedge.net/api/"
      ),
      
      
      
      selectizeInput(
        inputId = "gho_ind_code",
        label = "Code de l'indicateur API",
        choices = NULL,
        selected = NULL,
        options = list(placeholder = "Rechercher un indicateur…", maxOptions = 100)
      ),
      
      
      
      sliderInput(
        inputId = "gho_dates",
        label = "Années souhaitées",
        min = 2000,
        max = as.integer(format(Sys.Date(), "%Y")),
        value = c(2000, as.integer(format(Sys.Date(), "%Y"))),
        step = 1,
        sep = "",
        ticks = TRUE,
        width = "100%"
      ),
      
      actionButton(
        "fetch_gho",
        "Importer via API",
        class = "btn-primary",
        icon = icon("cloud-download-alt")
      ),
      
      verbatimTextOutput("gho_log")
    ),
    
    box(
      title = "2. Aperçu des données GHO importées",
      status = "info",
      width = 12,
      
      DTOutput("gho_preview"),
      
      downloadButton(
        "download_gho_raw",
        "Télécharger les données brutes",
        class = "btn-default"
      )
    ),
    
    box(
      title = "3. Correspondance et validation",
      status = "warning",
      width = 12,
      
      p(
        "Vérifiez les correspondances entre les colonnes GHO ",
        "et le schéma canonique GNARF avant de sauvegarder."
      ),
      
      actionButton(
        "verifier_correspondances_gho",
        "Vérifier les correspondances GHO → GNARF",
        class = "btn-warning",
        icon = icon("search")
      ),
      
      br(), br(),
      
      uiOutput("gho_mapping_status"),
      
      DTOutput("gho_mapping_preview"),
      
      h4("Différences et contrôles qualité"),
      DTOutput("gho_diff"),
      
      checkboxInput(
        "approve_gho_save",
        "Je confirme la validation de la base GHO restructurée.",
        value = FALSE
      ),
      
      actionButton(
        "valider_sauvegarder_gho",
        "Valider et sauvegarder dans SQLite",
        class = "btn-success",
        icon = icon("database")
      ),
      
      hr(),
      
      verbatimTextOutput("gho_validation_status"),
      
      DTOutput("gho_validated_preview"),
      
      downloadButton(
        "download_gho_validated",
        "Télécharger la base GHO validée",
        class = "btn-primary"
      )
    )
  )
}

# ---------- Fonctions pures (hors Shiny) ----------

normalize_indicator <- function(x) {
  normalize_text(x)
}

normalize_gho_name <- function(x) {
  normalize_text(x)
}

# Codes ISO3 autorisés pour l'import GHO : Région africaine de l'OMS.
WHO_AFRO_GHO_ISO3 <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
  "COM", "COD", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN",
  "GNB", "CIV", "KEN", "LSO", "LBR", "MDG", "MWI", "MLI", "MRT", "MUS",
  "MOZ", "NAM", "NER", "NGA", "COG", "RWA", "STP", "SEN", "SYC", "SLE",
  "ZAF", "SSD", "TGO", "UGA", "TZA", "ZMB", "ZWE"
)

split_gnarf_indicator <- function(x) {
  original <- trimws(as.character(x))
  original[is.na(original)] <- ""
  
  code <- rep(NA_character_, length(original))
  name <- original
  nonempty <- nzchar(original)
  
  # Cas particulier : indicateurs d'introduction d'une maladie.
  intro <- nonempty & grepl(
    "^GNARF[_[:space:]/-]+INTRODUCTION[_[:space:]/-]+.+$",
    original,
    ignore.case = TRUE
  )
  
  if (any(intro)) {
    disease <- sub(
      "^GNARF[_[:space:]/-]+INTRODUCTION[_[:space:]/-]+",
      "",
      original[intro],
      ignore.case = TRUE
    )
    disease <- trimws(disease)
    disease <- toupper(gsub("[_[:space:]/-]+", "-", disease))
    disease <- gsub("^-+|-+$", "", disease)
    code[intro] <- paste0("GNAR-INTR0-", disease)
    # Le nom conserve le libellé complet original pour la traçabilité.
    name[intro] <- paste0("Endemicity ", disease)#original[intro]
  }
  
  # Cas général : consommer les blocs initiaux en majuscules/chiffres.
  # Les séparateurs autorisés sont tiret, underscore, espace, slash et antislash.
  regular <- nonempty & !intro
  if (any(regular)) {
    sep <- "[-_[:space:]/\\\\]+"
    # Les blocs du code doivent être entièrement en majuscules/chiffres.
    # Le premier bloc contenant une minuscule marque le début du nom.
    pattern <- paste0(
      "^([A-Z0-9]+(?:", sep, "[A-Z0-9]+)*)", sep,
      "(.+)$"
    )
    matches <- regexec(pattern, original[regular], perl = TRUE)
    pieces <- regmatches(original[regular], matches)
    idx <- which(regular)
    
    for (j in seq_along(pieces)) {
      part <- pieces[[j]]
      if (length(part) >= 3L) {
        code[idx[j]] <- toupper(gsub("[-_[:space:]/\\\\]+", "-", part[2]))
        name[idx[j]] <- trimws(part[3])
      }
    }
  }
  
  data.frame(
    gnarf_indicator_original = original,
    gnarf_indicator_code = code,
    gnarf_indicator_name = name,
    stringsAsFactors = FALSE
  )
}

disambiguate_gnarf_codes <- function(labels) {
  
  labels <- as.character(labels)
  labels[is.na(labels)] <- ""
  
  parsed <- split_gnarf_indicator(labels)
  
  result <- parsed$gnarf_indicator_code
  
  valid <- !is.na(result) & nzchar(result)
  
  if (!any(valid)) {
    return(result)
  }
  
  # Mots génériques à ignorer dans la recherche distinctive
  stop_words <- c(
    "number", "of", "the", "a", "an", "and", "or",
    "for", "from", "in", "on", "with", "by", "to",
    "all", "total", "reported", "reporting",
    "indicator", "indicators", "data", "value",
    "cases", "case", "disease", "diseases",
    "number", "rate", "proportion", "percentage",
    "prevalence", "incidence", "status", "human"
  )
  
  # Nettoyer un libellé pour produire un suffixe
  make_suffix <- function(label, other_labels) {
    
    label_norm <- normalize_gnarf_text(label)
    other_norm <- normalize_gnarf_text(other_labels)
    
    words <- unique(strsplit(label_norm, " +")[[1]])
    
    words <- words[
      nzchar(words) &
        !words %in% stop_words &
        nchar(words) >= 2
    ]
    
    # Priorité aux mots qui distinguent ce libellé
    # des autres libellés possédant le même code.
    other_words <- unlist(
      strsplit(other_norm, " +"),
      use.names = FALSE
    )
    
    distinctive <- words[!words %in% other_words]
    
    # Si aucun mot totalement distinct n'existe,
    # conserver les mots significatifs du libellé.
    if (length(distinctive) == 0L) {
      distinctive <- words
    }
    
    if (length(distinctive) == 0L) {
      distinctive <- "INDICATOR"
    }
    
    suffix <- toupper(paste(distinctive, collapse = "-"))
    suffix <- gsub("[^A-Z0-9-]", "", suffix)
    suffix <- gsub("-+", "-", suffix)
    suffix <- gsub("^-|-$", "", suffix)
    
    suffix
  }
  
  # Identifier les groupes de codes répétés.
  code_groups <- split(which(valid), result[valid])
  
  for (group_name in names(code_groups)) {
    
    idx <- code_groups[[group_name]]
    
    # Ne rien changer si le code est unique.
    if (length(idx) < 2L) {
      next
    }
    
    group_labels <- labels[idx]
    group_names <- parsed$gnarf_indicator_name[idx]
    
    # Utiliser le nom extrait, ou le libellé original
    # si le nom extrait est vide.
    group_names[
      is.na(group_names) | !nzchar(trimws(group_names))
    ] <- group_labels[
      is.na(group_names) | !nzchar(trimws(group_names))
    ]
    
    suffixes <- vapply(
      seq_along(idx),
      function(j) {
        others <- group_names[-j]
        
        make_suffix(
          group_names[j],
          others
        )
      },
      character(1)
    )
    
    # Garantir un suffixe unique même si les libellés
    # ne possèdent pas de mot distinctif exploitable.
    suffixes <- make.unique(suffixes, sep = "-VAR-")
    
    result[idx] <- paste0(group_name, "-", suffixes)
  }
  
  result
}


map_one_gnarf_label <- function(
    dataset_type,
    label,
    dictionary,
    code_override = NULL
) {
  
  parsed <- parse_gnarf_label(label)
  
  code <- parsed$gnarf_indicator_code[1]
  name <- parsed$gnarf_indicator_name[1]
  
  # Utiliser le code différencié lorsqu'il est fourni.
  if (!is.null(code_override) &&
      length(code_override) == 1L &&
      !is.na(code_override) &&
      nzchar(code_override)) {
    code <- code_override
  }
  
  
  norm_label <- normalize_gnarf_text(label)
  norm_name <- normalize_gnarf_text(name)
  
  result <- data.frame(
    gnarf_type = dataset_type,
    gnarf_indicator_original = as.character(label),
    gnarf_indicator_code = code,
    gnarf_indicator_name = name,
    dictionary_indicator_code = NA_character_,
    dictionary_indicator_name = NA_character_,
    mapping_method = "Aucune correspondance exacte",
    mapping_status = "À mapper manuellement",
    stringsAsFactors = FALSE
  )
  
  if (is.null(dictionary) || nrow(dictionary) == 0L) {
    return(result)
  }
  
  dictionary$indicator_code <- trimws(
    as.character(dictionary$indicator_code)
  )
  
  dictionary$indicator_name <- trimws(
    as.character(dictionary$indicator_name)
  )
  
  # 1. Recherche par code exact
  code_candidates <- unique(c(
    as.character(label),
    as.character(code)
  ))
  
  code_candidates <- code_candidates[
    !is.na(code_candidates) &
      nzchar(trimws(code_candidates))
  ]
  
  code_hits <- which(
    toupper(dictionary$indicator_code) %in%
      toupper(code_candidates)
  )
  
  if (length(code_hits) > 0L) {
    pairs <- unique(
      dictionary[code_hits,
                 c("indicator_code", "indicator_name"),
                 drop = FALSE]
    )
    
    if (nrow(pairs) == 1L) {
      result$dictionary_indicator_code <- pairs$indicator_code[1]
      result$dictionary_indicator_name <- pairs$indicator_name[1]
      result$mapping_method <- "Code exact"
      result$mapping_status <-
        "Correspondance automatique confirmée"
      
      return(result)
    }
    
    result$mapping_method <- "Code ambigu"
    result$mapping_status <- "À mapper manuellement"
    
    return(result)
  }
  
  # 2. Recherche par nom normalisé
  dictionary_names <- normalize_gnarf_text(dictionary$indicator_name)
  
  names_to_compare <- unique(c(
    norm_label,
    norm_name
  ))
  
  names_to_compare <- names_to_compare[
    !is.na(names_to_compare) &
      nzchar(names_to_compare)
  ]
  
  name_hits <- which(
    dictionary_names %in% names_to_compare
  )
  
  if (length(name_hits) > 0L) {
    pairs <- unique(
      dictionary[name_hits,
                 c("indicator_code", "indicator_name"),
                 drop = FALSE]
    )
    
    if (nrow(pairs) == 1L) {
      result$dictionary_indicator_code <- pairs$indicator_code[1]
      result$dictionary_indicator_name <- pairs$indicator_name[1]
      result$mapping_method <- "Nom exact normalisé"
      result$mapping_status <-
        "Correspondance automatique confirmée"
      
      return(result)
    }
    
    result$mapping_method <- "Nom ambigu"
    result$mapping_status <- "À mapper manuellement"
  }
  
  result
}








read_gnarf_dataset <- function(file_info, dataset_type) {
  if (is.null(file_info) || nrow(file_info) < 1L) {
    stop("Sélectionnez le fichier GNARF à importer.")
  }
  
  path <- file_info$datapath[1]
  source_file <- basename(file_info$name[1])
  
  if (!file.exists(path)) stop("Fichier temporaire d'importation introuvable.")
  ext <- tolower(tools::file_ext(source_file))
  if (!ext %in% c("xls", "xlsx")) {
    stop("Format non pris en charge : utilisez un fichier .xls ou .xlsx.")
  }
  
  raw0 <- readxl::read_excel(
    path, col_names = FALSE, .name_repair = "minimal"
  )
  if (nrow(raw0) == 0L) stop("Le fichier GNARF est vide.")
  
  nonempty <- vapply(seq_len(nrow(raw0)), function(i) {
    row <- unlist(raw0[i, , drop = FALSE], use.names = FALSE)
    any(!is.na(row) & nzchar(trimws(as.character(row))))
  }, logical(1))
  header_row <- which(nonempty)[1]
  if (is.na(header_row)) stop("Aucune ligne d'en-tête détectée.")
  
  df <- readxl::read_excel(path, skip = header_row - 1L) |>
    janitor::clean_names() |>
    as.data.frame()
  
  if (nrow(df) == 0L) stop("Le fichier ne contient aucune donnée après l'en-tête.")
  
  # Harmoniser quelques variantes possibles des noms de colonnes.
  aliases <- list(
    data = c("data", "indicator", "indicator_name", "indicator_code", "indicateur"),
    period = c("period", "year", "annee", "time_period"),
    organisation_unit = c(
      "organisation_unit", "organization_unit", "organisationunit",
      "organizationunit", "country", "pays", "organisation_unit_name"
    ),
    value = c("value", "valeur", "status", "endemicity_status")
  )
  for (target in names(aliases)) {
    if (!target %in% names(df)) {
      candidate <- intersect(aliases[[target]], names(df))
      if (length(candidate) > 0L) df[[target]] <- df[[candidate[1]]]
    }
  }
  
  required <- c("data", "period", "organisation_unit", "value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop(
      "Colonnes obligatoires absentes dans ", source_file, " : ",
      paste(missing, collapse = ", "),
      ". Colonnes trouvées : ", paste(names(df), collapse = ", ")
    )
  }
  
  df$data <- trimws(as.character(df$data))
  #df$period <- as.character(df$period)
  # df$organisation_unit <- as.character(df$organisation_unit)
  df$period <- as.character(df$period)
  df$organisation_unit <- as.character(df$organisation_unit)
  
  df$period[is.na(df$period)] <- ""
  df$organisation_unit[is.na(df$organisation_unit)] <- ""
  df$data[is.na(df$data)] <- ""
  df$value[is.na(df$value)] <- ""
  df$indicator_original <- df$data
  df$gnarf_type <- dataset_type
  df$source_file <- source_file
  df$imported_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Pour le fichier numérique, catégoriser uniquement à titre indicatif.
  if (dataset_type == "numerique") {
    label <- normalize_gnarf_text(df$data)
    df$indicator_domain <- ifelse(
      grepl("prevalence|prevalent|human rabies cases", label), "Prevalence",
      ifelse(
        grepl("incidence|incident|new cases|new confirmed|new clinical cases|new PB cases|new", label),
        "Incidence",
        ifelse(
          grepl("mortality|mortality rate|deaths|death rate|death", label),
          "Mortalité",
          ifelse(
            grepl("treatment|treated|care|coverage|management", label),
            "Prise en charge",
            "Prevalence"#"Autre / à classifier"
          )
        )
      )
    )
  } else {
    df$endemicity_status_code <- as.character(df$value)
  }
  
  # Valeurs numériques conservées dans les colonnes d'origine ; colonne
  # numérique auxiliaire utile aux analyses lorsque la conversion est possible.
  # df$value_numeric <- suppressWarnings(as.numeric(as.character(df$value)))
  raw_value <- as.character(df$value)
  
  raw_value <- trimws(raw_value)
  
  raw_value <- gsub(",", ".", raw_value, fixed = TRUE)
  
  df$value_numeric <- suppressWarnings(
    as.numeric(raw_value)
  )
  df
}





parse_gnarf_label <- function(x) {
  parsed <- split_gnarf_indicator(x)
  
  data.frame(
    gnarf_indicator_code = parsed$gnarf_indicator_code,
    gnarf_indicator_name = parsed$gnarf_indicator_name,
    stringsAsFactors = FALSE
  )
}




load_local_gho <- function() {
  
  # Adapter ce chemin au nom réel du dictionnaire Excel.
  local_file <- "input/dictionnaire/gho_indicators.xlsx"
  
  empty_catalogue <- data.frame(
    indicator_code = character(),
    indicator_name = character(),
    language = character(),
    catalogue_source = character(),
    stringsAsFactors = FALSE
  )
  
  if (!file.exists(local_file)) {
    message(
      "Dictionnaire GHO local introuvable : ",
      local_file
    )
    return(empty_catalogue)
  }
  
  # Déclarer aliases dans l'environnement de la fonction, avant tryCatch,
  # afin qu'il soit toujours accessible pendant la normalisation des colonnes.
  aliases <- list(
    indicator_code = c(
      "indicator_code", "code", "gho_code",
      "code_indicateur", "indicator"
    ),
    indicator_name = c(
      "indicator_name", "name", "title",
      "nom_indicateur", "label"
    ),
    language = c("language", "langue"),
    catalogue_source = c("catalogue_source", "source")
  )
  
  tryCatch({
    
    sheets <- readxl::excel_sheets(local_file)
    
    if (length(sheets) == 0L) {
      stop("Le fichier Excel ne contient aucune feuille.")
    }
    
    df <- readxl::read_excel(
      local_file,
      sheet = sheets[1]
    )
    
    df <- janitor::clean_names(df)
    
    for (target in names(aliases)) {
      
      if (!target %in% names(df)) {
        
        candidates <- intersect(
          aliases[[target]],
          names(df)
        )
        
        if (length(candidates) > 0L) {
          df[[target]] <- df[[candidates[1]]]
        } else if (target %in% c(
          "indicator_code",
          "indicator_name"
        )) {
          stop(
            "Colonne obligatoire absente : ",
            target,
            ". Colonnes trouvées : ",
            paste(names(df), collapse = ", ")
          )
        } else {
          # Répéter la valeur par ligne, sans longueur incohérente.
          df[[target]] <- rep(
            if (target == "language") "en" else "Excel local",
            nrow(df)
          )
        }
      }
    }
    
    df <- as.data.frame(df)
    
    df$indicator_code <- trimws(
      as.character(df$indicator_code)
    )
    
    df$indicator_name <- trimws(
      as.character(df$indicator_name)
    )
    
    df$language <- as.character(df$language)
    
    df$catalogue_source <- as.character(
      df$catalogue_source
    )
    
    df <- df[
      !is.na(df$indicator_code) &
        nzchar(df$indicator_code) &
        !is.na(df$indicator_name) &
        nzchar(df$indicator_name),
      c(
        "indicator_code",
        "indicator_name",
        "language",
        "catalogue_source"
      ),
      drop = FALSE
    ]
    
    df <- unique(df)
    
    rownames(df) <- NULL
    
    df
    
  }, error = function(e) {
    
    message(
      "Erreur de lecture du dictionnaire GHO local : ",
      conditionMessage(e)
    )
    
    empty_catalogue
    
  })
}






combine_gho_catalogues <- function(api, local) {
  dplyr::bind_rows(api, local) |>
    dplyr::mutate(
      indicator_code = trimws(indicator_code),
      indicator_name = trimws(indicator_name),
      language = ifelse(
        is.na(language) | !nzchar(language),
        NA_character_, language
      )
    ) |>
    dplyr::distinct(
      indicator_code, indicator_name, language,
      .keep_all = TRUE
    )
}


init_mapping_db <- function() {
  con <- connect_tvd_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gho_catalogue (
      indicator_code TEXT NOT NULL,
      indicator_name TEXT NOT NULL,
      language TEXT,
      catalogue_source TEXT,
      refreshed_at TEXT,
      PRIMARY KEY (indicator_code, indicator_name, language)
    )
  ")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gnarf_gho_mapping (
      gnarf_indicator_original TEXT PRIMARY KEY,
      gnarf_indicator_code TEXT,
      gnarf_indicator_name TEXT,
      gho_indicator_code TEXT,
      gho_indicator_name TEXT,
      mapping_method TEXT,
      mapping_status TEXT NOT NULL,
      validated_by TEXT,
      validated_at TEXT
    )
  ")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gnarf_data_raw (
      import_id INTEGER PRIMARY KEY AUTOINCREMENT,
      imported_at TEXT NOT NULL,
      source_file TEXT,
      indicator_original TEXT,
      row_json TEXT NOT NULL
    )
  ")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gnarf_data_validated (
      validation_id INTEGER PRIMARY KEY AUTOINCREMENT,
      imported_at TEXT NOT NULL,
      source_file TEXT,
      indicator_original TEXT,
      gnarf_indicator_code TEXT,
      gnarf_indicator_name TEXT,
      gho_indicator_code TEXT,
      gho_indicator_name TEXT,
      mapping_method TEXT,
      row_json TEXT NOT NULL
    )
  ")
  
  invisible(TRUE)
}



dir.create(
  "input/dictionnaire",
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists("input/dictionnaire/gho_indicators.xlsx")) {
  writexl::write_xlsx(
    data.frame(
      indicator_code = character(),
      indicator_name = character(),
      language = character()
    ),
    "input/dictionnaire/gho_indicators.xlsx"
  )
}




read_indicator_dictionary <- function() {
  path <- find_indicator_dictionary_file()
  ext <- tolower(tools::file_ext(path))
  
  message("Lecture du dictionnaire : ", normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  ))
  
  # Lecture CSV
  if (ext == "csv") {
    raw <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8",
      na.strings = c("", "NA", "N/A", "NULL")
    )
    
    sheets <- list(raw)
    
  } else {
    sheet_names <- readxl::excel_sheets(path)
    
    if (length(sheet_names) == 0L) {
      stop("Le fichier Excel ne contient aucune feuille.")
    }
    
    sheets <- lapply(sheet_names, function(sheet_name) {
      raw <- readxl::read_excel(
        path,
        sheet = sheet_name,
        col_names = FALSE,
        .name_repair = "minimal"
      )
      
      if (nrow(raw) == 0L) {
        return(data.frame())
      }
      
      nonempty <- vapply(
        seq_len(nrow(raw)),
        function(i) {
          values <- unlist(
            raw[i, , drop = FALSE],
            use.names = FALSE
          )
          
          values <- trimws(as.character(values))
          
          any(
            !is.na(values) &
              nzchar(values)
          )
        },
        logical(1)
      )
      
      header_candidates <- which(nonempty)
      
      if (length(header_candidates) == 0L) {
        return(data.frame())
      }
      
      header_row <- header_candidates[1]
      
      result <- readxl::read_excel(
        path,
        sheet = sheet_name,
        skip = header_row - 1L
      )
      
      janitor::clean_names(result)
    })
  }
  
  sheets <- lapply(sheets, function(x) {
    if (is.null(x) || nrow(x) == 0L) {
      return(data.frame())
    }
    
    janitor::clean_names(as.data.frame(x))
  })
  
  dictionary <- dplyr::bind_rows(sheets)
  
  if (nrow(dictionary) == 0L) {
    stop("Le dictionnaire indicator est vide.")
  }
  
  names_found <- names(dictionary)
  
  # Colonnes acceptées pour le code
  code_aliases <- c(
    "indicator_code",
    "code",
    "code_indicateur",
    "indicator_id",
    "indicator_uid",
    "uid",
    "id",
    "gnarf_indicator_code",
    "indicator",
    "indicatorcode",
    "code_indicator"
  )
  
  # Colonnes acceptées pour le nom
  name_aliases <- c(
    "indicator_name",
    "name",
    "nom",
    "nom_indicateur",
    "indicator_label",
    "label",
    "title",
    "description",
    "indicator_description",
    "display_name",
    "data",
    "indicatorname",
    "libelle",
    "libelle_indicateur"
  )
  
  code_col <- intersect(code_aliases, names_found)
  name_col <- intersect(name_aliases, names_found)
  
  if (length(code_col) == 0L || length(name_col) == 0L) {
    stop(
      "Colonnes du dictionnaire non reconnues.\n\n",
      "Colonnes trouvées : ",
      paste(names_found, collapse = ", "),
      "\n\nColonnes de code acceptées : ",
      paste(code_aliases, collapse = ", "),
      "\n\nColonnes de nom acceptées : ",
      paste(name_aliases, collapse = ", ")
    )
  }
  
  result <- data.frame(
    indicator_code = trimws(
      as.character(dictionary[[code_col[1]]])
    ),
    indicator_name = trimws(
      as.character(dictionary[[name_col[1]]])
    ),
    stringsAsFactors = FALSE
  )
  
  result <- result[
    !is.na(result$indicator_code) &
      nzchar(result$indicator_code) &
      !is.na(result$indicator_name) &
      nzchar(result$indicator_name),
    ,
    drop = FALSE
  ]
  
  result <- dplyr::distinct(
    result,
    indicator_code,
    indicator_name
  )
  
  if (nrow(result) == 0L) {
    stop(
      "Le dictionnaire ne contient aucune paire code/nom valide."
    )
  }
  
  rownames(result) <- NULL
  
  attr(
    result,
    "source_path"
  ) <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
  
  result
}



load_indicator_dictionary_safe <- function() {
  tryCatch(
    read_indicator_dictionary(),
    error = function(e) {
      message(
        "Erreur dictionnaire indicator : ",
        conditionMessage(e)
      )
      
      attr(
        empty_indicator_dictionary(),
        "load_error"
      ) <- conditionMessage(e)
    }
  )
}





find_indicator_dictionary_file <- function() {
  dict_dir <- file.path("input", "dictionnaire")
  
  if (!dir.exists(dict_dir)) {
    stop(
      "Le dossier suivant est introuvable : ",
      normalizePath(dict_dir, winslash = "/", mustWork = FALSE)
    )
  }
  
  files <- list.files(
    dict_dir,
    pattern = "\\.(xlsx|xls|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0L) {
    stop(
      "Aucun fichier Excel ou CSV trouvé dans : ",
      normalizePath(dict_dir, winslash = "/", mustWork = FALSE)
    )
  }
  
  basenames <- tolower(basename(files))
  
  # Priorité aux noms attendus
  preferred <- c(
    "indicator.xlsx",
    "indicator.xls",
    "indicator.csv",
    "indicators.xlsx",
    "indicators.xls",
    "indicators.csv",
    "indicator_dictionary.xlsx",
    "indicator_dictionary.xls",
    "indicator_dictionary.csv"
  )
  
  selected <- files[match(preferred, basenames, nomatch = 0L)]
  
  if (length(selected) > 0L) {
    return(selected[1])
  }
  
  # Sinon, recherche de tout fichier dont le nom contient indicator
  indicator_files <- files[
    grepl("indicator", basename(files), ignore.case = TRUE)
  ]
  
  if (length(indicator_files) > 0L) {
    return(indicator_files[1])
  }
  
  stop(
    "Aucun dictionnaire indicator trouvé dans ",
    normalizePath(dict_dir, winslash = "/", mustWork = FALSE),
    ". Nommez le fichier indicator.xlsx."
  )
}






empty_indicator_dictionary <- function() {
  data.frame(
    indicator_code = character(),
    indicator_name = character(),
    stringsAsFactors = FALSE
  )
}

country_normalize_name <- function(x) {
  normalize_text(x)
}



country_find_column <- function(data, candidates, required = TRUE) {
  normalized <- janitor::make_clean_names(names(data))
  candidates <- janitor::make_clean_names(candidates)
  idx <- match(candidates, normalized, nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) > 0L) return(names(data)[idx[1]])
  if (required) {
    stop(
      "Colonne requise absente : ", paste(candidates, collapse = " / "),
      ". Colonnes trouvées : ", paste(names(data), collapse = ", ")
    )
  }
  NULL
}




# ---------- Module serveur ----------

srv_import_validation <- function(
    input,
    output,
    session,
    rv,
    sqlite_db_refresh
) {
  
  init_mapping_db()
  
  rv$indicator_dictionary <- load_indicator_dictionary_safe()
  
  # --- 4.1 Components & Maladies
  output$download_tpl_comp <- downloadHandler(
    filename = function() "template_components_diseases.xlsx",
    content = function(file) {
      write_xlsx(
        list(
          component = data.frame(
            component_name = character()
          ),
          disease = data.frame(
            component_name = character(),
            disease_name = character()
          )
        ),
        path = file
      )
    }
  )
  
  observeEvent(input$validate_comp, {
    req(input$file_comp)
    
    tryCatch({
      sheets <- excel_sheets(input$file_comp$datapath)
      
      if (!all(c("component", "disease") %in% sheets)) {
        stop("Le fichier doit contenir les feuilles component et disease.")
      }
      
      comp <- read_excel_safe(input$file_comp$datapath, "component")
      dis <- read_excel_safe(input$file_comp$datapath, "disease")
      
      if (!required_columns(comp, "component_name")) {
        stop("Colonne component_name manquante dans component.")
      }
      
      if (!required_columns(dis, c("component_name", "disease_name"))) {
        stop("Colonnes component_name ou disease_name manquantes.")
      }
      
      comp$component_name <- trimws(as.character(comp$component_name))
      dis$component_name <- trimws(as.character(dis$component_name))
      dis$disease_name <- trimws(as.character(dis$disease_name))
      
      comp <- comp[!is.na(comp$component_name) &
                     nzchar(comp$component_name), , drop = FALSE]
      dis <- dis[!is.na(dis$disease_name) &
                   nzchar(dis$disease_name), , drop = FALSE]
      
      dup_comp <- comp[duplicated(comp$component_name), , drop = FALSE]
      dup_dis <- dis[duplicated(dis[c("component_name",
                                      "disease_name")]), , drop = FALSE]
      
      diff_comp <- setdiff(
        unique(dis$component_name),
        unique(comp$component_name)
      )
      
      problems <- data.frame(
        type = character(),
        detail = character()
      )
      
      if (nrow(dup_comp) > 0) {
        problems <- rbind(problems, data.frame(
          type = "Doublon composant",
          detail = paste(unique(dup_comp$component_name),
                         collapse = ", ")
        ))
      }
      
      if (nrow(dup_dis) > 0) {
        problems <- rbind(problems, data.frame(
          type = "Doublon maladie",
          detail = paste(dup_dis$disease_name, collapse = ", ")
        ))
      }
      
      if (length(diff_comp) > 0) {
        problems <- rbind(problems, data.frame(
          type = "Composants inexistants",
          detail = paste(diff_comp, collapse = ", ")
        ))
      }
      
      if (nrow(problems) > 0) {
        output$check_results_comp <- renderDT({
          datatable(problems, options = list(pageLength = 10))
        })
        showNotification(
          "Validation échouée : corrigez les erreurs affichées.",
          type = "error"
        )
        return()
      }
      
      write_xlsx(comp, "input/dictionnaire/component.xlsx")
      write_xlsx(dis, "input/dictionnaire/disease.xlsx")
      
      rv$comp <- comp
      rv$diseases <- dis
      
      output$check_results_comp <- renderDT({
        datatable(data.frame(
          Resultat = c("Composants", "Maladies"),
          Nombre = c(nrow(comp), nrow(dis))
        ))
      })
      
      showNotification("Composants et maladies sauvegardés.")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error",
                       duration = NULL)
    })
  })
  
  output$download_copies_ui <- renderUI({
    tagList(
      downloadButton("download_comp_saved", "Télécharger composants"),
      downloadButton("download_disease_saved", "Télécharger maladies")
    )
  })
  
  output$download_comp_saved <- downloadHandler(
    filename = function() "component.xlsx",
    content = function(file) {
      req(file.exists("input/dictionnaire/component.xlsx"))
      file.copy("input/dictionnaire/component.xlsx", file,
                overwrite = TRUE)
    }
  )
  
  output$download_disease_saved <- downloadHandler(
    filename = function() "disease.xlsx",
    content = function(file) {
      req(file.exists("input/dictionnaire/disease.xlsx"))
      file.copy("input/dictionnaire/disease.xlsx", file,
                overwrite = TRUE)
    }
  )
  
  
  # --- 4.2 Indicateurs
  
  
  output$download_tpl_ind <- downloadHandler(
    filename = function() "template_indicators.xlsx",
    content = function(file) {
      write_xlsx(
        data.frame(
          indicator_code = character(),
          indicator_name = character(),
          component_name = character(),
          disease_name = character(),
          unit = character(),
          data_type = character(),
          source = character()
        ),
        path = file
      )
    }
  )
  
  observeEvent(input$validate_ind, {
    req(input$file_ind)
    
    tryCatch({
      df <- read_excel_safe(input$file_ind$datapath)
      
      required <- c("indicator_code", "indicator_name")
      
      if (!required_columns(df, required)) {
        stop(paste(
          "Colonnes obligatoires :",
          paste(required, collapse = ", ")
        ))
      }
      
      df$indicator_code <- trimws(as.character(df$indicator_code))
      df$indicator_name <- trimws(as.character(df$indicator_name))
      
      if (anyNA(df$indicator_code) ||
          any(!nzchar(df$indicator_code))) {
        stop("Les codes des indicateurs ne peuvent pas être vides.")
      }
      
      if (anyDuplicated(df$indicator_code)) {
        stop("Les codes des indicateurs doivent être uniques.")
      }
      
      # Publier le dictionnaire validé dans SQLite pour la vérification des codes.
      indicators_sqlite <- as.data.frame(df, check.names = FALSE)
      if (!"dictionary_indicator_code" %in% names(indicators_sqlite)) {
        indicators_sqlite$dictionary_indicator_code <- indicators_sqlite$indicator_code
      }
      if (!"dictionary_indicator_name" %in% names(indicators_sqlite)) {
        indicators_sqlite$dictionary_indicator_name <- indicators_sqlite$indicator_name
      }
      
      con_indicators <- connect_tvd_db()
      tryCatch({
        DBI::dbWithTransaction(con_indicators, {
          DBI::dbWriteTable(
            con_indicators,
            "indicator",
            indicators_sqlite,
            overwrite = TRUE,
            row.names = FALSE
          )
        })
      }, finally = {
        DBI::dbDisconnect(con_indicators)
      })
      
      write_xlsx(df, "input/dictionnaire/indicators.xlsx")
      rv$indicators <- df
      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      
      output$log_ind <- renderText(
        paste(
          "Validation réussie :", nrow(df),
          "indicateurs sauvegardés dans Excel et dans SQLite (table indicator).",
          "Utilisez ensuite le bouton de construction pour actualiser la base analytique."
          
        )
      )
      output$table_ind <- renderDT({
        datatable(df, options = list(pageLength = 10, scrollX = TRUE))
      })
    }, error = function(e) {
      output$log_ind <- renderText(conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  output$download_indicators_excel <- downloadHandler(
    filename = function() {
      paste0("Liste_indicateurs_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      indicators <- rv$indicators
      if (is.null(indicators) || nrow(indicators) == 0L) {
        saved_path <- file.path("input", "dictionnaire", "indicators.xlsx")
        if (file.exists(saved_path)) {
          indicators <- read_excel_safe(saved_path)
        } else {
          indicators <- rv$indicator_dictionary
        }
      }
      if (is.null(indicators) || !is.data.frame(indicators) || nrow(indicators) == 0L) {
        stop("Aucune liste d’indicateurs n’est disponible pour l’export.")
      }
      writexl::write_xlsx(
        list(Indicateurs = as.data.frame(indicators)),
        path = file
      )
    }
  )
  
  
  # --- 4.4 GNARF
  
  output$download_tpl_ind <- downloadHandler(
    filename = function() "template_indicators.xlsx",
    content = function(file) {
      write_xlsx(
        data.frame(
          indicator_code = character(),
          indicator_name = character(),
          component_name = character(),
          disease_name = character(),
          unit = character(),
          data_type = character(),
          source = character()
        ),
        path = file
      )
    }
  )
  
  observeEvent(input$validate_ind, {
    req(input$file_ind)
    
    tryCatch({
      df <- read_excel_safe(input$file_ind$datapath)
      
      required <- c("indicator_code", "indicator_name")
      
      if (!required_columns(df, required)) {
        stop(paste(
          "Colonnes obligatoires :",
          paste(required, collapse = ", ")
        ))
      }
      
      df$indicator_code <- trimws(as.character(df$indicator_code))
      df$indicator_name <- trimws(as.character(df$indicator_name))
      
      if (anyNA(df$indicator_code) ||
          any(!nzchar(df$indicator_code))) {
        stop("Les codes des indicateurs ne peuvent pas être vides.")
      }
      
      if (anyDuplicated(df$indicator_code)) {
        stop("Les codes des indicateurs doivent être uniques.")
      }
      
      # Publier le dictionnaire validé dans SQLite pour la vérification des codes.
      indicators_sqlite <- as.data.frame(df, check.names = FALSE)
      if (!"dictionary_indicator_code" %in% names(indicators_sqlite)) {
        indicators_sqlite$dictionary_indicator_code <- indicators_sqlite$indicator_code
      }
      if (!"dictionary_indicator_name" %in% names(indicators_sqlite)) {
        indicators_sqlite$dictionary_indicator_name <- indicators_sqlite$indicator_name
      }
      
      con_indicators <- connect_tvd_db()
      tryCatch({
        DBI::dbWithTransaction(con_indicators, {
          DBI::dbWriteTable(
            con_indicators,
            "indicator",
            indicators_sqlite,
            overwrite = TRUE,
            row.names = FALSE
          )
        })
      }, finally = {
        DBI::dbDisconnect(con_indicators)
      })
      
      write_xlsx(df, "input/dictionnaire/indicators.xlsx")
      rv$indicators <- df
      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      
      output$log_ind <- renderText(
        paste(
          "Validation réussie :", nrow(df),
          "indicateurs sauvegardés dans Excel et dans SQLite (table indicator).",
          "Utilisez ensuite le bouton de construction pour actualiser la base analytique."
          
        )
      )
      output$table_ind <- renderDT({
        datatable(df, options = list(pageLength = 10, scrollX = TRUE))
      })
    }, error = function(e) {
      output$log_ind <- renderText(conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  output$download_indicators_excel <- downloadHandler(
    filename = function() {
      paste0("Liste_indicateurs_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      indicators <- rv$indicators
      if (is.null(indicators) || nrow(indicators) == 0L) {
        saved_path <- file.path("input", "dictionnaire", "indicators.xlsx")
        if (file.exists(saved_path)) {
          indicators <- read_excel_safe(saved_path)
        } else {
          indicators <- rv$indicator_dictionary
        }
      }
      if (is.null(indicators) || !is.data.frame(indicators) || nrow(indicators) == 0L) {
        stop("Aucune liste d’indicateurs n’est disponible pour l’export.")
      }
      writexl::write_xlsx(
        list(Indicateurs = as.data.frame(indicators)),
        path = file
      )
    }
  )
  output$download_gnarf_master <- downloadHandler(
    filename = function() paste0("GNARF_master_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$gnarf_master)
      writexl::write_xlsx(rv$gnarf_master, file)
    }
  )
  
    # 4.4 GNARF : importation numérique + endémicité ----------------
  
  # Créer les tables techniques d'archive et de mapping si elles n'existent pas.
  init_mapping_db()
  
  empty_indicator_dictionary <- function() {
    data.frame(
      indicator_code = character(),
      indicator_name = character(),
      stringsAsFactors = FALSE
    )
  }
  
  rv$gnarf_numeric <- NULL
  rv$gnarf_endemicity <- NULL
  rv$gnarf_mapping <- NULL
  rv$gnarf_master <- NULL
  rv$gnarf_source_numeric <- NULL
  rv$gnarf_source_endemicity <- NULL
  
  normalize_gnarf_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x[is.na(x)] <- ""
    x <- tolower(trimws(x))
    x <- gsub("[^a-z0-9]+", " ", x)
    trimws(gsub("\\s+", " ", x))
  }
  
  find_indicator_dictionary_file <- function() {
    dict_dir <- file.path("input", "dictionnaire")
    
    if (!dir.exists(dict_dir)) {
      stop(
        "Le dossier suivant est introuvable : ",
        normalizePath(dict_dir, winslash = "/", mustWork = FALSE)
      )
    }
    
    files <- list.files(
      dict_dir,
      pattern = "\\.(xlsx|xls|csv)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(files) == 0L) {
      stop(
        "Aucun fichier Excel ou CSV trouvé dans : ",
        normalizePath(dict_dir, winslash = "/", mustWork = FALSE)
      )
    }
    
    basenames <- tolower(basename(files))
    
    # Priorité aux noms attendus
    preferred <- c(
      "indicator.xlsx",
      "indicator.xls",
      "indicator.csv",
      "indicators.xlsx",
      "indicators.xls",
      "indicators.csv",
      "indicator_dictionary.xlsx",
      "indicator_dictionary.xls",
      "indicator_dictionary.csv"
    )
    
    selected <- files[match(preferred, basenames, nomatch = 0L)]
    
    if (length(selected) > 0L) {
      return(selected[1])
    }
    
    # Sinon, recherche de tout fichier dont le nom contient indicator
    indicator_files <- files[
      grepl("indicator", basename(files), ignore.case = TRUE)
    ]
    
    if (length(indicator_files) > 0L) {
      return(indicator_files[1])
    }
    
    stop(
      "Aucun dictionnaire indicator trouvé dans ",
      normalizePath(dict_dir, winslash = "/", mustWork = FALSE),
      ". Nommez le fichier indicator.xlsx."
    )
  }
  
  read_indicator_dictionary <- function() {
    path <- find_indicator_dictionary_file()
    ext <- tolower(tools::file_ext(path))
    
    message("Lecture du dictionnaire : ", normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    ))
    
    # Lecture CSV
    if (ext == "csv") {
      raw <- utils::read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "UTF-8",
        na.strings = c("", "NA", "N/A", "NULL")
      )
      
      sheets <- list(raw)
      
    } else {
      sheet_names <- readxl::excel_sheets(path)
      
      if (length(sheet_names) == 0L) {
        stop("Le fichier Excel ne contient aucune feuille.")
      }
      
      sheets <- lapply(sheet_names, function(sheet_name) {
        raw <- readxl::read_excel(
          path,
          sheet = sheet_name,
          col_names = FALSE,
          .name_repair = "minimal"
        )
        
        if (nrow(raw) == 0L) {
          return(data.frame())
        }
        
        nonempty <- vapply(
          seq_len(nrow(raw)),
          function(i) {
            values <- unlist(
              raw[i, , drop = FALSE],
              use.names = FALSE
            )
            
            values <- trimws(as.character(values))
            
            any(
              !is.na(values) &
                nzchar(values)
            )
          },
          logical(1)
        )
        
        header_candidates <- which(nonempty)
        
        if (length(header_candidates) == 0L) {
          return(data.frame())
        }
        
        header_row <- header_candidates[1]
        
        result <- readxl::read_excel(
          path,
          sheet = sheet_name,
          skip = header_row - 1L
        )
        
        janitor::clean_names(result)
      })
    }
    
    sheets <- lapply(sheets, function(x) {
      if (is.null(x) || nrow(x) == 0L) {
        return(data.frame())
      }
      
      janitor::clean_names(as.data.frame(x))
    })
    
    dictionary <- dplyr::bind_rows(sheets)
    
    if (nrow(dictionary) == 0L) {
      stop("Le dictionnaire indicator est vide.")
    }
    
    names_found <- names(dictionary)
    
    # Colonnes acceptées pour le code
    code_aliases <- c(
      "indicator_code",
      "code",
      "code_indicateur",
      "indicator_id",
      "indicator_uid",
      "uid",
      "id",
      "gnarf_indicator_code",
      "indicator",
      "indicatorcode",
      "code_indicator"
    )
    
    # Colonnes acceptées pour le nom
    name_aliases <- c(
      "indicator_name",
      "name",
      "nom",
      "nom_indicateur",
      "indicator_label",
      "label",
      "title",
      "description",
      "indicator_description",
      "display_name",
      "data",
      "indicatorname",
      "libelle",
      "libelle_indicateur"
    )
    
    code_col <- intersect(code_aliases, names_found)
    name_col <- intersect(name_aliases, names_found)
    
    if (length(code_col) == 0L || length(name_col) == 0L) {
      stop(
        "Colonnes du dictionnaire non reconnues.\n\n",
        "Colonnes trouvées : ",
        paste(names_found, collapse = ", "),
        "\n\nColonnes de code acceptées : ",
        paste(code_aliases, collapse = ", "),
        "\n\nColonnes de nom acceptées : ",
        paste(name_aliases, collapse = ", ")
      )
    }
    
    result <- data.frame(
      indicator_code = trimws(
        as.character(dictionary[[code_col[1]]])
      ),
      indicator_name = trimws(
        as.character(dictionary[[name_col[1]]])
      ),
      stringsAsFactors = FALSE
    )
    
    result <- result[
      !is.na(result$indicator_code) &
        nzchar(result$indicator_code) &
        !is.na(result$indicator_name) &
        nzchar(result$indicator_name),
      ,
      drop = FALSE
    ]
    
    result <- dplyr::distinct(
      result,
      indicator_code,
      indicator_name
    )
    
    if (nrow(result) == 0L) {
      stop(
        "Le dictionnaire ne contient aucune paire code/nom valide."
      )
    }
    
    rownames(result) <- NULL
    
    attr(
      result,
      "source_path"
    ) <- normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    )
    
    result
  }
  
  
  load_indicator_dictionary_safe <- function() {
    tryCatch(
      read_indicator_dictionary(),
      error = function(e) {
        message(
          "Erreur dictionnaire indicator : ",
          conditionMessage(e)
        )
        
        attr(
          empty_indicator_dictionary(),
          "load_error"
        ) <- conditionMessage(e)
      }
    )
  }
  
  
  rv$indicator_dictionary <- load_indicator_dictionary_safe()
  
  
  # ============================================================
  # LISTE DES INDICATEURS GHO DEPUIS LE DICTIONNAIRE
  # ============================================================
  
  observe({
    # Le code sélectionné doit provenir du catalogue GHO (IndicatorCode),
    # et non du dictionnaire GNARF qui décrit des codes internes.
    local_catalogue <- load_local_gho()
    con <- connect_tvd_db()
    catalogue <- tryCatch({
      if ("gho_catalogue" %in% DBI::dbListTables(con)) {
        DBI::dbReadTable(con, "gho_catalogue")
      } else data.frame()
    }, error = function(e) data.frame(), finally = DBI::dbDisconnect(con))
    
    # Lorsque le catalogue local est vide, charger le catalogue officiel GHO.
    # Le résultat est mis en cache dans SQLite ; l'application reste utilisable
    # hors ligne grâce au catalogue local ou au cache.
    if (nrow(catalogue) == 0L && nrow(local_catalogue) == 0L) {
      catalogue <- tryCatch({
        response <- httr::GET(GHO_API_URL, query = list("$top" = 10000L),
                              httr::timeout(60),
                              httr::user_agent("SOP-Data-App-TVD"))
        httr::stop_for_status(response)
        payload <- jsonlite::fromJSON(
          httr::content(response, as = "text", encoding = "UTF-8"),
          flatten = TRUE
        )
        api <- as.data.frame(payload$value, stringsAsFactors = FALSE)
        code_col <- intersect(c("IndicatorCode", "indicator_code"), names(api))[1]
        name_col <- intersect(c("IndicatorName", "indicator_name"), names(api))[1]
        if (is.na(code_col) || is.na(name_col)) stop("Colonnes IndicatorCode/IndicatorName absentes.")
        api <- data.frame(indicator_code = as.character(api[[code_col]]),
                          indicator_name = as.character(api[[name_col]]),
                          language = NA_character_, catalogue_source = "API GHO",
                          stringsAsFactors = FALSE)
        api <- api[!is.na(api$indicator_code) & nzchar(api$indicator_code), , drop = FALSE]
        con <- connect_tvd_db()
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        if (nrow(api)) DBI::dbWriteTable(con, "gho_catalogue", api,
                                         append = TRUE, row.names = FALSE)
        api
      }, error = function(e) {
        message("Catalogue API GHO indisponible : ", conditionMessage(e))
        data.frame()
      })
    }
    
    dict <- dplyr::bind_rows(catalogue, local_catalogue)
    needed <- c("indicator_code", "indicator_name")
    if (!all(needed %in% names(dict)) || nrow(dict) == 0L) {
      updateSelectInput(
        session,
        inputId = "gho_ind_code",
        choices = character(0),
        selected = character(0)
      )
      return()
    }
    
    dict$indicator_code <- trimws(as.character(dict$indicator_code))
    dict$indicator_name <- trimws(as.character(dict$indicator_name))
    dict <- dict[!is.na(dict$indicator_code) & nzchar(dict$indicator_code), , drop = FALSE]
    dict <- dplyr::distinct(dict, indicator_code, .keep_all = TRUE)
    codes <- dict$indicator_code
    labels <- ifelse(is.na(dict$indicator_name) | !nzchar(dict$indicator_name),
                     codes, paste0(codes, " — ", dict$indicator_name))
    
    choices <- stats::setNames(codes, labels)
    
    # Conserver la sélection actuelle si elle reste disponible
    current <- isolate(input$gho_ind_code)
    
    selected_code <- if (
      length(current) == 1 &&
      !is.na(current) &&
      current %in% codes
    ) {
      current
    } else {
      character(0)
    }
    
    updateSelectizeInput(
      session,
      inputId = "gho_ind_code",
      choices = choices,
      selected = selected_code,
      server = TRUE
    )
  })
  
  
  # opérateur local de remplacement des valeurs nulles
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
  }
  
  output$indicator_dictionary_status <- renderText({
    dict <- rv$indicator_dictionary
    
    if (is.null(dict) || nrow(dict) == 0L) {
      load_error <- attr(dict, "load_error")
      
      if (!is.null(load_error)) {
        return(
          paste(
            "Dictionnaire non chargé :",
            load_error
          )
        )
      }
      
      return(
        paste(
          "Dictionnaire non chargé.",
          "Placez un fichier indicator.xlsx dans",
          "input/dictionnaire."
        )
      )
    }
    
    paste0(
      "Fichier : ",
      attr(dict, "source_path") %||% "indicator",
      "\n",
      "Indicateurs disponibles : ",
      nrow(dict),
      "\n",
      "Codes distincts : ",
      dplyr::n_distinct(dict$indicator_code)
    )
  })
  
  
  observeEvent(input$reload_indicator_dictionary, {
    dict <- load_indicator_dictionary_safe()
    rv$indicator_dictionary <- dict
    if (nrow(dict) == 0L) {
      showNotification(
        "Dictionnaire indicator non chargé. Vérifiez le fichier et ses colonnes.",
        type = "error", duration = NULL
      )
    } else {
      showNotification(
        paste(nrow(dict), "indicateurs chargés depuis le dictionnaire local."),
        type = "message"
      )
    }
  })
  
  # Lire un fichier GNARF .xls/.xlsx en détectant la ligne d'en-tête.
  read_gnarf_dataset <- function(file_info, dataset_type) {
    if (is.null(file_info) || nrow(file_info) < 1L) {
      stop("Sélectionnez le fichier GNARF à importer.")
    }
    
    path <- file_info$datapath[1]
    source_file <- basename(file_info$name[1])
    
    if (!file.exists(path)) stop("Fichier temporaire d'importation introuvable.")
    ext <- tolower(tools::file_ext(source_file))
    if (!ext %in% c("xls", "xlsx")) {
      stop("Format non pris en charge : utilisez un fichier .xls ou .xlsx.")
    }
    
    raw0 <- readxl::read_excel(
      path, col_names = FALSE, .name_repair = "minimal"
    )
    if (nrow(raw0) == 0L) stop("Le fichier GNARF est vide.")
    
    nonempty <- vapply(seq_len(nrow(raw0)), function(i) {
      row <- unlist(raw0[i, , drop = FALSE], use.names = FALSE)
      any(!is.na(row) & nzchar(trimws(as.character(row))))
    }, logical(1))
    header_row <- which(nonempty)[1]
    if (is.na(header_row)) stop("Aucune ligne d'en-tête détectée.")
    
    df <- readxl::read_excel(path, skip = header_row - 1L) |>
      janitor::clean_names() |>
      as.data.frame()
    
    if (nrow(df) == 0L) stop("Le fichier ne contient aucune donnée après l'en-tête.")
    
    # Harmoniser quelques variantes possibles des noms de colonnes.
    aliases <- list(
      data = c("data", "indicator", "indicator_name", "indicator_code", "indicateur"),
      period = c("period", "year", "annee", "time_period"),
      organisation_unit = c(
        "organisation_unit", "organization_unit", "organisationunit",
        "organizationunit", "country", "pays", "organisation_unit_name"
      ),
      value = c("value", "valeur", "status", "endemicity_status")
    )
    for (target in names(aliases)) {
      if (!target %in% names(df)) {
        candidate <- intersect(aliases[[target]], names(df))
        if (length(candidate) > 0L) df[[target]] <- df[[candidate[1]]]
      }
    }
    
    required <- c("data", "period", "organisation_unit", "value")
    missing <- setdiff(required, names(df))
    if (length(missing) > 0L) {
      stop(
        "Colonnes obligatoires absentes dans ", source_file, " : ",
        paste(missing, collapse = ", "),
        ". Colonnes trouvées : ", paste(names(df), collapse = ", ")
      )
    }
    
    df$data <- trimws(as.character(df$data))
    #df$period <- as.character(df$period)
    # df$organisation_unit <- as.character(df$organisation_unit)
    df$period <- as.character(df$period)
    df$organisation_unit <- as.character(df$organisation_unit)
    
    df$period[is.na(df$period)] <- ""
    df$organisation_unit[is.na(df$organisation_unit)] <- ""
    df$data[is.na(df$data)] <- ""
    df$value[is.na(df$value)] <- ""
    df$indicator_original <- df$data
    df$gnarf_type <- dataset_type
    df$source_file <- source_file
    df$imported_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # Pour le fichier numérique, catégoriser uniquement à titre indicatif.
    if (dataset_type == "numerique") {
      label <- normalize_gnarf_text(df$data)
      df$indicator_domain <- ifelse(
        grepl("prevalence|prevalent|human rabies cases", label), "Prevalence",
        ifelse(
          grepl("incidence|incident|new cases|new confirmed|new clinical cases|new PB cases|new", label),
          "Incidence",
          ifelse(
            grepl("mortality|mortality rate|deaths|death rate|death", label),
            "Mortalité",
            ifelse(
              grepl("treatment|treated|care|coverage|management", label),
              "Prise en charge",
              "Prevalence"#"Autre / à classifier"
            )
          )
        )
      )
    } else {
      df$endemicity_status_code <- as.character(df$value)
    }
    
    # Valeurs numériques conservées dans les colonnes d'origine ; colonne
    # numérique auxiliaire utile aux analyses lorsque la conversion est possible.
    # df$value_numeric <- suppressWarnings(as.numeric(as.character(df$value)))
    raw_value <- as.character(df$value)
    
    raw_value <- trimws(raw_value)
    
    raw_value <- gsub(",", ".", raw_value, fixed = TRUE)
    
    df$value_numeric <- suppressWarnings(
      as.numeric(raw_value)
    )
    df
  }
  
  parse_gnarf_label <- function(x) {
    parsed <- split_gnarf_indicator(x)
    
    data.frame(
      gnarf_indicator_code = parsed$gnarf_indicator_code,
      gnarf_indicator_name = parsed$gnarf_indicator_name,
      stringsAsFactors = FALSE
    )
  }
  
  
  # ============================================================
  # DIFFERENCIER AUTOMATIQUEMENT LES CODES GNARF IDENTIQUES
  # ============================================================
  
  disambiguate_gnarf_codes <- function(labels) {
    
    labels <- as.character(labels)
    labels[is.na(labels)] <- ""
    
    parsed <- split_gnarf_indicator(labels)
    
    result <- parsed$gnarf_indicator_code
    
    valid <- !is.na(result) & nzchar(result)
    
    if (!any(valid)) {
      return(result)
    }
    
    # Mots génériques à ignorer dans la recherche distinctive
    stop_words <- c(
      "number", "of", "the", "a", "an", "and", "or",
      "for", "from", "in", "on", "with", "by", "to",
      "all", "total", "reported", "reporting",
      "indicator", "indicators", "data", "value",
      "cases", "case", "disease", "diseases",
      "number", "rate", "proportion", "percentage",
      "prevalence", "incidence", "status", "human"
    )
    
    # Nettoyer un libellé pour produire un suffixe
    make_suffix <- function(label, other_labels) {
      
      label_norm <- normalize_gnarf_text(label)
      other_norm <- normalize_gnarf_text(other_labels)
      
      words <- unique(strsplit(label_norm, " +")[[1]])
      
      words <- words[
        nzchar(words) &
          !words %in% stop_words &
          nchar(words) >= 2
      ]
      
      # Priorité aux mots qui distinguent ce libellé
      # des autres libellés possédant le même code.
      other_words <- unlist(
        strsplit(other_norm, " +"),
        use.names = FALSE
      )
      
      distinctive <- words[!words %in% other_words]
      
      # Si aucun mot totalement distinct n'existe,
      # conserver les mots significatifs du libellé.
      if (length(distinctive) == 0L) {
        distinctive <- words
      }
      
      if (length(distinctive) == 0L) {
        distinctive <- "INDICATOR"
      }
      
      suffix <- toupper(paste(distinctive, collapse = "-"))
      suffix <- gsub("[^A-Z0-9-]", "", suffix)
      suffix <- gsub("-+", "-", suffix)
      suffix <- gsub("^-|-$", "", suffix)
      
      suffix
    }
    
    # Identifier les groupes de codes répétés.
    code_groups <- split(which(valid), result[valid])
    
    for (group_name in names(code_groups)) {
      
      idx <- code_groups[[group_name]]
      
      # Ne rien changer si le code est unique.
      if (length(idx) < 2L) {
        next
      }
      
      group_labels <- labels[idx]
      group_names <- parsed$gnarf_indicator_name[idx]
      
      # Utiliser le nom extrait, ou le libellé original
      # si le nom extrait est vide.
      group_names[
        is.na(group_names) | !nzchar(trimws(group_names))
      ] <- group_labels[
        is.na(group_names) | !nzchar(trimws(group_names))
      ]
      
      suffixes <- vapply(
        seq_along(idx),
        function(j) {
          others <- group_names[-j]
          
          make_suffix(
            group_names[j],
            others
          )
        },
        character(1)
      )
      
      # Garantir un suffixe unique même si les libellés
      # ne possèdent pas de mot distinctif exploitable.
      suffixes <- make.unique(suffixes, sep = "-VAR-")
      
      result[idx] <- paste0(group_name, "-", suffixes)
    }
    
    result
  }
  
  
  
  map_one_gnarf_label <- function(
    dataset_type,
    label,
    dictionary,
    code_override = NULL
  ) {
    
    parsed <- parse_gnarf_label(label)
    
    code <- parsed$gnarf_indicator_code[1]
    name <- parsed$gnarf_indicator_name[1]
    
    # Utiliser le code différencié lorsqu'il est fourni.
    if (!is.null(code_override) &&
        length(code_override) == 1L &&
        !is.na(code_override) &&
        nzchar(code_override)) {
      code <- code_override
    }
    
    norm_label <- normalize_gnarf_text(label)
    norm_name <- normalize_gnarf_text(name)
    
    result <- data.frame(
      gnarf_type = dataset_type,
      gnarf_indicator_original = as.character(label),
      gnarf_indicator_code = code,
      gnarf_indicator_name = name,
      dictionary_indicator_code = NA_character_,
      dictionary_indicator_name = NA_character_,
      mapping_method = "Aucune correspondance exacte",
      mapping_status = "À mapper manuellement",
      stringsAsFactors = FALSE
    )
    
    if (is.null(dictionary) || nrow(dictionary) == 0L) {
      return(result)
    }
    
    dictionary$indicator_code <- trimws(
      as.character(dictionary$indicator_code)
    )
    
    dictionary$indicator_name <- trimws(
      as.character(dictionary$indicator_name)
    )
    
    # 1. Recherche par code exact
    code_candidates <- unique(c(
      as.character(label),
      as.character(code)
    ))
    
    code_candidates <- code_candidates[
      !is.na(code_candidates) &
        nzchar(trimws(code_candidates))
    ]
    
    code_hits <- which(
      toupper(dictionary$indicator_code) %in%
        toupper(code_candidates)
    )
    
    if (length(code_hits) > 0L) {
      pairs <- unique(
        dictionary[code_hits,
                   c("indicator_code", "indicator_name"),
                   drop = FALSE]
      )
      
      if (nrow(pairs) == 1L) {
        result$dictionary_indicator_code <- pairs$indicator_code[1]
        result$dictionary_indicator_name <- pairs$indicator_name[1]
        result$mapping_method <- "Code exact"
        result$mapping_status <-
          "Correspondance automatique confirmée"
        
        return(result)
      }
      
      result$mapping_method <- "Code ambigu"
      result$mapping_status <- "À mapper manuellement"
      
      return(result)
    }
    
    # 2. Recherche par nom normalisé
    dictionary_names <- normalize_gnarf_text(dictionary$indicator_name)
    
    names_to_compare <- unique(c(
      norm_label,
      norm_name
    ))
    
    names_to_compare <- names_to_compare[
      !is.na(names_to_compare) &
        nzchar(names_to_compare)
    ]
    
    name_hits <- which(
      dictionary_names %in% names_to_compare
    )
    
    if (length(name_hits) > 0L) {
      pairs <- unique(
        dictionary[name_hits,
                   c("indicator_code", "indicator_name"),
                   drop = FALSE]
      )
      
      if (nrow(pairs) == 1L) {
        result$dictionary_indicator_code <- pairs$indicator_code[1]
        result$dictionary_indicator_name <- pairs$indicator_name[1]
        result$mapping_method <- "Nom exact normalisé"
        result$mapping_status <-
          "Correspondance automatique confirmée"
        
        return(result)
      }
      
      result$mapping_method <- "Nom ambigu"
      result$mapping_status <- "À mapper manuellement"
    }
    
    result
  }
  
  
  
  
  
  rebuild_gnarf_mapping <- function() {
    dict <- rv$indicator_dictionary
    if (is.null(dict) || nrow(dict) == 0L) {
      stop("Chargez le dictionnaire indicator avant de faire le mapping.")
    }
    
    labels <- list()
    if (!is.null(rv$gnarf_numeric) && nrow(rv$gnarf_numeric) > 0L) {
      labels[[length(labels) + 1L]] <- data.frame(
        gnarf_type = "numerique",
        gnarf_indicator_original = unique(rv$gnarf_numeric$indicator_original),
        stringsAsFactors = FALSE
      )
    }
    if (!is.null(rv$gnarf_endemicity) && nrow(rv$gnarf_endemicity) > 0L) {
      labels[[length(labels) + 1L]] <- data.frame(
        gnarf_type = "endemicite",
        gnarf_indicator_original = unique(rv$gnarf_endemicity$indicator_original),
        stringsAsFactors = FALSE
      )
    }
    if (length(labels) == 0L) {
      rv$gnarf_mapping <- NULL
      return(invisible(NULL))
    }
    
    unique_labels <- dplyr::bind_rows(labels) |>
      dplyr::distinct(gnarf_type, gnarf_indicator_original)
    
    
    # Analyser tous les libellés ensemble afin de détecter
    # les codes identiques avant de lancer le mapping.
    unique_labels$gnarf_indicator_code <- disambiguate_gnarf_codes(
      unique_labels$gnarf_indicator_original
    )
    
    mapping_rows <- lapply(seq_len(nrow(unique_labels)), function(i) {
      
      map_one_gnarf_label(
        dataset_type = unique_labels$gnarf_type[i],
        label = unique_labels$gnarf_indicator_original[i],
        dictionary = dict,
        code_override = unique_labels$gnarf_indicator_code[i]
      )
    })
    
    mapping <- dplyr::bind_rows(mapping_rows)
    
    
    # Préserver les validations déjà effectuées si l'utilisateur importe
    # les deux fichiers séparément ou recharge l'un d'eux.
    previous <- rv$gnarf_mapping
    if (!is.null(previous) && nrow(previous) > 0L) {
      final_status <- c("Confirmé manuellement", "Absence confirmée")
      for (i in seq_len(nrow(mapping))) {
        old_i <- which(
          previous$gnarf_type == mapping$gnarf_type[i] &
            previous$gnarf_indicator_original ==
            mapping$gnarf_indicator_original[i] &
            previous$mapping_status %in% final_status
        )
        if (length(old_i) > 0L) {
          j <- old_i[1]
          mapping$dictionary_indicator_code[i] <-
            previous$dictionary_indicator_code[j]
          mapping$dictionary_indicator_name[i] <-
            previous$dictionary_indicator_name[j]
          mapping$mapping_method[i] <- previous$mapping_method[j]
          mapping$mapping_status[i] <- previous$mapping_status[j]
        }
      }
    }
    
    mapping$mapping_id <- seq_len(nrow(mapping))
    rv$gnarf_mapping <- mapping
    invisible(mapping)
  }
  
  render_gnarf_import_log <- function() {
    numeric_n <- if (is.null(rv$gnarf_numeric)) 0L else nrow(rv$gnarf_numeric)
    endemicity_n <- if (is.null(rv$gnarf_endemicity)) 0L else nrow(rv$gnarf_endemicity)
    map_n <- if (is.null(rv$gnarf_mapping)) 0L else nrow(rv$gnarf_mapping)
    output$log_gnarf <- renderText({
      paste0(
        "Importation / structuration GNARF\n",
        "Lignes numériques : ", numeric_n, "\n",
        "Lignes endémicité : ", endemicity_n, "\n",
        "Indicateurs distincts analysés automatiquement : ", map_n, "\n",
        "Correspondances automatiques confirmées : ",
        if (is.null(rv$gnarf_mapping)) 0L else
          sum(rv$gnarf_mapping$mapping_status == "Correspondance automatique confirmée"),
        "\nÀ mapper manuellement : ",
        if (is.null(rv$gnarf_mapping)) 0L else
          sum(rv$gnarf_mapping$mapping_status == "À mapper manuellement")
      )
    })
  }
  
  import_one_gnarf <- function(file_info, dataset_type) {
    if (is.null(rv$indicator_dictionary) ||
        nrow(rv$indicator_dictionary) == 0L) {
      stop(
        "Le dictionnaire indicator n'est pas chargé. ",
        "Vérifiez input/dictionnaire puis cliquez sur 'Recharger'."
      )
    }
    dataset <- read_gnarf_dataset(file_info, dataset_type)
    if (dataset_type == "numerique") {
      rv$gnarf_numeric <- dataset
      rv$gnarf_source_numeric <- basename(file_info$name[1])
    } else {
      rv$gnarf_endemicity <- dataset
      rv$gnarf_source_endemicity <- basename(file_info$name[1])
    }
    rebuild_gnarf_mapping()
    render_gnarf_import_log()
  }
  
  observeEvent(input$import_gnarf_numeric, {
    tryCatch({
      import_one_gnarf(input$file_gnarf_numeric, "numerique")
      showNotification("Fichier GNARF numérique importé et structuré.")
    }, error = function(e) {
      output$log_gnarf <- renderText(conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })
  
  observeEvent(input$import_gnarf_endemicity, {
    tryCatch({
      import_one_gnarf(input$file_gnarf_endemicity, "endemicite")
      showNotification("Fichier GNARF endémicité importé et structuré.")
    }, error = function(e) {
      output$log_gnarf <- renderText(conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })
  
  observeEvent(input$import_both_gnarf, {
    tryCatch({
      if (is.null(input$file_gnarf_numeric) ||
          is.null(input$file_gnarf_endemicity)) {
        stop("Sélectionnez les deux fichiers GNARF avant l'importation combinée.")
      }
      if (is.null(rv$indicator_dictionary) ||
          nrow(rv$indicator_dictionary) == 0L) {
        stop("Le dictionnaire indicator n'est pas chargé.")
      }
      
      rv$gnarf_numeric <- read_gnarf_dataset(
        input$file_gnarf_numeric, "numerique"
      )
      rv$gnarf_endemicity <- read_gnarf_dataset(
        input$file_gnarf_endemicity, "endemicite"
      )
      rv$gnarf_source_numeric <- basename(input$file_gnarf_numeric$name[1])
      rv$gnarf_source_endemicity <- basename(input$file_gnarf_endemicity$name[1])
      rebuild_gnarf_mapping()
      render_gnarf_import_log()
      showNotification("Les deux fichiers GNARF ont été importés et structurés.")
    }, error = function(e) {
      output$log_gnarf <- renderText(conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })
  
  unresolved_gnarf_indices <- reactive({
    req(rv$gnarf_mapping)
    which(rv$gnarf_mapping$mapping_status == "À mapper manuellement")
  })
  
  unresolved_gnarf_data <- reactive({
    req(rv$gnarf_mapping)
    
    unresolved_mapping <- rv$gnarf_mapping |>
      dplyr::filter(
        mapping_status == "À mapper manuellement"
      ) |>
      dplyr::select(
        gnarf_type,
        gnarf_indicator_original,
        gnarf_indicator_code,
        gnarf_indicator_name,
        mapping_method,
        mapping_status
      )
    
    if (nrow(unresolved_mapping) == 0L) {
      return(data.frame())
    }
    
    result <- list()
    
    if (!is.null(rv$gnarf_numeric) &&
        nrow(rv$gnarf_numeric) > 0L) {
      
      numeric_data <- rv$gnarf_numeric |>
        dplyr::left_join(
          unresolved_mapping,
          by = c(
            "gnarf_type",
            "indicator_original" =
              "gnarf_indicator_original"
          )
        ) |>
        dplyr::filter(
          !is.na(mapping_status),
          mapping_status == "À mapper manuellement"
        )
      
      if (nrow(numeric_data) > 0L) {
        result[[length(result) + 1L]] <- numeric_data
      }
    }
    
    if (!is.null(rv$gnarf_endemicity) &&
        nrow(rv$gnarf_endemicity) > 0L) {
      
      endemicity_data <- rv$gnarf_endemicity |>
        dplyr::left_join(
          unresolved_mapping,
          by = c(
            "gnarf_type",
            "indicator_original" =
              "gnarf_indicator_original"
          )
        ) |>
        dplyr::filter(
          !is.na(mapping_status),
          mapping_status == "À mapper manuellement"
        )
      
      if (nrow(endemicity_data) > 0L) {
        result[[length(result) + 1L]] <- endemicity_data
      }
    }
    
    if (length(result) == 0L) {
      return(data.frame())
    }
    
    dplyr::bind_rows(result)
  })
  
  output$gnarf_validation_ui <- renderUI({
    mapping <- rv$gnarf_mapping
    
    if (is.null(mapping) || nrow(mapping) == 0L) {
      return(NULL)
    }
    
    unresolved <- which(
      mapping$mapping_status == "À mapper manuellement"
    )
    
    if (length(unresolved) == 0L) {
      return(NULL)
    }
    
    box(
      title = "3. Vérification et validation des correspondances",
      width = 12,
      status = "warning",
      solidHeader = FALSE,
      
      p(paste0(
        "Cette étape apparaît uniquement parce que ",
        length(unresolved),
        " indicateur(s) n'ont pas de correspondance exacte ",
        "dans le dictionnaire indicator. ",
        "Vous pouvez télécharger la base à mapper, ",
        "puis sélectionner une ligne pour proposer une association ",
        "manuelle ou confirmer l'absence de correspondance."
      )),
      
      downloadButton(
        "download_gnarf_manual_mapping",
        "Télécharger la base à mapper",
        class = "btn-warning"
      ),
      
      br(),
      br(),
      
      DTOutput("table_gnarf_diff"),
      
      selectInput(
        "selected_indicator_code",
        "Indicateur du dictionnaire indicator à associer",
        choices = NULL
      ),
      
      fluidRow(
        column(
          width = 6,
          actionButton(
            "confirm_gnarf_mapping",
            "Confirmer la correspondance",
            class = "btn-info"
          )
        ),
        column(
          width = 6,
          actionButton(
            "confirm_gnarf_no_match",
            "Confirmer l'absence de correspondance",
            class = "btn-warning"
          )
        )
      )
    )
  })
  
  
  output$table_gnarf_diff <- DT::renderDT({
    req(rv$gnarf_mapping)
    idx <- unresolved_gnarf_indices()
    unresolved <- rv$gnarf_mapping[idx, , drop = FALSE]
    DT::datatable(
      unresolved,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE)
    )
  })
  
  
  output$download_gnarf_manual_mapping <- downloadHandler(
    filename = function() {
      paste0(
        "GNARF_a_mapper_manuellement_",
        Sys.Date(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      data_to_download <- unresolved_gnarf_data()
      
      if (is.null(data_to_download) ||
          nrow(data_to_download) == 0L) {
        stop(
          "Aucune ligne ne nécessite actuellement un mapping manuel."
        )
      }
      
      # Organisation des colonnes importantes au début du fichier
      priority_columns <- c(
        "gnarf_type",
        "indicator_original",
        "gnarf_indicator_code",
        "gnarf_indicator_name",
        "period",
        "organisation_unit",
        "value",
        "value_numeric",
        "mapping_method",
        "mapping_status",
        "source_file",
        "imported_at"
      )
      
      existing_priority <- intersect(
        priority_columns,
        names(data_to_download)
      )
      
      remaining_columns <- setdiff(
        names(data_to_download),
        existing_priority
      )
      
      data_to_download <- data_to_download[
        ,
        c(existing_priority, remaining_columns),
        drop = FALSE
      ]
      
      writexl::write_xlsx(
        list(
          gnarf_a_mapper = data_to_download
        ),
        path = file
      )
    }
  )
  
  
  observeEvent(input$table_gnarf_diff_rows_selected, {
    req(rv$gnarf_mapping, rv$indicator_dictionary)
    selected_row <- input$table_gnarf_diff_rows_selected
    if (length(selected_row) != 1L) return()
    unresolved_idx <- unresolved_gnarf_indices()
    if (selected_row < 1L || selected_row > length(unresolved_idx)) return()
    selected_row <- unresolved_idx[selected_row]
    
    dict <- rv$indicator_dictionary
    choice_values <- paste(dict$indicator_code, dict$indicator_name, sep = "|||")
    choice_labels <- paste0(dict$indicator_code, " — ", dict$indicator_name)
    choices <- stats::setNames(choice_values, choice_labels)
    choices <- choices[!duplicated(names(choices))]
    current_code <- rv$gnarf_mapping$dictionary_indicator_code[selected_row]
    current_name <- rv$gnarf_mapping$dictionary_indicator_name[selected_row]
    current_value <- if (!is.na(current_code) && !is.na(current_name)) {
      paste(current_code, current_name, sep = "|||")
    } else ""
    updateSelectInput(
      session, "selected_indicator_code",
      choices = choices,
      selected = if (nzchar(current_value) && current_value %in% unname(choices)) {
        current_value
      } else character(0)
    )
  })
  
  observeEvent(input$confirm_gnarf_mapping, {
    req(rv$gnarf_mapping, rv$indicator_dictionary)
    selected_row <- input$table_gnarf_diff_rows_selected
    req(length(selected_row) == 1L, input$selected_indicator_code)
    unresolved_idx <- unresolved_gnarf_indices()
    req(selected_row >= 1L, selected_row <= length(unresolved_idx))
    selected_row <- unresolved_idx[selected_row]
    
    tryCatch({
      selected_parts <- strsplit(
        as.character(input$selected_indicator_code), "|||", fixed = TRUE
      )[[1]]
      if (length(selected_parts) != 2L) {
        stop("Sélection du dictionnaire invalide. Choisissez à nouveau le code et le nom.")
      }
      code <- selected_parts[1]
      name <- selected_parts[2]
      candidates <- rv$indicator_dictionary[
        rv$indicator_dictionary$indicator_code == code &
          rv$indicator_dictionary$indicator_name == name,
        , drop = FALSE
      ]
      candidates <- dplyr::distinct(candidates, indicator_code, indicator_name)
      if (nrow(candidates) != 1L) {
        stop("La paire code/nom sélectionnée est absente ou ambiguë dans indicator.")
      }
      
      rv$gnarf_mapping$dictionary_indicator_code[selected_row] <- code
      rv$gnarf_mapping$dictionary_indicator_name[selected_row] <- name
      rv$gnarf_mapping$mapping_method[selected_row] <- "Choix manuel"
      rv$gnarf_mapping$mapping_status[selected_row] <- "Confirmé manuellement"
      updateCheckboxInput(session, "approve_gnarf_save", value = FALSE)
      showNotification("Correspondance confirmée avec le dictionnaire indicator.")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })
  
  observeEvent(input$confirm_gnarf_no_match, {
    req(rv$gnarf_mapping)
    selected_row <- input$table_gnarf_diff_rows_selected
    req(length(selected_row) == 1L)
    unresolved_idx <- unresolved_gnarf_indices()
    req(selected_row >= 1L, selected_row <= length(unresolved_idx))
    selected_row <- unresolved_idx[selected_row]
    
    rv$gnarf_mapping$dictionary_indicator_code[selected_row] <- NA_character_
    rv$gnarf_mapping$dictionary_indicator_name[selected_row] <- NA_character_
    rv$gnarf_mapping$mapping_method[selected_row] <- "Absence confirmée manuellement"
    rv$gnarf_mapping$mapping_status[selected_row] <- "Absence confirmée"
    updateCheckboxInput(session, "approve_gnarf_save", value = FALSE)
    showNotification("Absence de correspondance confirmée.")
  })
  
  # Sauvegarde structurée des deux fichiers, des décisions de mapping et des archives.
  observeEvent(input$save_validated_gnarf, {
    req(rv$gnarf_numeric, rv$gnarf_endemicity, rv$gnarf_mapping)
    
    if (!isTRUE(input$approve_gnarf_save)) {
      showNotification("Cochez la confirmation avant la sauvegarde.", type = "error")
      return()
    }
    
    allowed_status <- c("Correspondance automatique confirmée", "Confirmé manuellement", "Absence confirmée")
    if (any(!rv$gnarf_mapping$mapping_status %in% allowed_status)) {
      showNotification(
        "Sauvegarde refusée : chaque indicateur doit être confirmé ou marqué sans correspondance.",
        type = "error", duration = NULL
      )
      return()
    }
    
    con <- NULL
    tryCatch({
      con <- connect_tvd_db()
      imported_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      map_cols <- rv$gnarf_mapping |>
        dplyr::select(
          gnarf_type, gnarf_indicator_original,
          gnarf_indicator_code, gnarf_indicator_name,
          dictionary_indicator_code, dictionary_indicator_name,
          mapping_method, mapping_status
        )
      
      numeric_validated <- dplyr::left_join(
        rv$gnarf_numeric, map_cols,
        by = c("gnarf_type", "indicator_original" = "gnarf_indicator_original")
      )
      endemicity_validated <- dplyr::left_join(
        rv$gnarf_endemicity, map_cols,
        by = c("gnarf_type", "indicator_original" = "gnarf_indicator_original")
      )
      
      DBI::dbWithTransaction(con, {
        DBI::dbWriteTable(
          con, "gnarf_numeric", as.data.frame(numeric_validated),
          overwrite = TRUE, row.names = FALSE
        )
        DBI::dbWriteTable(
          con, "gnarf_endemicity", as.data.frame(endemicity_validated),
          overwrite = TRUE, row.names = FALSE
        )
        DBI::dbWriteTable(
          con, "gnarf_indicator_mapping",
          as.data.frame(rv$gnarf_mapping),
          overwrite = TRUE, row.names = FALSE
        )
        
        # Historique brut JSON : garder la provenance de chaque ligne.
        raw_rows <- list()
        for (typ in c("numerique", "endemicite")) {
          dat <- if (typ == "numerique") rv$gnarf_numeric else rv$gnarf_endemicity
          if (!is.null(dat) && nrow(dat) > 0L) {
            raw_rows[[length(raw_rows) + 1L]] <- data.frame(
              imported_at = imported_at,
              source_file = paste0(typ, ":", unique(dat$source_file)[1]),
              indicator_original = dat$indicator_original,
              row_json = vapply(seq_len(nrow(dat)), function(i) {
                as.character(jsonlite::toJSON(
                  as.list(dat[i, , drop = FALSE]),
                  auto_unbox = TRUE, na = "null"
                ))
              }, character(1)),
              stringsAsFactors = FALSE
            )
          }
        }
        if (length(raw_rows) > 0L) {
          DBI::dbWriteTable(
            con,
            "gnarf_data_raw",
            dplyr::bind_rows(raw_rows),
            append = TRUE,
            row.names = FALSE
          )
        }
        
        
        
        validated_rows <- dplyr::bind_rows(
          numeric_validated, endemicity_validated
        )
        validated_rows$validation_saved_at <- imported_at
        DBI::dbWriteTable(
          con, "gnarf_data_validated_structured",
          as.data.frame(validated_rows),
          overwrite = TRUE, row.names = FALSE
        )
        if ("indicator" %in% DBI::dbListTables(con)) {
          rebuild_epidemiological_analysis(con)
        }
      })
      
      output$gnarf_validation_status <- renderText({
        paste0(
          "Sauvegarde SQLite réussie.\n",
          "Table numérique : ", nrow(numeric_validated), " lignes\n",
          "Table endémicité : ", nrow(endemicity_validated), " lignes\n",
          "Indicateurs confirmés : ",
          sum(rv$gnarf_mapping$mapping_status == "Confirmé manuellement"), "\n",
          "Absences confirmées : ",
          sum(rv$gnarf_mapping$mapping_status == "Absence confirmée"), "\n",
          "Base : ", DB_PATH
        )
      })
      showNotification("Les deux tables GNARF et le mapping validé ont été enregistrés.", type = "message")
    }, error = function(e) {
      output$gnarf_validation_status <- renderText(
        paste("Erreur de sauvegarde :", conditionMessage(e))
      )
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    }, finally = {
      if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    })
  })
  
  
  
  
  
  # Fusion uniquement après importation des deux tables et validation du mapping.
  observeEvent(input$merge_gnarf_tables, {
    tryCatch({
      req(rv$gnarf_numeric, rv$gnarf_endemicity, rv$gnarf_mapping)
      
      allowed_status <- c("Correspondance automatique confirmée", "Confirmé manuellement", "Absence confirmée")
      if (any(!rv$gnarf_mapping$mapping_status %in% allowed_status)) {
        stop("Validez toutes les correspondances avant la fusion.")
      }
      
      map_cols <- rv$gnarf_mapping |>
        dplyr::select(
          gnarf_type, gnarf_indicator_original,
          gnarf_indicator_code, gnarf_indicator_name,
          dictionary_indicator_code, dictionary_indicator_name,
          mapping_method, mapping_status
        )
      
      numeric_validated <- dplyr::left_join(
        rv$gnarf_numeric, map_cols,
        by = c("gnarf_type", "indicator_original" = "gnarf_indicator_original")
      )
      endemicity_validated <- dplyr::left_join(
        rv$gnarf_endemicity, map_cols,
        by = c("gnarf_type", "indicator_original" = "gnarf_indicator_original")
      )
      
      # bind_rows aligne les colonnes et crée NA là où un champ n'existe pas.
      master <- dplyr::bind_rows(numeric_validated, endemicity_validated) |>
        dplyr::mutate(
          gnarf_database = "GNARF",
          fusion_created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      
      
      con <- connect_tvd_db()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbWithTransaction(con, {
        DBI::dbWriteTable(
          con, "gnarf_master",
          as.data.frame(master),
          overwrite = TRUE, row.names = FALSE
        )
      })
      
      rv$gnarf_master <- master
      output$gnarf_merge_status <- renderText({
        paste0(
          "Fusion réussie : ", nrow(master), " lignes au total.\n",
          "Données numériques : ", nrow(numeric_validated), "\n",
          "Statuts d'endémicité : ", nrow(endemicity_validated), "\n",
          "Table SQLite : gnarf_master\nBase : ", DB_PATH
        )
      })
      showNotification("Les deux tables ont été fusionnées dans gnarf_master.", type = "message")
    }, error = function(e) {
      output$gnarf_merge_status <- renderText(
        paste("Échec de la fusion :", conditionMessage(e))
      )
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })
  
  refresh_analysis_tables(session)
  
  output$table_gnarf_master <- DT::renderDT({
    req(rv$gnarf_master)
    DT::datatable(
      head(rv$gnarf_master, 1000),
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$download_gnarf_master <- downloadHandler(
    filename = function() paste0("GNARF_master_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$gnarf_master)
      writexl::write_xlsx(rv$gnarf_master, file)
    }
  )

  # --- 4.5 Autres bases Excel
  # 1. STRUCTURE DU MODELE EXCEL
  # ============================================================
  
  variables_autres_bases <- c(
    "dictionary_indicator_code",
    "dictionary_indicator_name",
    "period",
    "organisation_unit",
    "ntd_age_groups_gnarf",
    "ntd_sex_with_unknown",
    "ntd_dimession",
    "value"
  )
  
  # ============================================================
  # 2. TELECHARGEMENT DU MODELE
  # ============================================================
  
  output$download_modele_autres_bases <- downloadHandler(
    
    filename = function() {
      paste0(
        "Modele_Autres_Bases_Excel_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      modele <- as.data.frame(
        setNames(
          replicate(
            length(variables_autres_bases),
            character(0),
            simplify = FALSE
          ),
          variables_autres_bases
        ),
        check.names = FALSE
      )
      
      # Feuille principale
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Dataset")
      
      openxlsx::writeData(
        wb,
        sheet = "Dataset",
        x = as.data.frame(
          matrix(
            nrow = 0,
            ncol = length(variables_autres_bases),
            dimnames = list(NULL, variables_autres_bases)
          ),
          check.names = FALSE
        ),
        withFilter = TRUE
      )
      
      # Mise en forme des en-têtes
      style_entete <- openxlsx::createStyle(
        fontColour = "#FFFFFF",
        fgFill = "#0878A4",
        textDecoration = "bold",
        halign = "center",
        valign = "center",
        wrapText = TRUE
      )
      
      openxlsx::addStyle(
        wb,
        sheet = "Dataset",
        style = style_entete,
        rows = 1,
        cols = seq_along(variables_autres_bases),
        gridExpand = TRUE
      )
      
      openxlsx::setColWidths(
        wb,
        sheet = "Dataset",
        cols = seq_along(variables_autres_bases),
        widths = "auto"
      )
      
      openxlsx::freezePane(
        wb,
        sheet = "Dataset",
        firstRow = TRUE
      )
      
      # Feuille explicative
      openxlsx::addWorksheet(wb, "Instructions")
      
      instructions <- data.frame(
        Variable = variables_autres_bases,
        Description = c(
          "Code unique de l'indicateur",
          "Nom de l'indicateur",
          "Période de référence",
          "Unité organisationnelle",
          "Groupe d'âge GNARF",
          "Sexe, y compris inconnu",
          "Dimension GNARF",
          "Valeur numérique de l'indicateur"
        )
      )
      
      openxlsx::writeData(
        wb,
        sheet = "Instructions",
        x = instructions,
        withFilter = TRUE
      )
      
      openxlsx::addStyle(
        wb,
        sheet = "Instructions",
        style = style_entete,
        rows = 1,
        cols = 1:2,
        gridExpand = TRUE
      )
      
      openxlsx::setColWidths(
        wb,
        sheet = "Instructions",
        cols = 1:2,
        widths = "auto"
      )
      
      openxlsx::saveWorkbook(
        wb,
        file = file,
        overwrite = TRUE
      )
    }
  )
  
  
  # ============================================================
  # 3. IMPORTATION DU FICHIER EXCEL
  # ============================================================
  
  autres_bases_data <- shiny::reactiveVal(NULL)
  autres_bases_erreurs <- shiny::reactiveVal(data.frame())
  
  
  
  
  
  
  # ============================================================
  # CONFIGURATION SQLITE
  # ============================================================
  
  # Réutiliser la connexion SQLite existante de votre application.
  # Exemple :
  # con <- DBI::dbConnect(
  #   RSQLite::SQLite(),
  #   "data/afro_factsheet.sqlite"
  # )
  
  TABLE_INDICATEURS <- "indicator"
  COLONNE_CODE_INDICATEUR <- "dictionary_indicator_code"
  
  normaliser_codes_indicateur <- function(x) {
    code <- toupper(trimws(as.character(x)))
    code[is.na(x)] <- NA_character_
    code
  }
  
  # Nom de la table cible pour les autres bases Excel
  TABLE_AUTRES_BASES <- "autres_bases_excel"
  
  # ============================================================
  # ETATS DE VALIDATION
  # ============================================================
  
  correspondances_ok <- shiny::reactiveVal(FALSE)
  codes_non_correspondants <- shiny::reactiveVal(NULL)
  
  # À réinitialiser à chaque nouvel import
  observeEvent(input$import_autres_bases_excel, {
    correspondances_ok(FALSE)
    codes_non_correspondants(NULL)
    autres_bases_data(NULL)
    autres_bases_erreurs(data.frame())
  })
  
  # ============================================================
  # VERIFICATION DES CODES INDICATEURS
  # ============================================================
  
  
  observeEvent(
    list(
      input$verifier_codes_autres_bases,
      input$verifier_autres_bases_excel
    ), {
      
      # 1. Vérifier qu'un fichier a été importé
      req(input$import_autres_bases_excel)
      
      tryCatch({
        
        # 2. Récupérer les données déjà contrôlées,
        # ou lire le fichier si elles ne sont pas disponibles.
        donnees <- autres_bases_data()
        
        if (is.null(donnees)) {
          
          donnees <- readxl::read_excel(
            input$import_autres_bases_excel$datapath,
            sheet = "Dataset",
            .name_repair = "minimal"
          )
          
          donnees <- as.data.frame(
            donnees,
            check.names = FALSE
          )
          
          # 3. Vérifier les huit colonnes attendues
          colonnes_requises <- c(
            "dictionary_indicator_code",
            "dictionary_indicator_name",
            "period",
            "organisation_unit",
            "ntd_age_groups_gnarf",
            "ntd_sex_with_unknown",
            "ntd_dimession",
            "value"
          )
          
          colonnes_manquantes <- setdiff(
            colonnes_requises,
            names(donnees)
          )
          
          if (length(colonnes_manquantes) > 0) {
            stop(
              paste(
                "Colonnes absentes :",
                paste(colonnes_manquantes, collapse = ", ")
              )
            )
          }
          
          donnees <- donnees[
            ,
            colonnes_requises,
            drop = FALSE
          ]
          
          # Enregistrer les données chargées
          autres_bases_data(donnees)
        }
        
        # Comparer avec le dictionnaire officiel de la base SQLite.
        con_verification <- connect_tvd_db()
        on.exit(DBI::dbDisconnect(con_verification), add = TRUE)
        
        tables_db <- DBI::dbListTables(con_verification)
        if (!TABLE_INDICATEURS %in% tables_db) {
          stop("La table des indicateurs est introuvable dans la base SQLite.")
        }
        
        colonnes_dictionnaire <- DBI::dbListFields(
          con_verification,
          TABLE_INDICATEURS
        )
        if (!COLONNE_CODE_INDICATEUR %in% colonnes_dictionnaire) {
          stop(
            "La colonne ", COLONNE_CODE_INDICATEUR,
            " est absente de la table indicator."
          )
        }
        
        dictionnaire <- DBI::dbReadTable(
          con_verification,
          TABLE_INDICATEURS
        )
        codes_officiels <- unique(normaliser_codes_indicateur(
          dictionnaire[[COLONNE_CODE_INDICATEUR]]
        ))
        codes_officiels <- codes_officiels[
          !is.na(codes_officiels) & nzchar(codes_officiels)
        ]
        if (length(codes_officiels) == 0L) {
          stop("Aucun code indicateur valide n'a été trouvé dans le dictionnaire.")
        }
        
        codes_excel <- normaliser_codes_indicateur(
          donnees$dictionary_indicator_code
        )
        codes_invalides <- is.na(codes_excel) |
          !nzchar(codes_excel) |
          !codes_excel %in% codes_officiels
        
        correspondances_ok(!any(codes_invalides))
        codes_non_correspondants(
          donnees[codes_invalides, , drop = FALSE]
        )
        
        if (any(codes_invalides)) {
          showNotification(
            paste(
              sum(codes_invalides),
              "ligne(s) contiennent un code vide ou absent du dictionnaire."
            ),
            type = "warning",
            duration = NULL
          )
        } else {
          showNotification(
            "Tous les codes indicateurs correspondent au dictionnaire.",
            type = "message",
            duration = 8
          )
        }
        
      }, error = function(e) {
        
        correspondances_ok(FALSE)
        
        showNotification(
          paste(
            "Erreur lors de la vérification :",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      })
      
    }
  )
  
  
  # ============================================================
  # AFFICHAGE DES DIFFERENCES
  # ============================================================
  
  output$table_codes_non_correspondants <- DT::renderDT({
    
    differences <- codes_non_correspondants()
    
    if (is.null(differences) || nrow(differences) == 0) {
      return(
        DT::datatable(
          data.frame(),
          options = list(dom = "t"),
          rownames = FALSE
        )
      )
    }
    
    DT::datatable(
      differences,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$statut_correspondance_autres_bases <-
    renderUI({
      
      if (is.null(autres_bases_data())) {
        return(
          tags$div(
            class = "alert alert-info",
            "Importez et vérifiez les données avant de comparer les codes."
          )
        )
      }
      
      if (is.null(codes_non_correspondants())) {
        return(
          tags$div(
            class = "alert alert-warning",
            "La correspondance des codes n'a pas encore été vérifiée."
          )
        )
      }
      
      if (isTRUE(correspondances_ok())) {
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          " Tous les codes indicateurs correspondent au dictionnaire."
        )
      } else {
        tags$div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          paste(
            nrow(codes_non_correspondants()),
            "ligne(s) à corriger."
          )
        )
      }
    })
  
  
  
  # ============================================================
  # BOUTONS DISPONIBLES APRES VALIDATION
  # ============================================================
  
  output$actions_autres_bases_validees <- renderUI({
    
    if (!isTRUE(correspondances_ok()) ||
        is.null(autres_bases_data()) ||
        nrow(autres_bases_data()) == 0) {
      return(NULL)
    }
    
    tagList(
      downloadButton(
        "exporter_autres_bases_validees",
        "Exporter Excel validé",
        class = "btn-success"
      )
    )
  })
  
  # ============================================================
  # EXPORT EXCEL
  # ============================================================
  
  output$exporter_autres_bases_validees <- downloadHandler(
    
    filename = function() {
      paste0(
        "Autres_Bases_Validees_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      req(isTRUE(correspondances_ok()))
      
      donnees <- autres_bases_data()
      req(!is.null(donnees), nrow(donnees) > 0)
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Dataset")
      
      openxlsx::writeData(
        wb,
        "Dataset",
        donnees,
        withFilter = TRUE
      )
      
      openxlsx::freezePane(
        wb,
        "Dataset",
        firstRow = TRUE
      )
      
      openxlsx::saveWorkbook(
        wb,
        file,
        overwrite = TRUE
      )
    }
  )
  
  # ============================================================
  # SAUVEGARDE SQLITE
  # ============================================================
  
  observeEvent(input$sauvegarder_autres_bases_sqlite, {
    
    if (!isTRUE(correspondances_ok())) {
      showNotification(
        "Vérifiez et validez les codes indicateurs avant la sauvegarde.",
        type = "warning",
        duration = 8
      )
      return()
    }
    
    donnees <- autres_bases_data()
    
    req(!is.null(donnees), nrow(donnees) > 0)
    
    tryCatch({
      
      con <- connect_tvd_db()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      # Refaire le contrôle côté serveur avant la sauvegarde
      # afin d'éviter une sauvegarde sans validation active.
      tables_db <- DBI::dbListTables(con)
      
      if (!TABLE_INDICATEURS %in% tables_db) {
        stop("La table des indicateurs est introuvable.")
      }
      
      dictionnaire <- DBI::dbReadTable(
        con,
        TABLE_INDICATEURS
      )
      if (!COLONNE_CODE_INDICATEUR %in% names(dictionnaire)) {
        stop(
          "La colonne ", COLONNE_CODE_INDICATEUR,
          " est absente de la table indicator."
        )
      }
      
      codes_officiels <- unique(normaliser_codes_indicateur(
        dictionnaire[[COLONNE_CODE_INDICATEUR]]
      ))
      
      codes_excel <- normaliser_codes_indicateur(
        donnees$dictionary_indicator_code
      )
      
      codes_invalides <- is.na(codes_excel) |
        !nzchar(codes_excel) |
        !codes_excel %in% codes_officiels
      
      if (any(codes_invalides)) {
        correspondances_ok(FALSE)
        codes_non_correspondants(
          donnees[codes_invalides, , drop = FALSE]
        )
        stop(
          "La vérification a échoué. Relancez le contrôle des correspondances."
        )
      }
      
      # Vérifier les colonnes de la table cible
      if (TABLE_AUTRES_BASES %in% tables_db) {
        
        colonnes_cibles <- DBI::dbListFields(
          con,
          TABLE_AUTRES_BASES
        )
        
        if (!all(names(donnees) %in% colonnes_cibles)) {
          stop(
            "Le schéma de la table cible ne correspond pas aux colonnes importées."
          )
        }
      }
      
      # Remplacer les doublons par le dernier import, en conservant
      # les autres périodes, pays, indicateurs et dimensions déjà chargés.
      to_text_frame <- function(data) {
        data <- as.data.frame(data, check.names = FALSE)
        data[] <- lapply(data, function(column) {
          value <- as.character(column)
          value[is.na(column)] <- NA_character_
          value
        })
        data
      }
      imported_data <- to_text_frame(donnees)
      existing_data <- if (TABLE_AUTRES_BASES %in% tables_db) {
        DBI::dbGetQuery(
          con,
          paste0('SELECT rowid AS "tvd_import_order", * FROM "',
                 TABLE_AUTRES_BASES, '" ORDER BY rowid')
        )
      } else {
        imported_data[0, , drop = FALSE]
      }
      if ("tvd_import_order" %in% names(existing_data)) {
        existing_data$tvd_import_order <- NULL
      }
      existing_data <- to_text_frame(existing_data)
      incoming_names <- names(imported_data)
      existing_data <- existing_data[, intersect(incoming_names, names(existing_data)), drop = FALSE]
      merged_data <- dplyr::bind_rows(existing_data, imported_data)
      merged_data <- deduplicate_autres_bases(merged_data)

      # Import, transformation vers le schéma analytique, base finale et
      # table fusionnée sont mis à jour dans une seule transaction SQLite.
      pipeline <- DBI::dbWithTransaction(con, {
        DBI::dbWriteTable(
          con,
          TABLE_AUTRES_BASES,
          merged_data,
          overwrite = TRUE,
          row.names = FALSE
        )
        analysis_rows <- rebuild_epidemiological_analysis(con)
        final_rows <- rebuild_final_analysis_database(con)
        fusion_result <- rebuild_ntd_fusionnee(con, export_excel = FALSE)
        list(
          source_rows = nrow(merged_data),
          analysis_rows = analysis_rows,
          final_rows = final_rows,
          fusion_rows = fusion_result$rows
        )
      })

      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      refresh_analysis_tables(session)
      showNotification(
        paste0(
          nrow(donnees), " ligne(s) importée(s) ; ",
          pipeline$source_rows, " ligne(s) uniques dans autres_bases_excel ; ",
          pipeline$analysis_rows, " lignes analytiques ; ",
          pipeline$final_rows, " lignes dans final_analysis_database ; ",
          pipeline$fusion_rows, " lignes dans ntd_fusionnee."
        ),
        type = "message",
        duration = 12
      )
      
    }, error = function(e) {
      
      showNotification(
        paste("Échec de la sauvegarde :", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })
  
  
  # ============================================================
  # 4. APERCU ET CONTROLE QUALITE
  # ============================================================
  
  output$table_autres_bases_excel <- DT::renderDT({
    
    req(autres_bases_data())
    
    DT::datatable(
      autres_bases_data(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$table_autres_bases_erreurs <- DT::renderDT({
    
    DT::datatable(
      autres_bases_erreurs(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # ============================================================
  # 5. INDICATEURS DU TABLEAU DE BORD
  # ============================================================
  
  output$autres_bases_nb_lignes <- renderValueBox({
    
    n <- if (is.null(autres_bases_data())) {
      0
    } else {
      nrow(autres_bases_data())
    }
    
    valueBox(
      value = n,
      subtitle = "Lignes importées",
      icon = icon("table"),
      color = "aqua"
    )
  })
  
  output$autres_bases_nb_indicateurs <- renderValueBox({
    
    n <- if (is.null(autres_bases_data())) {
      0
    } else {
      length(unique(
        autres_bases_data()$dictionary_indicator_code
      ))
    }
    
    valueBox(
      value = n,
      subtitle = "Indicateurs distincts",
      icon = icon("chart-bar"),
      color = "green"
    )
  })
  
  output$autres_bases_nb_pays <- renderValueBox({
    
    n <- if (is.null(autres_bases_data())) {
      0
    } else {
      length(unique(
        autres_bases_data()$organisation_unit
      ))
    }
    
    valueBox(
      value = n,
      subtitle = "Unités organisationnelles",
      icon = icon("globe"),
      color = "yellow"
    )
  })
  
  
  
  
  
  # --- 4.6 GHO API
  
  observeEvent(input$fetch_gho, {
    # req(input$gho_url, input$gho_ind_code)
    
    req(input$gho_url, input$gho_ind_code)
    
    indicator <- trimws(as.character(input$gho_ind_code))
    
    if (length(indicator) != 1L ||
        is.na(indicator) ||
        !nzchar(indicator)) {
      stop("Sélectionnez un indicateur GHO dans la liste.")
    }
    
    tryCatch({
      base_url <- trimws(input$gho_url)
      indicator <- trimws(input$gho_ind_code)
      
      if (!grepl("/$", base_url)) {
        base_url <- paste0(base_url, "/")
      }
      
      if (!grepl("^[A-Za-z0-9_]+$", indicator)) {
        stop("Code GHO invalide.")
      }
      
      response <- httr::GET(
        paste0(base_url, indicator),
        httr::timeout(60),
        httr::user_agent("SOP-Data-App-TVD")
      )
      
      httr::stop_for_status(response)
      
      parsed <- jsonlite::fromJSON(
        httr::content(response, as = "text", encoding = "UTF-8"),
        flatten = TRUE
      )
      
      if (is.null(parsed$value)) {
        stop("La réponse GHO ne contient pas de collection 'value'.")
      }
      
      df <- as.data.frame(parsed$value, stringsAsFactors = FALSE)
      
      if (nrow(df) == 0L) {
        stop("Aucune observation retournée par l'API.")
      }
      
      # Toujours limiter les observations à la liste officielle configurée
      # pour les 47 pays de la Région africaine de l'OMS.
      allowed_country_codes <- WHO_AFRO_GHO_ISO3
      gho_country_col <- intersect(
        c("SpatialDim", "Spatial_Dim", "Country", "CountryCode", "ISO3", "Location"),
        names(df)
      )
      if (!length(gho_country_col)) {
        stop("La réponse GHO ne contient pas de colonne de code pays (SpatialDim).")
      }
      gho_country_codes <- toupper(trimws(as.character(df[[gho_country_col[1]]])))
      df <- df[
        !is.na(gho_country_codes) & gho_country_codes %in% allowed_country_codes,
        ,
        drop = FALSE
      ]
      if (nrow(df) == 0L) {
        stop("Aucune observation GHO ne correspond aux codes ISO3 des 47 pays de la Région africaine de l'OMS.")
      }
      # Filtre optionnel par plage d'années
      selected_years <- suppressWarnings(as.integer(input$gho_dates))
      
      if (length(selected_years) == 2L && all(is.finite(selected_years))) {
        start_year <- min(selected_years)
        end_year <- max(selected_years)
        
        time_col <- intersect(
          c("TimeDim", "Time_Dim", "Year", "year"),
          names(df)
        )
        
        if (length(time_col)) {
          years <- suppressWarnings(as.integer(
            substr(as.character(df[[time_col[1]]]), 1, 4)
          ))
          
          df <- df[
            !is.na(years) &
              years >= start_year &
              years <= end_year,
            ,
            drop = FALSE
          ]
        }
      }
      
      if (nrow(df) == 0L) {
        stop("Aucune observation ne correspond à la période sélectionnée.")
      }
      
      rv$gho <- df
      rv$gho_mapping <- NULL
      rv$gho_validated <- NULL
      rv$gho_differences <- NULL
      rv$gho_mapping_ok <- FALSE
      
      updateCheckboxInput(
        session, "approve_gho_save", value = FALSE
      )
      output$gho_preview <- DT::renderDT({
        DT::datatable(
          head(rv$gho, 500),
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 10)
        )
      })
      
      output$gho_log <- renderText(paste0(
        "Importation GHO réussie.\n",
        "Code API : ", indicator, "\n",
        "Région : Afrique (", length(WHO_AFRO_GHO_ISO3), " pays ISO3)\n",
        "Observations : ", nrow(df), "\n",
        "Cliquez sur « Vérifier les correspondances GHO → GNARF »."
      ))
      
      showNotification("Données GHO importées.", type = "message")
      
    }, error = function(e) {
      output$gho_log <- renderText(
        paste("Erreur API GHO :", conditionMessage(e))
      )
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # Export des données brutes
  output$download_gho_raw <- downloadHandler(
    filename = function() paste0("GHO_raw_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$gho)
      writexl::write_xlsx(rv$gho, file)
    }
  )
  
      
  
  
  # Vérifier les correspondances de variables et préparer le schéma cible
  observeEvent(input$verifier_correspondances_gho, {
    req(rv$gho)
    
    tryCatch({
      source_df <- as.data.frame(rv$gho, check.names = FALSE)
      source_df <- janitor::clean_names(source_df)
      
      mapping <- gho_mapping_table(source_df)
      
      # Le code et le nom de l'indicateur sont des métadonnées
      # fournies par la requête et le dictionnaire.
      indicator <- gho_lookup_indicator(input$gho_ind_code)
      
      mapping <- dplyr::bind_rows(
        data.frame(
          variable_cible = "dictionary_indicator_code",
          variable_source = NA_character_,
          statut = if (indicator$matched) {
            "Correspondance dictionnaire"
          } else {
            "Code GHO non trouvé dans le dictionnaire"
          }
        ),
        data.frame(
          variable_cible = "dictionary_indicator_name",
          variable_source = NA_character_,
          statut = if (indicator$matched) {
            "Correspondance dictionnaire"
          } else {
            "Nom canonique non trouvé"
          }
        ),
        mapping
      )
      
      # Construire la base canonique sans supprimer les colonnes
      # sources de la base brute conservée séparément.
      target <- as.data.frame(
        setNames(
          replicate(
            length(gho_canonical_columns),
            rep(NA_character_, nrow(source_df)),
            simplify = FALSE
          ),
          gho_canonical_columns
        ),
        check.names = FALSE
      )
      
      target$dictionary_indicator_code <- indicator$code
      target$dictionary_indicator_name <- indicator$name
      
      for (i in seq_len(nrow(mapping))) {
        target_name <- mapping$variable_cible[i]
        source_name <- mapping$variable_source[i]
        
        if (!is.na(source_name) &&
            source_name %in% names(source_df) &&
            target_name %in% names(target)) {
          target[[target_name]] <- as.character(source_df[[source_name]])
        }
      }
      
      # La valeur est convertie en numérique quand c'est possible.
      # Les valeurs non numériques sont signalées plus bas.
      raw_value <- trimws(as.character(target$value))
      raw_value <- gsub(",", ".", raw_value, fixed = TRUE)
      numeric_value <- suppressWarnings(as.numeric(raw_value))
      
      invalid_values <- !is.na(raw_value) &
        nzchar(raw_value) &
        is.na(numeric_value)
      
      target$value <- numeric_value
      
      # Contrôles structurels et qualité
      diff_list <- list()
      
      add_diff <- function(type, detail, n = 1L) {
        data.frame(
          type = type,
          detail = detail,
          nombre = as.integer(n),
          stringsAsFactors = FALSE
        )
      }
      
      missing_required <- mapping |>
        dplyr::filter(statut == "OBLIGATOIRE : absente")
      
      if (nrow(missing_required) > 0L) {
        diff_list[[length(diff_list) + 1L]] <- add_diff(
          "Variable obligatoire absente",
          paste(missing_required$variable_cible, collapse = ", "),
          nrow(missing_required)
        )
      }
      
      if (!indicator$matched) {
        diff_list[[length(diff_list) + 1L]] <- add_diff(
          "Indicateur non associé au dictionnaire",
          paste(
            "Code API :", input$gho_ind_code,
            "- le code source est conservé, mais n'est pas certifié GNARF."
          )
        )
      }
      
      if (any(invalid_values)) {
        diff_list[[length(diff_list) + 1L]] <- add_diff(
          "Valeur non numérique",
          "Certaines valeurs ne peuvent pas être converties en nombre.",
          sum(invalid_values)
        )
      }
      
      if (any(is.na(target$period) | !nzchar(target$period))) {
        diff_list[[length(diff_list) + 1L]] <- add_diff(
          "Période manquante",
          "Des observations ne comportent pas de période.",
          sum(is.na(target$period) | !nzchar(target$period))
        )
      }
      
      if (anyDuplicated(target)) {
        diff_list[[length(diff_list) + 1L]] <- add_diff(
          "Doublons potentiels",
          paste(
            "Lignes entièrement identiques :",
            sum(duplicated(target))
          ),
          sum(duplicated(target))
        )
      }
      
      differences <- if (length(diff_list)) {
        dplyr::bind_rows(diff_list)
      } else {
        data.frame(
          type = character(),
          detail = character(),
          nombre = integer()
        )
      }
      
      # Bloquer la validation si les variables obligatoires manquent,
      # ou si les champs structurants comportent des valeurs manquantes.
      structurally_valid <- nrow(missing_required) == 0L &&
        !any(is.na(target$period) | !nzchar(target$period)) &&
        !any(is.na(target$organisation_unit_code) |
               !nzchar(target$organisation_unit_code)) &&
        !any(invalid_values)
      
      rv$gho_mapping <- mapping
      rv$gho_validated <- target
      rv$gho_differences <- differences
      rv$gho_mapping_ok <- structurally_valid
      
      updateCheckboxInput(
        session, "approve_gho_save", value = FALSE
      )
      
      showNotification(
        if (structurally_valid) {
          "Structure GHO préparée. Examinez les différences avant la sauvegarde."
        } else {
          "La validation est bloquée : corrigez les erreurs structurelles."
        },
        type = if (structurally_valid) "message" else "error",
        duration = NULL
      )
      
    }, error = function(e) {
      rv$gho_mapping_ok <- FALSE
      output$gho_validation_status <- renderText(
        paste("Erreur de mapping GHO :", conditionMessage(e))
      )
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # État des correspondances
  output$gho_mapping_status <- renderUI({
    if (is.null(rv$gho_mapping)) {
      return(tags$div(
        class = "alert alert-info",
        "Importez les données, puis lancez la vérification."
      ))
    }
    
    if (isTRUE(rv$gho_mapping_ok)) {
      tags$div(
        class = "alert alert-success",
        paste(
          "Structure canonique créée.",
          "Vérifiez les différences et la correspondance de l'indicateur."
        )
      )
    } else {
      tags$div(
        class = "alert alert-danger",
        "Validation structurelle impossible. Consultez les différences."
      )
    }
  })
  
  output$gho_mapping_preview <- DT::renderDT({
    req(rv$gho_mapping)
    DT::datatable(
      rv$gho_mapping,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$gho_diff <- DT::renderDT({
    differences <- rv$gho_differences
    
    if (is.null(differences) || nrow(differences) == 0L) {
      return(DT::datatable(
        data.frame(
          Resultat = "Aucune différence structurelle détectée."
        ),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    DT::datatable(
      differences,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$gho_validated_preview <- DT::renderDT({
    req(rv$gho_validated)
    DT::datatable(
      head(rv$gho_validated, 500),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Valider puis sauvegarder dans une table dédiée
  observeEvent(input$valider_sauvegarder_gho, {
    req(rv$gho, rv$gho_validated, rv$gho_mapping)
    
    if (!isTRUE(rv$gho_mapping_ok)) {
      showNotification(
        "Sauvegarde refusée : la structure canonique n'est pas valide.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    if (!isTRUE(input$approve_gho_save)) {
      showNotification(
        "Cochez la confirmation avant la sauvegarde.",
        type = "error"
      )
      return()
    }
    
    con <- NULL
    
    tryCatch({
      con <- connect_tvd_db()
      
      # Recontrôler la table et les colonnes juste avant la transaction.
      if (!all(gho_canonical_columns %in% names(rv$gho_validated))) {
        stop("Le schéma canonique est incomplet.")
      }
      
      final_data <- rv$gho_validated[
        , gho_canonical_columns, drop = FALSE
      ]
      
      final_data$source <- "GHO"
      final_data$gho_indicator_code <- input$gho_ind_code
      final_data$source_imported_at <- format(
        Sys.time(), "%Y-%m-%d %H:%M:%S"
      )

      # Les anciennes versions de gho_data_validated ne contiennent pas
      # toujours organisation_unit_code (ou d'autres champs ajoutés ensuite).
      ensure_gho_table_schema(con, final_data)
      
      DBI::dbWithTransaction(con, {
        DBI::dbWriteTable(
          con,
          "gho_data_validated",
          final_data,
          append = DBI::dbExistsTable(con, "gho_data_validated"),
          row.names = FALSE
        )
        
        DBI::dbWriteTable(
          con,
          "gho_variable_mapping",
          rv$gho_mapping,
          overwrite = TRUE,
          row.names = FALSE
        )
        if ("indicator" %in% DBI::dbListTables(con)) {
          analysis_rows <- rebuild_epidemiological_analysis(con)

          country_file <- file.path(
            "input", "dictionnaire", "Country_list.xlsx"
          )
          if (file.exists(country_file)) {
            final_rows <- rebuild_final_analysis_database(con)
            fusion_build <- rebuild_ntd_fusionnee(
              con,
              export_excel = FALSE
            )
            fusion_rows <- fusion_build$rows
          }
        }
      })
      
      pipeline_status <- c(
        if (exists("analysis_rows", inherits = FALSE)) paste0(
          "epidemiological_analysis actualisée : ", analysis_rows, " lignes\n"
        ) else "",
        if (exists("final_rows", inherits = FALSE)) paste0(
          "final_analysis_database actualisée : ", final_rows, " lignes\n"
        ) else "",
        if (exists("fusion_rows", inherits = FALSE)) paste0(
          "ntd_fusionnee actualisée : ", fusion_rows, " lignes\n"
        ) else if ("indicator" %in% DBI::dbListTables(con)) paste0(
          "Base finale et ntd_fusionnee non reconstruites : fichier ",
          "input/dictionnaire/Country_list.xlsx introuvable.\n"
        ) else ""
      )

      output$gho_validation_status <- renderText(paste0(
        "Validation et sauvegarde terminées.\n",
        "Source : GHO\n",
        "Observations sauvegardées : ", nrow(final_data), "\n",
        "Table : gho_data_validated\n",
        "Table de correspondance : gho_variable_mapping\n",
        paste0(pipeline_status, collapse = ""),
        "Base SQLite : ", DB_PATH, "\n",
        "Correspondance officielle de l'indicateur : ",
        if (grepl(
          "Correspondance dictionnaire",
          paste(rv$gho_mapping$statut, collapse = " ")
        )) "à contrôler dans le tableau de mapping" else
          "non confirmée automatiquement"
      ))
      
      refresh_analysis_tables(session)
      
      showNotification(
        "La base GHO structurée a été enregistrée dans SQLite.",
        type = "message",
        duration = 8
      )
      
    }, error = function(e) {
      output$gho_validation_status <- renderText(
        paste("Échec de sauvegarde :", conditionMessage(e))
      )
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
    }, finally = {
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    })
  })
  
  # Télécharger la version restructurée
  output$download_gho_validated <- downloadHandler(
    filename = function() paste0(
      "GHO_validated_", format(Sys.Date(), "%Y%m%d"), ".xlsx"
    ),
    content = function(file) {
      req(rv$gho_validated)
      req(isTRUE(rv$gho_mapping_ok))
      writexl::write_xlsx(rv$gho_validated, file)
    }
  )
  
  
  
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
  
  lister_fichiers_sqlite <- function() {
    files <- list.files(
      "input/upload",
      pattern = "\\.xlsx$",
      full.names = FALSE,
      ignore.case = TRUE
    )
    files[!est_fichier_referentiel_pays(files)]
  }
  
  rv$sqlite_files <- shiny::reactiveVal(lister_fichiers_sqlite())
  analysis_db_status <- shiny::reactiveVal("La base analytique n’a pas encore été construite.")
  
  refresh_sqlite_files <- function() {
    files <- lister_fichiers_sqlite()
    rv$sqlite_files(files)
    updateCheckboxGroupInput(
      session,
      "selected_tables",
      choices = files,
      selected = character()
    )
  }
  
  output$sqlite_files_ui <- renderUI({
    files <- rv$sqlite_files()
    
    if (length(files) == 0L) {
      return(
        div(
          class = "alert alert-info",
          icon("folder-open"),
          " Aucun fichier .xlsx trouvé dans input/upload."
        )
      )
    }
    
    checkboxGroupInput(
      "selected_tables",
      "1. Choisissez les fichiers à charger :",
      choices = files,
      selected = character()
    )
  })
  
  output$sqlite_selection_summary <- renderUI({
    files <- rv$sqlite_files()
    selected <- input$selected_tables
    
    if (length(files) == 0L) {
      return(tags$span(class = "text-muted", "Aucun fichier disponible"))
    }
    if (is.null(selected) || length(selected) == 0L) {
      return(tags$span(class = "text-muted", paste(length(files), "fichier(s) disponible(s) — aucun sélectionné")))
    }
    
    tags$span(
      class = "text-info",
      icon("check-square"),
      paste(length(selected), "fichier(s) sélectionné(s) sur", length(files))
    )
  })
  
  output$sqlite_log_ui <- renderUI({
    message <- as.character(rv$log_sqlite)
    if (!length(message) || is.na(message[[1]]) || !nzchar(trimws(message[[1]]))) {
      message <- "Aucune opération SQLite n’a encore été exécutée."
    } else {
      message <- message[[1]]
    }

    style <- if (isTRUE(grepl("Erreur", message, fixed = TRUE))) {
      "alert alert-danger"
    } else {
      "alert alert-success"
    }
    div(class = style, message)
  })
  
  output$analysis_db_status <- renderText(analysis_db_status())
  
  output$download_analysis_db_ui <- renderUI({
    sqlite_db_refresh()
    con <- connect_tvd_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    if (!"epidemiological_analysis" %in% DBI::dbListTables(con)) return(NULL)
    downloadButton(
      "download_analysis_db",
      "Télécharger la base analytique (Excel)",
      icon = icon("download"),
      class = "btn-info"
    )
  })
  
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
  output$sqlite_tables <- renderDT({
    sqlite_db_refresh()
    con_tables <- connect_tvd_db()
    on.exit(DBI::dbDisconnect(con_tables), add = TRUE)
    
    tables <- DBI::dbListTables(con_tables)
    DT::datatable(
      data.frame(Table = tables),
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip")
    )
  })
  
  observeEvent(input$build_analysis_db, {
    con <- NULL
    tryCatch({
      con <- connect_tvd_db()
      rows <- rebuild_epidemiological_analysis(con)
      analysis_db_status(paste0(
        "Base analytique actualisée : ", rows,
        " observations ; sources GNARF et GHO ; jointure au dictionnaire par code indicateur."
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
  observeEvent(input$refresh_files, {
    refresh_sqlite_files()
  })
  
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
  
  # --- 4.75 Country / Localisation / Staff
  
  
  country_sheet_data <- shiny::reactiveVal(NULL)
  country_match_data <- shiny::reactiveVal(NULL)
  country_list_match_data <- shiny::reactiveVal(NULL)
  country_workbook_ready <- shiny::reactiveVal(FALSE)
  country_save_result <- shiny::reactiveVal(NULL)
  
  country_template_columns <- list(
    Localisation = c("ISO3", "Name", "Latitude", "Longitude"),
    Country_list = c(
      "flag", "cca3", "name", "official", "capital", "subregion",
      "area", "population", "languages", "ref_data", "data_source",
      "country_class"
    ),
    Staff_List = c(
      "Name", "Email", "Countries", "Position", "Grade", "Telephone",
      "Office Affiliation", "Responsibility", "Languages", "Diseases",
      "Level Geo"
    )
  )
  
  output$download_country_localisation_template <- downloadHandler(
    filename = function() "Modeles_Localisation_pays.xlsx",
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      for (sheet in names(country_template_columns)) {
        openxlsx::addWorksheet(wb, sheet)
        empty_data <- as.data.frame(
          setNames(
            replicate(length(country_template_columns[[sheet]]), character(0), simplify = FALSE),
            country_template_columns[[sheet]]
          ),
          check.names = FALSE
        )
        openxlsx::writeData(wb, sheet, empty_data, withFilter = TRUE)
        openxlsx::freezePane(wb, sheet, firstRow = TRUE)
        openxlsx::setColWidths(
          wb, sheet, cols = seq_along(country_template_columns[[sheet]]),
          widths = "auto"
        )
      }
      openxlsx::addWorksheet(wb, "Instructions")
      openxlsx::writeData(
        wb,
        "Instructions",
        data.frame(
          Feuille = c("Localisation", "Country_list", "Staff_List"),
          Utilisation = c(
            "Référence canonique des pays : code ISO3, nom et coordonnées.",
            "Les noms seront harmonisés à partir du code cca3.",
            "La colonne Countries accepte plusieurs pays séparés par des virgules ou des points-virgules."
          ),
          check.names = FALSE
        ),
        withFilter = TRUE
      )
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  country_normalize_name <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x[is.na(x)] <- ""
    x <- tolower(trimws(x))
    x <- gsub("[^a-z0-9]+", " ", x)
    trimws(gsub("\\s+", " ", x))
  }
  
  country_find_column <- function(data, candidates, required = TRUE) {
    normalized <- janitor::make_clean_names(names(data))
    candidates <- janitor::make_clean_names(candidates)
    idx <- match(candidates, normalized, nomatch = 0L)
    idx <- idx[idx > 0L]
    if (length(idx) > 0L) return(names(data)[idx[1]])
    if (required) {
      stop(
        "Colonne requise absente : ", paste(candidates, collapse = " / "),
        ". Colonnes trouvées : ", paste(names(data), collapse = ", ")
      )
    }
    NULL
  }
  
  observeEvent(input$country_localisation_file, {
    country_sheet_data(NULL)
    country_match_data(NULL)
    country_list_match_data(NULL)
    country_workbook_ready(FALSE)
    country_save_result(NULL)
  })
  
  observeEvent(input$process_country_localisation, {
    req(input$country_localisation_file)
    country_sheet_data(NULL)
    country_match_data(NULL)
    country_list_match_data(NULL)
    country_workbook_ready(FALSE)
    country_save_result(NULL)
    
    tryCatch({
      path <- input$country_localisation_file$datapath
      sheets_found <- readxl::excel_sheets(path)
      sheet_lookup <- stats::setNames(sheets_found, tolower(trimws(sheets_found)))
      required_sheets <- names(country_template_columns)
      missing_sheets <- required_sheets[!tolower(required_sheets) %in% names(sheet_lookup)]
      if (length(missing_sheets) > 0L) {
        stop("Feuille(s) absente(s) : ", paste(missing_sheets, collapse = ", "))
      }
      
      sheets <- stats::setNames(lapply(required_sheets, function(sheet) {
        actual_sheet <- sheet_lookup[[tolower(sheet)]]
        data <- readxl::read_excel(
          path,
          sheet = actual_sheet,
          .name_repair = "minimal"
        ) |>
          as.data.frame(check.names = FALSE)
        keep <- if (nrow(data) > 0L) {
          vapply(seq_len(nrow(data)), function(i) {
            row <- unlist(data[i, , drop = FALSE], use.names = FALSE)
            any(!is.na(row) & nzchar(trimws(as.character(row))))
          }, logical(1))
        } else logical()
        data[keep, , drop = FALSE]
      }), required_sheets)
      
      loc <- sheets$Localisation
      loc_code_col <- country_find_column(loc, c("ISO3", "cca3"))
      loc_name_col <- country_find_column(loc, c("Name", "country_name"))
      if (nrow(loc) == 0L) stop("La feuille Localisation ne contient aucun pays.")
      loc_codes <- toupper(trimws(as.character(loc[[loc_code_col]])))
      loc_names <- trimws(as.character(loc[[loc_name_col]]))
      if (anyNA(loc_codes) || any(!nzchar(loc_codes)) || anyNA(loc_names) || any(!nzchar(loc_names))) {
        stop("La feuille Localisation doit avoir un code ISO3 et un nom pour chaque pays.")
      }
      if (anyDuplicated(loc_codes)) stop("La feuille Localisation contient des codes ISO3 en double.")
      if (anyDuplicated(country_normalize_name(loc_names))) {
        stop("La feuille Localisation contient des noms de pays en double après normalisation.")
      }
      
      country_list <- sheets$Country_list
      country_code_col <- country_find_column(country_list, c("cca3", "ISO3"))
      country_list_name_col <- country_find_column(country_list, c("name", "country_name"))
      country_official_col <- country_find_column(country_list, c("official", "official_name"), required = FALSE)
      country_list_codes <- toupper(trimws(as.character(country_list[[country_code_col]])))
      if (nrow(country_list) == 0L) stop("La feuille Country_list ne contient aucun pays.")
      canonical_by_code <- stats::setNames(loc_names, loc_codes)
      unknown_codes <- unique(country_list_codes[
        is.na(country_list_codes) | !nzchar(country_list_codes) |
          !country_list_codes %in% names(canonical_by_code)
      ])
      if (length(unknown_codes) > 0L) {
        stop(
          "Codes cca3 absents de la feuille Localisation : ",
          paste(unknown_codes, collapse = ", ")
        )
      }
      country_list_review <- data.frame(
        cca3 = country_list_codes,
        name_in_file = as.character(country_list[[country_list_name_col]]),
        canonical_name = unname(canonical_by_code[country_list_codes]),
        result = ifelse(
          country_normalize_name(country_list[[country_list_name_col]]) ==
            country_normalize_name(unname(canonical_by_code[country_list_codes])),
          "Nom conforme",
          "Sera harmonisé par le code cca3"
        ),
        stringsAsFactors = FALSE
      )
      
      staff <- sheets$Staff_List
      staff_countries_col <- country_find_column(staff, c("Countries", "Country", "Pays"))
      
      alias_values <- c(loc_names, as.character(country_list[[country_list_name_col]]))
      alias_targets <- c(
        loc_names,
        unname(canonical_by_code[country_list_codes])
      )
      if (!is.null(country_official_col)) {
        alias_values <- c(alias_values, as.character(country_list[[country_official_col]]))
        alias_targets <- c(alias_targets, unname(canonical_by_code[country_list_codes]))
      }
      alias_keys <- country_normalize_name(alias_values)
      valid_alias <- nzchar(alias_keys) & !is.na(alias_targets)
      alias_keys <- alias_keys[valid_alias]
      alias_targets <- alias_targets[valid_alias]
      keep_alias <- !duplicated(alias_keys)
      alias_keys <- alias_keys[keep_alias]
      alias_targets <- alias_targets[keep_alias]
      
      staff_country_values <- as.character(staff[[staff_countries_col]])
      country_tokens <- unique(trimws(unlist(strsplit(
        staff_country_values,
        "\\s*[,;|]\\s*"
      ), use.names = FALSE)))
      country_tokens <- country_tokens[!is.na(country_tokens) & nzchar(country_tokens)]
      if (length(country_tokens) == 0L) {
        matches <- data.frame(
          country_in_file = character(),
          canonical_name = character(),
          match_type = character(),
          stringsAsFactors = FALSE
        )
      } else {
        canonical_keys <- country_normalize_name(loc_names)
        matches <- lapply(country_tokens, function(token) {
          token_key <- country_normalize_name(token)
          exact <- match(token_key, canonical_keys, nomatch = 0L)
          alias <- match(token_key, alias_keys, nomatch = 0L)
          if (exact > 0L) {
            proposed <- loc_names[exact]
            method <- "Nom standard"
          } else if (alias > 0L) {
            proposed <- alias_targets[alias]
            method <- "Alias reconnu"
          } else {
            distance <- as.numeric(utils::adist(token_key, canonical_keys)[1, ])
            closest <- which.min(distance)
            threshold <- max(2L, floor(nchar(token_key) * 0.25))
            proposed <- if (length(closest) && distance[closest] <= threshold) {
              loc_names[closest]
            } else ""
            method <- if (nzchar(proposed)) "Suggestion à confirmer" else "À corriger manuellement"
          }
          data.frame(
            country_in_file = token,
            canonical_name = proposed,
            match_type = method,
            stringsAsFactors = FALSE
          )
        }) |>
          dplyr::bind_rows()
      }
      
      country_sheet_data(sheets)
      country_match_data(matches)
      country_list_match_data(country_list_review)
      country_workbook_ready(TRUE)
      showNotification(
        paste("Classeur lu :", length(sheets), "feuilles séparées ;", nrow(matches), "nom(s) de pays à vérifier."),
        type = "message",
        duration = 8
      )
    }, error = function(e) {
      country_workbook_ready(FALSE)
      country_save_result(NULL)
      showNotification(
        paste("Échec de l’analyse du classeur :", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })
  
  
  output$country_localisation_ready <- renderText({
    if (isTRUE(country_workbook_ready())) "TRUE" else "FALSE"
  })
  outputOptions(output, "country_localisation_ready", suspendWhenHidden = FALSE)
  
  output$country_localisation_status <- renderUI({
    if (!is.null(input$country_localisation_file) && !isTRUE(country_workbook_ready())) {
      return(tags$div(
        class = "alert alert-info",
        "Importez le classeur puis cliquez sur « Séparer les feuilles et vérifier les pays »."
      ))
    }
    if (isTRUE(country_workbook_ready())) {
      matches <- country_match_data()
      alias_count <- sum(matches$match_type == "Alias reconnu", na.rm = TRUE)
      suggestion_count <- sum(matches$match_type == "Suggestion à confirmer", na.rm = TRUE)
      unmatched_count <- sum(matches$match_type == "À corriger manuellement", na.rm = TRUE)
      return(tags$div(
        class = "alert alert-success",
        paste(
          "Référentiel chargé.", alias_count, "alias reconnu(s),",
          suggestion_count, "suggestion(s) à confirmer,",
          unmatched_count, "nom(s) à corriger manuellement."
        )
      ))
    }
    tags$div(class = "alert alert-info", "Téléchargez le modèle ou importez un classeur Excel pour commencer.")
  })
  
  country_preview_output <- function(sheet_name) {
    DT::renderDT({
      sheets <- country_sheet_data()
      req(sheets)
      DT::datatable(
        head(sheets[[sheet_name]], 1000),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  }
  output$country_preview_localisation <- country_preview_output("Localisation")
  output$country_preview_list <- country_preview_output("Country_list")
  output$country_preview_staff <- country_preview_output("Staff_List")
  
  output$country_match_review <- DT::renderDT({
    matches <- country_match_data()
    req(matches)
    display <- matches
    names(display) <- c("Nom trouvé dans Staff_List", "Pays standard proposé", "Résultat")
    DT::datatable(
      display,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 2))),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$country_list_match_review <- DT::renderDT({
    review <- country_list_match_data()
    req(review)
    display <- review
    names(display) <- c("Code cca3", "Nom dans le fichier", "Nom standard", "Résultat")
    DT::datatable(
      display,
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })
  
  observeEvent(input$country_match_review_cell_edit, {
    info <- input$country_match_review_cell_edit
    matches <- country_match_data()
    req(matches)
    if (info$col == 2L && info$row >= 1L && info$row <= nrow(matches)) {
      matches$canonical_name[info$row] <- trimws(as.character(info$value))
      matches$match_type[info$row] <- "Modifié par l’utilisateur"
      country_match_data(matches)
    }
  })
  
  observeEvent(input$apply_country_corrections, {
    sheets <- country_sheet_data()
    matches <- country_match_data()
    req(sheets, matches)
    
    tryCatch({
      loc <- sheets$Localisation
      loc_code_col <- country_find_column(loc, c("ISO3", "cca3"))
      loc_name_col <- country_find_column(loc, c("Name", "country_name"))
      country_list <- sheets$Country_list
      country_code_col <- country_find_column(country_list, c("cca3", "ISO3"))
      country_list_name_col <- country_find_column(country_list, c("name", "country_name"))
      staff <- sheets$Staff_List
      staff_countries_col <- country_find_column(staff, c("Countries", "Country", "Pays"))
      
      loc_codes <- toupper(trimws(as.character(loc[[loc_code_col]])))
      loc_names <- trimws(as.character(loc[[loc_name_col]]))
      canonical_by_code <- stats::setNames(loc_names, loc_codes)
      valid_targets <- country_normalize_name(loc_names)
      proposed_keys <- country_normalize_name(matches$canonical_name)
      unresolved <- nzchar(matches$country_in_file) &
        (!nzchar(proposed_keys) | !proposed_keys %in% valid_targets)
      if (any(unresolved)) {
        stop(
          "Corrigez les noms sans pays standard valide : ",
          paste(matches$country_in_file[unresolved], collapse = ", ")
        )
      }
      if (nrow(country_list) > 0L) {
        list_codes <- toupper(trimws(as.character(country_list[[country_code_col]])))
        if (anyNA(list_codes) || any(!nzchar(list_codes)) || any(!list_codes %in% names(canonical_by_code))) {
          stop("Country_list contient un code cca3 vide ou absent de Localisation.")
        }
        country_list_corrections <- sum(
          country_normalize_name(country_list[[country_list_name_col]]) !=
            country_normalize_name(unname(canonical_by_code[list_codes])),
          na.rm = TRUE
        )
        country_list[[country_list_name_col]] <- unname(canonical_by_code[list_codes])
      } else {
        country_list_corrections <- 0L
      }
      
      correction_keys <- country_normalize_name(matches$country_in_file)
      correction_values <- loc_names[match(proposed_keys, valid_targets)]
      correction_map <- stats::setNames(correction_values, correction_keys)
      staff_values <- as.character(staff[[staff_countries_col]])
      staff[[staff_countries_col]] <- vapply(staff_values, function(value) {
        if (is.na(value) || !nzchar(trimws(value))) return(value)
        tokens <- trimws(unlist(strsplit(value, "\\s*[,;|]\\s*", perl = TRUE), use.names = FALSE))
        keys <- country_normalize_name(tokens)
        corrected <- unname(correction_map[keys])
        corrected[is.na(corrected) | !nzchar(corrected)] <- tokens[is.na(corrected) | !nzchar(corrected)]
        paste(corrected, collapse = ", ")
      }, character(1))
      
      output_dir <- file.path("input", "dictionnaire")
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      final_sheets <- list(
        Localisation = loc,
        Country_list = country_list,
        Staff_List = staff
      )
      output_files <- character(length(final_sheets))
      names(output_files) <- names(final_sheets)
      for (sheet in names(final_sheets)) {
        target <- file.path(output_dir, paste0(sheet, ".xlsx"))
        openxlsx::write.xlsx(
          stats::setNames(list(final_sheets[[sheet]]), sheet),
          file = target,
          overwrite = TRUE,
          rowNames = FALSE
        )
        output_files[sheet] <- target
      }
      
      # Enregistrer les trois référentiels corrigés dans SQLite en remplacement
      # des versions précédentes afin de ne pas accumuler de doublons.
      country_con <- connect_tvd_db()
      tryCatch({
        DBI::dbWithTransaction(country_con, {
          DBI::dbWriteTable(
            country_con, "localisation", as.data.frame(loc),
            overwrite = TRUE, row.names = FALSE
          )
          DBI::dbWriteTable(
            country_con, "country_list", as.data.frame(country_list),
            overwrite = TRUE, row.names = FALSE
          )
          DBI::dbWriteTable(
            country_con, "staff_list", as.data.frame(staff),
            overwrite = TRUE, row.names = FALSE
          )
        })
      }, finally = {
        DBI::dbDisconnect(country_con)
      })
      sqlite_db_refresh(sqlite_db_refresh() + 1L)
      refresh_analysis_tables(session)
      
      country_sheet_data(final_sheets)
      country_save_result(list(
        files = output_files,
        tables = c("localisation", "country_list", "staff_list"),
        staff_corrected = sum(matches$country_in_file != matches$canonical_name, na.rm = TRUE),
        list_corrected = country_list_corrections
      ))
      showNotification(
        paste(
          "Les corrections sont appliquées. Les copies Excel sont dans input/dictionnaire et les tables SQLite ont été remplacées : localisation, country_list, staff_list."
        ),
        type = "message",
        duration = 8
      )
    }, error = function(e) {
      country_save_result(NULL)
      showNotification(
        paste("Échec de l’application des corrections :", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })
  
  
  output$country_localisation_save_status <- renderUI({
    result <- country_save_result()
    if (is.null(result)) return(NULL)
    tags$div(
      class = "alert alert-success",
      tags$strong("Copies créées. "),
      paste(
        result$staff_corrected, "correction(s) dans Staff_List et",
        result$list_corrected, "nom(s) harmonisé(s) dans Country_list.",
        "Tables SQLite remplacées :",
        paste(result$tables, collapse = ", "), ". Fichiers Excel :"
      ),
      tags$ul(lapply(basename(result$files), tags$li))
    )
  })
  
  
  rv$indicator_dictionary <- load_indicator_dictionary_safe()
  
  
  # ============================================================
  # LISTE DES INDICATEURS GHO DEPUIS LE DICTIONNAIRE
  # ============================================================
  
  observe({
    # Le code sélectionné doit provenir du catalogue GHO (IndicatorCode),
    # et non du dictionnaire GNARF qui décrit des codes internes.
    local_catalogue <- load_local_gho()
    con <- connect_tvd_db()
    catalogue <- tryCatch({
      if ("gho_catalogue" %in% DBI::dbListTables(con)) {
        DBI::dbReadTable(con, "gho_catalogue")
      } else data.frame()
    }, error = function(e) data.frame(), finally = DBI::dbDisconnect(con))
    
    # Lorsque le catalogue local est vide, charger le catalogue officiel GHO.
    # Le résultat est mis en cache dans SQLite ; l'application reste utilisable
    # hors ligne grâce au catalogue local ou au cache.
    if (nrow(catalogue) == 0L && nrow(local_catalogue) == 0L) {
      catalogue <- tryCatch({
        response <- httr::GET(GHO_API_URL, query = list("$top" = 10000L),
                              httr::timeout(60),
                              httr::user_agent("SOP-Data-App-TVD"))
        httr::stop_for_status(response)
        payload <- jsonlite::fromJSON(
          httr::content(response, as = "text", encoding = "UTF-8"),
          flatten = TRUE
        )
        api <- as.data.frame(payload$value, stringsAsFactors = FALSE)
        code_col <- intersect(c("IndicatorCode", "indicator_code"), names(api))[1]
        name_col <- intersect(c("IndicatorName", "indicator_name"), names(api))[1]
        if (is.na(code_col) || is.na(name_col)) stop("Colonnes IndicatorCode/IndicatorName absentes.")
        api <- data.frame(indicator_code = as.character(api[[code_col]]),
                          indicator_name = as.character(api[[name_col]]),
                          language = NA_character_, catalogue_source = "API GHO",
                          stringsAsFactors = FALSE)
        api <- api[!is.na(api$indicator_code) & nzchar(api$indicator_code), , drop = FALSE]
        con <- connect_tvd_db()
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        if (nrow(api)) DBI::dbWriteTable(con, "gho_catalogue", api,
                                         append = TRUE, row.names = FALSE)
        api
      }, error = function(e) {
        message("Catalogue API GHO indisponible : ", conditionMessage(e))
        data.frame()
      })
    }
    
    dict <- dplyr::bind_rows(catalogue, local_catalogue)
    needed <- c("indicator_code", "indicator_name")
    if (!all(needed %in% names(dict)) || nrow(dict) == 0L) {
      updateSelectInput(
        session,
        inputId = "gho_ind_code",
        choices = character(0),
        selected = character(0)
      )
      return()
    }
    
    dict$indicator_code <- trimws(as.character(dict$indicator_code))
    dict$indicator_name <- trimws(as.character(dict$indicator_name))
    dict <- dict[!is.na(dict$indicator_code) & nzchar(dict$indicator_code), , drop = FALSE]
    dict <- dplyr::distinct(dict, indicator_code, .keep_all = TRUE)
    codes <- dict$indicator_code
    labels <- ifelse(is.na(dict$indicator_name) | !nzchar(dict$indicator_name),
                     codes, paste0(codes, " — ", dict$indicator_name))
    
    choices <- stats::setNames(codes, labels)
    
    # Conserver la sélection actuelle si elle reste disponible
    current <- isolate(input$gho_ind_code)
    
    selected_code <- if (
      length(current) == 1 &&
      !is.na(current) &&
      current %in% codes
    ) {
      current
    } else {
      character(0)
    }
    
    updateSelectizeInput(
      session,
      inputId = "gho_ind_code",
      choices = choices,
      selected = selected_code,
      server = TRUE
    )
  })
  
  
  
# opérateur local de remplacement des valeurs nulles
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

output$indicator_dictionary_status <- renderText({
  dict <- rv$indicator_dictionary
  
  if (is.null(dict) || nrow(dict) == 0L) {
    load_error <- attr(dict, "load_error")
    
    if (!is.null(load_error)) {
      return(
        paste(
          "Dictionnaire non chargé :",
          load_error
        )
      )
    }
    
    return(
      paste(
        "Dictionnaire non chargé.",
        "Placez un fichier indicator.xlsx dans",
        "input/dictionnaire."
      )
    )
  }
  
  paste0(
    "Fichier : ",
    attr(dict, "source_path") %||% "indicator",
    "\n",
    "Indicateurs disponibles : ",
    nrow(dict),
    "\n",
    "Codes distincts : ",
    dplyr::n_distinct(dict$indicator_code)
  )
})


observeEvent(input$reload_indicator_dictionary, {
  dict <- load_indicator_dictionary_safe()
  rv$indicator_dictionary <- dict
  if (nrow(dict) == 0L) {
    showNotification(
      "Dictionnaire indicator non chargé. Vérifiez le fichier et ses colonnes.",
      type = "error", duration = NULL
    )
  } else {
    showNotification(
      paste(nrow(dict), "indicateurs chargés depuis le dictionnaire local."),
      type = "message"
    )
  }
})

# Lire un fichier GNARF .xls/.xlsx en détectant la ligne d'en-tête.


read_gnarf_dataset <- function(file_info, dataset_type) {
  if (is.null(file_info) || nrow(file_info) < 1L) {
    stop("Sélectionnez le fichier GNARF à importer.")
  }
  
  path <- file_info$datapath[1]
  source_file <- basename(file_info$name[1])
  
  if (!file.exists(path)) stop("Fichier temporaire d'importation introuvable.")
  ext <- tolower(tools::file_ext(source_file))
  if (!ext %in% c("xls", "xlsx")) {
    stop("Format non pris en charge : utilisez un fichier .xls ou .xlsx.")
  }
  
  raw0 <- readxl::read_excel(
    path, col_names = FALSE, .name_repair = "minimal"
  )
  if (nrow(raw0) == 0L) stop("Le fichier GNARF est vide.")
  
  nonempty <- vapply(seq_len(nrow(raw0)), function(i) {
    row <- unlist(raw0[i, , drop = FALSE], use.names = FALSE)
    any(!is.na(row) & nzchar(trimws(as.character(row))))
  }, logical(1))
  header_row <- which(nonempty)[1]
  if (is.na(header_row)) stop("Aucune ligne d'en-tête détectée.")
  
  df <- readxl::read_excel(path, skip = header_row - 1L) |>
    janitor::clean_names() |>
    as.data.frame()
  
  if (nrow(df) == 0L) stop("Le fichier ne contient aucune donnée après l'en-tête.")
  
  # Harmoniser quelques variantes possibles des noms de colonnes.
  aliases <- list(
    data = c("data", "indicator", "indicator_name", "indicator_code", "indicateur"),
    period = c("period", "year", "annee", "time_period"),
    organisation_unit = c(
      "organisation_unit", "organization_unit", "organisationunit",
      "organizationunit", "country", "pays", "organisation_unit_name"
    ),
    value = c("value", "valeur", "status", "endemicity_status")
  )
  for (target in names(aliases)) {
    if (!target %in% names(df)) {
      candidate <- intersect(aliases[[target]], names(df))
      if (length(candidate) > 0L) df[[target]] <- df[[candidate[1]]]
    }
  }
  
  required <- c("data", "period", "organisation_unit", "value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop(
      "Colonnes obligatoires absentes dans ", source_file, " : ",
      paste(missing, collapse = ", "),
      ". Colonnes trouvées : ", paste(names(df), collapse = ", ")
    )
  }
  
  df$data <- trimws(as.character(df$data))
  #df$period <- as.character(df$period)
  # df$organisation_unit <- as.character(df$organisation_unit)
  df$period <- as.character(df$period)
  df$organisation_unit <- as.character(df$organisation_unit)
  
  df$period[is.na(df$period)] <- ""
  df$organisation_unit[is.na(df$organisation_unit)] <- ""
  df$data[is.na(df$data)] <- ""
  df$value[is.na(df$value)] <- ""
  df$indicator_original <- df$data
  df$gnarf_type <- dataset_type
  df$source_file <- source_file
  df$imported_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Pour le fichier numérique, catégoriser uniquement à titre indicatif.
  if (dataset_type == "numerique") {
    label <- normalize_gnarf_text(df$data)
    df$indicator_domain <- ifelse(
      grepl("prevalence|prevalent|human rabies cases", label), "Prevalence",
      ifelse(
        grepl("incidence|incident|new cases|new confirmed|new clinical cases|new PB cases|new", label),
        "Incidence",
        ifelse(
          grepl("mortality|mortality rate|deaths|death rate|death", label),
          "Mortalité",
          ifelse(
            grepl("treatment|treated|care|coverage|management", label),
            "Prise en charge",
            "Prevalence"#"Autre / à classifier"
          )
        )
      )
    )
  } else {
    df$endemicity_status_code <- as.character(df$value)
  }
  
  # Valeurs numériques conservées dans les colonnes d'origine ; colonne
  # numérique auxiliaire utile aux analyses lorsque la conversion est possible.
  # df$value_numeric <- suppressWarnings(as.numeric(as.character(df$value)))
  raw_value <- as.character(df$value)
  
  raw_value <- trimws(raw_value)
  
  raw_value <- gsub(",", ".", raw_value, fixed = TRUE)
  
  df$value_numeric <- suppressWarnings(
    as.numeric(raw_value)
  )
  df
}

parse_gnarf_label <- function(x) {
  parsed <- split_gnarf_indicator(x)
  
  data.frame(
    gnarf_indicator_code = parsed$gnarf_indicator_code,
    gnarf_indicator_name = parsed$gnarf_indicator_name,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# DIFFERENCIER AUTOMATIQUEMENT LES CODES GNARF IDENTIQUES
# ============================================================

disambiguate_gnarf_codes <- function(labels) {
  
  labels <- as.character(labels)
  labels[is.na(labels)] <- ""
  
  parsed <- split_gnarf_indicator(labels)
  
  result <- parsed$gnarf_indicator_code
  
  valid <- !is.na(result) & nzchar(result)
  
  if (!any(valid)) {
    return(result)
  }
  
  # Mots génériques à ignorer dans la recherche distinctive
  stop_words <- c(
    "number", "of", "the", "a", "an", "and", "or",
    "for", "from", "in", "on", "with", "by", "to",
    "all", "total", "reported", "reporting",
    "indicator", "indicators", "data", "value",
    "cases", "case", "disease", "diseases",
    "number", "rate", "proportion", "percentage",
    "prevalence", "incidence", "status", "human"
  )
  
  # Nettoyer un libellé pour produire un suffixe
  make_suffix <- function(label, other_labels) {
    
    label_norm <- normalize_gnarf_text(label)
    other_norm <- normalize_gnarf_text(other_labels)
    
    words <- unique(strsplit(label_norm, " +")[[1]])
    
    words <- words[
      nzchar(words) &
        !words %in% stop_words &
        nchar(words) >= 2
    ]
    
    # Priorité aux mots qui distinguent ce libellé
    # des autres libellés possédant le même code.
    other_words <- unlist(
      strsplit(other_norm, " +"),
      use.names = FALSE
    )
    
    distinctive <- words[!words %in% other_words]
    
    # Si aucun mot totalement distinct n'existe,
    # conserver les mots significatifs du libellé.
    if (length(distinctive) == 0L) {
      distinctive <- words
    }
    
    if (length(distinctive) == 0L) {
      distinctive <- "INDICATOR"
    }
    
    suffix <- toupper(paste(distinctive, collapse = "-"))
    suffix <- gsub("[^A-Z0-9-]", "", suffix)
    suffix <- gsub("-+", "-", suffix)
    suffix <- gsub("^-|-$", "", suffix)
    
    suffix
  }
  
  # Identifier les groupes de codes répétés.
  code_groups <- split(which(valid), result[valid])
  
  for (group_name in names(code_groups)) {
    
    idx <- code_groups[[group_name]]
    
    # Ne rien changer si le code est unique.
    if (length(idx) < 2L) {
      next
    }
    
    group_labels <- labels[idx]
    group_names <- parsed$gnarf_indicator_name[idx]
    
    # Utiliser le nom extrait, ou le libellé original
    # si le nom extrait est vide.
    group_names[
      is.na(group_names) | !nzchar(trimws(group_names))
    ] <- group_labels[
      is.na(group_names) | !nzchar(trimws(group_names))
    ]
    
    suffixes <- vapply(
      seq_along(idx),
      function(j) {
        others <- group_names[-j]
        
        make_suffix(
          group_names[j],
          others
        )
      },
      character(1)
    )
    
    # Garantir un suffixe unique même si les libellés
    # ne possèdent pas de mot distinctif exploitable.
    suffixes <- make.unique(suffixes, sep = "-VAR-")
    
    result[idx] <- paste0(group_name, "-", suffixes)
  }
  
  result
}


  # Etat partage recu via rv.
}
