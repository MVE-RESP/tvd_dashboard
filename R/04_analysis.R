# ================================================================
# 04_analysis.R — ETL analytique + 6 produits d'information + rapport UN
# ================================================================

ui_tab_analysis <- function() {
  tabItem(
    tabName = "analysis",
    h4("Analyses régionales"),
    box(
      title = "Choisir une maladie, une période et une source",
      status = "primary", solidHeader = FALSE, width = 12,
      p("Explorez les statuts d’endémicité et les indicateurs enregistrés dans le datawarehouse.",
        class = "text-muted"),
      fluidRow(
        column(4, selectInput("regional_source", "Source des données",
                              choices = c("GNARF" = "gnarf", "GHO" = "gho"),
                              selected = "gnarf")),
        column(8, uiOutput("regional_filters"))
      ),
      fluidRow(
        column(6, actionButton("regional_generate_heatmap", "Générer la carte d’endémicité",
                               icon = icon("th"), class = "btn-primary")),
        column(6, actionButton("regional_generate_factsheet", "Afficher la fiche régionale",
                               icon = icon("chart-bar"), class = "btn-info"))
      )
    ),
    tabsetPanel(
      tabPanel("A. Endemicity Heatmap", br(), uiOutput("endemicity_summary"),
               fluidRow(column(3, uiOutput("regional_map_period_filter")),
                        column(9, DT::DTOutput("endemicity_heatmap")))),
      tabPanel("B. Regional factsheet", br(), uiOutput("regional_factsheet_summary"),
               downloadButton("download_regional_factsheet", "Exporter la fiche en Excel",
                              class = "btn-success"),
               h4("Résumé régional par indicateur"), DT::DTOutput("regional_indicator_summary"),
               h4("Tendances régionales"), plotOutput("regional_trend_plot", height = "420px"),
               h4("Résultats par pays"), DT::DTOutput("regional_country_summary")),
      tabPanel("C. Country profile", br(), uiOutput("regional_country_filter"),
               actionButton("regional_generate_country_profile", "Afficher le profil du pays",
                            icon = icon("globe"), class = "btn-info"),
               br(), br(), uiOutput("country_profile_summary"),
               h4("Indicateurs et statuts du pays"), DT::DTOutput("country_profile_table"),
               h4("Tendances du pays"), plotOutput("country_profile_plot", height = "420px")),
      tabPanel("Exploration libre", br(),
               selectInput("analysis_table", "Table à analyser", choices = NULL),
               uiOutput("analysis_filters"), DT::DTOutput("analysis_data"),
               plotOutput("analysis_plot", height = "350px"),
               leaflet::leafletOutput("analysis_map", height = "500px"))
    )
  )
}

ui_tab_produits <- function() {
  tabItem(
    tabName = "produits",
    h4("Rapport d’analyse de la base analytique"),
    p("Le rapport est produit à partir de reports/Analyse_base_analytique.Rmd et de la table SQLite ntd_fusionnee."),
    p("Les filtres sont disponibles dans le rapport HTML généré."),
    actionButton("generate_rmd_report", "Générer / actualiser le rapport",
                 icon = icon("file-alt"), class = "btn-primary"),
    downloadButton("download_rmd_report", "Télécharger le rapport HTML",
                   class = "btn-success"),
    br(), br(),
    uiOutput("rmd_report_status"),
    uiOutput("rmd_report_frame")
  )
}
ui_tab_un_report <- function() {
  
  tabItem(
    
    tabName = "un_report",
    
    div(
      class = "un-report-header",
      
      h2(
        "Rapport analytique style Nations Unies",
        class = "page-heading"
      ),
      
      p(
        paste(
          "Visualisation intégrée des six produits",
          "d'information avec les filtres sélectionnés ci-dessous."
        ),
        class = "page-subtitle"
      )
    ),
    
    box(
      title = "Filtres partagés du rapport",
      status = "primary",
      width = 12,
      collapsible = TRUE,
      
      fluidRow(
        column(
          width = 3,
          uiOutput("rep_year_ui")
        ),
        column(
          width = 3,
          uiOutput("rep_disease_ui")
        ),
        column(
          width = 3,
          uiOutput("rep_country_ui")
        ),
        column(
          width = 3,
          uiOutput("rep_indicator_ui")
        )
      ),
      
      helpText(
        "Ces filtres pilotent toutes les sections du rapport ci-dessous."
      )
    ),
    
    box(
      title = "Aperçu du rapport",
      status = "info",
      width = 12,
      
      shinycssloaders::withSpinner(
        ui = htmlOutput("un_report_frame"),
        type = 6,
        color = un_palette$blue
      )
    ),
    
    box(
      title = "Export",
      status = "success",
      width = 12,
      
      downloadButton(
        outputId = "download_un_report_html",
        label = "HTML UN-Style",
        class = "btn-primary"
      ),
      
      downloadButton(
        outputId = "download_un_report_docx",
        label = "Word",
        class = "btn-default"
      ),
      
      downloadButton(
        outputId = "download_un_report_pdf",
        label = "PDF",
        class = "btn-default"
      )
    )
  )
}


# ================================================================
# SERVEUR — ANALYSES ET PRODUITS D'INFORMATION
# ================================================================

srv_analysis <- function(
    input,
    output,
    session,
    rv,
    sqlite_db_refresh = NULL
) {
  
  # --------------------------------------------------------------
  # 0. Fonctions utilitaires internes
  # --------------------------------------------------------------
  
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
  
  clean_choices <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & nzchar(x)]
    sort(unique(x))
  }
  
  find_col <- function(data, candidates) {
    
    if (is.null(data) || !is.data.frame(data)) {
      return(NULL)
    }
    
    found <- intersect(candidates, names(data))
    
    if (length(found) == 0) {
      return(NULL)
    }
    
    found[[1]]
  }
  
  first_existing_object <- function(objects) {
    
    for (object in objects) {
      
      if (
        !is.null(object) &&
        is.data.frame(object) &&
        nrow(object) > 0
      ) {
        return(object)
      }
    }
    
    NULL
  }

  read_analysis_table <- function() {
    # Les modules d'import écrivent dans SQLite; ils ne gardent pas
    # nécessairement une copie des données dans rv.
    con <- tryCatch(connect_tvd_db(), error = function(e) NULL)
    if (is.null(con)) return(NULL)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    tables <- tryCatch(DBI::dbListTables(con), error = function(e) character())
    for (table in c("ntd_fusionnee", "final_analysis_database",
                    "epidemiological_analysis", "gnarf_master")) {
      if (!table %in% tables) next
      data <- tryCatch(DBI::dbReadTable(con, table), error = function(e) NULL)
      if (is.data.frame(data) && nrow(data) > 0L) return(data)
    }
    NULL
  }
  
  first_selected <- function(x) {
    
    if (is.null(x) || length(x) == 0) {
      return(NULL)
    }
    
    x[[1]]
  }
  
  empty_plot <- function(message = "Aucune donnée disponible") {
    
    graphics::plot.new()
    
    graphics::text(
      x = 0.5,
      y = 0.5,
      labels = message,
      cex = 1.2,
      col = "#666666"
    )
  }
  
  filter_value <- function(data, column, selected) {
    
    if (
      is.null(data) ||
      is.null(column) ||
      is.null(selected) ||
      length(selected) == 0 ||
      identical(selected, "__ALL__")
    ) {
      return(data)
    }
    
    data[
      as.character(data[[column]]) %in% as.character(selected),
      ,
      drop = FALSE
    ]
  }
  
  make_selectize <- function(
    input_id,
    label,
    choices,
    multiple = TRUE,
    include_all = TRUE
  ) {
    
    choices <- clean_choices(choices)
    
    if (include_all) {
      
      final_choices <- c(
        "Toutes les valeurs" = "__ALL__",
        stats::setNames(choices, choices)
      )
      
      selected <- "__ALL__"
      
    } else {
      
      final_choices <- stats::setNames(choices, choices)
      
      selected <- if (length(choices) > 0) choices[[1]] else NULL
    }
    
    shiny::selectizeInput(
      inputId = input_id,
      label = label,
      choices = final_choices,
      selected = selected,
      multiple = multiple,
      options = list(
        plugins = if (multiple) list("remove_button") else NULL,
        placeholder = paste("Sélectionner", tolower(label))
      )
    )
  }
  
  # --------------------------------------------------------------
  # 1. Base analytique provenant de rv
  # --------------------------------------------------------------
  
  analysis_raw <- shiny::reactive({
    
    # Force éventuellement la réévaluation après actualisation SQLite.
    if (
      !is.null(sqlite_db_refresh) &&
      shiny::is.reactive(sqlite_db_refresh)
    ) {
      sqlite_db_refresh()
    }
    
    data <- first_existing_object(
      list(
        rv$analysis_data,
        rv$analytic_data,
        rv$base_analytique,
        rv$base_analysis,
        rv$combined_data,
        rv$merged_data,
        rv$indicator_data,
        rv$indicators_data,
        rv$gnarf_data,
        rv$gnarf_master,
        rv$data
      )
    )
    if (is.null(data)) data <- read_analysis_table()
    data
  })

  output$products_data_status <- shiny::renderUI({
    data <- analysis_raw()
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
      return(shiny::div(
        class = "alert alert-warning",
        "Aucune donnée analytique trouvée. Chargez les données GNARF/GHO, " ,
        "ou construisez la table ntd_fusionnee dans le module SQLite."
      ))
    }
    shiny::div(
      class = "alert alert-info",
      paste(format(nrow(data), big.mark = " "),
            "observations chargées pour les produits d'information.")
    )
  })
  
  # --------------------------------------------------------------
  # 2. Harmonisation des colonnes
  # --------------------------------------------------------------
  
  analysis_data <- shiny::reactive({
    
    data <- analysis_raw()
    
    shiny::req(!is.null(data))
    
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    
    year_col <- find_col(
      data,
      c(
        "year",
        "indicator_year",
        "annee",
        "period",
        "time_dimension_value",
        "numeric_value_year"
      )
    )
    
    disease_col <- find_col(
      data,
      c(
        "indicator_disease_name",
        "disease_name",
        "disease",
        "maladie",
        "component_disease_name"
      )
    )
    
    country_col <- find_col(
      data,
      c(
        "country_name",
        "country",
        "pays",
        "location_name",
        "spatial_dim"
      )
    )
    
    iso_col <- find_col(
      data,
      c(
        "country_iso",
        "iso3",
        "iso_3_code",
        "iso_code",
        "country_code",
        "spatial_dim_type"
      )
    )
    
    indicator_col <- find_col(
      data,
      c(
        "dictionary_indicator_name",
        "indicator_name",
        "indicator",
        "indicator_label",
        "gho_display"
      )
    )
    
    indicator_code_col <- find_col(
      data,
      c(
        "dictionary_indicator_code",
        "indicator_code",
        "indicator_id",
        "gho_code"
      )
    )
    
    value_col <- find_col(
      data,
      c(
        "numeric_value",
        "value",
        "indicator_value",
        "numeric",
        "numeric_value_numeric",
        "fact_value_numeric",
        "all_cases",
        "ntd_all_ages_ntd_female",
        "ntd_all_ages_ntd_male",
        "ntd_all_ages_ntd_unknown_both_sexes"
      )
    )
    
    endemicity_col <- find_col(
      data,
      c(
        "status_endemicity",
        "endemicity_status",
        "endemicity",
        "endem_status",
        "indicator_endemicity"
      )
    )

    source_col <- find_col(data, c("indicator_ref_data", "source", "data_source"))

    data$.source <- if (!is.null(source_col)) {
      as.character(data[[source_col]])
    } else {
      NA_character_
    }
    
    data$.year <- if (!is.null(year_col)) {
      as.character(data[[year_col]])
    } else {
      NA_character_
    }
    
    data$.disease <- if (!is.null(disease_col)) {
      as.character(data[[disease_col]])
    } else {
      NA_character_
    }
    
    data$.country <- if (!is.null(country_col)) {
      as.character(data[[country_col]])
    } else {
      NA_character_
    }
    
    data$.country_iso <- if (!is.null(iso_col)) {
      toupper(as.character(data[[iso_col]]))
    } else {
      NA_character_
    }
    
    data$.indicator <- if (!is.null(indicator_col)) {
      as.character(data[[indicator_col]])
    } else {
      NA_character_
    }
    
    data$.indicator_code <- if (!is.null(indicator_code_col)) {
      as.character(data[[indicator_code_col]])
    } else {
      NA_character_
    }
    
    data$.value <- if (!is.null(value_col)) {
      suppressWarnings(as.numeric(data[[value_col]]))
    } else {
      NA_real_
    }
    
    data$.endemicity <- if (!is.null(endemicity_col)) {
      as.character(data[[endemicity_col]])
    } else {
      NA_character_
    }
    
    data
  })
  
  # --------------------------------------------------------------
  # 3. Choix partagés
  # --------------------------------------------------------------
  
  years <- shiny::reactive({
    clean_choices(analysis_data()$.year)
  })
  
  diseases <- shiny::reactive({
    clean_choices(analysis_data()$.disease)
  })
  
  countries <- shiny::reactive({
    clean_choices(analysis_data()$.country)
  })
  
  indicators <- shiny::reactive({
    clean_choices(analysis_data()$.indicator)
  })

  # --------------------------------------------------------------
  # 4. Filtres Heatmap
  # --------------------------------------------------------------
  
  output$hm_year_ui <- shiny::renderUI({
    make_selectize(
      "hm_year",
      "Année",
      years(),
      multiple = TRUE
    )
  })
  
  output$hm_disease_ui <- shiny::renderUI({
    make_selectize(
      "hm_disease",
      "Maladie",
      diseases(),
      multiple = TRUE
    )
  })
  
  output$hm_country_ui <- shiny::renderUI({
    make_selectize(
      "hm_country",
      "Pays",
      countries(),
      multiple = TRUE
    )
  })
  
  heatmap_data <- shiny::reactive({
    
    data <- analysis_data()
    
    data <- filter_value(data, ".year", input$hm_year)
    data <- filter_value(data, ".disease", input$hm_disease)
    data <- filter_value(data, ".country", input$hm_country)
    
    data <- data[
      !is.na(data$.country) &
        nzchar(data$.country) &
        !is.na(data$.disease) &
        nzchar(data$.disease),
      ,
      drop = FALSE
    ]
    
    data
  })
  
  heatmap_figure <- shiny::reactive({
    
    data <- heatmap_data()
    
    shiny::req(nrow(data) > 0)
    
    if (all(is.na(data$.endemicity))) {
      
      data$.heat_value <- data$.value
      
      fill_scale <- ggplot2::scale_fill_gradient(
        low = "#E8F4FA",
        high = "#005EB8",
        na.value = "#EEEEEE"
      )
      
      fill_name <- "Valeur"
      
    } else {
      
      data$.heat_value <- data$.endemicity
      
      fill_scale <- ggplot2::scale_fill_discrete(
        na.translate = TRUE,
        na.value = "#EEEEEE"
      )
      
      fill_name <- "Endémicité"
    }
    
    ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .disease,
        y = .country,
        fill = .heat_value
      )
    ) +
      ggplot2::geom_tile(
        colour = "white",
        linewidth = 0.3
      ) +
      fill_scale +
      ggplot2::labs(
        title = "Heatmap de l'endémicité",
        x = "Maladie",
        y = "Pays",
        fill = fill_name
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        ),
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          face = "bold",
          colour = "#19486A"
        )
      )
  })
  
  output$heatmap_plot <- shiny::renderPlot({
    
    data <- heatmap_data()
    
    if (nrow(data) == 0) {
      empty_plot("Aucune donnée pour les filtres sélectionnés")
      return(invisible(NULL))
    }
    
    print(heatmap_figure())
  })
  
  # --------------------------------------------------------------
  # 5. Synthèse d'endémicité
  # --------------------------------------------------------------
  
  output$ts_disease_ui <- shiny::renderUI({
    make_selectize(
      "ts_disease",
      "Maladie",
      diseases(),
      multiple = TRUE
    )
  })
  
  output$ts_year_ui <- shiny::renderUI({
    make_selectize(
      "ts_year",
      "Année",
      years(),
      multiple = TRUE
    )
  })
  
  synthese_data <- shiny::reactive({
    
    data <- analysis_data()
    
    data <- filter_value(data, ".disease", input$ts_disease)
    data <- filter_value(data, ".year", input$ts_year)
    
    data <- data[
      !is.na(data$.disease) &
        nzchar(data$.disease),
      ,
      drop = FALSE
    ]
    
    if (nrow(data) == 0) {
      return(data.frame())
    }
    
    result <- data |>
      dplyr::group_by(
        Maladie = .disease,
        Endemicite = .endemicity
      ) |>
      dplyr::summarise(
        Nombre_pays = dplyr::n_distinct(.country),
        Nombre_observations = dplyr::n(),
        .groups = "drop"
      )
    
    as.data.frame(result)
  })
  
  output$synthese_table <- DT::renderDT({
    
    data <- synthese_data()
    
    DT::datatable(
      data,
      rownames = FALSE,
      filter = "top",
      extensions = c("Buttons", "Responsive"),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        responsive = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ lignes",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ lignes",
          infoEmpty = "Aucune donnée",
          zeroRecords = "Aucun résultat trouvé",
          paginate = list(
            first = "Premier",
            previous = "Précédent",
            `next` = "Suivant",
            last = "Dernier"
          )
        )
      )
    )
  })
  
  # --------------------------------------------------------------
  # 6. Tableau détaillé
  # --------------------------------------------------------------
  
  output$td_disease_ui <- shiny::renderUI({
    make_selectize(
      "td_disease",
      "Maladie",
      diseases(),
      multiple = TRUE
    )
  })
  
  output$td_country_ui <- shiny::renderUI({
    make_selectize(
      "td_country",
      "Pays",
      countries(),
      multiple = TRUE
    )
  })
  
  detail_data <- shiny::reactive({
    
    data <- analysis_data()
    
    data <- filter_value(data, ".disease", input$td_disease)
    data <- filter_value(data, ".country", input$td_country)
    
    data.frame(
      Annee = data$.year,
      Pays = data$.country,
      Code_ISO = data$.country_iso,
      Maladie = data$.disease,
      Indicateur = data$.indicator,
      Code_indicateur = data$.indicator_code,
      Valeur = data$.value,
      Endemicite = data$.endemicity,
      stringsAsFactors = FALSE
    )
  })
  
  output$detail_table <- DT::renderDT({
    
    DT::datatable(
      detail_data(),
      rownames = FALSE,
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        deferRender = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        language = list(
          search = "Rechercher :",
          zeroRecords = "Aucune donnée",
          infoEmpty = "Aucune donnée",
          paginate = list(
            previous = "Précédent",
            `next` = "Suivant"
          )
        )
      )
    )
  })
  
  # --------------------------------------------------------------
  # 7. Tendances des indicateurs
  # --------------------------------------------------------------
  
  output$tr_indicator_ui <- shiny::renderUI({
    make_selectize(
      "tr_indicator",
      "Indicateur",
      indicators(),
      multiple = TRUE
    )
  })
  
  output$tr_country_ui <- shiny::renderUI({
    make_selectize(
      "tr_country",
      "Pays",
      countries(),
      multiple = TRUE
    )
  })
  
  output$tr_year_ui <- shiny::renderUI({
    make_selectize(
      "tr_year",
      "Année",
      years(),
      multiple = TRUE
    )
  })
  
  trend_data <- shiny::reactive({
    
    data <- analysis_data()
    
    data <- filter_value(data, ".indicator", input$tr_indicator)
    data <- filter_value(data, ".country", input$tr_country)
    data <- filter_value(data, ".year", input$tr_year)
    
    data <- data[
      !is.na(data$.value) &
        !is.na(data$.year) &
        !is.na(data$.country),
      ,
      drop = FALSE
    ]
    
    if (nrow(data) == 0) {
      return(data)
    }
    
    result <- data |>
      dplyr::group_by(
        Annee = .year,
        Pays = .country,
        Indicateur = .indicator
      ) |>
      dplyr::summarise(
        Valeur = mean(.value, na.rm = TRUE),
        .groups = "drop"
      )
    
    result$Annee_num <- suppressWarnings(
      as.numeric(as.character(result$Annee))
    )
    
    result
  })
  
  trend_figure <- shiny::reactive({
    
    data <- trend_data()
    
    shiny::req(nrow(data) > 0)
    
    ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Annee_num,
        y = Valeur,
        colour = Pays,
        group = interaction(Pays, Indicateur)
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_wrap(
        ~Indicateur,
        scales = "free_y"
      ) +
      ggplot2::labs(
        title = "Tendance des indicateurs",
        x = "Année",
        y = "Valeur",
        colour = "Pays"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          colour = "#19486A"
        ),
        legend.position = "bottom"
      )
  })
  
  output$trend_plot <- shiny::renderPlot({
    
    if (nrow(trend_data()) == 0) {
      empty_plot("Aucune donnée numérique disponible")
      return(invisible(NULL))
    }
    
    print(trend_figure())
  })
  
  # --------------------------------------------------------------
  # 8. Comparaison entre pays
  # --------------------------------------------------------------
  
  output$cp_indicator_ui <- shiny::renderUI({
    make_selectize(
      "cp_indicator",
      "Indicateur",
      indicators(),
      multiple = TRUE
    )
  })
  
  output$cp_country_ui <- shiny::renderUI({
    make_selectize(
      "cp_country",
      "Pays",
      countries(),
      multiple = TRUE
    )
  })
  
  compare_data <- shiny::reactive({
    
    data <- analysis_data()
    
    data <- filter_value(data, ".indicator", input$cp_indicator)
    data <- filter_value(data, ".country", input$cp_country)
    
    data <- data[
      !is.na(data$.value) &
        !is.na(data$.country) &
        !is.na(data$.indicator),
      ,
      drop = FALSE
    ]
    
    if (nrow(data) == 0) {
      return(data)
    }
    
    data |>
      dplyr::group_by(
        Pays = .country,
        Indicateur = .indicator
      ) |>
      dplyr::summarise(
        Valeur = mean(.value, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  compare_figure <- shiny::reactive({
    
    data <- compare_data()
    
    shiny::req(nrow(data) > 0)
    
    ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = stats::reorder(Pays, Valeur),
        y = Valeur,
        fill = Indicateur
      )
    ) +
      ggplot2::geom_col(
        position = "dodge"
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Comparaison entre pays",
        x = "Pays",
        y = "Valeur moyenne",
        fill = "Indicateur"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          colour = "#19486A"
        ),
        legend.position = "bottom"
      )
  })
  
  output$compare_plot <- shiny::renderPlot({
    
    if (nrow(compare_data()) == 0) {
      empty_plot("Aucune donnée disponible pour la comparaison")
      return(invisible(NULL))
    }
    
    print(compare_figure())
  })
  
  # --------------------------------------------------------------
  # 9. Cartes
  # --------------------------------------------------------------
  
  output$map1_disease_ui <- shiny::renderUI({
    make_selectize(
      "map1_disease",
      "Maladie",
      diseases(),
      multiple = FALSE
    )
  })
  
  output$map1_year_ui <- shiny::renderUI({
    make_selectize(
      "map1_year",
      "Année",
      years(),
      multiple = FALSE
    )
  })
  
  output$map2_indicator_ui <- shiny::renderUI({
    make_selectize(
      "map2_indicator",
      "Indicateur",
      indicators(),
      multiple = FALSE
    )
  })
  
  output$map2_year_ui <- shiny::renderUI({
    make_selectize(
      "map2_year",
      "Année",
      years(),
      multiple = FALSE
    )
  })
  
  africa_shape <- shiny::reactive({
    
    candidates <- list(
      rv$africa_shape,
      rv$africa_sf,
      rv$map_africa,
      rv$shape_africa,
      rv$country_shape,
      rv$spatial_data
    )
    
    for (object in candidates) {
      
      if (
        !is.null(object) &&
        inherits(object, "sf") &&
        nrow(object) > 0
      ) {
        return(object)
      }
    }
    
    NULL
  })
  
  map_shape_iso_column <- function(shape) {
    
    find_col(
      shape,
      c(
        "ISO_3_CODE",
        "ISO3",
        "ADM0_A3",
        "ISO_A3",
        "country_iso",
        "iso3"
      )
    )
  }
  
  map_shape_country_column <- function(shape) {
    
    find_col(
      shape,
      c(
        "ADM0_NAME",
        "NAME",
        "NAME_EN",
        "country_name",
        "ADMIN",
        "SOVEREIGNT"
      )
    )
  }
  
  endemic_map_figure <- shiny::reactive({
    
    shape <- africa_shape()
    
    shiny::req(!is.null(shape))
    
    data <- analysis_data()
    
    data <- filter_value(
      data,
      ".disease",
      first_selected(input$map1_disease)
    )
    
    data <- filter_value(
      data,
      ".year",
      first_selected(input$map1_year)
    )
    
    summary_data <- data |>
      dplyr::filter(
        !is.na(.country_iso) | !is.na(.country)
      ) |>
      dplyr::group_by(
        country_iso = .country_iso,
        country_name = .country
      ) |>
      dplyr::summarise(
        endemicity = dplyr::first(
          stats::na.omit(.endemicity)
        ),
        .groups = "drop"
      )
    
    iso_shape <- map_shape_iso_column(shape)
    country_shape <- map_shape_country_column(shape)
    
    if (
      !is.null(iso_shape) &&
      any(!is.na(summary_data$country_iso))
    ) {
      
      shape$.join_code <- toupper(
        as.character(shape[[iso_shape]])
      )
      
      summary_data$.join_code <- toupper(
        as.character(summary_data$country_iso)
      )
      
    } else {
      
      shiny::req(!is.null(country_shape))
      
      shape$.join_code <- tolower(
        trimws(as.character(shape[[country_shape]]))
      )
      
      summary_data$.join_code <- tolower(
        trimws(as.character(summary_data$country_name))
      )
    }
    
    map_data <- dplyr::left_join(
      shape,
      summary_data,
      by = ".join_code"
    )
    
    ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = endemicity),
        colour = "white",
        linewidth = 0.2
      ) +
      ggplot2::scale_fill_discrete(
        na.value = "#E5E5E5"
      ) +
      ggplot2::labs(
        title = "Carte de l'endémicité",
        fill = "Endémicité"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          colour = "#19486A"
        ),
        legend.position = "right"
      )
  })
  
  output$map_endemic_plot <- shiny::renderPlot({
    
    if (is.null(africa_shape())) {
      empty_plot(
        paste(
          "Couche géographique absente.",
          "Chargez une couche sf dans rv$africa_shape."
        )
      )
      return(invisible(NULL))
    }
    
    tryCatch(
      print(endemic_map_figure()),
      error = function(error) {
        empty_plot(
          paste("Carte non disponible :", error$message)
        )
      }
    )
  })
  
  numeric_map_figure <- shiny::reactive({
    
    shape <- africa_shape()
    
    shiny::req(!is.null(shape))
    
    data <- analysis_data()
    
    data <- filter_value(
      data,
      ".indicator",
      first_selected(input$map2_indicator)
    )
    
    data <- filter_value(
      data,
      ".year",
      first_selected(input$map2_year)
    )
    
    summary_data <- data |>
      dplyr::filter(
        !is.na(.value),
        !is.na(.country_iso) | !is.na(.country)
      ) |>
      dplyr::group_by(
        country_iso = .country_iso,
        country_name = .country
      ) |>
      dplyr::summarise(
        value = mean(.value, na.rm = TRUE),
        .groups = "drop"
      )
    
    iso_shape <- map_shape_iso_column(shape)
    country_shape <- map_shape_country_column(shape)
    
    if (
      !is.null(iso_shape) &&
      any(!is.na(summary_data$country_iso))
    ) {
      
      shape$.join_code <- toupper(
        as.character(shape[[iso_shape]])
      )
      
      summary_data$.join_code <- toupper(
        as.character(summary_data$country_iso)
      )
      
    } else {
      
      shiny::req(!is.null(country_shape))
      
      shape$.join_code <- tolower(
        trimws(as.character(shape[[country_shape]]))
      )
      
      summary_data$.join_code <- tolower(
        trimws(as.character(summary_data$country_name))
      )
    }
    
    map_data <- dplyr::left_join(
      shape,
      summary_data,
      by = ".join_code"
    )
    
    ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = value),
        colour = "white",
        linewidth = 0.2
      ) +
      ggplot2::scale_fill_gradient(
        low = "#E8F4FA",
        high = "#005EB8",
        na.value = "#E5E5E5"
      ) +
      ggplot2::labs(
        title = "Carte de l'indicateur numérique",
        fill = "Valeur"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          colour = "#19486A"
        )
      )
  })
  
  output$map_numeric_plot <- shiny::renderPlot({
    
    if (is.null(africa_shape())) {
      empty_plot(
        paste(
          "Couche géographique absente.",
          "Chargez une couche sf dans rv$africa_shape."
        )
      )
      return(invisible(NULL))
    }
    
    tryCatch(
      print(numeric_map_figure()),
      error = function(error) {
        empty_plot(
          paste("Carte non disponible :", error$message)
        )
      }
    )
  })
  
  # --------------------------------------------------------------
  # 10. Filtres partagés du rapport UN
  # --------------------------------------------------------------
  
  output$rep_year_ui <- shiny::renderUI({
    make_selectize(
      "rep_year",
      "Année",
      years(),
      multiple = TRUE
    )
  })
  
  output$rep_disease_ui <- shiny::renderUI({
    make_selectize(
      "rep_disease",
      "Maladie",
      diseases(),
      multiple = TRUE
    )
  })
  
  output$rep_country_ui <- shiny::renderUI({
    make_selectize(
      "rep_country",
      "Pays",
      countries(),
      multiple = TRUE
    )
  })
  
  output$rep_indicator_ui <- shiny::renderUI({
    make_selectize(
      "rep_indicator",
      "Indicateur",
      indicators(),
      multiple = TRUE
    )
  })
  
  report_data <- shiny::reactive({
    
    data <- analysis_data()
    
    data <- filter_value(data, ".year", input$rep_year)
    data <- filter_value(data, ".disease", input$rep_disease)
    data <- filter_value(data, ".country", input$rep_country)
    data <- filter_value(data, ".indicator", input$rep_indicator)
    
    data
  })
  
  output$un_report_frame <- shiny::renderUI({
    
    data <- report_data()
    
    shiny::req(!is.null(data))
    
    number_countries <- dplyr::n_distinct(
      data$.country[!is.na(data$.country)]
    )
    
    number_diseases <- dplyr::n_distinct(
      data$.disease[!is.na(data$.disease)]
    )
    
    number_indicators <- dplyr::n_distinct(
      data$.indicator[!is.na(data$.indicator)]
    )
    
    number_observations <- nrow(data)
    
    values_available <- sum(!is.na(data$.value))
    
    endemicity_available <- sum(
      !is.na(data$.endemicity) &
        nzchar(data$.endemicity)
    )
    
    shiny::tagList(
      
      shiny::tags$style(
        shiny::HTML(
          "
          .un-kpi-grid {
            display: grid;
            grid-template-columns: repeat(4, minmax(160px, 1fr));
            gap: 15px;
            margin-bottom: 20px;
          }

          .un-kpi-card {
            border-top: 5px solid #009EDB;
            background: #FFFFFF;
            padding: 18px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.10);
            border-radius: 4px;
          }

          .un-kpi-value {
            color: #19486A;
            font-size: 30px;
            font-weight: 700;
          }

          .un-kpi-label {
            color: #555555;
            font-size: 14px;
          }

          .un-report-section {
            background: #FFFFFF;
            border-left: 5px solid #009EDB;
            padding: 18px;
            margin-top: 15px;
          }

          @media (max-width: 900px) {
            .un-kpi-grid {
              grid-template-columns: repeat(2, 1fr);
            }
          }
          "
        )
      ),
      
      shiny::div(
        class = "un-kpi-grid",
        
        shiny::div(
          class = "un-kpi-card",
          shiny::div(
            class = "un-kpi-value",
            format(
              number_observations,
              big.mark = " ",
              scientific = FALSE
            )
          ),
          shiny::div(
            class = "un-kpi-label",
            "Observations"
          )
        ),
        
        shiny::div(
          class = "un-kpi-card",
          shiny::div(
            class = "un-kpi-value",
            number_countries
          ),
          shiny::div(
            class = "un-kpi-label",
            "Pays"
          )
        ),
        
        shiny::div(
          class = "un-kpi-card",
          shiny::div(
            class = "un-kpi-value",
            number_diseases
          ),
          shiny::div(
            class = "un-kpi-label",
            "Maladies"
          )
        ),
        
        shiny::div(
          class = "un-kpi-card",
          shiny::div(
            class = "un-kpi-value",
            number_indicators
          ),
          shiny::div(
            class = "un-kpi-label",
            "Indicateurs"
          )
        )
      ),
      
      shiny::div(
        class = "un-report-section",
        
        shiny::tags$h3(
          "Résumé analytique",
          style = "color:#19486A;font-weight:700;"
        ),
        
        shiny::tags$p(
          paste0(
            "Le jeu de données filtré contient ",
            format(
              number_observations,
              big.mark = " ",
              scientific = FALSE
            ),
            " observations couvrant ",
            number_countries,
            " pays, ",
            number_diseases,
            " maladies et ",
            number_indicators,
            " indicateurs."
          )
        ),
        
        shiny::tags$p(
          paste0(
            values_available,
            " observations comportent une valeur numérique et ",
            endemicity_available,
            " comportent une information d'endémicité."
          )
        )
      )
    )
  })
  
  # --------------------------------------------------------------
  # 11. Exports Excel
  # --------------------------------------------------------------
  
  export_xlsx <- function(data, file, sheet_name) {
    
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      
      openxlsx::write.xlsx(
        data,
        file = file,
        sheetName = sheet_name,
        overwrite = TRUE
      )
      
    } else if (requireNamespace("writexl", quietly = TRUE)) {
      
      writexl::write_xlsx(
        stats::setNames(list(data), sheet_name),
        path = file
      )
      
    } else {
      
      stop(
        paste(
          "Installez le package openxlsx ou writexl",
          "pour permettre l'exportation Excel."
        )
      )
    }
  }
  
  output$dl_hm_xlsx <- shiny::downloadHandler(
    
    filename = function() {
      paste0("heatmap_endemicite_", Sys.Date(), ".xlsx")
    },
    
    content = function(file) {
      export_xlsx(
        heatmap_data(),
        file,
        "Heatmap"
      )
    }
  )
  
  output$dl_syn_xlsx <- shiny::downloadHandler(
    
    filename = function() {
      paste0("synthese_endemicite_", Sys.Date(), ".xlsx")
    },
    
    content = function(file) {
      export_xlsx(
        synthese_data(),
        file,
        "Synthese"
      )
    }
  )
  
  output$dl_det_xlsx <- shiny::downloadHandler(
    
    filename = function() {
      paste0("detail_indicateurs_", Sys.Date(), ".xlsx")
    },
    
    content = function(file) {
      export_xlsx(
        detail_data(),
        file,
        "Detail"
      )
    }
  )
  
  output$dl_tr_xlsx <- shiny::downloadHandler(
    
    filename = function() {
      paste0("tendance_indicateurs_", Sys.Date(), ".xlsx")
    },
    
    content = function(file) {
      export_xlsx(
        trend_data(),
        file,
        "Tendances"
      )
    }
  )
  
  output$dl_cp_xlsx <- shiny::downloadHandler(
    
    filename = function() {
      paste0("comparaison_pays_", Sys.Date(), ".xlsx")
    },
    
    content = function(file) {
      export_xlsx(
        compare_data(),
        file,
        "Comparaison"
      )
    }
  )
  
  # --------------------------------------------------------------
  # 12. Fonction générique d'export des graphiques
  # --------------------------------------------------------------
  
  register_plot_download <- function(
    output_id,
    plot_reactive,
    prefix,
    extension,
    width = 12,
    height = 8
  ) {
    
    output[[output_id]] <- shiny::downloadHandler(
      
      filename = function() {
        paste0(prefix, "_", Sys.Date(), ".", extension)
      },
      
      content = function(file) {
        
        device <- switch(
          extension,
          png = "png",
          pdf = grDevices::cairo_pdf,
          svg = "svg",
          extension
        )
        
        ggplot2::ggsave(
          filename = file,
          plot = plot_reactive(),
          device = device,
          width = width,
          height = height,
          units = "in",
          dpi = 300,
          bg = "white"
        )
      }
    )
  }
  
  register_plot_download(
    "dl_hm_png",
    heatmap_figure,
    "heatmap_endemicite",
    "png"
  )
  
  register_plot_download(
    "dl_hm_svg",
    heatmap_figure,
    "heatmap_endemicite",
    "svg"
  )
  
  register_plot_download(
    "dl_hm_pdf",
    heatmap_figure,
    "heatmap_endemicite",
    "pdf"
  )
  
  register_plot_download(
    "dl_tr_png",
    trend_figure,
    "tendance_indicateurs",
    "png"
  )
  
  register_plot_download(
    "dl_tr_svg",
    trend_figure,
    "tendance_indicateurs",
    "svg"
  )
  
  register_plot_download(
    "dl_tr_pdf",
    trend_figure,
    "tendance_indicateurs",
    "pdf"
  )
  
  register_plot_download(
    "dl_cp_png",
    compare_figure,
    "comparaison_pays",
    "png"
  )
  
  register_plot_download(
    "dl_cp_svg",
    compare_figure,
    "comparaison_pays",
    "svg"
  )
  
  register_plot_download(
    "dl_cp_pdf",
    compare_figure,
    "comparaison_pays",
    "pdf"
  )
  
  register_plot_download(
    "dl_m1_png",
    endemic_map_figure,
    "carte_endemicite",
    "png"
  )
  
  register_plot_download(
    "dl_m1_svg",
    endemic_map_figure,
    "carte_endemicite",
    "svg"
  )
  
  register_plot_download(
    "dl_m1_pdf",
    endemic_map_figure,
    "carte_endemicite",
    "pdf"
  )
  
  register_plot_download(
    "dl_m2_png",
    numeric_map_figure,
    "carte_indicateur",
    "png"
  )
  
  register_plot_download(
    "dl_m2_svg",
    numeric_map_figure,
    "carte_indicateur",
    "svg"
  )
  
  register_plot_download(
    "dl_m2_pdf",
    numeric_map_figure,
    "carte_indicateur",
    "pdf"
  )
  
  # --------------------------------------------------------------
  # 13. Exports PDF des tableaux
  # --------------------------------------------------------------
  
  output$dl_syn_pdf <- shiny::downloadHandler(
    
    filename = function() {
      paste0("synthese_endemicite_", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      
      grDevices::pdf(
        file,
        width = 11.69,
        height = 8.27
      )
      
      graphics::plot.new()
      
      graphics::title(
        main = "Synthèse de l'endémicité"
      )
      
      data <- synthese_data()
      
      if (nrow(data) > 0) {
        
        text_data <- utils::capture.output(
          print(
            data,
            row.names = FALSE
          )
        )
        
        graphics::text(
          0.02,
          0.92,
          paste(text_data, collapse = "\n"),
          adj = c(0, 1),
          family = "mono",
          cex = 0.65
        )
        
      } else {
        
        graphics::text(
          0.5,
          0.5,
          "Aucune donnée disponible"
        )
      }
      
      grDevices::dev.off()
    }
  )
  
  output$dl_det_pdf <- shiny::downloadHandler(
    
    filename = function() {
      paste0("detail_indicateurs_", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      
      grDevices::pdf(
        file,
        width = 11.69,
        height = 8.27
      )
      
      graphics::plot.new()
      
      graphics::title(
        main = "Détail des indicateurs"
      )
      
      data <- utils::head(
        detail_data(),
        40
      )
      
      if (nrow(data) > 0) {
        
        text_data <- utils::capture.output(
          print(
            data,
            row.names = FALSE
          )
        )
        
        graphics::text(
          0.01,
          0.92,
          paste(text_data, collapse = "\n"),
          adj = c(0, 1),
          family = "mono",
          cex = 0.45
        )
        
      } else {
        
        graphics::text(
          0.5,
          0.5,
          "Aucune donnée disponible"
        )
      }
      
      grDevices::dev.off()
    }
  )
  
  # --------------------------------------------------------------
  # 14. Exports du rapport UN
  # --------------------------------------------------------------
  
  output$download_un_report_html <- shiny::downloadHandler(
    
    filename = function() {
      paste0("rapport_UN_", Sys.Date(), ".html")
    },
    
    content = function(file) {
      
      data <- report_data()
      
      number_countries <- dplyr::n_distinct(
        data$.country[!is.na(data$.country)]
      )
      
      number_diseases <- dplyr::n_distinct(
        data$.disease[!is.na(data$.disease)]
      )
      
      number_indicators <- dplyr::n_distinct(
        data$.indicator[!is.na(data$.indicator)]
      )
      
      html_content <- paste0(
        "<!DOCTYPE html>",
        "<html lang='fr'>",
        "<head>",
        "<meta charset='UTF-8'>",
        "<meta name='viewport' content='width=device-width, initial-scale=1'>",
        "<title>Rapport analytique Nations Unies</title>",
        "<style>",
        "body{font-family:Arial,sans-serif;margin:40px;color:#333;}",
        "h1,h2{color:#19486A;}",
        ".header{border-bottom:6px solid #009EDB;padding-bottom:15px;}",
        ".grid{display:grid;grid-template-columns:repeat(4,1fr);gap:15px;margin-top:25px;}",
        ".card{border-top:5px solid #009EDB;padding:18px;box-shadow:0 2px 8px rgba(0,0,0,.12);}",
        ".value{font-size:28px;font-weight:bold;color:#19486A;}",
        ".section{margin-top:30px;border-left:5px solid #009EDB;padding:18px;background:#f7f9fb;}",
        "</style>",
        "</head>",
        "<body>",
        "<div class='header'>",
        "<h1>Rapport analytique style Nations Unies</h1>",
        "<p>Date de production : ",
        format(Sys.Date(), "%d/%m/%Y"),
        "</p>",
        "</div>",
        "<div class='grid'>",
        "<div class='card'><div class='value'>",
        nrow(data),
        "</div><div>Observations</div></div>",
        "<div class='card'><div class='value'>",
        number_countries,
        "</div><div>Pays</div></div>",
        "<div class='card'><div class='value'>",
        number_diseases,
        "</div><div>Maladies</div></div>",
        "<div class='card'><div class='value'>",
        number_indicators,
        "</div><div>Indicateurs</div></div>",
        "</div>",
        "<div class='section'>",
        "<h2>Résumé analytique</h2>",
        "<p>Le rapport est produit à partir des filtres sélectionnés ",
        "dans le tableau de bord.</p>",
        "</div>",
        "</body>",
        "</html>"
      )
      
      writeLines(
        html_content,
        con = file,
        useBytes = TRUE
      )
    }
  )
  
  output$download_un_report_docx <- shiny::downloadHandler(
    
    filename = function() {
      paste0("rapport_UN_", Sys.Date(), ".docx")
    },
    
    content = function(file) {
      
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop(
          "Installez le package officer pour exporter le rapport Word."
        )
      }
      
      data <- report_data()
      
      document <- officer::read_docx()
      
      document <- officer::body_add_heading(
        document,
        "Rapport analytique style Nations Unies",
        level = 1
      )
      
      document <- officer::body_add_par(
        document,
        paste(
          "Date de production :",
          format(Sys.Date(), "%d/%m/%Y")
        )
      )
      
      document <- officer::body_add_heading(
        document,
        "Résumé analytique",
        level = 2
      )
      
      document <- officer::body_add_par(
        document,
        paste0(
          "Le jeu de données filtré contient ",
          nrow(data),
          " observations, ",
          dplyr::n_distinct(data$.country, na.rm = TRUE),
          " pays, ",
          dplyr::n_distinct(data$.disease, na.rm = TRUE),
          " maladies et ",
          dplyr::n_distinct(data$.indicator, na.rm = TRUE),
          " indicateurs."
        )
      )
      
      print(
        document,
        target = file
      )
    }
  )
  
  output$download_un_report_pdf <- shiny::downloadHandler(
    
    filename = function() {
      paste0("rapport_UN_", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      
      data <- report_data()
      
      grDevices::pdf(
        file,
        width = 8.27,
        height = 11.69
      )
      
      graphics::par(
        mar = c(2, 2, 3, 2)
      )
      
      graphics::plot.new()
      
      graphics::text(
        0.05,
        0.95,
        "Rapport analytique style Nations Unies",
        adj = c(0, 1),
        cex = 1.6,
        font = 2,
        col = "#19486A"
      )
      
      graphics::segments(
        0.05,
        0.91,
        0.95,
        0.91,
        col = "#009EDB",
        lwd = 5
      )
      
      summary_text <- paste0(
        "Date de production : ",
        format(Sys.Date(), "%d/%m/%Y"),
        "\n\n",
        "Nombre d'observations : ",
        nrow(data),
        "\n",
        "Nombre de pays : ",
        dplyr::n_distinct(data$.country, na.rm = TRUE),
        "\n",
        "Nombre de maladies : ",
        dplyr::n_distinct(data$.disease, na.rm = TRUE),
        "\n",
        "Nombre d'indicateurs : ",
        dplyr::n_distinct(data$.indicator, na.rm = TRUE)
      )
      
      graphics::text(
        0.05,
        0.84,
        summary_text,
        adj = c(0, 1),
        cex = 1
      )
      
      grDevices::dev.off()
    }
  )

  # --------------------------------------------------------------
  # 15. Rapport R Markdown utilisé comme sortie du projet
  # --------------------------------------------------------------
  rmd_report <- shiny::reactiveVal(NULL)
  rmd_report_dir <- tempfile(
    pattern = paste0("tvd_report_", gsub("[^A-Za-z0-9]", "", session$token), "_")
  )
  rmd_resource_prefix <- paste0(
    "tvd_report_", gsub("[^A-Za-z0-9]", "", session$token)
  )
  rmd_resource_state <- new.env(parent = emptyenv())
  rmd_resource_state$registered <- FALSE
  remove_rmd_resource <- function() {
    if (!isTRUE(rmd_resource_state$registered)) return(invisible(FALSE))
    suppressWarnings(try(shiny::removeResourcePath(rmd_resource_prefix), silent = TRUE))
    rmd_resource_state$registered <- FALSE
    invisible(TRUE)
  }
  rmd_input <- file.path(ROOT_DIR, "reports", "Analyse_base_analytique.Rmd")
  session$onSessionEnded(function() {
    remove_rmd_resource()
    if (dir.exists(rmd_report_dir)) unlink(rmd_report_dir, recursive = TRUE)
  })

  observeEvent(input$generate_rmd_report, {
    rmd_report(list(status = "running"))
    tryCatch({
      if (!file.exists(rmd_input)) {
        stop("Fichier R Markdown introuvable : ", rmd_input)
      }
      pandoc_info <- rmarkdown::find_pandoc(cache = FALSE)
      if (utils::compareVersion(as.character(pandoc_info$version), "2.8") < 0) {
        program_files <- Sys.getenv("ProgramFiles", "C:/Program Files")
        pandoc_dirs <- c(
          Sys.getenv("RSTUDIO_PANDOC", ""),
          file.path(program_files, "RStudio", "resources", "app", "bin",
                    "quarto", "bin", "tools")
        )
        pandoc_dirs <- pandoc_dirs[nzchar(pandoc_dirs) & dir.exists(pandoc_dirs)]
        for (pandoc_dir in pandoc_dirs) {
          pandoc_info <- rmarkdown::find_pandoc(cache = FALSE, dir = pandoc_dir)
          if (utils::compareVersion(as.character(pandoc_info$version), "2.8") >= 0) break
        }
      }
      if (utils::compareVersion(as.character(pandoc_info$version), "2.8") < 0) {
        stop("Pandoc 2.8 ou plus est requis pour générer le rapport HTML.")
      }
      dir.create(rmd_report_dir, recursive = TRUE, showWarnings = FALSE)
      rmd_source_dir <- file.path(rmd_report_dir, "source")
      dir.create(rmd_source_dir, recursive = TRUE, showWarnings = FALSE)
      local_rmd <- file.path(rmd_source_dir, basename(rmd_input))
      if (!file.copy(rmd_input, local_rmd, overwrite = TRUE)) {
        stop("Impossible de préparer le fichier R Markdown pour le rendu.")
      }
      rmd_css <- file.path(ROOT_DIR, "reports", "rapport-who.css")
      if (file.exists(rmd_css) &&
          !file.copy(rmd_css, file.path(rmd_source_dir, basename(rmd_css)),
                     overwrite = TRUE)) {
        stop("Impossible de préparer la feuille de style du rapport.")
      }
      output_file <- file.path(rmd_report_dir, "Analyse_base_analytique.html")
      shape_path <- file.path(ROOT_DIR, "input", "shapefile", "country_simplified.gpkg")
      logo_path <- file.path(ROOT_DIR, "www", "logowho.png")
      # Le rendu se fait dans un processus R séparé : certains emplacements
      # OneDrive ne permettent pas à R de revenir au répertoire courant après knit.
      r_literal <- function(x) {
        if (is.null(x) || length(x) != 1L || is.na(x)) {
          stop("Un chemin du rapport est absent ou invalide.")
        }
        encodeString(gsub("\\\\", "/", as.character(x)), quote = "\"")
      }
      render_script <- file.path(rmd_report_dir, "render_report.R")
      render_lines <- c(
        ".libPaths(c(strsplit(Sys.getenv('TVD_R_LIBS'), .Platform$path.sep)[[1]], .libPaths()))",
        "pandoc_info <- rmarkdown::find_pandoc(cache = FALSE)",
        "if (utils::compareVersion(as.character(pandoc_info$version), '2.8') < 0) {",
        "  program_files <- Sys.getenv('ProgramFiles', 'C:/Program Files')",
        "  candidates <- c(Sys.getenv('RSTUDIO_PANDOC', ''), file.path(program_files, 'RStudio/resources/app/bin/quarto/bin/tools'))",
        "  for (candidate in candidates[nzchar(candidates) & dir.exists(candidates)]) {",
        "    pandoc_info <- rmarkdown::find_pandoc(cache = FALSE, dir = candidate)",
        "    if (utils::compareVersion(as.character(pandoc_info$version), '2.8') >= 0) break",
        "  }",
        "}",
        "if (utils::compareVersion(as.character(pandoc_info$version), '2.8') < 0) stop('Pandoc 2.8 ou plus est requis.')",
        paste0(
          "rmarkdown::render(input = ", r_literal(local_rmd),
          ", output_format = 'html_document', output_file = ",
          r_literal(basename(output_file)), ", output_dir = ",
          r_literal(rmd_report_dir), ", params = list(db_file = ",
          r_literal(normalizePath(DB_PATH, winslash = '/', mustWork = TRUE)),
          ", table_name = 'ntd_fusionnee', shape_file = ",
          if (file.exists(shape_path)) r_literal(shape_path) else "NULL",
          ", logo_file = ", if (file.exists(logo_path)) r_literal(logo_path) else "NULL",
          ", report_title = 'Analyse de la base analytique NTD', filters = NULL), quiet = TRUE)"
        )
      )
      writeLines(render_lines, render_script, useBytes = TRUE)
      stdout_file <- file.path(rmd_report_dir, "render_stdout.log")
      stderr_file <- file.path(rmd_report_dir, "render_stderr.log")
      ps_quote <- function(x) {
        if (is.null(x) || length(x) == 0L || is.na(x[[1]])) x <- ""
        paste0("'", gsub("'", "''", as.character(x[[1]]), fixed = TRUE), "'")
      }
      pandoc_dir_env <- if (is.character(pandoc_info$dir) &&
                            length(pandoc_info$dir) == 1L &&
                            !is.na(pandoc_info$dir) && nzchar(pandoc_info$dir)) {
        pandoc_info$dir
      } else ""
      powershell_script <- file.path(rmd_report_dir, "run_render.ps1")
      writeLines(c(
        "$ErrorActionPreference = 'Stop'",
        paste0("$env:TVD_R_LIBS = ", ps_quote(paste(.libPaths(), collapse = .Platform$path.sep))),
        paste0("$env:RSTUDIO_PANDOC = ", ps_quote(pandoc_dir_env)),
        paste0(
          "$proc = Start-Process -FilePath ",
          ps_quote(file.path(R.home("bin"), "Rscript.exe")),
          " -ArgumentList @('--vanilla', ", ps_quote(render_script),
          ") -WorkingDirectory ", ps_quote(rmd_report_dir),
          " -Wait -PassThru -RedirectStandardOutput ", ps_quote(stdout_file),
          " -RedirectStandardError ", ps_quote(stderr_file)
        ),
        "exit $proc.ExitCode"
      ), powershell_script, useBytes = TRUE)
      process_output <- suppressWarnings(system2(
        "powershell.exe",
        args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", powershell_script),
        stdout = TRUE,
        stderr = TRUE
      ))
      process_status <- attr(process_output, "status")
      if (!is.null(process_status) && process_status != 0L) {
        details <- c(
          if (file.exists(stderr_file)) readLines(stderr_file, warn = FALSE),
          process_output
        )
        stop(paste(details[nzchar(details)], collapse = "\n"))
      }
      rendered <- output_file
      if (!file.exists(rendered)) stop("Le rapport HTML n'a pas été créé.")

      remove_rmd_resource()
      shiny::addResourcePath(rmd_resource_prefix, rmd_report_dir)
      rmd_resource_state$registered <- TRUE
      rmd_report(list(
        status = "ready",
        file = rendered,
        url = paste0("/", rmd_resource_prefix, "/", basename(rendered))
      ))
      showNotification("Rapport R Markdown généré.", type = "message")
    }, error = function(e) {
      rmd_report(list(status = "error", message = conditionMessage(e)))
      showNotification(paste("Échec de génération du rapport :", conditionMessage(e)),
                       type = "error", duration = 12)
    })
  }, ignoreInit = TRUE)

  output$rmd_report_status <- shiny::renderUI({
    report <- rmd_report()
    if (is.null(report)) {
      return(shiny::div(class = "alert alert-info",
                        "Cliquez sur « Générer / actualiser le rapport » pour produire le rapport à partir de la base SQLite."))
    }
    if (identical(report$status, "running")) {
      return(shiny::div(class = "alert alert-info", "Génération du rapport en cours…"))
    }
    if (identical(report$status, "error")) {
      return(shiny::div(class = "alert alert-danger", report$message))
    }
    shiny::div(class = "alert alert-success", "Rapport généré à partir de ntd_fusionnee.")
  })

  output$rmd_report_frame <- shiny::renderUI({
    report <- rmd_report()
    if (is.null(report) || !identical(report$status, "ready")) return(NULL)
    shiny::tags$iframe(
      id = "rmd_report_iframe",
      src = report$url,
      title = "Rapport d’analyse de la base analytique",
      onload = paste(
        "try { const d = this.contentDocument;",
        "this.style.height = Math.max(d.documentElement.scrollHeight, d.body.scrollHeight, 1000) + 'px';",
        "} catch (e) {}"
      ),
      style = "width:100%; min-height:1000px; border:0; background:#fff;"
    )
  })

  output$download_rmd_report <- shiny::downloadHandler(
    filename = function() paste0("Analyse_base_analytique_", Sys.Date(), ".html"),
    content = function(file) {
      report <- rmd_report()
      if (is.null(report) || !identical(report$status, "ready") ||
          !file.exists(report$file)) {
        stop("Générez d'abord le rapport R Markdown.")
      }
      if (!file.copy(report$file, file, overwrite = TRUE)) {
        stop("Impossible de préparer le téléchargement du rapport.")
      }
    }
  )
  
  # --------------------------------------------------------------
  # 16. Retour invisible pour diagnostic
  # --------------------------------------------------------------
  
  invisible(
    list(
      analysis_data = analysis_data,
      heatmap_data = heatmap_data,
      synthese_data = synthese_data,
      detail_data = detail_data,
      trend_data = trend_data,
      compare_data = compare_data,
      report_data = report_data
    )
  )
}
