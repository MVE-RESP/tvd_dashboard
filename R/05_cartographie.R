# ================================================================
# 05_cartographie.R — Cartes interactives + choroplèthes shapefile
# ================================================================

# ---------- UI de l'onglet Shapefiles ----------
ui_tab_shapefiles <- function() {
  tabItem(tabName = "shapefiles",
          h4("Gestion des Shapefiles"),
          
          box(title = "Importation shapefile (ZIP)", status = "primary", width = 12,
              fileInput("file_shp", "Archive ZIP contenant .shp, .shx, .dbf, .prj",
                        accept = ".zip"),
              actionButton("validate_shp", "Valider et extraire",
                           class = "btn-success"),
              hr(),
              verbatimTextOutput("shp_validation_log")
          ),
          
          box(title = "Shapefiles disponibles", status = "info", width = 12,
              DT::DTOutput("shp_list_table"),
              hr(),
              uiOutput("shp_select_ui"),
              actionButton("load_shp", "Charger la couche",
                           class = "btn-primary")
          ),
          
          box(title = "Aperçu cartographique", status = "success", width = 12,
              leaflet::leafletOutput("shp_preview_map", height = "600px"),
              hr(),
              fluidRow(
                column(6, downloadButton("dl_shp_png", "Export PNG")),
                column(6, downloadButton("dl_shp_geojson", "Export GeoJSON"))
              )
          ),
          
          box(title = "Métadonnées de la couche", status = "warning", width = 12,
              verbatimTextOutput("shp_metadata"),
              hr(),
              DT::DTOutput("shp_attributes_table")
          )
  )
}

# ================================================================
# Fonctions pures (hors Shiny)
# ================================================================

# ---- Extraction sécurisée d'un ZIP shapefile ----
extract_shapefile_zip <- function(zip_path, dest_dir) {
  if (!file.exists(zip_path)) stop("Fichier ZIP introuvable : ", zip_path)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  files <- utils::unzip(zip_path, list = TRUE)
  required_ext <- c("shp", "shx", "dbf")
  found_ext <- tolower(tools::file_ext(files$Name))
  
  missing <- setdiff(required_ext, found_ext)
  if (length(missing))
    stop("Composants manquants dans le ZIP : ",
         paste(missing, collapse = ", "))
  
  utils::unzip(zip_path, exdir = dest_dir, overwrite = TRUE)
  shp_file <- list.files(dest_dir, pattern = "\\.shp$",
                         full.names = TRUE, recursive = TRUE)[1]
  if (is.na(shp_file) || !file.exists(shp_file))
    stop("Aucun fichier .shp trouvé après extraction.")
  
  shp_file
}

# ---- Lecture + reprojection WGS84 ----
read_shapefile_safe <- function(shp_path, target_crs = 4326) {
  sf_obj <- sf::st_read(shp_path, quiet = TRUE,
                        stringsAsFactors = FALSE)
  if (is.na(sf::st_crs(sf_obj))) {
    warning("CRS absent : attribution WGS84 par défaut.")
    sf::st_crs(sf_obj) <- target_crs
  } else if (sf::st_crs(sf_obj)$epsg != target_crs) {
    sf_obj <- sf::st_transform(sf_obj, crs = target_crs)
  }
  # Simplification légère pour accélérer le rendu
  if (nrow(sf_obj) > 50) {
    sf_obj <- rmapshaper::ms_simplify(sf_obj, keep = 0.1,
                                      keep_shapes = TRUE)
  }
  sf_obj
}

# ---- Détection auto de la colonne "pays" ----
detect_country_column <- function(sf_obj) {
  candidates <- c("country", "pays", "name", "nom", "admin",
                  "name_0", "name_1", "cntry_name", "country_name",
                  "iso3", "iso_a3", "adm0_name")
  cols <- tolower(names(sf_obj))
  hit <- intersect(candidates, cols)
  if (length(hit)) names(sf_obj)[cols == hit[1]][1] else names(sf_obj)[1]
}

# ---- Métadonnées shapefile ----
shapefile_metadata <- function(sf_obj) {
  bbox <- sf::st_bbox(sf_obj)
  list(
    n_features   = nrow(sf_obj),
    n_fields     = ncol(sf_obj) - 1L,
    geom_type    = as.character(unique(sf::st_geometry_type(sf_obj))),
    crs          = sf::st_crs(sf_obj)$input,
    bbox         = sprintf("xmin=%.3f, ymin=%.3f, xmax=%.3f, ymax=%.3f",
                           bbox["xmin"], bbox["ymin"],
                           bbox["xmax"], bbox["ymax"]),
    country_col  = detect_country_column(sf_obj),
    fields       = paste(setdiff(names(sf_obj), "geometry"),
                         collapse = ", ")
  )
}

# ---- Carte Leaflet de base ----
build_base_leaflet <- function(sf_obj, label_col = NULL) {
  label_col <- label_col %||% detect_country_column(sf_obj)
  labels <- if (label_col %in% names(sf_obj))
    as.character(sf_obj[[label_col]]) else seq_len(nrow(sf_obj))
  
  leaflet::leaflet(sf_obj) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                              group = "Clair") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                              group = "Satellite") |>
    leaflet::addPolygons(
      fillColor   = un_palette$blue,
      fillOpacity = 0.5,
      color       = un_palette$navy,
      weight      = 1,
      label       = labels,
      highlightOptions = leaflet::highlightOptions(
        weight = 3, color = un_palette$red, fillOpacity = 0.75,
        bringToFront = TRUE)
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Clair", "Satellite"),
      options    = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addScaleBar(position = "bottomleft")
}

# ---- Choroplèthe endémicité ----
build_endemic_choropleth <- function(sf_obj, endemic_df,
                                     join_col_sf, join_col_df,
                                     disease, year) {
  df <- endemic_df |>
    dplyr::filter(.data$disease == !!disease,
                  .data$year    == !!year) |>
    dplyr::mutate(join_key = normalize_text(.data[[join_col_df]]))
  
  sf_obj <- sf_obj |>
    dplyr::mutate(join_key = normalize_text(.data[[join_col_sf]]))
  
  merged <- dplyr::left_join(sf_obj, df, by = "join_key")
  
  pal <- leaflet::colorFactor(
    palette = unname(un_endemic_colors),
    domain  = names(un_endemic_colors),
    na.color = "#EEEEEE"
  )
  
  leaflet::leaflet(merged) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      fillColor = ~pal(endemicity_status),
      fillOpacity = 0.8,
      color = "white", weight = 1,
      label = ~sprintf("%s — %s",
                       get(join_col_sf),
                       endemicity_status %||% "n/a")
    ) |>
    leaflet::addLegend(pal = pal,
                       values = names(un_endemic_colors),
                       title = sprintf("Endémicité %s (%s)", disease, year),
                       position = "bottomright")
}

# ---- Choroplèthe indicateur numérique ----
build_numeric_choropleth <- function(sf_obj, num_df,
                                     join_col_sf, join_col_df,
                                     indicator, year) {
  df <- num_df |>
    dplyr::filter(.data$indicator == !!indicator,
                  .data$year      == !!year) |>
    dplyr::mutate(join_key = normalize_text(.data[[join_col_df]]))
  
  sf_obj <- sf_obj |>
    dplyr::mutate(join_key = normalize_text(.data[[join_col_sf]]))
  
  merged <- dplyr::left_join(sf_obj, df, by = "join_key")
  
  pal <- leaflet::colorNumeric(
    palette  = "YlOrRd",
    domain   = merged$value,
    na.color = "#EEEEEE"
  )
  
  leaflet::leaflet(merged) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      fillColor   = ~pal(value),
      fillOpacity = 0.8,
      color = "white", weight = 1,
      label = ~sprintf("%s : %s",
                       get(join_col_sf),
                       formatC(value, big.mark = " ", digits = 2,
                               format = "fg"))
    ) |>
    leaflet::addLegend(pal = pal, values = ~value,
                       title = sprintf("%s (%s)", indicator, year),
                       position = "bottomright")
}

# ---- Export PNG statique (via ggplot2) ----
export_shapefile_png <- function(sf_obj, file, title = "Shapefile") {
  g <- ggplot2::ggplot(sf_obj) +
    ggplot2::geom_sf(fill = un_palette$blue,
                     color = un_palette$navy, size = 0.2) +
    ggplot2::labs(title = title) +
    theme_un() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
  ggplot2::ggsave(file, plot = g, width = 12, height = 8,
                  dpi = 150, bg = "white")
}

# ================================================================
# Module serveur
# ================================================================
srv_cartographie <- function(input, output, session, rv,
                             sqlite_db_refresh) {
  
  # Reactive values locaux
  shp_loaded   <- reactiveVal(NULL)   # objet sf
  shp_meta     <- reactiveVal(NULL)
  shp_log      <- reactiveVal("En attente d'importation…")
  
  # ---------- 1. Validation et extraction ----------
  observeEvent(input$validate_shp, {
    req(input$file_shp)
    shp_log("Extraction en cours…")
    
    tryCatch({
      zip_src   <- input$file_shp$datapath
      base_name <- tools::file_path_sans_ext(input$file_shp$name)
      dest_dir  <- file.path("input", "shapefile", "extracted",
                             safe_table_name(base_name))
      
      # Copie du ZIP original
      file.copy(zip_src,
                file.path("input", "shapefile",
                          paste0(safe_table_name(base_name), ".zip")),
                overwrite = TRUE)
      
      shp_path <- extract_shapefile_zip(zip_src, dest_dir)
      sf_obj   <- read_shapefile_safe(shp_path)
      
      shp_loaded(sf_obj)
      shp_meta(shapefile_metadata(sf_obj))
      shp_log(sprintf("✓ Shapefile « %s » chargé — %d entités.",
                      base_name, nrow(sf_obj)))
      
      showNotification("Shapefile importé avec succès.",
                       type = "message")
    }, error = function(e) {
      shp_log(paste("✗ Erreur :", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error",
                       duration = 8)
    })
  })
  
  output$shp_validation_log <- renderText({ shp_log() })
  
  # ---------- 2. Liste des shapefiles disponibles ----------
  shp_available <- reactive({
    invalidateLater(5000, session)
    dirs <- list.dirs("input/shapefile/extracted", recursive = FALSE)
    if (!length(dirs)) return(data.frame(
      Nom = character(), Fichier = character(),
      Taille_Mo = numeric(), Modifie = as.POSIXct(character())
    ))
    do.call(rbind, lapply(dirs, function(d) {
      shp <- list.files(d, pattern = "\\.shp$",
                        full.names = TRUE, recursive = TRUE)[1]
      if (is.na(shp)) return(NULL)
      info <- file.info(shp)
      data.frame(
        Nom       = basename(d),
        Fichier   = basename(shp),
        Taille_Mo = round(info$size / 1024^2, 2),
        Modifie   = info$mtime,
        Chemin    = shp,
        stringsAsFactors = FALSE
      )
    }))
  })
  
  output$shp_list_table <- DT::renderDT({
    df <- shp_available()
    DT::datatable(df[, setdiff(names(df), "Chemin"), drop = FALSE],
                  rownames = FALSE,
                  options = list(pageLength = 5, dom = "tp"),
                  selection = "single")
  })
  
  output$shp_select_ui <- renderUI({
    df <- shp_available()
    if (!nrow(df))
      return(helpText("Aucun shapefile disponible. Importez un ZIP ci-dessus."))
    selectInput("shp_select", "Shapefile à charger :",
                choices = stats::setNames(df$Chemin, df$Nom))
  })
  
  # ---------- 3. Chargement d'une couche existante ----------
  observeEvent(input$load_shp, {
    req(input$shp_select)
    tryCatch({
      sf_obj <- read_shapefile_safe(input$shp_select)
      shp_loaded(sf_obj)
      shp_meta(shapefile_metadata(sf_obj))
      shp_log(sprintf("✓ Couche « %s » rechargée (%d entités).",
                      basename(input$shp_select), nrow(sf_obj)))
      
      # Exposition de la couche aux autres modules
      rv$current_shapefile <- sf_obj
      rv$current_shapefile_name <- basename(input$shp_select)
      
      showNotification("Couche chargée.", type = "message")
    }, error = function(e) {
      shp_log(paste("✗ Erreur chargement :", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  # ---------- 4. Aperçu cartographique ----------
  output$shp_preview_map <- leaflet::renderLeaflet({
    sf_obj <- shp_loaded()
    if (is.null(sf_obj)) {
      return(leaflet::leaflet() |>
               leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
               leaflet::setView(lng = 20, lat = 0, zoom = 2))
    }
    build_base_leaflet(sf_obj)
  })
  
  # ---------- 5. Métadonnées ----------
  output$shp_metadata <- renderPrint({
    meta <- shp_meta()
    if (is.null(meta)) {
      cat("Aucune couche active.")
      return(invisible())
    }
    cat("Entités        :", meta$n_features, "\n")
    cat("Champs         :", meta$n_fields, "\n")
    cat("Type géométrie :", meta$geom_type, "\n")
    cat("CRS            :", meta$crs, "\n")
    cat("Bounding box   :", meta$bbox, "\n")
    cat("Colonne pays   :", meta$country_col, "\n")
    cat("Champs dispo.  :", meta$fields, "\n")
  })
  
  output$shp_attributes_table <- DT::renderDT({
    sf_obj <- shp_loaded()
    req(sf_obj)
    df <- sf::st_drop_geometry(sf_obj)
    DT::datatable(utils::head(df, 100),
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ---------- 6. Exports ----------
  output$dl_shp_png <- downloadHandler(
    filename = function()
      paste0("shapefile_", Sys.Date(), ".png"),
    content = function(file) {
      sf_obj <- shp_loaded(); req(sf_obj)
      export_shapefile_png(sf_obj, file,
                           title = rv$current_shapefile_name %||% "Shapefile")
    }
  )
  
  output$dl_shp_geojson <- downloadHandler(
    filename = function()
      paste0("shapefile_", Sys.Date(), ".geojson"),
    content = function(file) {
      sf_obj <- shp_loaded(); req(sf_obj)
      sf::st_write(sf_obj, file, driver = "GeoJSON",
                   delete_dsn = TRUE, quiet = TRUE)
    }
  )
  
  # ---------- 7. Expose les constructeurs de choroplèthes ----------
  # Permet au module 04_analysis.R de produire les cartes du produit 6
  rv$make_endemic_map <- function(disease, year) {
    sf_obj <- shp_loaded()
    req(sf_obj, rv$endemic_data)
    meta <- shapefile_metadata(sf_obj)
    build_endemic_choropleth(
      sf_obj      = sf_obj,
      endemic_df  = rv$endemic_data,
      join_col_sf = meta$country_col,
      join_col_df = "country",
      disease     = disease,
      year        = year
    )
  }
  
  rv$make_numeric_map <- function(indicator, year) {
    sf_obj <- shp_loaded()
    req(sf_obj, rv$numeric_data)
    meta <- shapefile_metadata(sf_obj)
    build_numeric_choropleth(
      sf_obj      = sf_obj,
      num_df      = rv$numeric_data,
      join_col_sf = meta$country_col,
      join_col_df = "country",
      indicator   = indicator,
      year        = year
    )
  }
}
