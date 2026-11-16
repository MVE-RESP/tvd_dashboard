# ================================================================
# APP.R — SOP Data App for TVD (orchestrateur)
# ================================================================

# 1. Packages
required_packages <- c(
  "shiny", "shinydashboard", "shinydashboardPlus", "DT", "readxl", "writexl",
  "ggplot2", "scales", "DBI", "RSQLite", "sf", "dplyr", "tidyr", "janitor",
  "httr", "jsonlite", "leaflet", "rmarkdown", "knitr", "rmapshaper",
  "openxlsx", "plotly", "kableExtra", "flextable", "showtext", "sysfonts",
  "patchwork", "viridis", "shinycssloaders", "gridExtra", "officer"
)
source("R/dependencies.R", local = FALSE)
missing <- required_packages[!vapply(required_packages, requireNamespace,
                                     logical(1), quietly = TRUE)]
if (length(missing)) stop("Packages manquants : ", paste(missing, collapse = ", "))
invisible(lapply(required_packages, library, character.only = TRUE))

# 2. Chemins racine
ROOT_DIR <- normalizePath(getOption("shiny.appDir", getwd()),
                          winslash = "/", mustWork = TRUE)
DB_PATH  <- file.path("input", "tvd_datawarehouse.sqlite")

for (d in c("R","input/dictionnaire","input/shapefile","input/shapefile/extracted",
            "input/arborescence","input/upload","input/load_dt","output","reports")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# 3. Chargement modulaire — ORDRE IMPORTANT
source("R/01_utils.R",             local = FALSE)
source("R/02_import_validation.R", local = FALSE)
source("R/03_processing.R",        local = FALSE)
source("R/04_analysis.R",          local = FALSE)
source("R/05_cartographie.R",      local = FALSE)

# 4. UI
ui <- dashboardPage(
  dashboardHeader(title = "TVD DASHBOARD"),
  dashboardSidebar(sidebarMenu(
    menuItem("Components & Maladies", tabName = "comp_dis",             icon = icon("list")),
    menuItem("Indicateurs",           tabName = "indicators",           icon = icon("tasks")),
    menuItem("Shapefiles",            tabName = "shapefiles",           icon = icon("map")),
    menuItem("Base GNARF",            tabName = "gnarf",                icon = icon("database")),
    menuItem("Autres Bases (Excel)",  tabName = "other_db",             icon = icon("file-excel")),
    menuItem("Localisation pays",     tabName = "country_localisation", icon = icon("globe-africa")),
    menuItem("API GHO",               tabName = "gho_api",              icon = icon("cloud-download-alt")),
    menuItem("Datawarehouse SQLite",  tabName = "sqlite",               icon = icon("server")),
    menuItem("Analyse régionale",     tabName = "analysis",             icon = icon("chart-line")),
    menuItem("Base Fusionnée",        tabName = "fusion",               icon = icon("layer-group")),
    menuItem("Rapport analytique",    tabName = "produits",             icon = icon("file-alt"))
  )),
  dashboardBody(
    tags$head(
     # tags$link(rel = "stylesheet", type = "text/css", href = "www/un-style.css"),
      tags$style(HTML("
      
      /* =====================================================
         PALETTE INSTITUTIONNELLE
         ===================================================== */
      
      :root {
        --who-navy: #08265F;
        --who-blue: #0096D6;
        --who-orange: #F36C2B;
        --who-green: #64C3A5;
        --who-dark: #062D3D;
        --who-gold: #F2A900;
        --who-bg: #F4F7FB;
        --who-border: #DCE4EE;
        --who-text: #263445;
        --who-muted: #667382;
      }
      
      /* =====================================================
         BANDEAU SUPERIEUR
         ===================================================== */
      
      .main-header .logo {
        background-color: var(--who-navy) !important;
        color: #FFFFFF !important;
        font-weight: 700;
        font-size: 17px;
        text-align: left;
        padding-left: 18px;
        border-bottom: 0;
      }
      
      .main-header .logo:hover {
        background-color: var(--who-navy) !important;
      }
      
      .main-header .navbar {
        background-color: var(--who-navy) !important;
      }
      
      .main-header .sidebar-toggle {
        color: #FFFFFF !important;
      }
      
      .main-header .navbar .sidebar-toggle:hover {
        background-color: #123A7A !important;
      }
      
      /* =====================================================
         MENU LATERAL CLAIR
         ===================================================== */
      
      .main-sidebar,
      .left-side {
        background-color: #FFFFFF !important;
        border-right: 1px solid var(--who-border);
      }
      
      .sidebar-menu > li > a {
        color: #303B49 !important;
        font-size: 14px;
        padding: 14px 12px;
        border-left: 4px solid transparent;
      }
      
      .sidebar-menu > li > a:hover {
        background-color: #EEF5FC !important;
        color: var(--who-navy) !important;
        border-left-color: var(--who-blue);
      }
      
      .sidebar-menu > li.active > a,
      .sidebar-menu > li.active > a:hover {
        background-color: #EAF3FC !important;
        color: var(--who-navy) !important;
        border-left: 4px solid var(--who-blue);
        font-weight: 700;
      }
      
      .sidebar-menu .treeview-menu {
        background-color: #F8FAFD !important;
      }
      
      .sidebar-menu .treeview-menu > li > a {
        color: #475467 !important;
        padding: 10px 8px 10px 25px;
      }
      
      .sidebar-section-label {
        color: #718096;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 1.2px;
        padding: 22px 15px 8px;
        border-top: 1px solid var(--who-border);
        margin-top: 12px;
      }
      
      .sidebar-footer {
        margin: 30px 14px 15px;
        padding: 15px;
        border: 1px solid var(--who-border);
        border-radius: 8px;
        color: var(--who-navy);
        font-size: 12px;
        background: #F8FAFD;
      }
      
      .sidebar-footer p {
        color: var(--who-muted);
        margin: 5px 0 0;
      }
      
      /* =====================================================
         FOND ET TITRES
         ===================================================== */
      
      .content-wrapper,
      .right-side {
        background-color: var(--who-bg) !important;
      }
      
      .content {
        padding: 25px 28px 35px;
      }
      
      .page-heading {
        color: var(--who-navy);
        font-size: 29px;
        font-weight: 750;
        line-height: 1.3;
        margin: 4px 0 8px;
      }
      
      .page-subtitle {
        color: var(--who-muted);
        font-size: 14px;
        margin-bottom: 24px;
      }
      
      .section-heading {
        color: var(--who-navy);
        font-size: 21px;
        font-weight: 700;
        margin: 18px 0 15px;
      }
      
      .institutional-rule {
        height: 3px;
        background: linear-gradient(
          to right,
          var(--who-blue) 0,
          var(--who-blue) 100px,
          #DCE4EE 100px,
          #DCE4EE 100%
        );
        margin: 20px 0 25px;
      }
      
      /* =====================================================
         CARTES ET PANNEAUX
         ===================================================== */
      
      .box {
        border: 1px solid var(--who-border) !important;
        border-top: 3px solid var(--who-blue) !important;
        border-radius: 8px !important;
        box-shadow: 0 2px 8px rgba(8, 38, 95, 0.045) !important;
        background: #FFFFFF !important;
        margin-bottom: 22px;
      }
      
      .box.box-info {
        border-top-color: var(--who-blue) !important;
      }
      
      .box.box-primary {
        border-top-color: var(--who-navy) !important;
      }
      
      .box.box-success {
        border-top-color: var(--who-green) !important;
      }
      
      .box.box-warning {
        border-top-color: var(--who-gold) !important;
      }
      
      .box-header {
        padding: 15px 18px 12px !important;
        border-bottom: 1px solid #EDF0F5;
      }
      
      .box-title {
        color: var(--who-navy) !important;
        font-size: 16px !important;
        font-weight: 700 !important;
      }
      
      .box-body {
        padding: 18px !important;
      }
      
      /* =====================================================
         INDICATEURS CLES
         ===================================================== */
      
      .small-box {
        border-radius: 7px !important;
        box-shadow: none !important;
        min-height: 155px;
        margin-bottom: 18px;
        overflow: hidden;
      }
      
      .small-box h3 {
        font-size: 30px !important;
        font-weight: 750 !important;
        margin: 12px 0 8px !important;
      }
      
      .small-box p {
        font-size: 14px !important;
        line-height: 1.45;
      }
      
      .small-box .icon {
        top: 12px !important;
        right: 12px !important;
      }
      
      .small-box .icon > i {
        font-size: 42px !important;
        opacity: 0.22;
      }
      
      .kpi-blue {
        background-color: #0799D0 !important;
        color: #FFFFFF !important;
      }
      
      .kpi-orange {
        background-color: #F36C2B !important;
        color: #FFFFFF !important;
      }
      
      .kpi-green {
        background-color: #64C3A5 !important;
        color: #073D35 !important;
      }
      
      .kpi-navy {
        background-color: #062D3D !important;
        color: #FFFFFF !important;
      }
      
      .kpi-gold {
        background-color: #F2A900 !important;
        color: #292400 !important;
      }
      
      /* =====================================================
         BOUTONS ET FORMULAIRES
         ===================================================== */
      
      .btn {
        border-radius: 5px !important;
        font-weight: 600;
        padding: 8px 13px;
      }
      
      .btn-primary {
        background-color: var(--who-navy) !important;
        border-color: var(--who-navy) !important;
      }
      
      .btn-primary:hover {
        background-color: #123A7A !important;
      }
      
      .btn-success {
        background-color: #16856B !important;
        border-color: #16856B !important;
      }
      
      .form-control,
      .selectize-input {
        border-radius: 5px !important;
        border-color: #CBD5E1 !important;
        box-shadow: none !important;
      }
      
      .control-label {
        color: #344054;
        font-weight: 600;
      }
      
      /* =====================================================
         TABLEAUX
         ===================================================== */
      
      table.dataTable {
        border-collapse: collapse !important;
        width: 100% !important;
      }
      
      table.dataTable thead th {
        background: #EDF3FA !important;
        color: var(--who-navy) !important;
        font-weight: 700 !important;
        border-bottom: 1px solid #D5DFEA !important;
      }
      
      table.dataTable tbody tr:hover {
        background-color: #F0F7FC !important;
      }
      
      /* =====================================================
         STATUTS ET INFORMATIONS
         ===================================================== */
      
      .status-ok {
        color: #16803C;
        font-weight: 700;
      }
      
      .status-ko {
        color: #B42318;
        font-weight: 700;
      }
      
      .small-note {
        color: var(--who-muted);
        font-size: 12px;
      }
      
      .report-note {
        background: #FFFFFF;
        border: 1px solid var(--who-border);
        border-left: 4px solid var(--who-blue);
        border-radius: 5px;
        padding: 13px 16px;
        color: #526173;
        font-size: 12px;
        margin-top: 12px;
      }
      
      /* =====================================================
         ADAPTATION AUX ECRANS ET GRAPHIQUES
         ===================================================== */
      
      .plotly,
      .plot-container {
        max-width: 100%;
      }
      
      @media (max-width: 767px) {
        .content {
          padding: 15px 10px;
        }
        
        .page-heading {
          font-size: 23px;
        }
        
        .small-box h3 {
          font-size: 25px !important;
        }
      }
      
      "))
    ),
    tabItems(
      ui_tab_comp_dis(),       # dans 02_import_validation.R
      ui_tab_indicators(),
      ui_tab_shapefiles(),     # dans 05_cartographie.R
      ui_tab_gnarf(),
      ui_tab_other_db(),
      ui_tab_country_loc(),
      ui_tab_gho_api(),
      ui_tab_sqlite(),         # dans 03_processing.R
      ui_tab_analysis(),       # dans 04_analysis.R
      ui_tab_fusion(),
      ui_tab_produits()
    )
  )
)

# 5. Serveur
server <- function(input, output, session) {
  
  # état global partagé entre modules
  rv <- reactiveValues()
  sqlite_db_refresh <- reactiveVal(0L)
  
  # branchement des modules serveur (chaque module expose un srv_* qui enregistre
  # ses observers/outputs dans la session courante)
  srv_import_validation (input, output, session, rv, sqlite_db_refresh)
  srv_processing        (input, output, session, rv, sqlite_db_refresh)
  srv_analysis          (input, output, session, rv, sqlite_db_refresh)
  srv_cartographie      (input, output, session, rv, sqlite_db_refresh)
}

shinyApp(ui, server)

