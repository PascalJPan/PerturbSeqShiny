library(topGO)
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyFiles)
library(shinyjs)
library(glue)
library(stringr)
library(BPCells)
library(plyranges)
library(DT)
library(tidyverse)
library(here)
library(purrr)
library(shinycssloaders)
library(ggrepel)
library(plotly)
library(ggbeeswarm)
library(shinyscreenshot)
library(patchwork)
library(cowplot)
library(ggupset)
library(ggpp)
library(RColorBrewer)
library(ggblend)
library(scales)

here::i_am("app.R")

# Load all UI, server and helper functions
folders <- c("R/ui", "R/server", "R/helper", "R/style")
for (folder in folders) {
  files <- list.files(here(folder), full.names = TRUE, pattern = "\\.R$")
  for (f in files) {
    message(">> Sourcing: ", f)
    source(f, local = FALSE)
  }
}

# Handy aliases
select <- dplyr::select
tags   <- shiny::tags

# Data
tf_list <- sort(readRDS(here("data", "TF_lists", "TF_list_94.rds")))
TF_filter_information <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))

# App colors
colors_app <- list(
  human      = "#C91C52",
  cynomolgus = "#009E9A",
  TF         = "#D62728",
  DR         = "#FFC300"
)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),
  
  # ---- Head (CSS variables + styles) ----
  shiny::tags$head(
    # CSS variables
    tags$style(HTML(glue::glue("
      :root {{
        --header-control-h: 40px;
        --main-color: {colors_app$cynomolgus};
        --main-color-light: #83D5D3;
        --second-color: {colors_app$human};
        --color-human: {colors_app$human};
        --color-cynomolgus: {colors_app$cynomolgus};
      }}
    "))),
    
    # Global / component styles
    tags$style(HTML("
      /* ---- Sticky full-bleed header ---- */
      .full-bleed-header {
        position: sticky;
        top: 0;
        z-index: 1000;
        padding: 15px 30px;
        background-color: var(--main-color-light);
        display: flex;
        align-items: center;
        gap: 30px;
        color: #ffffff !important;
        margin-left: -15px;
        margin-right: -15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.12);
      }

      .full-bleed-header .selectize-input {
        color: #ffffff !important;
        background-color: #111 !important;
        border-color: #444 !important;
      }

      .full-bleed-header .selectize-control.single .selectize-input::before {
        color: #ffffff;
        opacity: 1;
      }

      .full-bleed-header .selectize-input input::placeholder {
        color: #ffffff;
        opacity: 1;
      }

      .full-bleed-header .selectize-control.single .selectize-input:after {
        border-top-color: #ffffff !important;
      }

      .full-bleed-header .form-group {
        margin-bottom: 0 !important;
      }

      /* Selectize height + vertical centering */
      #selected_tf + .selectize-control .selectize-input {
        min-height: var(--header-control-h);
        display: flex;
        align-items: center;
        padding: 8px 12px;
      }

      .header-label {
        color: #fff;
        margin: 0;
        height: var(--header-control-h);
        display: flex;
        align-items: center;
        font-weight: 600;
      }

      .header-images {
        margin-left: auto;
        display: flex;
        align-items: center;
        justify-content: flex-end;
        gap: 15px;
        margin-right: 0;
      }

      .header-img {
        height: 60px;
        margin-top: 0;
        margin-bottom: 0;
      }

      .flip-img { transform: scaleX(-1); }
      
      
      /* ---- Spacers (used inside other uis) ---- */ 
      .species-spacer {
        width: auto;
        height: 5vh;
      }

      /* ---- Tab panel styling ---- */
      .nav { margin-bottom: 0; padding-left: 0; }

      .nav-tabs {
        border-bottom: 2px solid #ddd;
        display: flex;
        justify-content: space-between;
        padding-left: 0;
        margin: 0;
      }

      .nav-tabs > li > a {
        margin-right: 0 !important;
        margin-left: 0 !important;
        color: #555;
        border: none !important;
        background-color: transparent !important;
        font-weight: 500;
        padding: 20px 15px;
        border-bottom: 3px solid transparent;
      }

      .nav-tabs > li > a:hover {
        color: var(--main-color);
        border-bottom: 3px solid var(--main-color);
        background-color: transparent;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: var(--main-color) !important;
        border: none !important;
        border-bottom: 3px solid var(--main-color) !important;
        background-color: transparent !important;
        font-weight: 400;
      }

      .nav-tabs:before,
      .nav-tabs:after { content: none !important; display: none !important; }

      /* ---- Font sizes ---- */
      body, label, input, button, select, .selectize-input, .form-control {
        font-size: 16px;
      }

      @media (max-width: 1300px) {
        body, label, input, button, select, .selectize-input, .form-control {
          font-size: 13px !important;
        }
      }

      /* ---- Slider (ionRangeSlider shiny skin) ---- */
      .irs--shiny .irs-bar,
      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
        background: var(--main-color) !important;
        border-color: var(--main-color) !important;
      }

      .irs--shiny .irs-handle > i:first-child {
        background: var(--main-color) !important;
      }

      .irs--shiny .irs-handle {
        border-color: var(--main-color) !important;
      }

      .irs--shiny .irs-line {
        background: #e6e6e6 !important;
      }
    "))
  ),
  
  # ---- Header ----
  div(
    class = "full-bleed-header",
    tags$label(
      "Choose a Transcription Factor",
      `for` = "selected_tf",
      class = "control-label header-label"
    ),
    selectizeInput(
      "selected_tf",
      label   = NULL,
      choices = tf_list,
      selected = "SOX4",
      options = list(placeholder = "Type or select a TF"),
      width   = "200px"
    ),
    actionButton(
      "debug", "Debug",
      style = "border-radius:5px; width:100px; height:40px; padding:0; margin:0"
    ),
    actionButton(
      "go", "Screenshot",
      style = "border-radius:5px; width:100px; height:40px; padding:0; margin:0"
    ),
    div(
      class = "header-images",
      tags$img(src = "data/pngs/Cynomolgus.png", class = "header-img flip-img"),
      tags$img(src = "data/pngs/Human.png", class = "header-img flip-img")
    )
  ),
  
  # ---- Tabs ----
  div(
    style = "margin-left: 15px; margin-right: 15px; margin-bottom: 100px;",
    tabsetPanel(
      id       = "tabs",
      selected = "info",
      tabPanel("Information",                 value = "info",     information_ui("")),
      tabPanel("TF Characteristics",          value = "tfchar",   tf_characteristics_ui("tfchar")),
      tabPanel("gRNA Characteristics",        value = "grnachar", grna_characteristics_ui("grnachar")),
      tabPanel("Cell Number and Expression",  value = "expr",     mini_ui("expression")),
      tabPanel("Knockdown Efficiency",        value = "knock",    mini_ui("knockdown_efficiency")),
      tabPanel("Cell Type and Stemness",      value = "stemness", mini_ui("stemness")),
      tabPanel("gRNA Enrichment",             value = "enrich",   mini_ui("enrichment")),
      tabPanel("DE and DR Analysis",          value = "dedrdt",   mini_ui("dedrdtanalysis")),
      tabPanel("Conservation",                value = "cons",     mini_ui("conservation"))
    )
  )
)

server <- function(input, output, session) {
  
  # Keep last valid TF if input becomes empty
  last_valid_tf <- reactiveVal(tf_list[[1]])
  
  safe_selected_tf <- reactive({
    val <- input$selected_tf
    if (shiny::isTruthy(val)) {
      last_valid_tf(val)
      return(val)
    } else {
      return(last_valid_tf())
    }
  })
  
  selected_tf <- safe_selected_tf
  
  # Debug: print full input structure
  observeEvent(input$debug, {
    iv <- reactiveValuesToList(input)
    cat(paste(capture.output(str(iv)), collapse = "\n"), "\n")
  })
  
  # Screenshot button
  observeEvent(input$go, {
    screenshot()
  })
  
  # Lazy-load tabs with shared API
  smart_tab_loader(
    input, tab_id = "tfchar", module_id = "tfchar",
    server_fn = tf_characteristics_server, selected_tf = selected_tf, colors_app = colors_app
  )
  
  smart_tab_loader(
    input, tab_id = "grnachar", module_id = "grnachar",
    server_fn = grna_characteristics_server, selected_tf = selected_tf, colors_app = colors_app
  )
  
  smart_tab_loader(
    input, tab_id = "expr", module_id = "expression",
    server_fn = expression_server, selected_tf = selected_tf, colors_app = colors_app
  )
  
  smart_tab_loader(
    input, tab_id = "knock", module_id = "knockdown_efficiency",
    server_fn = knockdown_efficiency_server, selected_tf = selected_tf, colors_app = colors_app
  )
  
  smart_tab_loader(
    input, tab_id = "stemness", module_id = "stemness",
    server_fn = stemness_server, selected_tf = selected_tf, colors_app = colors_app
  )
  
  smart_tab_loader(
    input, tab_id = "enrich", module_id = "enrichment",
    server_fn = enrichment_server, selected_tf = selected_tf, colors_app = colors_app
  )
  
  smart_tab_loader(
    input, tab_id = "dedrdt", module_id = "dedrdtanalysis",
    server_fn = DE_DR_DT_analysis_server, selected_tf = selected_tf, colors_app = colors_app
  )
  
  smart_tab_loader(
    input, tab_id = "cons", module_id = "conservation",
    server_fn = conservation_server, selected_tf = selected_tf, colors_app = colors_app
  )
}

shinyApp(ui, server)
