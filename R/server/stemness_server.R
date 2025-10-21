stemness_server <- function(id, selected_tf, is_active, colors_app = colors_app) {
  
  ct_colors <- setNames(c("grey50", 
                          "#ddf5ff", "#90e0ef", "#00b4d8", "#023e8a",
                          "#68d8d6", "#07beb8",
                          "#ffcad4", "#FF4782","#FF4242", "#cb0b0a","#8e0413", 
                          "#c5edac" ,"#81c14b", "#1a7431",
                          "lightgrey"),
                        nm= gsub("_", " ", c("iPSCs", 
                                             "early_ectoderm", "astrocyte_progenitor","granule_precursor_cells","neurons",  
                                             "neural_crest_I", "neural_crest_II",
                                             "fibroblasts","smooth_muscle_cells", "cardiac_progenitor_cells","cardiac_endothelial_cells","cardiac_fibroblasts",
                                             "early_epithelial_cells" ,"epithelial_cells","hepatocytes",
                                             "unassigned")))
  
  if (!exists("seu")) {
    seu <- readRDS(here("data", "stemness", "seu_QCfilt_gRNAfilt_controlFilt.rds"))
    seu$cell_type <- gsub("_", " ", seu$cell_type)
  }
  
  TF_filter_information <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- TF latch ---
    last_tf_shown <- reactiveVal(NULL)
    observeEvent(list(selected_tf(), is_active()), {
      req(is_active())
      tf <- selected_tf()
      if (!identical(tf, last_tf_shown())) last_tf_shown(tf)
    }, ignoreInit = FALSE)
    
    current_tf <- reactive({
      req(last_tf_shown())
      last_tf_shown()
    })
    
    filtered_seu <- reactive({
      subset(seu, subset = perturbed_TF %in% c(current_tf(), "NT_control"))
    })
    
    show_plots <- reactive({
      is_TF_not_filtered_out(TF_filter_information, current_tf(), "F2")
    })
    
    filter_message <- reactive({
      m <- TF_filter_information |>
        dplyr::filter(TF == current_tf()) |>
        dplyr::pull(filter_message)
      if (length(m) == 0) "No information available for this TF." else m[1]
    })
    
    # --- Dynamic UI ---
    output$content <- renderUI({
      req(current_tf())
      
      if (!show_plots()) {
        div(class = "p-4",
            h3("This TF is filtered out"),
            p(filter_message()))
      } else {
        # your original stemness_ui layout
        tagList(
          fluidRow(
            column(
              width = 12,
              section_header("Cell type proportions"),
              fluidRow(
                column(
                  width = 6,
                  species_header("Human"),
                  styled_plot(ns("cell_type_barplot_human"), height="70vh", width = "100%")
                ),
                column(
                  width = 6,
                  species_header("Cynomolgus"),
                  styled_plot(ns("cell_type_barplot_cynomolgus"), height="70vh", width = "100%")
                )
              ),
              section_header("Stemness distributions across cells"),
              fluidRow(
                column(
                  width = 6,
                  species_header("Human"),
                  styled_plot(ns("stemness_histogram_human"), height="70vh", width = "100%")
                ),
                column(
                  width = 6,
                  species_header("Cynomolgus"),
                  styled_plot(ns("stemness_histogram_cynomolgus"), height="70vh", width = "100%")
                )
              ),
              section_header("Cell type and stemness UMAPs"),
              fluidRow(
                column(2,
                       div(style = "padding: 10px 15px; margin-bottom: 10px;",
                           actionButton(ns("start_tf_umap"), "Render TF UMAP")))
              ),
              column(
                width = 12,
                styled_ui(ns("tf_umap_slot"), height = "70vh", width = "100%")
                
              ),
              fluidRow(
                column(2,
                       div(style = "padding: 10px 15px; margin-bottom: 10px;",
                           actionButton(ns("start_cell_type_umap"), "Render Cell Type UMAP")))
              ),
              column(
                width = 12,
                styled_ui(ns("cell_type_umap_slot"), height = "70vh", width = "100%")
                
              ),
              fluidRow(
                column(2,
                       div(style = "padding: 10px 15px; margin-bottom: 10px;",
                           actionButton(ns("start_stemness_umap"), "Render Stemness UMAP")))
              ),
              column(
                width = 12,
                styled_ui(ns("stemness_umap_slot"), height = "70vh", width = "100%")
              )
            )
          )
        )
      }
    })
    
    # --- Plots ---
    output$cell_type_barplot_human <- renderPlot({
      req(show_plots())
      stacked_cell_type_barplot_plot(filtered_seu(), current_tf(), ct_colors, "human")
    }, res = 120) %>% bindCache(current_tf())
    
    output$cell_type_barplot_cynomolgus <- renderPlot({
      req(show_plots())
      stacked_cell_type_barplot_plot(filtered_seu(), current_tf(), ct_colors, "cynomolgus")
    }, res = 120) %>% bindCache(current_tf())
    
    output$stemness_histogram_human <- renderPlot({
      req(show_plots())
      stemness_violin_plot(filtered_seu(), current_tf(), "human", highlight_color = colors_app$human)
    }, res = 120) %>% bindCache(current_tf())
    
    output$stemness_histogram_cynomolgus <- renderPlot({
      req(show_plots())
      stemness_violin_plot(filtered_seu(), current_tf(), "cynomolgus", highlight_color = colors_app$cynomolgus)
    }, res = 120) %>% bindCache(current_tf())
    
    
    
    # UMAP slots
    output$tf_umap_slot <- renderUI({
      if (isTRUE(input$start_tf_umap > 0)) {
        styled_plot(ns("tf_umap"), height = "70vh", width = "100%")
      } else {
        blank_placeholder(height = "70vh")
      }
    })
    
    output$cell_type_umap_slot <- renderUI({
      if (isTRUE(input$start_cell_type_umap > 0)) {
        styled_plot(ns("cell_type_umap"), height = "70vh", width = "100%")
      } else {
        blank_placeholder(height = "70vh")
      }
    })
    
    output$stemness_umap_slot <- renderUI({
      if (isTRUE(input$start_stemness_umap > 0)) {
        styled_plot(ns("stemness_umap"), height = "70vh", width = "100%")
      } else {
        blank_placeholder(height = "70vh")
      }
    })
    
    # --- UMAP buttons ---
    observeEvent(input$start_tf_umap, {
      output$tf_umap <- renderPlot({
        req(show_plots())
        
        p <- plot_umap_highlight(
          seu, current_tf(), "perturbed_TF", "species", "umap_per_species",
          text_size = 13,
          point_size = 2,
          color = colors_app$TF,
          other_color = "lightgrey"
        )

        p
      }, res = 120)
    })
    
    observeEvent(input$start_cell_type_umap, {
      output$cell_type_umap <- renderPlot({
        req(show_plots())
        p <- plot_umap_split(seu, "cell_type", "species", "umap_per_species",
                             colors = ct_colors, text_size = 13, point_size = 2, blend=FALSE) 
        p
      }, res = 120)
    })
    
    observeEvent(input$start_stemness_umap, {
      output$stemness_umap <- renderPlot({
        req(show_plots())
        req("stemness_score" %in% colnames(seu@meta.data))  
        
        pal <- RColorBrewer::brewer.pal(9, "YlGnBu")
        
        p <- plot_umap_split(
          seu, "stemness_score", "species", "umap_per_species",
          colors = rev(pal),
          legend_title = "stemness\nindex",
          text_size = 13,
          point_size = 2,
          blend=FALSE
        ) 
        
        p
      }, res = 120)
    })
  })
}
