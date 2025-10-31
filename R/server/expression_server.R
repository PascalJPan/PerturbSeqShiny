expression_server <- function(id, selected_tf, is_active, colors_app) {
  # One-time loads
  TF_filter_information  <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))
  cell_numbers_per_gRNA  <- readRDS(here("data", "enrichment", "cell_numbers_per_gRNA.rds"))
  cell_numbers_per_gRNA_and_individual  <- readRDS(here("data", "enrichment", "cell_numbers_per_gRNA_and_individual.rds"))
  ctrl_gene_stats_by_species        <- readRDS(here("data", "expression", "ctrl_gene_stats_by_species.rds"))
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Keep last TF while tab is active
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
    
    # Gate + message
    show_plots <- reactive({
      is_TF_not_filtered_out(TF_filter_information, current_tf(), "F0")
    })
    
    filter_message <- reactive({
      m <- TF_filter_information %>%
        dplyr::filter(TF == current_tf()) %>%
        dplyr::pull(filter_message)
      if (length(m) == 0) "No information available for this TF." else m[1]
    })
    
    # Dynamic UI
    output$content <- renderUI({
      req(current_tf())
      
      if (!show_plots()) {
        div(
          class = "p-4",
          h3("This TF is filtered out"),
          p(filter_message())
        )
      } else {
        tagList(
          fluidRow(
            column(
              width = 12,
              section_header("Cell numbers per species and gRNA"),
              column(
                width = 6,
                species_header("Human", color_preset = "human"),
                shiny::tags$p("Total Cells: ", textOutput(ns("human_cell_count"), inline = TRUE)),
                styled_plotly_plot(ns("cell_count_per_grna_human"), height = "70vh"),
                div(class = "species-spacer")
              ),
              column(
                width = 6,
                species_header("Cynomolgus", color_preset = "cynomolgus"),
                shiny::tags$p("Total Cells: ", textOutput(ns("cynomolgus_cell_count"), inline = TRUE)),
                styled_plotly_plot(ns("cell_count_per_grna_cynomolgus"), height = "70vh"),
                div(class = "species-spacer")
              ),
              info_button_right(ns, "cn_info_btn")
            ),
            column(
              width = 12,
              section_header("Baseline expression of TF in iPSCs"),

              column(
                width = 6,
                species_header("Human"),
                styled_plot(ns("iPSCs_expr_percent_human"), height = "35vh"),
              ),
              column(
                width = 6,
                species_header("Cynomolgus"),
                styled_plot(ns("iPSCs_expr_percent_cynomolgus"), height = "35vh"),
              ),
              column(
                width = 6,
                species_header("Human"),
                styled_plot(ns("iPSCs_expr_mean_human"), height = "35vh")
              ),
              column(
                width = 6,
                species_header("Cynomolgus"),
                styled_plot(ns("iPSCs_expr_mean_cynomolgus"), height = "35vh")
              ),
              info_button_right(ns, "expr_info_btn")
            )
          )
        )
      }
    })
    
    info_button_bind(
      session, input,
      btn_id = "cn_info_btn",
      title  = "Cell numbers",
      text   = "The cell numbers per gRNA are shown before filtering. An exclusion criteria afterwards was that... "
    )
    
    info_button_bind(
      session, input,
      btn_id = "expr_info_btn",
      title  = "Expression in iPSCs",
      html   = "The expression of the <i>selected TF</i> is shown in comparison to the expression of all TFs from ... "
    )
    
    # Small text summaries
    output$human_cell_count <- renderText({
      cn <- cell_numbers_per_gRNA %>%
        dplyr::filter(perturbed_TF == current_tf(), species == "human") %>%
        dplyr::summarize(cell_number_human = sum(cell_count), .groups = "drop")
      as.character(cn$cell_number_human)
    })
    
    output$cynomolgus_cell_count <- renderText({
      cn <- cell_numbers_per_gRNA_and_individual %>%
        dplyr::filter(perturbed_TF == current_tf(), species == "cynomolgus") %>%
        dplyr::summarize(cell_number_cynomolgus = sum(cell_count), .groups = "drop")
      as.character(cn$cell_number_cynomolgus)
    })
    
    # Plots
    output$cell_count_per_grna_human <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      cell_number_barplot(cell_numbers_per_gRNA_and_individual, tf, "human", split_by_individual = TRUE)
    }) %>% bindCache(current_tf())
    
    output$cell_count_per_grna_cynomolgus <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      cell_number_barplot(cell_numbers_per_gRNA_and_individual, tf, "cynomolgus", split_by_individual = TRUE)
    }) %>% bindCache(current_tf())
    
    output$iPSCs_expr_percent_human <- renderPlot({
      req(show_plots())
      tf <- current_tf()
      iPSCs_expr_hist_plot(ctrl_gene_stats_by_species, tf, "human", x_col = "frac_expr", x_label = "Percent of cells", color = colors_app$human)
    }) %>% bindCache(current_tf())
    
    output$iPSCs_expr_percent_cynomolgus <- renderPlot({
      req(show_plots())
      tf <- current_tf()
      iPSCs_expr_hist_plot(ctrl_gene_stats_by_species, tf, "cynomolgus", x_col = "frac_expr", x_label = "Percent of cells", color = colors_app$cynomolgus)
    }) %>% bindCache(current_tf())
    
    output$iPSCs_expr_mean_human <- renderPlot({
      req(show_plots())
      tf <- current_tf()
      iPSCs_expr_hist_plot(ctrl_gene_stats_by_species, tf, "human", x_col = "mean_count", x_label = "Mean expression", percent_x_scale = FALSE, color = colors_app$human)
    }) %>% bindCache(current_tf())
    
    output$iPSCs_expr_mean_cynomolgus <- renderPlot({
      req(show_plots())
      tf <- current_tf()
      iPSCs_expr_hist_plot(ctrl_gene_stats_by_species, tf, "cynomolgus", x_col = "mean_count", x_label = "Mean expression", percent_x_scale = FALSE, color = colors_app$cynomolgus)
    }) %>% bindCache(current_tf())
  })
}
