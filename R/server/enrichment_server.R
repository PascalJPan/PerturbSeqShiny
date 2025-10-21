enrichment_server <- function(id, selected_tf, is_active, colors_app) {
  # One-time loads
  TF_filter_information <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))
  enrichment_data       <- readRDS(here("data", "enrichment", "lib_vs_final_perc_sum.rds"))
  
  enrichment_data_human      <- dplyr::filter(enrichment_data, species == "human")
  enrichment_data_cynomolgus <- dplyr::filter(enrichment_data, species == "cynomolgus")
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1) Keep last TF while tab is active
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
    
    # 2) Filter gate + message
    show_plots <- reactive({
      is_TF_not_filtered_out(TF_filter_information, current_tf(), "F2")
    })
    
    filter_message <- reactive({
      m <- TF_filter_information |>
        dplyr::filter(TF == current_tf()) |>
        dplyr::pull(filter_message)
      if (length(m) == 0) "No information available for this TF." else m[1]
    })
    
    # 3) Dynamic UI body (pairs with mini_ui -> uiOutput(ns("content")))
    output$content <- renderUI({
      req(current_tf())
      
      if (!show_plots()) {
        div(class = "p-4",
            h3("This TF is filtered out"),
            p(filter_message()))
      } else {
        tagList(
          fluidRow(
            column(
              width = 12,
              section_header("gRNA enrichment per species"),
              column(
                width = 6,
                species_header("Human"),
                plotlyOutput(ns("enrichment_human"), height = "70vh"),
                div(class = "species-spacer")
              ),
              column(
                width = 6,
                species_header("Cynomolgus"),
                plotlyOutput(ns("enrichment_cynomolgus"), height = "70vh"),
                div(class = "species-spacer")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              section_header("gRNA enrichment across species"),
              column(
                width = 12,
                plotlyOutput(ns("enrichment_between_species"), height = "90vh", width = "100%")
              )
            )
          )
        )
      }
    })
    
    # 4) Plots (cache by TF)
    output$enrichment_human <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      validate(need(tf %in% enrichment_data$perturbed_TF,
                    "No enrichment data for this TF (human)."))
      plot_species_enrichment(enrichment_data_human, tf, color = colors_app$TF)
    }) %>% bindCache(current_tf())
    
    output$enrichment_cynomolgus <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      validate(need(tf %in% enrichment_data$perturbed_TF,
                    "No enrichment data for this TF (cynomolgus)."))
      plot_species_enrichment(enrichment_data_cynomolgus, tf, color = colors_app$TF)
    }) %>% bindCache(current_tf())
    
    output$enrichment_between_species <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      validate(need(tf %in% enrichment_data$perturbed_TF,
                    "No cross-species enrichment data for this TF."))
      plot_human_vs_cyno_enrichment(enrichment_data, tf, color = colors_app$TF)
    }) %>% bindCache(current_tf())
  })
}
