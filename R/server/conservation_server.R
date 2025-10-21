conservation_server <- function(id, selected_tf, is_active, colors_app) {
  # One-time data loads
  TF_filter_information       <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))
  TF_ds_summarised_metrics    <- readRDS(here("data", "dream_output", "TF_68_summarised_metrics_500_2000.rds"))
  sel_rationale               <- readRDS(here("data", "TF_characteristics", "TF_selection_rationale.rds"))
  sel_rationale_processed     <- process_selection_rationale(sel_rationale)
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1) Keep last TF shown while tab is active
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
    
    # 2) Filter gate and message for current TF
    show_plots <- reactive({
      is_TF_not_filtered_out(TF_filter_information, current_tf(), "F4")
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
        div(
          class = "p-4",
          h3("This TF is filtered out"),
          p(filter_message())
        )
      } else {
        tagList(
          # Floating control panel
          shiny::tags$div(
            style = paste(
              "position:fixed; top:50vh; left:20px; width:18vw; z-index:1050;",
              "background:white; padding:15px; border:1px solid #ddd;",
              "border-radius:10px; box-shadow:0 0 10px rgba(0,0,0,0.1);"
            ),
            radioButtons(
              ns("cs_metric_type"),
              "Conservation metric based on...",
              choices = c(
                "DRT"     = "DRT_DE_union_log10_lm_score",
                "DT"      = "DT_DE_union_log10_lm_score",
                "DR"      = "DE_union_log10_lm_score",
                "DRT_tls" = "DRT_DE_union_log10_tls_score"
              ),
              selected = "DRT_DE_union_log10_lm_score"
            ),
            sliderInput(
              ns("de_union_filter"),
              "TF filter (DE_union of at least...)",
              min = 0, max = 20, value = 5, step = 1
            ),
            textOutput(ns("n_tfs_kept"))
          ),
          
          # Main content (space for floating panel)
          shiny::tags$div(
            style = "margin-left: 25vw;",
            fluidRow(
              style = "height: 100vh;",
              column(
                width = 12,
                section_header("Model fit"),
                plotlyOutput(ns("conservation_linear_model"), height = "70vh"),
                styled_table(ns("conservation_score_table"), height = "20vh", width = "90%")
              )
            ),
            fluidRow(
              style = "height: 100vh;",
              column(
                width = 12,
                section_header("Ranking in divergence"),
                styled_plot(ns("conservation_ranked_barplot"), height = "100vh")
              )
            ),
            fluidRow(
              style = "height: 70vh; margin-bottom: 200px;",
              column(
                width = 12,
                section_header("Divergence by selection rationale"),
                styled_plotly_plot(ns("conservation_by_rational_plot"), height = "70vh")
              )
            )
          )
        )
      }
    })
    
    # 4) Data reactives
    filtered_data <- reactive({
      req(is_active(), show_plots(), input$de_union_filter, input$cs_metric_type)
      
      TFs_to_keep <- TF_ds_summarised_metrics %>%
        dplyr::filter(permuted == FALSE, n_DE_union >= input$de_union_filter) %>%
        dplyr::distinct(target_TF) %>%
        dplyr::pull()
      
      filtered <- TF_ds_summarised_metrics %>%
        dplyr::filter(target_TF %in% TFs_to_keep)
      
      add_con_score_metrics_simplified(
        filtered,
        downsampled = FALSE,
        reference_TF_df = NULL
      )
    })
    
    # 5) Small info text
    output$n_tfs_kept <- renderText({
      req(is_active(), show_plots(), input$de_union_filter)
      n_tfs <- TF_ds_summarised_metrics %>%
        dplyr::filter(permuted == FALSE, n_DE_union >= input$de_union_filter) %>%
        dplyr::distinct(target_TF) %>%
        nrow()
      paste("TFs kept:", n_tfs)
    })
    
    # 6) Plots and tables
    
    # Model fit (plotly)
    output$conservation_linear_model <- plotly::renderPlotly({
      req(show_plots(), input$cs_metric_type)
      cs_df <- filtered_data()
      validate(need(
        current_tf() %in% cs_df$target_TF,
        "TF was removed due to the DE union filter. (see panel on the left)"
      ))
      con_model_plot(cs_df, current_tf(), input$cs_metric_type, color = colors_app$TF)
    })
    
    # Score table (DT)
    output$conservation_score_table <- DT::renderDT({
      req(show_plots(), input$cs_metric_type)
      cs_df <- filtered_data()
      validate(need(
        current_tf() %in% cs_df$target_TF,
        "TF was removed due to the DE union filter. (see panel on the left)"
      ))
      cs_df %>%
        dplyr::filter(target_TF == current_tf()) %>%
        dplyr::mutate(conservation_score = round(!!rlang::sym(input$cs_metric_type), 3)) %>%
        dplyr::select(
          target_TF, conservation_score,
          DRT_obs, DT_obs, DR_obs,
          DE_union_obs, DE_human_obs, DE_cyno_obs
        ) %>%
        stats::setNames(c(
          "TF", "Divergence Score", "n_DRT", "n_DT", "n_DR",
          "n_DE_union", "n_DE_human", "n_DE_cynomolgus"
        ))
    },
    options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE),
    extensions = "Buttons")
    
    # Ranked barplot
    output$conservation_ranked_barplot <- renderPlot({
      req(show_plots(), input$cs_metric_type)
      cs_df <- filtered_data()
      con_barplot(cs_df, current_tf(), input$cs_metric_type, color = colors_app$TF)
    }, res = 120) %>%
      bindCache(current_tf(), input$de_union_filter, input$cs_metric_type)
    
    # Conservation by selection rationale (plotly)
    output$conservation_by_rational_plot <- plotly::renderPlotly({
      req(show_plots(), input$cs_metric_type)
      cs_df <- filtered_data()
      con_rational_plot(cs_df, current_tf(), sel_rationale_processed, input$cs_metric_type, color = colors_app$TF)
    })
  })
}
