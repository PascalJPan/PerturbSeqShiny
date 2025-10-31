permutation_server <- function(id, selected_tf, is_active, colors_app) {
  
  # One-time data loads
  TF_filter_information       <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))
  TF_cs_metrics               <- readRDS(here("data", "dream_output", "TF_ds_perm_score_metrics_68.rds"))
  TF_summarised_metrics       <- readRDS(here("data", "dream_output", "TF_68_summarised_metrics_500_2000_perm_included.rds"))
  
  # MORE SPEED NEEDED? -> the output of the following section can be run and saved outside the app and loaded here, 
  # this will speed the initial loading for the page up by quite a bit, but removing not needed metrics (esp. bootstraps) will do the same.
  perm_metrics_all_nested <- TF_summarised_metrics %>%
    dplyr::group_by(target_TF) %>%
    dplyr::group_modify(~ perm_metrics_with_CIs(.x)) %>% #Adapt this function in R/helper to change what metrics are available, make sure to add the options on the toggle bar on the left and to name the metrics with the ending _estimate and the CIs with _lwr and _upr
    dplyr::ungroup()
  
  sel_rationale               <- readRDS(here("data", "TF_characteristics", "TF_selection_rationale.rds"))
  sel_rationale_processed     <- process_selection_rationale(sel_rationale)
  
  cs_metric_type <- "perm_score"
  
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
              "position:fixed; top:25vh; left:20px; width:18vw; z-index:100;",
              "background:white; padding:15px; border:1px solid #ddd;",
              "border-radius:10px; box-shadow:0 0 10px rgba(0,0,0,0.1);"
            ),
            sliderInput(
              ns("dr_div_mean_filter"),
              "Mean DR div of at least...",
              min = 0, max = 20, value = 0, step = 1
            ),
            # sliderInput(
            #   ns("dr_div_median_filter"),
            #   "Median DR div of at least...",
            #   min = 0, max = 20, value = 0, step = 1
            # ),
            checkboxInput(
              ns("perm_touches_zero_filter"),
              label = "Filter low influence TFs (DR div == 0...)",
              value = TRUE
            ),
            radioButtons(
              ns("metric"),
              label   = "Metric",
              choices = c(
                "Mean"        = "perm_score_mean",
                "Median"      = "perm_score_median",
                "Log10 mean"  = "perm_log10_score_mean",
                "Bootstrap 1DR"  = "perm_score_mean_bootstrap_1DR",
                "Bootstrap 1SE"  = "perm_score_mean_bootstrap_1SE",
                "Bootstrap 1SE log10"  = "perm_score_mean_log10_bootstrap_1SE"
              ),
              selected = "perm_score_mean_bootstrap_1SE",
              inline = FALSE
            ),
            # sliderInput(
            #   ns("uncertainty_filter"),
            #   "Maximal uncertainty of ...",
            #   min = 0, max = 5, value = 5, step = 0.05
            # ),
            sliderInput(
              ns("max_perm_score_filter"),
              "Perm scores below...",
              min = 1, max = 10, value = 1, step = 1
            ),
            textOutput(ns("n_tfs_kept"))
          ),
          
          # Main content (space for floating panel)
          shiny::tags$div(
            style = "margin-left: 25vw;",
            fluidRow(
              column(
                width = 12,
                section_header("Permutated DR number vs Observed"),
                plotlyOutput(ns("perm_beeswarm"), height = "70vh"),
                styled_table(ns("perm_stats_table"), height = "20vh", width = "90%")
              )
            ),
            fluidRow(
              column(
                width = 12,
                section_header("Divergence with confidence interval"),
                styled_plotly_plot(ns("conservation_with_CI_plot_small"), height = "35vh"),
                styled_table(ns("div_score_with_CI_table"), height = "20vh", width = "90%")
              )
            ),
            fluidRow(
              column(
                width = 12,
                styled_plotly_plot(ns("conservation_with_CI_plot_big"), height = "90vh")
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
    
    
    metric_cols <- reactive({
      req(input$metric)
      base <- input$metric
      list(
        est = paste0(base, "_estimate"),
        lwr = paste0(base, "_lwr"),
        upr = paste0(base, "_upr"),
        scores_list = paste0(base, "_scores_list")
      )
    })
    
    # 4) Data Filters
    tfs_from_mean_div_filter <- reactive({
      req(is_active(), show_plots(), input$dr_div_mean_filter)
      TF_cs_metrics %>%
        dplyr::filter(DR_div >= input$dr_div_mean_filter) %>%
        dplyr::distinct(target_TF) %>%
        dplyr::pull()
    })
    
    # tfs_from_median_div_filter <- reactive({
    #   req(is_active(), show_plots(), input$dr_div_median_filter)
    #   TF_cs_metrics %>%
    #     dplyr::filter(DR_div >= input$dr_div_median_filter) %>%
    #     dplyr::distinct(target_TF) %>%
    #     dplyr::pull()
    # })
    
    #Filtering all cases where min(DR_div) == 0 and max_DR_div >= min_DR_obs
    #So where the perm clooud touches zero and the observed number is in the cloud
    #This excludes four more TFs then when using min_DR_obs == 0 as second criteria, but all of these have only 1 DR and its still not possible to say much: "TFDP1"   "ZFP82"   "ZNF639"  "ZSCAN21"
    TFs_perm_touches_zero_filter <- reactive({
      req(is_active(), show_plots())

      if (input$perm_touches_zero_filter) {
        TF_summarised_metrics %>%
          group_by(target_TF) %>%
          summarise(
            min_DR_div = min(n_DR[permuted]),
            max_DR_div = max(n_DR[permuted]),
            min_DR_obs = min(n_DR[!permuted])
          ) %>%
          filter(min_DR_div > 0 | max_DR_div < min_DR_obs) %>%
          pull(target_TF)
      } else {
        TF_summarised_metrics %>%
        pull(target_TF) %>%
        unique()
      }
    })
    
    # tfs_low_perm_uncertainty <- reactive({
    #   req(is_active(), show_plots(), input$uncertainty_filter)
    #   
    #   cols <- metric_cols()
    #   
    #   perm_metrics_all_nested %>% 
    #     mutate(uncertainty_range = abs(.data[[cols$upr]] - .data[[cols$lwr]])) %>% 
    #     filter(uncertainty_range <= input$uncertainty_filter) %>% 
    #     dplyr::pull(target_TF)
    # })
    
    tfs_below_perm_score_threshold <- reactive({
      req(is_active(), show_plots(),input$max_perm_score_filter) 
      
      cols <- metric_cols()
      
      perm_metrics_all_nested %>% 
        mutate(estimation = .data[[cols$est]]) %>% 
        filter(estimation <= input$max_perm_score_filter) %>% 
        dplyr::pull(target_TF)
    })
    
    
    tfs_final <- reactive({
      sets <- list(
        tfs_from_mean_div_filter(),
        #tfs_from_median_div_filter(),
        TFs_perm_touches_zero_filter(),
        #tfs_low_perm_uncertainty(),
        tfs_below_perm_score_threshold())
      
      # debug
      message("---- Debug: Inputs to Reduce ----")
      for (i in seq_along(sets)) {
        cat("\nSet", i, "type:", class(sets[[i]]), " len:", length(sets[[i]]), "\n")
        print(utils::head(sets[[i]]))
      }
      message("-------------------------------")
      
      # safe intersect across all sets
      Reduce(intersect, sets)
    })
    
    
    # Filtered outputs
    filtered_data <- reactive({
      tfs_final_output <- tfs_final()
      
      req(is_active(), show_plots(), tfs_final())
      TF_cs_metrics %>%
        dplyr::filter(target_TF %in% tfs_final())
    })
    
    filtered_data_full <- reactive({
      req(is_active(), show_plots(), tfs_final())
      TF_summarised_metrics %>%
        dplyr::filter(target_TF %in% tfs_final())
    })
    
    perm_metrics_all_nested_filtered <- reactive({
      req(is_active(), show_plots(), tfs_final())
      perm_metrics_all_nested %>%
        dplyr::filter(target_TF %in% tfs_final())
    })
    
    # Info text
    output$n_tfs_kept <- renderText({
      paste("TFs kept:", length(tfs_final()))
    })
    
    
    # 6) Plots and tables
    
   
    output$perm_beeswarm <- plotly::renderPlotly({
      req(show_plots())
      cs_df <- filtered_data_full()
      validate(need(
        current_tf() %in% cs_df$target_TF,
        "TF was removed due to the DE union filter. (see panel on the left)"
      ))
      p <- suppressWarnings(perm_beeswarm_plot(cs_df, current_tf(), color = colors_app$TF))
      plotly::ggplotly(p, tooltip = c("text", "y"))
    })
    
    # Score table (DT)
    output$perm_stats_table <- DT::renderDT({
      req(show_plots())
      cs_df <- filtered_data()
      validate(need(
        current_tf() %in% cs_df$target_TF,
        "TF was removed due to the DE union filter. (see panel on the left)"
      ))
      cs_df %>%
        dplyr::filter(target_TF == current_tf()) %>%
        # dplyr::mutate(conservation_score = round(!!rlang::sym("perm_score"), 3)) %>%
        # dplyr::mutate(perm_z_score = round(perm_z_score, 1)) %>%
        # dplyr::mutate(perm_z_score_to_zero = round(perm_z_score_to_zero, 1)) %>%
        # dplyr::mutate(div_score_perm_z = round(1 - (perm_z_score/perm_z_score_to_zero), 1)) %>%
        dplyr::select(
          target_TF, 
          #conservation_score, div_score_perm_z, perm_z_score, perm_z_score_to_zero,
          DR_obs, DR_div,
          DE_union_obs, DE_human_obs, DE_cyno_obs
        ) %>%
        stats::setNames(c(
          "TF", 
          #"Divergence Score", "Div z", "p_z", "p_z_0", 
          "n_DR", "n_DR_perm","n_DE_union", "n_DE_human", "n_DE_cynomolgus"
        ))
    },
    options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE),
    extensions = "Buttons")
    
    
    
    output$div_score_with_CI_table <- DT::renderDT({
       req(show_plots())
      perm_df <- perm_metrics_all_nested_filtered()
      cols <- metric_cols()
      
      validate(need(all(c(cols$est, cols$lwr, cols$upr, cols$scores_list) %in% names(perm_df)),
                    "Selected metric columns not found."))
      selected_TF <- current_tf()
      
      selected_df <- perm_df %>% 
        filter(target_TF == selected_TF) %>% 
        select(.data[[cols$lwr]],.data[[cols$est]],.data[[cols$upr]])
      
      colnames(selected_df) <- c("CI_lwr","divergence","CI_upr")
      
      signif(selected_df,2)
        
      
    },
    options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE),
    extensions = "Buttons")
    
    
    
    
    # Confidence Interval Plots
    output$conservation_with_CI_plot_small <- plotly::renderPlotly({
      req(show_plots())
      perm_df <- perm_metrics_all_nested_filtered()
      cols <- metric_cols()
      
      validate(need(all(c(cols$est, cols$lwr, cols$upr, cols$scores_list) %in% names(perm_df)),
                    "Selected metric columns not found."))
      
      plot_perm_score_uncertainty_plotly(
        df               = perm_df,
        estimate_col     = cols$est,
        lwr_col          = cols$lwr,
        upr_col          = cols$upr,
        selected_TF      = current_tf(),
        selected_TF_only = TRUE,
        scores_col = cols$scores_list,
        target_TF_color = colors_app$TF
      )
    })
    
    output$conservation_with_CI_plot_big <- plotly::renderPlotly({
      req(show_plots())
      perm_df <- perm_metrics_all_nested_filtered()
      cols <- metric_cols()
      
      validate(need(all(c(cols$est, cols$lwr, cols$upr, cols$scores_list) %in% names(perm_df)),
                    "Selected metric columns not found."))
      
      plot_perm_score_uncertainty_plotly(
        df               = perm_df,
        estimate_col     = cols$est,
        lwr_col          = cols$lwr,
        upr_col          = cols$upr,
        selected_TF      = current_tf(),
        scores_col = cols$scores_list,
        selected_TF_only = FALSE,
        target_TF_color = colors_app$TF
      )
    })
    
    # Conservation by selection rationale (plotly)
    output$conservation_by_rational_plot <- plotly::renderPlotly({
      req(show_plots(), cs_metric_type)
      cols <- metric_cols()
      perm_df <- perm_metrics_all_nested_filtered()
      
      
      #cs_df <- filtered_data()
      con_rational_plot(perm_df, current_tf(), sel_rationale_processed, cols$est, color = colors_app$TF)
    })
  })
}
