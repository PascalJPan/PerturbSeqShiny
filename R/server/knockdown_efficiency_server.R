knockdown_efficiency_server <- function(id, selected_tf, is_active, colors_app) {
  TF_filter_information <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))
  knockdown_data        <- readRDS(here("data", "knockdown_efficiency", "tf_de.rds"))
  knockdown_data_human      <- dplyr::filter(knockdown_data, species == "human")
  knockdown_data_cynomolgus <- dplyr::filter(knockdown_data, species == "cynomolgus")
  
  # Cache heavy object once
  if (!exists("seu")) {
    seu <- readRDS(here("data", "stemness", "seu_QCfilt_gRNAfilt_controlFilt.rds"))
    seu$cell_type <- gsub("_", " ", seu$cell_type)
  }
  
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
    
    # Dynamic UI
    output$content <- renderUI({
      req(current_tf())
      if (!show_plots()) {
        div(class = "p-4", h3("This TF is filtered out"), p(filter_message()))
      } else {
        tagList(
          fluidRow(
            column(
              width = 12,
              section_header("Knockdown efficiency per gRNA"),
              column(
                width = 6,
                species_header("Human"),
                styled_plotly_plot(ns("gRNA_knockdown_efficiency_human"), height = "70vh"),
                div(class = "species-spacer")
              ),
              column(
                width = 6,
                species_header("Cynomolgus"),
                styled_plotly_plot(ns("gRNA_knockdown_efficiency_cynomolgus"), height = "70vh"),
                div(class = "species-spacer")
              )
            ),
            column(
              width = 12,
              section_header("Distribution of logcounts per gRNA"),
              column(
                width = 6,
                species_header("Human"),
                styled_plot(ns("logcounts_violinplot_human"), height = "70vh"),
                div(class = "species-spacer")
              ),
              column(
                width = 6,
                species_header("Cynomolgus"),
                styled_plot(ns("logcounts_violinplot_cynomolgus"), height = "70vh"),
                div(class = "species-spacer")
              )
            ),
            column(
              width = 12,
              section_header("TF logFC per baseline expression"),
              column(
                width = 6,
                species_header("Human"),
                styled_plotly_plot(ns("MA_plot_human"), height = "70vh")
              ),
              column(
                width = 6,
                species_header("Cynomolgus"),
                styled_plotly_plot(ns("MA_plot_cynomolgus"), height = "70vh")
              )
            )
          )
        )
      }
    })
    
    # Plots
    output$gRNA_knockdown_efficiency_human <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      validate(need(tf %in% knockdown_data_human$perturbed_TF,
                    "No knockdown data for this TF in human."))
      plot_tf_knockdown_beeswarm(knockdown_data_human, tf)
    }) %>% bindCache(current_tf())
    
    output$gRNA_knockdown_efficiency_cynomolgus <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      validate(need(tf %in% knockdown_data_cynomolgus$perturbed_TF,
                    "No knockdown data for this TF in cynomolgus."))
      plot_tf_knockdown_beeswarm(knockdown_data_cynomolgus, tf)
    }) %>% bindCache(current_tf())
    
    output$logcounts_violinplot_human <- renderPlot({
      req(show_plots())
      logcounts_violin_plot(filtered_seu(), current_tf(), "human", highlight_color = colors_app$human)
    }, res = 120) %>% bindCache(current_tf())
    
    output$logcounts_violinplot_cynomolgus <- renderPlot({
      req(show_plots())
      logcounts_violin_plot(filtered_seu(), current_tf(), "cynomolgus", highlight_color = colors_app$cynomolgus)
    }, res = 120) %>% bindCache(current_tf())
    
    output$MA_plot_human <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      validate(need(tf %in% knockdown_data_human$perturbed_TF,
                    "No knockdown data for this TF in human."))
      ma_plot_knockdown(knockdown_data_human, tf, highlight_color = colors_app$human)
    }) %>% bindCache(current_tf())
    
    output$MA_plot_cynomolgus <- plotly::renderPlotly({
      req(show_plots())
      tf <- current_tf()
      validate(need(tf %in% knockdown_data_cynomolgus$perturbed_TF,
                    "No knockdown data for this TF in cynomolgus."))
      ma_plot_knockdown(knockdown_data_cynomolgus, tf, highlight_color = colors_app$cynomolgus)
    }) %>% bindCache(current_tf())
  })
}