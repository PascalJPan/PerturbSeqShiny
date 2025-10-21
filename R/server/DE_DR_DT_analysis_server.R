DE_DR_DT_analysis_server <- function(id, selected_tf, is_active, colors_app) {
  # Constants
  p_value_threshold <- 0.05
  
  # One-time data for filter checks
  TF_filter_information <- readRDS(here("data", "TF_lists", "TF_filter_information.rds"))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 0) Dataset choice (defaults to all_cells until the radio exists)
    dataset_choice <- reactive({
      if (shiny::isTruthy(input$dataset_choice)) input$dataset_choice else "all_cells"
    })
    
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
    
    # 2) Filter gate + message (F3 for all_cells, F4 for downsampled)
    show_plots <- reactive({
      req(current_tf())
      flag <- if (dataset_choice() == "all_cells") "F3" else "F4"
      is_TF_not_filtered_out(TF_filter_information, current_tf(), flag)
    })
    
    filter_message <- reactive({
      m <- TF_filter_information |>
        dplyr::filter(TF == current_tf()) |>
        dplyr::pull(filter_message)
      if (length(m) == 0) "No information available for this TF." else m[1]
    })
    
    # 3) Dynamic UI (dataset toggle always shown)
    output$content <- renderUI({
      req(current_tf())
      shiny::tagList(
        # Dataset toggle
        div(
          class = "p-3",
          fluidRow(
            column(
              width = 6,
              section_header("Dataset choice"),
              shiny::tags$p(HTML("All plots on this tab are affected by this selection.<br>This includes the GO term enrichment analysis.")),
              radioButtons(
                ns("dataset_choice"),
                label = "",
                choiceNames  = c("All cells", "Downsampled"),
                choiceValues = c("all_cells", "downsampled"),
                selected     = dataset_choice()
              )
            )
          )
        ),
        # Body
        if (!show_plots()) {
          div(class = "p-4", h3("This TF is filtered out"), p(filter_message()))
        } else {
          tagList(
            fluidRow(
              id = ns("de_tables"),
              style = "height: 120vh;",
              column(
                width = 12,
                section_header("Differential expression analysis"),
                column(
                  width = 6,
                  species_header("Human"),
                  styled_plot(ns("vulcano_DE_human"), height = "70vh"),
                  species_header("DE genes - Human", color_preset = "human"),
                  styled_table(ns("DE_list_human"), height = "70vh", width = "90%")
                ),
                column(
                  width = 6,
                  species_header("Cynomolgus"),
                  styled_plot(ns("vulcano_DE_cyno"), height = "70vh"),
                  species_header("DE genes - Cynomolgus", color_preset = "cynomolgus"),
                  styled_table(ns("DE_list_cyno"), height = "70vh", width = "90%")
                )
              )
            ),
            fluidRow(
              style = "height: 90vh;",
              column(
                width = 12,
                uiOutput(ns("scatterplot_section_header")),
                column(
                  width = 6,
                  radioButtons(
                    ns("scatterplot_DR_DT_selection"),
                    label = "",
                    choiceNames  = c(
                      "Differentially Regulated (DR)",
                      "Differentially Targeted (DT)",
                      "Differentially Regulated and Targeted (DRT)"
                    ),
                    choiceValues = c("DR", "DT", "DRT"),
                    selected = "DR"
                  )
                ),
                column(
                  width = 12,
                  styled_plot(ns("scatterplot_human_vs_cyno_lfc"), height = "70vh"),
                  uiOutput(ns("scatterplot_table_header")),
                  styled_table(ns("DR_DT_DRG_list"), height = "70vh", width = "90%")
                )
              )
            ),
            fluidRow(
              style = "height: 90vh;",
              column(
                width = 12,
                section_header("GO term analysis"),
                column(
                  width = 2,
                  radioButtons(
                    ns("topgo_filter_DR_DT"),
                    label = "Differential logic filter",
                    choiceNames  = c("DR", "DT", "DRT", "no filter"),
                    choiceValues = c("DR", "DT", "DRT", "whatever"),
                    selected     = "whatever"
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("topgo_filter_DE_human"),
                    label = "Human DE category",
                    choiceNames  = c("upregulated", "downregulated", "not DE", "no filter"),
                    choiceValues = c("DE_human_up", "DE_human_down", "not_DE_human", "whatever"),
                    selected     = "whatever"
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("topgo_filter_DE_cyno"),
                    label = "Cynomolgus DE category",
                    choiceNames  = c("upregulated", "downregulated", "not DE", "no filter"),
                    choiceValues = c("DE_cyno_up", "DE_cyno_down", "not_DE_cyno", "whatever"),
                    selected     = "whatever"
                  )
                ),
                column(width = 12, styled_table(ns("topgo_gene_selection"), height = "70vh")),
                column(
                  width = 12,
                  div(
                    style = "padding: 10px 15px; margin-bottom: 10px;",
                    actionButton(ns("starttopgo"), "Run TopGO analysis with selected gene set")
                  )
                ),
                column(
                  width = 12,
                  styled_plot(ns("topgo_de_plot"), height = "70vh", width = "100%", spinner = TRUE)
                ),
                column(
                  width = 12,
                  species_header("Most significant GO terms"),
                  styled_table(ns("topgo_terms"), height = "70vh", width = "100%")
                )
              )
            )
          )
        }
      )
    })
    
    # 4) Data reactives loaded per TF (when showing plots)
    dream_ds <- reactiveVal(NULL)
    dream_not_ds <- reactiveVal(NULL)
    dream_concise_ds <- reactiveVal(NULL)
    dream_concise_not_ds <- reactiveVal(NULL)
    
    observeEvent(list(current_tf(), dataset_choice()), {
      req(is_active(), show_plots())
      tf <- current_tf()
      
      if (dataset_choice() != "all_cells") {
        dream_concise_not_ds(NULL)
        
        ds <- readRDS(here("data","dream_output","500_2000", tf, "TF68_downsampl_dream","permutation_output_0.rds"))
        dream_ds(ds)
        dream_concise_ds(preprocess_dream_output(ds, p_value_threshold = p_value_threshold))
      } else {
        dream_concise_ds(NULL)
        
        nds <- readRDS(here("data","dream_output","all_TFs_not_downsampled", tf, "TF81_run","permutation_output_0.rds"))
        dream_not_ds(nds)
        dream_concise_not_ds(preprocess_dream_output(nds, p_value_threshold = p_value_threshold))
      }
    }, ignoreInit = FALSE)
    
    concise_ds     <- reactive({ req(dream_concise_ds());     dream_concise_ds() })
    concise_not_ds <- reactive({ req(dream_concise_not_ds()); dream_concise_not_ds() })
    
    active_concise <- reactive({
      if (dataset_choice() == "all_cells") concise_not_ds() else concise_ds()
    })
    
    # 5) DE tables + volcano plots
    output$DE_list_human <- DT::renderDT({
      req(show_plots())
      df <- active_concise() %>%
        dplyr::select(gene, logFC_human, adj.P.Val_human, logFC_cynomolgus, adj.P.Val_cynomolgus) %>%
        dplyr::filter(adj.P.Val_human < p_value_threshold) %>%
        dplyr::mutate(
          logFC_cynomolgus  = round(logFC_cynomolgus, 3),
          logFC_human       = round(logFC_human, 3),
          adj.P.Val_cynomolgus = ifelse(adj.P.Val_cynomolgus < 0.001, "< 0.001", round(adj.P.Val_cynomolgus, 3)),
          adj.P.Val_human      = ifelse(adj.P.Val_human      < 0.001, "< 0.001", round(adj.P.Val_human, 3))
        )
      
      DT::datatable(
        df,
        options = list(
          paging = TRUE,
          searching = TRUE,
          info = TRUE,
          lengthChange = FALSE,
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE,
        selection = "none"
      )
    })
    
    output$DE_list_cyno <- DT::renderDT({
      req(show_plots())
      df <- active_concise() %>%
        dplyr::select(gene, logFC_cynomolgus, adj.P.Val_cynomolgus, logFC_human, adj.P.Val_human) %>%
        dplyr::filter(adj.P.Val_cynomolgus < p_value_threshold) %>%
        dplyr::mutate(
          logFC_cynomolgus  = round(logFC_cynomolgus, 3),
          logFC_human       = round(logFC_human, 3),
          adj.P.Val_cynomolgus = ifelse(adj.P.Val_cynomolgus < 0.001, "< 0.001", round(adj.P.Val_cynomolgus, 3)),
          adj.P.Val_human      = ifelse(adj.P.Val_human      < 0.001, "< 0.001", round(adj.P.Val_human, 3))
        )
      
      DT::datatable(
        df,
        options = list(
          paging = TRUE,
          searching = TRUE,
          info = TRUE,
          lengthChange = FALSE,
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE,
        selection = "none"
      )
    })
    
    volcano_inputs <- reactive({
      req(show_plots())
      cds <- active_concise()
      
      dream_output_human <- cds %>%
        dplyr::select(gene, logFC_human, adj.P.Val_human, DE_human) %>%
        dplyr::mutate(significant = dplyr::if_else(DE_human, "yes", "no"))
      
      dream_output_cyno <- cds %>%
        dplyr::select(gene, logFC_cynomolgus, adj.P.Val_cynomolgus, DE_cyno) %>%
        dplyr::mutate(significant = dplyr::if_else(DE_cyno, "yes", "no"))
      
      top_genes_human <- get_top_genes(
        dream_output_human, "logFC_human", "adj.P.Val_human",
        p_value_threshold = p_value_threshold
      )
      top_genes_cyno <- get_top_genes(
        dream_output_cyno, "logFC_cynomolgus", "adj.P.Val_cynomolgus",
        p_value_threshold = p_value_threshold
      )
      
      y_lim_high_h <- -log10(min(dream_output_human$adj.P.Val_human, na.rm = TRUE))
      y_lim_high_c <- -log10(min(dream_output_cyno$adj.P.Val_cynomolgus, na.rm = TRUE))
      y_lim_high <- min(max(y_lim_high_h, y_lim_high_c), 100)
      
      x_lim_high_h <- max(abs(dream_output_human$logFC_human), na.rm = TRUE)
      x_lim_high_c <- max(abs(dream_output_cyno$logFC_cynomolgus), na.rm = TRUE)
      x_lim_high <- max(x_lim_high_h, x_lim_high_c)
      
      list(
        human_df   = dream_output_human,
        cyno_df    = dream_output_cyno,
        top_human  = top_genes_human,
        top_cyno   = top_genes_cyno,
        y_lim_high = y_lim_high,
        x_lim_high = x_lim_high
      )
    })
    
    output$vulcano_DE_human <- renderPlot({
      req(show_plots())
      vi <- volcano_inputs()
      plot_volcano(
        df = vi$human_df,
        logFC_col = "logFC_human",
        pval_col  = "adj.P.Val_human",
        de_col    = "significant",
        color     = colors_app$human,
        title     = "",
        top_genes = vi$top_human,
        y_lim_high = vi$y_lim_high,
        x_lim_high = vi$x_lim_high
      )
    }, res = 120)
    
    output$vulcano_DE_cyno <- renderPlot({
      req(show_plots())
      vi <- volcano_inputs()
      plot_volcano(
        df = vi$cyno_df,
        logFC_col = "logFC_cynomolgus",
        pval_col  = "adj.P.Val_cynomolgus",
        de_col    = "significant",
        color     = colors_app$cynomolgus,
        title     = "",
        top_genes = vi$top_cyno,
        y_lim_high = vi$y_lim_high,
        x_lim_high = vi$x_lim_high
      )
    }, res = 120)
    
    # 6) DR/DT scatter + table
    output$scatterplot_section_header <- renderUI({
      req(input$scatterplot_DR_DT_selection)
      label <- switch(
        input$scatterplot_DR_DT_selection,
        DR  = "Differential regulation between species (DR)",
        DT  = "Differential targeting between species (DT)",
        DRT = "Differential regulation and targeting between species (DRT)"
      )
      section_header(label)
    })
    
    output$scatterplot_table_header <- renderUI({
      req(input$scatterplot_DR_DT_selection)
      label <- switch(
        input$scatterplot_DR_DT_selection,
        DR  = "DR genes",
        DT  = "DT genes",
        DRT = "DRT genes"
      )
      species_header(label)
    })
    
    output$scatterplot_human_vs_cyno_lfc <- renderPlot({
      req(show_plots(), input$scatterplot_DR_DT_selection)
      cds <- active_concise()
      
      scatter_df <- cds %>%
        dplyr::select(gene, logFC_human, logFC_cynomolgus, DR, DT, DRT, adj.P.Val_interaction) %>%
        dplyr::mutate(highlighted = dplyr::case_when(
          input$scatterplot_DR_DT_selection == "DR"  ~ as.logical(DR),
          input$scatterplot_DR_DT_selection == "DT"  ~ as.logical(DT),
          input$scatterplot_DR_DT_selection == "DRT" ~ as.logical(DRT),
          TRUE ~ FALSE
        ))
      
      top_genes_D <- get_top_genes(
        scatter_df,
        logFC_col = "adj.P.Val_interaction",
        pval_col  = "adj.P.Val_interaction",
        n = 10, p_value_threshold = p_value_threshold
      )
      
      plot_scatter_lfc(
        df = scatter_df,
        highlight_col    = input$scatterplot_DR_DT_selection,
        highlight_label  = input$scatterplot_DR_DT_selection,
        color_highlight  = colors_app$DR,
        top_genes        = top_genes_D,
        title            = ""
      )
    }, res = 120)
    
    output$DR_DT_DRG_list <- DT::renderDT({
      req(show_plots(), input$scatterplot_DR_DT_selection)
      cds <- active_concise()
      cat <- input$scatterplot_DR_DT_selection
      
      df <- cds %>%
        dplyr::filter(.data[[cat]] %in% TRUE) %>%
        dplyr::transmute(
          gene,
          logFC_interaction,
          adj.P.Val_interaction,
          logFC_human,
          adj.P.Val_human,
          logFC_cynomolgus,
          adj.P.Val_cynomolgus
        ) %>%
        dplyr::arrange(adj.P.Val_interaction) %>%
        dplyr::mutate(
          dplyr::across(c(logFC_interaction, logFC_human, logFC_cynomolgus), ~ round(., 3)),
          adj.P.Val_interaction = dplyr::case_when(
            is.na(adj.P.Val_interaction) ~ NA_character_,
            adj.P.Val_interaction < 0.001 ~ "< 0.001",
            TRUE ~ as.character(round(adj.P.Val_interaction, 3))
          ),
          adj.P.Val_human = dplyr::case_when(
            is.na(adj.P.Val_human) ~ NA_character_,
            adj.P.Val_human < 0.001 ~ "< 0.001",
            TRUE ~ as.character(round(adj.P.Val_human, 3))
          ),
          adj.P.Val_cynomolgus = dplyr::case_when(
            is.na(adj.P.Val_cynomolgus) ~ NA_character_,
            adj.P.Val_cynomolgus < 0.001 ~ "< 0.001",
            TRUE ~ as.character(round(adj.P.Val_cynomolgus, 3))
          )
        )
      
      DT::datatable(
        df,
        options = list(
          paging = TRUE,
          searching = TRUE,
          info = TRUE,
          lengthChange = FALSE,
          pageLength = 10
        ),
        rownames = FALSE,
        selection = "none"
      )
    })
    
    # 7) topGO
    downstream_genes_active <- reactive({
      req(show_plots())
      active_concise() %>%
        dplyr::mutate(
          DE_human_up   = DE_human & logFC_human > 0,
          DE_human_down = DE_human & logFC_human < 0,
          DE_cyno_up    = DE_cyno  & logFC_cynomolgus > 0,
          DE_cyno_down  = DE_cyno  & logFC_cynomolgus < 0
        ) %>%
        dplyr::select(
          gene, DR, DT, DRT,
          DE_human, DE_human_up, DE_human_down,
          DE_cyno,  DE_cyno_up,  DE_cyno_down
        )
    })
    
    filtered_genes <- reactive({
      req(show_plots(), input$topgo_filter_DR_DT, input$topgo_filter_DE_human, input$topgo_filter_DE_cyno)
      selected_genes <- downstream_genes_active()
      
      if (input$topgo_filter_DR_DT != "whatever")
        selected_genes <- dplyr::filter(selected_genes, !!rlang::sym(input$topgo_filter_DR_DT))
      
      if (input$topgo_filter_DE_human != "whatever") {
        selected_genes <- if (input$topgo_filter_DE_human == "not_DE_human")
          dplyr::filter(selected_genes, !DE_human)
        else
          dplyr::filter(selected_genes, !!rlang::sym(input$topgo_filter_DE_human))
      }
      
      if (input$topgo_filter_DE_cyno != "whatever") {
        selected_genes <- if (input$topgo_filter_DE_cyno == "not_DE_cyno")
          dplyr::filter(selected_genes, !DE_cyno)
        else
          dplyr::filter(selected_genes, !!rlang::sym(input$topgo_filter_DE_cyno))
      }
      
      selected_genes
    })
    
    output$topgo_gene_selection <- DT::renderDT({
      req(show_plots())
      DT::datatable(
        filtered_genes(),
        options = list(
          paging = TRUE,
          searching = TRUE,
          info = TRUE,
          lengthChange = FALSE,
          pageLength = 10
        ),
        rownames = FALSE,
        selection = "none"
      )
    })
    
    # Keep topGO plot blank until button is pressed AND get back to placeholder if dataset, TF or gene list changes
    render_topgo_placeholder(output, show_plots)
    
    observeEvent(
      list(
        current_tf(),
        dataset_choice(),
        input$topgo_filter_DR_DT,
        input$topgo_filter_DE_human,
        input$topgo_filter_DE_cyno
      ),
      {
        req(is_active())
        if (!show_plots()) return()
        render_topgo_placeholder(output, show_plots)
      },
      ignoreInit = TRUE
    )
    
    # Run topGO on demand
    observeEvent(input$starttopgo, {
      req(is_active(), show_plots())
      sel_df <- filtered_genes()
      selected_genes <- sel_df$gene
      gene_list_background <- downstream_genes_active()$gene
      
      if (length(selected_genes) == 0) {
        showNotification("No genes selected for GO analysis.\nUse a different filter.", type = "error")
        return()
      }
      if (length(selected_genes) == length(gene_list_background)) {
        showNotification("All genes selected for GO analysis.\nUse a filter.", type = "error")
        return()
      }
      if (length(gene_list_background) == 0) {
        showNotification("Gene background list is empty — cannot run GO analysis.", type = "error")
        return()
      }
      
      output$topgo_de_plot <- renderPlot({
        req(show_plots())
        plot.new(); par(mar = c(0, 0, 0, 0))
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5, "Running topGo...", cex = 1)
      }, res = 250)
      
      withProgress(message = "Running topGO…", value = 0, {
        incProgress(0.4, detail = "Enrichment")
        go_results <- go_analysis_smart(
          selected_genes, gene_list_background,
          node_number = 10, pvalueCutoff = 0.05, qvalueCutoff = 0.2,
          min_background_genes = 10, min_interest_hits = 2
        )
        incProgress(0.6, detail = "Rendering")
        
        if (!is.null(go_results)) {
          output$topgo_de_plot <- renderPlot({
            topgo_elim_plot(go_results, title = "")
          }, res = 250)
        } else {
          output$topgo_de_plot <- renderPlot({
            req(show_plots())
            par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
            plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
            rect(0, 0, 1, 1, col = "white", border = NA)
            rect(0, 0, 1, 1, border = "#aaaaaa", lwd = 2)
            text(0.5, 0.5, "No significant GO terms found.", cex = 0.5)
          }, res = 250)
        }
        
        output$topgo_terms <- DT::renderDT({
          DT::datatable(
            go_results,
            options = list(
              paging = TRUE,
              searching = TRUE,
              info = TRUE,
              lengthChange = FALSE,
              pageLength = 10
            ),
            rownames = FALSE,
            selection = "none"
          )
        })
      })
    })
  })
}
