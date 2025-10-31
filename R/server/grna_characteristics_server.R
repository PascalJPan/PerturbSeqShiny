grna_characteristics_server <- function(id, selected_tf, is_active, colors_app) {
  
  genomes <- c("hg38", "macFas6")
  
  gRNA_libraries <- readRDS(here("data", "grna_characteristics","gRNA_libraries.rds"))
  
  # Gene models
  all_gene_models <- setNames(lapply(genomes, function(genome) {
    gene_model_names <- gsub(".rds", "", list.files(here("data", "grna_characteristics", genome, "gene_models")))
    gene_models <- lapply(gene_model_names, function(name) {
      readRDS(here("data", "grna_characteristics", genome, "gene_models", paste0(name, ".rds")))
    })
    setNames(gene_models, gene_model_names)
  }), genomes)
  
  # TSS genomic ranges
  all_tss <- setNames(lapply(genomes, function(genome) {
    tss_names <- gsub(".rds", "", list.files(here("data", "grna_characteristics", genome, "tss")))
    tss_list <- lapply(tss_names, function(name) {
      readRDS(here("data", "grna_characteristics", genome, "tss", paste0(name, ".rds")))
    })
    setNames(tss_list, tss_names)
  }), genomes)
  
  # Designed gRNAs
  all_grnas <- setNames(lapply(genomes, function(genome) {
    readRDS(here("data", "grna_characteristics", genome, "gRNAs", "gRNAs.rds"))
  }), genomes)
  
  # ATAC tracks
  genome2species <- c(hg38 = "Human", macFas6 = "Cyno")
  all_atacBW <- setNames(lapply(genomes, function(genome) {
    atac_files <- list.files(here("data", "grna_characteristics", genome, "atac"), recursive = TRUE, full.names = TRUE)
    atac_names <- str_split(atac_files, "\\/|\\.", simplify = TRUE)[, 5]
    names(atac_files) <- atac_names
    spec_idx <- grep(genome2species[genome], atac_names)
    ordered_idx <- c(spec_idx, setdiff(seq_along(atac_names), spec_idx))
    atac_files[ordered_idx]
  }), genomes)
  
  # Nanopore reads
  all_nanoporeBAM <- setNames(lapply(genomes, function(genome) {
    here("data", "grna_characteristics", genome, "nanopore_reads", "nanopore.bam")
  }), genomes)
  
  # Module Server logic
  moduleServer(id, function(input, output, session) {
    last_tf <- reactiveVal(NULL)
    
    observe({
      req(is_active())
      current <- selected_tf()
      
      if (!identical(current, last_tf())) {
        last_tf(current)
        
        output$human_grna_table <- DT::renderDT({
          
          df <- gRNA_libraries %>% 
            filter(species == "human", perturbed_TF == selected_tf()) %>% 
            mutate(chr = str_remove(seqnames, "^chr")) %>% 
            select(gRNA,gRNA_sequence,position,strand,chr)
          
          DT::datatable(
            df,
            options = list(
              #dom = 't',       # only show the table
              paging = FALSE,  # disable pagination
              searching = FALSE, # disable search bar
              info = FALSE,    # remove "Showing x of y entries"
              lengthChange = FALSE,
              scrollX = TRUE
            ),
            rownames = FALSE,
            selection = "none"
          )
        }) 
        
        output$cyno_grna_table <- DT::renderDT({

          df <- gRNA_libraries %>% 
            filter(species == "cynomolgus", perturbed_TF == selected_tf()) %>% 
            mutate(chr = str_remove(seqnames, "^chr")) %>% 
            select(gRNA,gRNA_sequence,position,strand,chr)
          
          DT::datatable(
            df,
            options = list(
              #dom = 't',       # only show the table
              paging = FALSE,  # disable pagination
              searching = FALSE, # disable search bar
              info = FALSE,     # remove "Showing x of y entries"
              lengthChange = FALSE,
              
              scrollX = TRUE
            ),
            rownames = FALSE,
            selection = "none"
          )
        }) 
        
        # Wait for button pressed and display a blank plot until pressed
        output$gviz_plot_hg38 <- renderPlot({
          plot.new()  # creates a blank plot
          par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
          plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
          
          rect(0, 0, 1, 1, col = "white", border = NA)
          
          rect(0, 0, 1, 1, border = "#aaaaaa", lty = "dashed", lwd = 2)
          
          text(0.5, 0.5, "Click the button to render Gviz plots" , cex = 0.5)
        }, res=250)
        
        output$gviz_plot_macFas6 <- renderPlot({
          plot.new()  # creates a blank plot
          par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
          plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
          
          rect(0, 0, 1, 1, col = "white", border = NA)
          
          rect(0, 0, 1, 1, border = "#aaaaaa", lty = "dashed", lwd = 2)
          
          text(0.5, 0.5, "Click the button to render Gviz plots" , cex = 0.5)
        }, res=250)
        
        # Only trigger plot rendering when the button is clicked
        observeEvent(input$startgviz, {
          withProgress(message = "Render Gviz plots", value = 0.5, {
            
          # Generate plot for hg38
          output$gviz_plot_hg38 <- renderPlot({
            req(is_active())
            req(selected_tf())
            
            p3 <- make_gRNA_gviz(
              gn          = selected_tf(),
              gen         = "hg38",
              offset      = isolate(input$offset),
              gviz_focus  = isolate(input$gviz_focus),
              gene_models = all_gene_models[["hg38"]],
              tss         = all_tss[["hg38"]],
              grnas       = all_grnas[["hg38"]],
              atacBW      = all_atacBW[["hg38"]],
              nanoporeBAM = all_nanoporeBAM[["hg38"]],
              color = colors_app$human
            )
            
            p3
          }, res = 120)
          
          # Generate plot for macFas6
          output$gviz_plot_macFas6 <- renderPlot({
            req(is_active())
            req(selected_tf())
            
            p3 <- make_gRNA_gviz(
              gn          = selected_tf(),
              gen         = "macFas6",
              offset      = isolate(input$offset),
              gviz_focus  = isolate(input$gviz_focus),
              gene_models = all_gene_models[["macFas6"]],
              tss         = all_tss[["macFas6"]],
              grnas       = all_grnas[["macFas6"]],
              atacBW      = all_atacBW[["macFas6"]],
              nanoporeBAM = all_nanoporeBAM[["macFas6"]],
              color = colors_app$cynomolgus
            )
            
            
            p3
          }, res = 120)
          
          incProgress(0.7)
          
          })
          
        })
        
      }
      
    })
  })
}
