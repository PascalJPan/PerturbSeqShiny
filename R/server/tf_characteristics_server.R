tf_characteristics_server <- function(id, selected_tf, is_active, colors_app) {
  
  # One time loads
  motifs         <- readRDS(here("data", "TF_characteristics", "TFBS_motiflist_4logo.RDS"))
  motif_IC_sum   <- readRDS(here("data", "TF_characteristics", "motif_IC.RDS"))
  phastCons      <- readRDS(here("data", "TF_characteristics", "phastCons.rds"))
  TF_families    <- readRDS(here("data", "TF_characteristics", "TF_families.rds"))
  tf2go          <- readRDS(here("data", "TF_characteristics", "tf2go.rds"))
  paralogs       <- readRDS(here("data", "TF_characteristics", "paralogs.rds"))
  sel_rationale  <- readRDS(here("data", "TF_characteristics", "TF_selection_rationale.rds"))
  sel_rationale_processed <- process_selection_rationale(sel_rationale)
  
  
  # Server
  moduleServer(id, function(input, output, session) {
    last_tf <- reactiveVal(NULL)
    
    observe({
      req(is_active())
      current <- selected_tf()
      if (!identical(current, last_tf())) {
        last_tf(current)
        
        output$selection_rationale <- renderText({
          rationale <- sel_rationale_processed$selection_rationale[sel_rationale_processed$TF == current]
          paste(rationale)
        })
        
        # TF family table
        output$TF_families <- DT::renderDT({
          DT::datatable(TF_families[[current]], 
                        options = list(dom = 't'),
                        rownames = FALSE,
                        selection = "none")
        }) 
        
        
        # GO term table
        output$GO_terms <- DT::renderDT({
          DT::datatable(tf2go[[current]], 
                        options = list(dom = 't'),
                        rownames = FALSE,
                        selection = "none")
        })
        
        # TFBS motif logo
        output$motifs <- renderPlot({
          universalmotif::view_motifs(motifs[[current]])
        }, res = 120)
        
        # Information content
        output$IC <- renderText({
          get_IC(current, motif_IC_sum)
        })
        
        # PhastCons histogram
        output$phastCons <- renderPlot({
          plot_phastCons(
            current,
            gn_list   = sort(phastCons$gene_name),
            phastCons = phastCons,
            color = colors_app$TF
          )
        }, res = 120)

      }
    })
  })
}
