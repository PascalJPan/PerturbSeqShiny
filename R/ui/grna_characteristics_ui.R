grna_characteristics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        section_header("gRNA sequences and binding positions"),
        column(
          width = 6,
          species_header("Human"),
          styled_table(ns("human_grna_table"),  width = "90%"),
          div(class = "species-spacer")
        ),
        column(
          width = 6,
          species_header("Cynomolgus"),
          styled_table(ns("cyno_grna_table"),  width = "90%"),
          div(class = "species-spacer")
        ),
        
      section_header("Context of the genomic region"),
      fluidRow(
        column(2,
               radioButtons(
                 ns("gviz_focus"),
                 label = "Area",
                 choiceNames = c("Transcription start site only","Entire gene body"),
                 choiceValues = c("tss","gene_body"),
                 selected = "tss"
               )
        ),
        column(4,
               sliderInput(
                 ns("offset"),
                 label = "Padding for genomic region",
                 min = 0,
                 max = 10000,
                 value = 2000,
                 step = 500
               )
        ),
        column(12,
        div(
          style = "padding: 10px 15px; margin-bottom: 10px;",
          actionButton(ns("startgviz"), "Render Gviz Plots")
        ))
      ),
      column(
        width = 6,
        species_header("Human"),
        styled_plot(ns("gviz_plot_hg38"), height="1200px", width = "100%")
      ),
      column(
        width = 6,
        species_header("Cynomolgus"),
        styled_plot(ns("gviz_plot_macFas6"), height="1200px", width = "100%")
      )
      )
    )
  )
}