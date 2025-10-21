tf_characteristics_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(column(
      width = 12,
      section_header("TF selection rationale"),
      shiny::tags$p(
        "This TF was included in this experiment because of: ",
        textOutput(ns("selection_rationale"), inline = TRUE)
      )
    )),
    fluidRow(
      width = 12,
      column(
        width = 6,
        section_header("TF family"),
        styled_table(ns("TF_families")),
        div(class = "species-spacer")),
      column(
        width = 6,
        section_header("Functional annotation"),
        styled_table(ns("GO_terms")),
        div(class = "species-spacer")
      )), 
    fluidRow(column(
      width = 12,
      section_header("TF binding sites motifs"),
      styled_plot(ns("motifs"), height = "50vh"),
      verbatimTextOutput(ns("IC"))
    )),
    fluidRow(column(
      width = 12,
      section_header("PhastCons100way of TF in comparison to other TFs"),
      styled_plot(ns("phastCons"), height = "50vh")
    ))
  )
}