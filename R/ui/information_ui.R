 information_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h2("Information"),
        h3("About the Data"),
        p("This app presents results from one of the first cross-species single-cell CRISPRi Perturb-seq screens,", br(),
           "comparing regulatory responses of 94 transcription factors (TFs)", br(), "
           in human (Homo sapiens) and cynomolgus macaque (Macaca fascicularis) induced pluripotent stem cells (iPSCs). ", br(), "
           The dataset contains 277,000 high-quality single-cell transcriptomes.", br(), "
           Perturbations were performed with KRAB-dCas9 and species-specific gRNAs targeting TF transcription start sites."),
        
        h3("Layout Overview"),
        shiny::tags$ol(
          shiny::tags$li("Information – Overview of the project, app structure, and key resources"),
          shiny::tags$li("TF Characteristics – Selection criteria, TF family, biological function, motifs, and PhastCons conservation scores"),
          shiny::tags$li("gRNA Characteristics – Sequence features, genomic position, and targeted region"),
          shiny::tags$li("Cell Numbers and Expression – Cell counts per gRNA and baseline expression levels"),
          shiny::tags$li("Knockdown Efficiency – Knockdown percentage plots and identification of the most effective gRNAs"),
          shiny::tags$li("Cell Type and Stemness – Barplots and UMAPs visualizing cell identity and stemness profiles"),
          shiny::tags$li("gRNA Enrichment – Enrichment of cell populations within and between species after knockdown"),
          shiny::tags$li("DE and DR Analysis – Volcano plots, logFC comparisons, and GO term enrichment results"),
          shiny::tags$li("Conservation – Model fits, TF divergence ranking, and grouping by selection rationale")
        ),
        
        
        h3("Links"),
        p(
          "📂 ", a("GitHub repository", href = "GITHUB_LINK", target = "_blank"),
          br(),
          "📊 ", a("Data download", href = "DATA_LINK", target = "_blank")
        ),
        h3("Contact"),
        p(
          "pascal.stuempfl@outlook.com (can be changed to another e-mail adress later)"
        ),
      )
    )
  )
}
