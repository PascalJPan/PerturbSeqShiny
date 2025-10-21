mini_ui <- function(id) {
  ns <- NS(id)
  shinycssloaders::withSpinner(
    uiOutput(ns("content")),
    type = 4, color = "#777"  
  )
}