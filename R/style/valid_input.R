valid_input <- function() {
  !is.null(values$lastAction) &&
    values$lastAction != "data" &&
    input$gene != ""
}