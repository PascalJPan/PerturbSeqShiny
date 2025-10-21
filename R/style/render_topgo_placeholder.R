render_topgo_placeholder <- function(output, show_plots) {
  
  # placeholder
  output$topgo_de_plot <- renderPlot({
    req(show_plots())
    par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
    rect(0, 0, 1, 1, col = "white", border = NA)
    rect(0, 0, 1, 1, border = "#aaaaaa", lty = "dashed", lwd = 2)
    text(0.5, 0.5, "Click the button to run GO analysis", cex = 0.5)
  }, res = 250)
  
  # clear the GO terms table until a run happens
  output$topgo_terms <- DT::renderDT({
    DT::datatable(
      data.frame(),
      options = list(paging = FALSE, searching = FALSE, info = FALSE),
      rownames = FALSE,
      selection = "none"
    )
  })
}