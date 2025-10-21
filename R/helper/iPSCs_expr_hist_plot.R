iPSCs_expr_hist_plot <- function(expr, selected_TF, species = c("human", "cynomolgus"), color = "#D62728") {
  species <- match.arg(species)
  # histogram plot for expression in iPSCs
  
  # Choose species-specific expression column
  x_col <- if (species == "human") "percent_expr_human" else "percent_expr_cyno"
  
  # Expression percentage for the selected TF
  gx <- unique(na.omit(expr[[x_col]][expr$gene_name == selected_TF]))
  gx <- if (length(gx)) gx[1] else NA_real_
  
  # Base histogram
  p <- ggplot(expr, aes(x = .data[[x_col]])) +
    geom_histogram(col = "white", fill = "grey50", binwidth = 2.5, center = 1.25) +
    theme_bw(base_size = 20) +
    labs(x = "Percent of cells", y = "Number of TFs expressed")
  
  # Get max bin count for consistent placement
  gb <- ggplot_build(p)
  ymax <- tryCatch(max(gb$data[[1]]$count, na.rm = TRUE), error = function(e) NA_real_)
  
  # Highlight selected TF
  if (is.finite(gx) && is.finite(ymax)) {
    p <- p +
      annotate("segment", x = gx, xend = gx, y = 0, yend = ymax * 1.02,
               linetype = "dashed", linewidth = 0.4, colour = color) +
      annotate("label", x = gx, y = ymax * 1.05, label = selected_TF,
               size = 6, colour = color, fill = NA, label.size = NA)
  }
  
  p
}
