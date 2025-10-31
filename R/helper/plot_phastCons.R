plot_phastCons <- function(gn, gn_list, phastCons, color = "#D62728") {
  # Plot histogram of mean conservation scores and highlight a selected gene
  
  p <- phastCons %>%
    dplyr::filter(gene_name %in% gn_list) %>%
    ggplot(aes(x = meanCons)) +
    geom_histogram(
      col = "white", fill = "grey50",
      binwidth = 0.025, center = 0.0125
    ) +
    theme_bw(base_size = 13) +
    labs(x = "mean phastCons across coding sequence", y = "Number of TFs")
  
  # Determine the maximum bin count for consistent line/label placement
  gb <- ggplot_build(p)
  ymax <- tryCatch(max(gb$data[[1]]$count, na.rm = TRUE), error = function(e) NA_real_)
  
  # X-position for the selected gene
  gx <- phastCons$meanCons[phastCons$gene_name == gn]
  gx <- gx[is.finite(gx)][1]
  
  
  if (is.finite(gx) && is.finite(ymax)) {
    p <- p +
      annotate("segment",
               x = gx, xend = gx, y = 0, yend = ymax * 1.02,
               linetype = "dashed", linewidth = 0.4, colour = color) +
      annotate("label",
               x = gx, y = ymax * 1.05, label = gn,
               size = 4, colour = color, fill = NA, label.size = NA)
  }
  
  p
}
