topgo_elim_plot <- function (GOdata, title, nodes = 10) {
  library(ggplot2)
  library(stringr)  
  
  GOdata$Term <- factor(GOdata$Term, levels = GOdata$Term[order(GOdata$GeneRatio, decreasing = FALSE)])
  
  
  plot <- ggplot(GOdata, aes(x = GeneRatio, y = Term)) +
    geom_point(aes(size = Significant, color = Fisher.elim)) +
    scale_color_gradient(low = "red", high = "blue", trans = "log10", name = "p.adjust") +
    scale_size_continuous(name = "Count") +
    labs(
      title = title,
      x = "GeneRatio",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      axis.text.y = element_text(size = 6, hjust = 1, , color = "black"),
      axis.text.x = element_text(size = 6, hjust = 1, , color = "black"),
      axis.title.x = element_text(size = 8, color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.5, "lines")
    )
  
  return(plot)
}