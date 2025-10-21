plot_scatter_lfc <- function(df, highlight_col, highlight_label, color_highlight, top_genes, title) {
  # Plot the logFC in human and cynomolgus for all downstream genes after perturbation
  
  df <- df %>%
    mutate(
      highlighted = as.logical(.data[[highlight_col]])
    )
  
  
  ggplot(df, aes(x = logFC_human, y = logFC_cynomolgus)) +
    geom_point(aes(fill = highlighted), 
               shape = 21, color = "black", size = 1.5, stroke = 0.1, alpha = 0.6) +
    scale_fill_manual(
      values = c("TRUE" = color_highlight, "FALSE" = "grey70"),
      labels = c("TRUE" = "yes", "FALSE" = "no")
    ) +
    ggrepel::geom_label_repel(
      data = top_genes,
      aes(label = gene),
      fill = "white",           
      color = color_highlight,          
      size = 3,
      box.padding = 0.3,
      segment.color = color_highlight,
      max.overlaps = Inf
    ) +
    labs(
      x = "logFC Human",
      y = "logFC Cynomolgus",
      fill = highlight_label,
      title = title)+
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      legend.position = "bottom"
    )
}