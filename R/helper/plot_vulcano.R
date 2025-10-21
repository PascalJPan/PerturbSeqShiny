plot_volcano <- function(df, logFC_col, pval_col, de_col, color, title, top_genes, y_lim_high = 20, x_lim_high = 20) {
  # Vulcano plots for downstream genes after perturbation
  
  # Create capped y-values
  df <- df %>%
    mutate(y_val = pmin(-log10(!!sym(pval_col)), y_lim_high),
           is_capped = -log10(!!sym(pval_col)) > y_lim_high)
  
  top_genes <- top_genes %>%
    mutate(y_val = pmin(-log10(!!sym(pval_col)), y_lim_high),
           is_capped = -log10(!!sym(pval_col)) > y_lim_high)
  
  ggplot(df, aes(x = !!sym(logFC_col), y = y_val, fill = !!sym(de_col))) +
    # Regular points
    geom_point(data = df %>% filter(!is_capped),
               shape = 21, color = "black", size = 1.5, stroke = 0.1, alpha = 0.6) +
    # Capped points as triangles
    geom_point(data = df %>% filter(is_capped),
               shape = 24, fill = color, color = "black", size = 2, stroke = 0.2, alpha = 0.8) +
    # Top genes
    geom_point(data = top_genes,
               aes(x = !!sym(logFC_col), y = y_val),
               shape = 21, fill = color, color = "black", size = 1.5, stroke = 0.1) +
    # Labels
    ggrepel::geom_label_repel(
      data = top_genes,
      aes(x = !!sym(logFC_col), y = y_val, label = gene),
      fill = "white",
      color = color,
      size = 3,
      box.padding = 0.3,
      segment.color = color,
      max.overlaps = Inf
    ) +
    scale_fill_manual(values = c("no" = "grey70", "yes" = color)) +
    labs(x = "logFC", y = "-log10(adj.P.Val)", title = title) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
          legend.position = "bottom") +
    coord_cartesian(ylim = c(0, y_lim_high * 1.1), xlim = c(-x_lim_high * 1.05, x_lim_high * 1.05))
}
