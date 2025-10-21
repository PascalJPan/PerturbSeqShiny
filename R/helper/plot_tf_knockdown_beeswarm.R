plot_tf_knockdown_beeswarm <- function(df, selected_TF) {
  library(ggbeeswarm)
  
  # Prepare data: group selected TF vs others, and create hover text
  dat <- df %>%
    mutate(
      TF_group = if_else(perturbed_TF == selected_TF, selected_TF, "other"),
      TF_group = factor(TF_group, levels = c(selected_TF, "other")),
      tooltip = sprintf(
        "gRNA: %s<br>TF: %s<br>log2FC: %.2f",
        gRNA,
        perturbed_TF,
        logFC
      )
    )
  
  p <- ggplot(dat, aes(x = TF_group, y = logFC, text = tooltip)) +
    # Beeswarm points colored by significance
    geom_beeswarm(
      aes(fill = significant),
      shape = 21,
      color = "black",
      stroke = 0.3,
      size = 2,
      alpha = 0.8
    ) +
    # Median crossbar per group
    stat_summary(
      fun = median,
      geom = "crossbar",
      inherit.aes = FALSE,
      aes(x = TF_group, y = logFC),
      width = 0.4,
      linewidth = 0.6,
      color = "black",
      alpha = 0.6
    ) +
    # Fill scale for significance
    scale_fill_manual(
      values = c("FALSE" = "grey30", "TRUE" = "chartreuse4"),
      name = "Significant\nknockdown",
      labels = c("No", "Yes")
    ) +
    labs(
      x = NULL,
      y = "log2 FC"
    ) +
    theme_bw(base_size = 13) +
    theme(
      axis.text.x = element_text(size = 13, color = "black"),
      legend.key.size = unit(1, "lines"),
      legend.spacing = unit(0.2, "cm")
    )
  
  ggplotly(p)
}
