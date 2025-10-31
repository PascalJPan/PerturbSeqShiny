perm_beeswarm_plot <- function(cs_metric_data, target_TF, color = "#D62728") {
  
  df_plot <- cs_metric_data %>%
    dplyr::filter(target_TF == !!target_TF)
  
  mean_dr_div <- df_plot %>%
    dplyr::filter(permuted) %>%
    dplyr::summarize(mean_DR = mean(n_DR, na.rm = TRUE))
  
  max_y <- max(df_plot$n_DR, na.rm = TRUE) * 1.1
  
  ggplot() +
    geom_violin(
      data = dplyr::filter(df_plot, permuted == TRUE),
      aes(x = "", y = n_DR, text = "Permuted (distribution)"),
      fill = "#A6CEE3", color = "gray40", alpha = 0.6, width = 0.6, trim = FALSE
    ) +
    ggbeeswarm::geom_beeswarm(
      data = dplyr::filter(df_plot, permuted == TRUE),
      aes(x = "", y = n_DR, text = "Permuted"),
      alpha = 0.5, size = 1.5, color = "gray40", cex = 2
    ) +
    geom_point(
      data = dplyr::filter(df_plot, permuted == FALSE),
      aes(x = "", y = n_DR, text = "Observed"),   # <-- add x = ""
      color = color, size = 3
    ) +
    geom_hline(
      data = dplyr::filter(df_plot, permuted == FALSE),
      aes(yintercept = n_DR),
      color = color, linetype = "dashed", linewidth = 0.5
    ) +
    geom_hline(
      data = mean_dr_div,
      aes(yintercept = mean_DR),
      color = "lightgray", linetype = "dashed", linewidth = 0.4
    ) +
    labs(
      subtitle = "Gray = permuted; Red = observed; Light gray = permuted mean",
      x = NULL, y = "DR number"
    ) +
    coord_cartesian(ylim = c(0, max_y)) +
    theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}
