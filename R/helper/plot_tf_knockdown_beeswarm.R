plot_tf_knockdown_beeswarm <- function(df, selected_TF) {

  dat <- df %>%
    mutate(
      TF_group = if_else(perturbed_TF == selected_TF, selected_TF, "other"),
      TF_group = factor(TF_group, levels = c(selected_TF, "other")),
      logFC_num = suppressWarnings(as.numeric(logFC)),
      tooltip = sprintf("TF: %s\ngRNA: %s\nlog2FC: %s",
                        perturbed_TF,
                        as.character(gRNA),
                        ifelse(is.na(logFC_num), "NA", sprintf("%.2f", logFC_num)))
    )
  
  p <- ggplot(dat, aes(x = TF_group, y = logFC_num, text = tooltip)) +
    geom_beeswarm(aes(fill = significant),
                  shape = 21, color = "black", stroke = 0.3, size = 2, alpha = 0.8) +
    stat_summary(fun = median, geom = "crossbar",
                 inherit.aes = FALSE, aes(x = TF_group, y = logFC_num),
                 width = 0.4, linewidth = 0.6, color = "black", alpha = 0.6) +
    scale_fill_manual(values = c("FALSE" = "grey30", "TRUE" = "chartreuse4"),
                      name = "Significant\nknockdown", labels = c("No", "Yes")) +
    labs(x = NULL, y = "log\u2082FC (perturbed vs control)") +
    theme_bw(base_size = 13) +
    theme(axis.text.x = element_text(size = 13, color = "black"),
          legend.key.size = unit(1, "lines"),
          legend.spacing = unit(0.2, "cm"))
  
  ggplotly(p, tooltip = "text") 
  # %>%
  #   layout(yaxis = list(title = "log<sub>2</sub>FC (perturbed vs control)"),
  #          showlegend = TRUE)
}
