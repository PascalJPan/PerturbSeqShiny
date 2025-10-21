con_barplot <- function(cs_df, selected_TF, con_score_name, color = "#D62728") {
  con_score_sym <- sym(con_score_name)
  
  # Mark selected TF
  cs_df <- cs_df %>%
    mutate(
      highlight = if_else(target_TF == selected_TF, "selected", "other"),
      target_TF = factor(target_TF, levels = target_TF)
    ) %>%
    arrange(desc(!!con_score_sym)) %>%
    mutate(target_TF = factor(target_TF, levels = target_TF))
  
  ggplot(cs_df, aes(
    x = target_TF,
    y = !!con_score_sym,
    fill = highlight
  )) +
    geom_col(width = 0.6, alpha = 0.9, na.rm = TRUE) +
    scale_fill_manual(values = c("selected" = color, "other" = "grey70")) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    coord_flip() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.title.y = element_text(size = 13),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
    )
}
