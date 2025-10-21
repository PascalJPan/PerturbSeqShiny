stemness_violin_plot <- function(
    seu,
    current_TF,
    species,
    text_size    = 12,
    violin_alpha = 0.7,
    violin_width = 0.6,
    highlight_color = "#FF4242",
    control_color   = "lightgrey"
) {
  
  df <- seu@meta.data %>%
    dplyr::filter(.data$species == !!species,
                  .data$perturbed_TF %in% c(current_TF, "NT_control")) %>%
    dplyr::mutate(
      group = ifelse(.data$perturbed_TF == current_TF, current_TF, "NT_control"),
      group = factor(group, levels = c(current_TF, "NT_control"))
    )
  
  if (nrow(df) == 0 || !("stemness_score" %in% names(df))) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("No data for species '", species,
                                "' and TF '", current_TF, "' (or missing stemness_score)."),
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void()
    )
  }
  
  df <- df %>% dplyr::filter(!is.na(.data$stemness_score))
  
  ggplot(df, aes(x = group, y = stemness_score, fill = group)) +
    geom_violin(width = violin_width, alpha = violin_alpha, trim = FALSE,
                draw_quantiles = c(0.5)) +
    scale_fill_manual(
      values = setNames(c(highlight_color, control_color),
                        c(current_TF, "NT_control")),
      guide = "none"
    ) +
    labs(x = NULL, y = "Stemness Score") +
    theme_minimal(base_size = text_size) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}