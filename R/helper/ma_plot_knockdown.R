ma_plot_knockdown <- function(
    knockdown,
    perturbed_tf, 
    grna_col = "gRNA",
    x_col = "mean_expr_control",
    y_col = "logFC",
    tf_col = "perturbed_TF",
    point_size = 0.8,
    point_alpha = 0.4,
    seg_linewidth = 0.05,
    base_size = 13,
    highlight_color = "#FF4242",
    other_color = "grey30"
) {
  stopifnot(all(c(x_col, y_col, tf_col) %in% colnames(knockdown)))
  
  has_grna <- grna_col %in% colnames(knockdown)
  
  df <- knockdown %>%
    dplyr::mutate(
      .x  = .data[[x_col]],
      .y  = .data[[y_col]],
      .tf = .data[[tf_col]],
      .gr = if (has_grna) .data[[grna_col]] else NA_character_,
      .highlight = (.tf == perturbed_tf),
      .pt_text = paste0(
        "TF: ", .tf,
        if (has_grna) paste0("\n", grna_col, ": ", .gr) else "",
        "\nmean expr (control): ", round(.x, 2),
        "\nlog2FC: ", round(.y, 2),
        "\nselected TF: ", .highlight
      )
    )
  
  # vertical range per x (like your original, no species)
  seg_df <- df %>%
    group_by(.tf, .x) %>%
    summarise(min_y = min(.y, na.rm = TRUE),
              max_y = max(.y, na.rm = TRUE),
              .groups = "drop")
  
  
  suppressWarnings({
  
  
  p<- ggplot(df) +
    geom_segment(
      data = seg_df,
      aes(x = .x, xend = .x, y = min_y, yend = max_y),
      linewidth = seg_linewidth
    ) +
    # non-highlighted points
    geom_point(
      data = df %>% filter(!.highlight),
      aes(x = .x, y = .y, color = .highlight, text = .pt_text),
      size = point_size,
      alpha = point_alpha
    ) +
    # highlighted points (full opacity, bigger)
    geom_point(
      data = df %>% filter(.highlight),
      aes(x = .x, y = .y, color = .highlight, text = .pt_text),
      size = point_size * 2.2,   # adjust multiplier to taste
      alpha = 1
    ) +
    scale_color_manual(
      values = c("TRUE" = highlight_color, "FALSE" = other_color),
      guide = "none"
    ) +
    theme_bw(base_size = base_size) +
    xlab("mean expression\n(control)") +
    ylab("log\u2082FC (perturbed vs control)") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.2) +
    theme(
      panel.spacing = unit(0.2, "lines"),
      axis.title = element_text(margin = margin(r = -40))
    )
  
  })
  
  ggplotly(p, tooltip = "text") %>%
    layout(showlegend = FALSE) # yaxis = list(title = "log<sub>2</sub>FC (perturbed vs control)")
}
