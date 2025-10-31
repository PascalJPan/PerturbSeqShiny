plot_perm_score_uncertainty <- function(
    df,
    estimate_col,
    lwr_col,
    upr_col,
    selected_TF = NULL,
    selected_TF_only = FALSE,
    scores_col = NULL,              
    target_TF_color = "#D62728",
    not_selected_color = "blue",
    jitter_width = 0.18,
    jitter_alpha = 0.15,
    jitter_size  = 0.8,
    point_alpha  = 1,
    point_size   = 2.0,
    line_width   = 0.6,
    violin_alpha = 0.25
) {
  
  df_col <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .is_selected = if (!is.null(selected_TF)) target_TF == selected_TF else FALSE,
      .col  = ifelse(.is_selected, target_TF_color, not_selected_color),
      .fill = .col
    )
  
  if (selected_TF_only && !is.null(selected_TF)) {
    df_col <- df_col %>% dplyr::filter(target_TF == selected_TF)
  }
  
  if (nrow(df_col) == 0) {
    return(ggplot() + theme_void() + ggtitle("No data for selected TF"))
  }
  
  df_col <- df_col %>%
    dplyr::arrange(dplyr::desc(.data[[estimate_col]])) %>%
    dplyr::mutate(target_TF = factor(target_TF, levels = unique(target_TF)))
  
  scores_sym <- rlang::sym(scores_col)
  
  jitter_df <- df_col %>%
    dplyr::select(target_TF, .is_selected, .col, .fill, !!scores_sym) %>%
    tidyr::unnest(!!scores_sym, keep_empty = FALSE) %>%
    dplyr::rename(.perm_score = !!scores_sym)
  
  ggplot(df_col, aes(y = target_TF)) +
    geom_violin(
      data = jitter_df,
      aes(x = .perm_score, y = target_TF, fill = .fill),
      scale = "width", trim = TRUE, alpha = violin_alpha,
      linewidth = 0.2, inherit.aes = FALSE
    ) +
    scale_fill_identity() +
    geom_jitter(
      data = jitter_df,
      aes(y = target_TF, x = .perm_score),
      width = 0, height = jitter_width,
      alpha = jitter_alpha, size = jitter_size,
      inherit.aes = FALSE
    ) +
    geom_linerange(
      aes(xmin = .data[[lwr_col]], xmax = .data[[upr_col]], color = .col),
      linewidth = line_width, show.legend = FALSE
    ) +
    geom_point(
      aes(x = .data[[estimate_col]], color = .col),
      size = point_size, alpha = point_alpha, show.legend = FALSE
    ) +
    scale_color_identity() +
    labs(x = "Permutation score", y = "") +
    theme_bw() +
    theme(
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 11),
      axis.title.y = element_text(size = 13),
      axis.text.y = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}
