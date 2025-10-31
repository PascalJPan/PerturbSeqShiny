plot_perm_score_uncertainty_plotly <- function(
    df,
    estimate_col,
    lwr_col,
    upr_col,
    selected_TF = NULL,
    selected_TF_only = FALSE,
    scores_col = NULL,
    max_scores_list_values_plotted = 100,
    target_TF_color = "#D62728",
    not_selected_color = "blue",
    jitter_width   = 0.18,
    jitter_alpha   = 0.15,
    jitter_size    = 0.8,
    point_alpha    = 1,
    point_size     = 2.0,
    line_width     = 0.6,
    violin_alpha   = 0.25
) {
  require(plotly)
  
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
    return(plotly::ggplotly(ggplot() + theme_void() + ggtitle("No data for selected TF")))
  }
  
  df_col <- df_col %>%
    dplyr::mutate(CI_range = .data[[upr_col]] - .data[[lwr_col]]) %>% 
    dplyr::arrange(dplyr::desc(CI_range)) %>%
    dplyr::arrange(dplyr::desc(.data[[estimate_col]])) %>%
    dplyr::mutate(target_TF = factor(target_TF, levels = unique(target_TF))) %>%
    # helpful hover text for points / CI bars
    dplyr::mutate(
      .hover_point = sprintf(
        "TF: %s<br>estimate: %.3f<br>CI: [%.3f, %.3f]",
        target_TF, as.numeric(.data[[estimate_col]]),
        as.numeric(.data[[lwr_col]]), as.numeric(.data[[upr_col]])
      )
    )
  
  scores_sym <- rlang::sym(scores_col)
  
  jitter_df <- df_col %>%
    dplyr::select(target_TF, .is_selected, .col, .fill, !!scores_sym) %>%
    tidyr::unnest(!!scores_sym, keep_empty = FALSE) %>%
    dplyr::rename(.perm_score = !!scores_sym) %>%
    dplyr::mutate(
      .perm_score = as.numeric(.perm_score),
      .hover_violin = sprintf("TF: %s<br>bootstrap score: %.4f", target_TF, .perm_score)
    ) %>%
    dplyr::group_by(target_TF) %>%
    dplyr::slice_head(n = max_scores_list_values_plotted) %>% 
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  
  # after you build jitter_df
  violin_df <- jitter_df %>%
    dplyr::group_by(target_TF) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  
  df_col <- df_col %>%
    droplevels()  # helpful when selected_TF_only = TRUE
  
  # ... build jitter_df, violin_df, df_col as before ...
  
  p <- ggplot(df_col, aes(x = target_TF)) +
    geom_violin(
      data = violin_df,
      aes(x = target_TF, y = .perm_score, fill = .fill, group = target_TF),
      scale = "width", trim = TRUE, alpha = violin_alpha,
      linewidth = 0, show.legend = FALSE, inherit.aes = FALSE
    ) +
    scale_fill_identity() +
    geom_jitter(
      data = jitter_df,
      aes(x = target_TF, y = .perm_score),
      width = jitter_width, height = 0,
      alpha = jitter_alpha, size = jitter_size,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    geom_linerange(
      aes(ymin = .data[[lwr_col]], ymax = .data[[upr_col]], color = .col),
      linewidth = line_width, show.legend = FALSE
    ) +
    geom_point(
      aes(y = .data[[estimate_col]], color = .col),
      size = point_size, alpha = point_alpha, show.legend = FALSE
    ) +
    scale_color_identity() +
    labs(x = "", y = "Permutation score") +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    coord_flip()
  
  plt <- plotly::ggplotly(p, tooltip = c("x","y")) %>%
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("select2d","lasso2d")) %>%
    plotly::layout(
      xaxis = list(showgrid = TRUE, zeroline = TRUE, showspikes = FALSE),
      yaxis = list(showgrid = TRUE, zeroline = TRUE, showspikes = FALSE),
      hovermode = "closest",
      showlegend = FALSE
    )
  
  # Turn hover off everywhere first
  plt <- plotly::style(plt, hoverinfo = "skip")
  
  plt
  
}
