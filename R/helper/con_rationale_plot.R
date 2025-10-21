con_rational_plot <- function(
    cs_df,
    selected_TF,
    sel_rationale_processed,
    con_score_name,
    highlight_cols = "TF",
    color = "#D62728"
) {
  cs_df <- cs_df %>% mutate(TF = target_TF)
  
  joined_df <- left_join(cs_df, sel_rationale_processed, by = "TF") %>%
    tidyr::separate_rows(selection_rationale, sep = ", ") %>%
    mutate(
      selection_rationale = factor(
        selection_rationale,
        levels = c(
          "pluripotency factor", "highly expressed in iPSCs", "conserved sequence",
          "diverged sequence", "conserved network", "diverged network",
          "non-KRAB C2H2 zinc finger", "KRAB C2H2 zinc finger",
          "universal stripe factor", "literature search", "other"
        )
      )
    ) %>%
    rowwise() %>%
    mutate(
      highlight = if_else(
        any(c_across(all_of(highlight_cols)) %in% selected_TF),
        "selected", "other"
      ),
      tooltip_text = sprintf(
        "TF: %s<br>Rationale: %s<br>Score: %.2f",
        TF, selection_rationale, .data[[con_score_name]]
      )
    ) %>%
    ungroup()
  
  # Distinct scores per TF-rationale for summary stats
  summary_df <- joined_df %>% distinct(TF, selection_rationale, .data[[con_score_name]])
  
  # Mean per rationale for labeling
  mean_labels <- summary_df %>%
    group_by(selection_rationale) %>%
    summarize(mean_score = mean(.data[[con_score_name]]), .groups = "drop") %>%
    mutate(label = sprintf("%.2f", mean_score))
  
  p <- ggplot(joined_df, aes(
    x = selection_rationale,
    y = .data[[con_score_name]],
    text = tooltip_text
  )) +
    geom_quasirandom(
      aes(fill = highlight),
      shape = 21, color = "black",
      size = 2.5, stroke = 0.2, alpha = 0.7
    ) +
    stat_summary(
      data = summary_df, inherit.aes = FALSE,
      aes(x = selection_rationale, y = .data[[con_score_name]]),
      fun = mean, geom = "crossbar",
      linewidth = 0.3, width = 0.2, color = color
    ) +
    geom_text(
      data = mean_labels, inherit.aes = FALSE,
      aes(x = selection_rationale, y = mean_score, label = label),
      position = position_nudge(x = 0.25),
      size = 3, color = color
    ) +
    scale_fill_manual(
      values = c(selected = color, other = "lightgrey"),
      guide = "none"
    ) +
    labs(x = "Selection Rationale", y = "Divergence Score") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  ggplotly(p, tooltip = "text")
}
