plot_human_vs_cyno_enrichment <- function(df, selected_TF, color) {
  # a scatterplot of enrichment in human vs cynomolgus
  
  wide <- df %>%
    dplyr::select(perturbed_TF, species, enrichment, enrichment_overall) %>%
    tidyr::pivot_wider(
      names_from = species,
      values_from = enrichment,
      names_prefix = "enrichment_"
    ) %>%
    dplyr::rename(
      enrichment_human = enrichment_human,
      enrichment_cyno  = enrichment_cynomolgus
    ) %>%
    dplyr::mutate(
      highlight = perturbed_TF == selected_TF,
      tooltip = sprintf(
        "TF: %s<br>Human: %.2f<br>Cyno: %.2f",
        perturbed_TF, enrichment_human, enrichment_cyno
      )
    )
  
  p <- ggplot(wide, aes(x = enrichment_human, y = enrichment_cyno, text = tooltip)) +
    geom_abline(intercept = 0, slope = 1, color = "grey60", linetype = "dashed") +
    
    # Base points: solid black fill & thin black outline (no legend)
    geom_point(
      shape = 21, size = 3, alpha = 0.8,
      fill = "black", color = "black", stroke = 0.3,
      show.legend = FALSE
    ) +
    
    geom_point(
      data = subset(wide, highlight),
      aes(color = "selected"),
      shape = 21, size = 3, fill = NA, stroke = 1,
      show.legend = TRUE
    ) +
    
    scale_color_manual(values = c(selected = color), name = "Category") +
    guides(color = guide_legend(override.aes = list(fill = NA, shape = 21, size = 4, stroke = 1))) +
    
    labs(x = "Enrichment (human)", y = "Enrichment (cynomolgus)", title = "") +
    theme_bw(base_size = 13) +
    theme(
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 11),
      legend.box   = "vertical",
      legend.spacing = unit(0.2, "cm")
    ) +
    coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))
  
  # Clean plotly's auto legend labels just in case
  plt <- ggplotly(p, tooltip = "text")
  for (i in seq_along(plt$x$data)) {
    nm <- plt$x$data[[i]]$name
    if (!is.null(nm) && nzchar(nm)) {
      nm <- sub(",.*$", "", nm)
      nm <- gsub("^\\(|\\)$", "", nm)
      plt$x$data[[i]]$name <- nm
      plt$x$data[[i]]$legendgroup <- nm
    }
  }
  
  plt$x$layout$legend$title$text <- NULL
  return(plt)
}
