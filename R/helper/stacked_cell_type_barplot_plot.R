stacked_cell_type_barplot_plot <- function(seu, current_TF, ct_colors, species, text_size = 12) {

  ct_levels <- names(ct_colors)
  
  # 1) subset to species + keep only current_TF and NT_control
  df <- seu@meta.data %>%
    dplyr::filter(.data$species == !!species,
                  .data$perturbed_TF %in% c(current_TF, "NT_control")) %>%
    dplyr::mutate(
      cell_type = factor(gsub("_", " ", .data$cell_type), levels = ct_levels),
      group = ifelse(.data$perturbed_TF == current_TF, current_TF, "NT_control"),
      group = factor(group, levels = c(current_TF, "NT_control"))
    )
  
  # If no rows after filtering, return an empty plot
  if (nrow(df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("No cells for species '", species,
                                "' with TF = '", current_TF,
                                "' or 'NT_control'."),
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void()
    )
  }
  
  # 2) count cells per (group, cell_type) and complete zeros
  counts <- df %>%
    dplyr::count(group, cell_type, name = "n") %>%
    tidyr::complete(
      group = factor(c(current_TF, "NT_control"),
                     levels = c(current_TF, "NT_control")),
      cell_type = factor(ct_levels, levels = ct_levels),
      fill = list(n = 0)
    )
  
  # 3) stacked percent bars (two columns)
  ggplot(counts, aes(x = group, y = n, fill = cell_type)) +
    geom_col(position = "fill", width = 0.7) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = ct_colors, breaks = ct_levels, name = "Cell type") +
    labs(x = NULL, y = "Percent of cells") +
    theme_minimal(base_size = text_size) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      panel.grid.major.x = element_blank()
    )
}
