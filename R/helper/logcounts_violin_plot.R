logcounts_violin_plot <- function(
    seu,
    current_TF,
    species,
    text_size    = 12,
    point_alpha  = 0.3,
    point_size   = 0.3,
    box_alpha    = 0.7,
    box_width    = 0.6,
    bees_width   = 0.18,
    highlight_color = "#FF4242",
    control_color   = "lightgrey"
) {
  
  # Extract logcounts for the current_TF
  if (!current_TF %in% rownames(seu@assays$RNA$counts)) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("No logcounts for TF '", current_TF, "' found."),
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void()
    )
  }
  
  logcounts_vec <- Matrix::colSums(seu@assays$RNA$data[current_TF, , drop = FALSE])
  
  if (species == "human") {
    df <- seu@meta.data %>%
      mutate(logcounts = logcounts_vec) %>%
      dplyr::filter(.data$species == !!"human",
                    .data$perturbed_TF %in% c(current_TF, "NT_control")) %>%
      dplyr::mutate(
        group = ifelse(.data$perturbed_TF == current_TF, .data$gRNA, "NT_control")
      )
  } else if (species == "cynomolgus") {
    df <- seu@meta.data %>%
      mutate(logcounts = logcounts_vec) %>%
      dplyr::filter(.data$species == !!"cynomolgus",
                    .data$perturbed_TF %in% c(current_TF, "NT_control")) %>%
      dplyr::mutate(
        group = ifelse(.data$perturbed_TF == current_TF, .data$gRNA, "NT_control")
      )
  } else {stop("species is not present...")}
  
  # Order x-axis: all gRNAs first (sorted), then NT_control
  g_levels <- df %>%
    dplyr::filter(.data$perturbed_TF == current_TF) %>%
    dplyr::pull(.data$gRNA) %>%
    unique() %>%
    sort()
  df <- df %>%
    dplyr::mutate(group = factor(.data$group, levels = c(g_levels, "NT_control")))
  
  # handle empty
  if (nrow(df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("No data for species '", species,
                                "' and TF '", current_TF, "'."),
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void()
    )
  }
  
  ggplot(
    df,
    aes(x = group, y = logcounts,
        fill = ifelse(group == "NT_control", "NT_control", "Perturbed"))
  ) +
    geom_violin(width = box_width, alpha = box_alpha, draw_quantiles = c(0.5)) +
    ggbeeswarm::geom_quasirandom(
      data = df %>% dplyr::filter(group != "NT_control"), 
      aes(x = group, y = logcounts),
      width = bees_width,
      dodge.width = 0,
      alpha = point_alpha,
      size = point_size
    ) +
    scale_fill_manual(values = c("Perturbed" = highlight_color,
                                 "NT_control" = control_color),
                      guide = "none") +
    labs(
      x = NULL,
      y = paste0("logcounts of ", current_TF)
    ) +
    theme_minimal(base_size = text_size) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      #axis.text.y = element_text(angle = 90),
      panel.grid.minor = element_blank()
    ) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
}