plot_species_enrichment <- function(df, selected_TF, color) {
  # Scatterplot for species enrichment
  
  # Filter for species and classify
  dat <- df %>%
    mutate(
      perturbed_TF = ifelse(perturbed_TF == "NT_control", "cntrl", perturbed_TF),
      highlight = perturbed_TF == selected_TF,
      enrichment = case_when(
        enrichment %in% sort(enrichment)[1:5] ~ "depleted",
        enrichment %in% sort(enrichment, decreasing = TRUE)[1:5] ~ "enriched",
        TRUE ~ "none"
      ),
      tooltip = sprintf(
        "TF: %s<br>Library: %.2f<br>Cells: %.2f<br>Status: %s",
        perturbed_TF,
        overrep_lib,
        overrep_final,
        enrichment
      )
    )
  lims <- range(c(dat$overrep_lib, dat$overrep_final), na.rm = TRUE)
  
  p <- ggplot(dat, aes(overrep_lib, overrep_final, text = tooltip)) +
    geom_abline(intercept = 0, slope = 1, color = "grey60", linetype = "dashed") +
    
    # Status legend (fill only)
    geom_point(
      aes(fill = enrichment),
      shape = 21, size = 3, alpha = 0.8,
      color = "black", stroke = 0.3,   # constants, not mapped
      show.legend = TRUE
    ) +
    
    # Highlight legend (color only)
    geom_point(
      data = subset(dat, highlight),
      aes(color = "selected"),
      shape = 21, size = 3, fill = NA, stroke = 1,
      show.legend = TRUE
    ) +
    
    scale_fill_manual(values = c(depleted="indianred2", enriched="chartreuse4", none="black"),
                      name = "") +
    scale_color_manual(values = c(selected = color), name = "Category") +
    
    guides(
      fill  = guide_legend(order = 1),
      color = guide_legend(order = 2, override.aes = list(fill = NA, shape = 21, size = 4, stroke = 1)),
      # ensure no accidental legends from stroke/size/etc
      stroke = "none", size = "none"
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.box = "vertical",
      legend.spacing = unit(0.2, "cm")
    ) +
    coord_cartesian(xlim = c(-2.1, 2.1), ylim = c(-2.1, 2.1))
  
  plt <- ggplotly(p, tooltip = "text")
  
  for (i in seq_along(plt$x$data)) {
    nm <- plt$x$data[[i]]$name
    if (!is.null(nm) && nzchar(nm)) {
      nm <- sub(",.*$", "", nm)         # drop everything after the first comma
      nm <- gsub("^\\(|\\)$", "", nm)   # remove any leading "(" or trailing ")"
      plt$x$data[[i]]$name <- nm
      plt$x$data[[i]]$legendgroup <- nm
    }
  }
  
  plt$x$layout$legend$title$text <- NULL
  return(plt)
}