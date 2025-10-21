cell_number_barplot <- function(cell_numbers_per_gRNA, perturbed_TF_selected, species_selected) {
  
  df <- cell_numbers_per_gRNA %>% 
    filter(species == species_selected, perturbed_TF == perturbed_TF_selected)
  
  if (species_selected == "human") {
    shades <- c(
      "#C12E5D",  # original
      "#D35C7A",
      "#E47B97",
      "#F59AB4",
      "#FFC0D1",
      "#FFD6E1",
      "#FFE9EF",
      "#FFF4F7"
    )
  } else {
    shades <- c(
      "#3AA6A6",  # original
      "#5BB7B7",
      "#7CC8C8",
      "#9DD9D9",
      "#BEEAEA",
      "#D2F3F3",  
      "#E6FAFA",
      "#E6FAFA",
      "#F5FEFE"
    )
  }
  
  # Plot
  ggplot(df, aes(x = species, y = cell_count, fill = gRNA)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = cell_count),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "white"
    ) +
    scale_fill_manual(values = shades) + 
    labs(
      y = "Cell number",
      x = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.text.y = element_text(hjust = 1)
    )
}
