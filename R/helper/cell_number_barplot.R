cell_number_barplot <- function(cell_numbers_per_gRNA,
                                perturbed_TF_selected,
                                species_selected,
                                split_by_individual = TRUE) {
  # filter data
  df <- cell_numbers_per_gRNA %>%
    filter(species == species_selected,
           perturbed_TF == perturbed_TF_selected)
  
  # choose shade palette by species
  if (species_selected == "human") {
    base_shades <- c("#C12E5D","#D35C7A","#E47B97","#F59AB4",
                     "#FFC0D1","#FFD6E1","#FFE9EF","#FFF4F7")
  } else {
    base_shades <- c("#3AA6A6","#5BB7B7","#7CC8C8","#9DD9D9",
                     "#BEEAEA","#D2F3F3","#E6FAFA","#F5FEFE")
  }
  
  # make a palette long enough for the number of gRNAs
  n_g <- dplyr::n_distinct(df$gRNA)
  shades <- base_shades #grDevices::colorRampPalette(base_shades)(max(2, n_g)) 
  
  # x variable: either Individual (side-by-side) or single species
  if (split_by_individual) {
    df <- df %>% mutate(individual = as.factor(individual))
    x_var <- "individual"
    x_lab <- "Individual"
  } else {
    df <- df %>% mutate(species = factor(species, levels = species_selected))
    x_var <- "species"
    x_lab <- NULL
  }
  
  # plot
  ggplot(df, aes_string(x = x_var, y = "cell_count", fill = "gRNA")) +
    geom_col(width = 0.7) +
    geom_text(aes(label = cell_count),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white") +
    scale_fill_manual(values = shades) +
    labs(y = "Cell number", x = x_lab) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.text.y = element_text(hjust = 1)
    )
}
