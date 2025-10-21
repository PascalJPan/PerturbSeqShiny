# Highlight perturbed cells on the UMAP (across all cells), split by species
plot_umap_highlight <- function(
    seu,
    current_tf,
    highlight_col = "perturbed_TF",
    split_by = "species",
    reduction = "umap_per_species",
    text_size = 30,
    point_size = 5,
    color = "#D62728",
    other_color = "lightgrey",
    legend_title = ""
) {
  stopifnot(!missing(current_tf))
  
  # Temporary meta column for plotting
  hl_col <- ".__highlight__"
  on.exit(try(seu@meta.data[[hl_col]] <- NULL, silent = TRUE), add = TRUE)
  
  # Label cells as Perturbed vs Other
  v_equal <- as.character(seu@meta.data[[highlight_col]]) == as.character(current_tf)
  seu@meta.data[[hl_col]] <- factor(ifelse(v_equal, "Perturbed", "Other"),
                                    levels = c("Other", "Perturbed"))
  
  cols <- c("Other" = other_color, "Perturbed" = color)
  
  if (is.null(legend_title)) {
    legend_title <- paste0(gsub("_", " ", highlight_col), " == ", current_tf)
  }
  
  plot_umap_split(
    seu = seu,
    color_by = hl_col,
    split_by = split_by,
    colors = cols,
    reduction = reduction,
    blend = FALSE,
    text_size = text_size,
    point_size = point_size,
    legend_title = legend_title
  )
}
