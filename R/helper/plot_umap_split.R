plot_umap_split <- function(seu, color_by, split_by ="individual", colors = NULL, reduction = "umap_per_indiv", blend = FALSE, text_size = 30, point_size = 3, legend_title = color_by) {
  # plot UMAPs per individual/species
  
  if (color_by %in% rownames(seu)) {
    
    seu@meta.data[[color_by]] <- as(seu[["RNA"]]$data, Class = "dgCMatrix")[color_by, ]
    
    if (legend_title == color_by) legend_title <- paste0(color_by, "\nexpression\n(logcounts)")
    
  }
  
  if (legend_title == color_by) {
    
    legend_title <- wrap_labels(gsub("\\_", " ", legend_title), 15)
    
  }
  
  umap_df <- seu[[reduction]]@cell.embeddings %>% 
    as.data.frame() %>% 
    rownames_to_column("cell") %>% 
    left_join(seu@meta.data,by = join_by(cell)) %>% 
    dplyr::rename(umap_1 = .data[[paste0(gsub("\\.|\\_", "", reduction), "_1")]], umap_2 = .data[[paste0(gsub("\\.|\\_", "", reduction), "_2")]])
  

    umap_df <- umap_df %>% arrange(.data[[color_by]] == "Perturbed")

  
  
  if (!is.numeric(umap_df[[color_by]]) & !is.null(names(colors))) {
    
    colors_filt <- colors[names(colors) %in% umap_df[[color_by]]]
    umap_df[[color_by]] <- factor(umap_df[[color_by]], names(colors_filt))
    
  }
  
  p <- ggplot(umap_df, aes(x = umap_1, y = umap_2, fill = .data[[color_by]])) +
    theme_bw(base_size = text_size) +
    facet_wrap(~.data[[split_by]], scales = "free") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.height = unit(text_size / 25, "cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.spacing = ggplot2::unit(0.1, "lines"),
          strip.background = element_blank(),
          strip.text.x = element_blank())
  
  if (blend & !is.numeric(umap_df[[color_by]])) {
    
    p <- p +
      geom_point(stroke = 0.0, shape = 21, alpha = 0.5, size = point_size) %>%  partition(vars(get(color_by))) * (blend("lighten") + blend("multiply", alpha = 0.5))
    
  } else {
    
    p <- p +
      geom_point(stroke = 0.0, shape = 21, alpha = 0.5, size = point_size)  
    
  }
  
  if (is.numeric(umap_df[[color_by]])) {
    
    if (is.null(colors)) colors <- rev(c("#C1E8FF", "#8ABBDE", "#00557C", "#002951"))
    
    p <- p +
      scale_fill_gradientn(colors = colors)
    
  } else {
    
    if (!is.null(colors)) {
      
      p <- p +
        scale_fill_manual(values = colors, name = legend_title)
      
    } else {
      
      p <- p +
        labs(fill = legend_title)
      
    }
    
    p <- p +
      guides(fill = guide_legend(override.aes = list(size = text_size / 3.75, alpha = 1, byrow = TRUE)))
    
  }
  
  p
  
}