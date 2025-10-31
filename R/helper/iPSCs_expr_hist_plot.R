iPSCs_expr_hist_plot <- function(expr_df, selected_TF, species = c("human","cynomolgus"),
                                 x_col, x_label = "Percent of cells", percent_x_scale = TRUE, color = "#D62728") {
  species <- match.arg(species)
  
  expr_df <- expr_df %>% 
    filter(species == .env$species)   
  
  gx <- unique(na.omit(expr_df[[x_col]][expr_df$gene == selected_TF]))
  gx <- if (length(gx)) gx[1] else NA_real_
  
  if (percent_x_scale) {
    p <- ggplot(expr_df, aes(x = .data[[x_col]])) +
      geom_histogram(col = "white", fill = "grey50", binwidth = 0.05) +  # 5% bins on 0–1
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_bw(base_size = 20) +
      labs(x = x_label, y = "Number of genes")
  } else {
    x_end_value <- max(30,gx + 5)
    
    p <- ggplot(expr_df, aes(x = .data[[x_col]])) +
      #ylim(0,10000) +
      xlim(-1,x_end_value) +
      geom_histogram(col = "white", fill = "grey50", binwidth = 1) + 
      theme_bw(base_size = 20) +
      labs(x = x_label, y = "Number of genes")
  }

  
  gb <- ggplot_build(p)
  ymax <- tryCatch(max(gb$data[[1]]$count, na.rm = TRUE), error = function(e) NA_real_)
  
  if (is.finite(gx) && is.finite(ymax)) {
    p <- p +
      annotate("segment", x = gx, xend = gx, y = 0, yend = ymax * 1.02,
               linetype = "dashed", linewidth = 0.4, colour = color) +
      annotate("label", x = gx, y = ymax * 1.05, label = selected_TF,
               size = 6, colour = color, fill = NA, label.size = NA)
  }
  p
}
