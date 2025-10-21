con_model_plot <- function(cs_df, selected_TF, con_score_name, color = "#D62728") {
  # OLS cases
  if (con_score_name %in% c("DRT_DE_union_log10_lm_score",
                            "DT_DE_union_log10_lm_score",
                            "DE_union_log10_lm_score")) {
    if (con_score_name == "DRT_DE_union_log10_lm_score") {
      model <- stats::lm(DRT_log10 ~ DE_union_log10, data = cs_df)
      y_axis_title <- "log10(DRT + 1)"
      y_col <- "DRT_log10"
      y_col_without_log <- "DRT_obs"
    } else if (con_score_name == "DT_DE_union_log10_lm_score") {
      model <- stats::lm(DT_log10 ~ DE_union_log10, data = cs_df)
      y_axis_title <- "log10(DT + 1)"
      y_col <- "DT_log10"
      y_col_without_log <- "DT_obs"
    } else { # DE_union_log10_lm_score
      model <- stats::lm(DR_log10 ~ DE_union_log10, data = cs_df)
      y_axis_title <- "log10(DR + 1)"
      y_col <- "DR_log10"
      y_col_without_log <- "DR_obs"
    }
    
    print(cs_df)
    
    df <- cs_df %>%
      dplyr::mutate(
        highlight = factor(dplyr::if_else(target_TF == selected_TF, "selected", "other"),
                           levels = c("selected", "other")),
        tooltip_text = paste0(
          "TF: ", target_TF,
          "<br>DE_union: ", round(DE_union_obs, 2),
          "<br>", y_col_without_log, ": ", round(.data[[y_col_without_log]], 2),
          "<br>Score: ",  round(cs_df[[con_score_name]],2)
        )
      )
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = DE_union_log10, y = .data[[y_col]], text = tooltip_text)) +
      ggplot2::geom_point(ggplot2::aes(fill = highlight), shape = 21, color = "black",
                          size = 3, stroke = 0.1, alpha = 0.6) +
      ggplot2::scale_fill_manual(values = c(selected = color, other = "grey70")) +
      ggplot2::geom_abline(slope = stats::coef(model)[2], intercept = stats::coef(model)[1],
                           linetype = "dashed", color = "black") +
      ggplot2::labs(x = "log10(DE union + 1)", y = y_axis_title) +
      ggplot2::theme_minimal() +
      ggplot2::xlim(0, 4) + ggplot2::ylim(0, 4)
    
    p_out <- plotly::ggplotly(p, tooltip = "text")
    p_out$x$layout$legend$title$text <- NULL
    return(p_out)
  }
  
  # TLS case
  if (con_score_name == "DRT_DE_union_log10_tls_score") {
    y_col <- "DRT_log10"
    y_axis_title <- "log10(DRT + 1)"
    y_col_without_log <- "DRT_obs"
    
    # TLS via SVD
    xy <- cs_df[, c("DE_union_log10", y_col)]
    xy_centered <- scale(xy, center = TRUE, scale = FALSE)
    sv <- svd(xy_centered)
    v <- sv$v[, 1]
    tls_slope <- v[2] / v[1]
    means <- colMeans(xy)
    tls_intercept <- means[[y_col]] - tls_slope * means[["DE_union_log10"]]
    
    df <- cs_df %>%
      dplyr::mutate(
        highlight = factor(dplyr::if_else(target_TF == selected_TF, "selected", "other"),
                           levels = c("selected", "other")),
        tooltip_text = paste0(
          "TF: ", target_TF,
          "<br>DE_union: ", round(DE_union_obs, 2),
          "<br>", y_col_without_log, ": ", round(.data[[y_col_without_log]], 2),
          "<br>Score: ", round(cs_df[[con_score_name]],2)
        )
      )
    
    # residual segment data for selected TF (if present)
    sel <- df %>% dplyr::filter(target_TF == selected_TF)
    have_sel <- nrow(sel) == 1 && is.finite(sel$DE_union_log10) && is.finite(sel[[y_col]])
    seg_layer <- NULL
    if (have_sel) {
      x0 <- sel$DE_union_log10[1]
      y0 <- sel[[y_col]][1]
      x_proj <- (x0 + tls_slope * (y0 - tls_intercept)) / (tls_slope^2 + 1)
      y_proj <- tls_slope * x_proj + tls_intercept
      seg_layer <- ggplot2::geom_segment(
        data = data.frame(x0 = x0, y0 = y0, x_proj = x_proj, y_proj = y_proj),
        ggplot2::aes(x = x0, y = y0, xend = x_proj, yend = y_proj),
        inherit.aes = FALSE,
        color = color, linetype = "solid", linewidth = 0.6, alpha = 0.7
      )
    }
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = DE_union_log10, y = .data[[y_col]], text = tooltip_text)) +
      ggplot2::geom_point(ggplot2::aes(fill = highlight), shape = 21, color = "black",
                          size = 3, stroke = 0.1, alpha = 0.6) +
      ggplot2::scale_fill_manual(values = c(selected = color, other = "grey70"), guide = "none") +
      ggplot2::geom_abline(slope = tls_slope, intercept = tls_intercept,
                           linetype = "dashed", color = "black") +
      seg_layer +                                                   # <- adds the segment or NULL
      ggplot2::labs(x = "log10(DE union + 1)", y = y_axis_title) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::xlim(0, 4) + ggplot2::ylim(0, 4)
    
    p_out <- plotly::ggplotly(p, tooltip = "text")
    p_out$x$layout$legend$title$text <- NULL
    return(p_out)
  }
  
  stop("Unhandled con_score_name: ", con_score_name)
}
