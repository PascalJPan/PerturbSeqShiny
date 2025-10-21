compute_tls_residuals <- function(df, id_col, x_col, y_col) {
  # computes the total least square residuals (opposed to vertical residuals)
  
  
  # Center the x and y columns
  xy_mat <- scale(df[, c(x_col, y_col)], center = TRUE, scale = FALSE)
  
  # Perform SVD
  svd_res <- svd(xy_mat)
  v <- svd_res$v[, 1]  # principal direction (max variance)
  
  # TLS slope and intercept
  tls_slope <- v[2] / v[1]
  means <- colMeans(df[, c(x_col, y_col)])
  tls_intercept <- means[[y_col]] - tls_slope * means[[x_col]]
  
  # Compute signed orthogonal residuals
  signed_residuals <- -(tls_slope * df[[x_col]] - df[[y_col]] + tls_intercept) /
    sqrt(tls_slope^2 + 1)
  
  # Return original data frame with added residual and model info
  df %>%
    mutate(
      DRT_DE_union_log10_tls_score = signed_residuals,
      DRT_tls_slope = tls_slope,
      DRT_tls_intercept = tls_intercept
    )
}
