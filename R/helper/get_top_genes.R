get_top_genes <- function(df, logFC_col, pval_col, n = 5, p_value_threshold) {
  # get top logFC genes
  
  top_up <- df %>% filter(!!sym(logFC_col) > 0, !!sym(pval_col) < p_value_threshold) %>%
    arrange(!!sym(pval_col)) %>% slice_head(n = n)
  top_down <- df %>% filter(!!sym(logFC_col) < 0, !!sym(pval_col) < p_value_threshold) %>%
    arrange(!!sym(pval_col)) %>% slice_head(n = n)
  bind_rows(top_up, top_down)
}
