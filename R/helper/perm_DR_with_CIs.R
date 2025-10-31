perm_DR_with_CIs <- function(df_tf, conf_level = 0.9) {
  n_DR_div_vec <- df_tf %>% filter(permuted) %>% pull(n_DR)
  
  # summaries + renaming
  summary <- summarizeStat(n_DR_div_vec, "mean", conf_level)
  return(summary)

}
