perm_metrics_with_CIs <- function(df_tf, conf_level = 0.95) {
  n_DR_obs <- df_tf %>% filter(!permuted) %>% pull(n_DR)
  n_DR_div_vec <- df_tf %>% filter(permuted) %>% pull(n_DR)
  
  # scores
  n_DR_div_vec_no_zeros <- n_DR_div_vec
  n_DR_div_vec_no_zeros[n_DR_div_vec_no_zeros == 0] <- 1
  
  perm_scores_vec_no_obs_add <-  n_DR_obs / n_DR_div_vec_no_zeros
  perm_scores_vec <- (n_DR_obs + 0.001) / (n_DR_div_vec + 1)
  perm_log10_scores_vec <- log10(perm_scores_vec)
  
  # Bootstrap approach
  ## mean 1DR
  bootstrap_1DR_mean_summary <- perm_bootstrap_CI(DR_obs = n_DR_obs, DR_div_vec = n_DR_div_vec, SE_obs = NULL, 
                                bootstrap_replicates = 1000, seed = 1)
  
  bootstrap_1DR_mean_summary_df <- data.frame(bootstrap_1DR_mean_summary$df)
  colnames(bootstrap_1DR_mean_summary_df) <- paste0("perm_score_mean_bootstrap_1DR_", colnames(bootstrap_1DR_mean_summary_df))
  
  ## mean 1SE
  bootstrap_1SE_mean_summary <- perm_bootstrap_CI(DR_obs = n_DR_obs, DR_div_vec = n_DR_div_vec, SE_obs = 1, 
                                                  bootstrap_replicates = 1000, seed = 1)
  
  bootstrap_1SE_mean_summary_df <- data.frame(bootstrap_1SE_mean_summary$df)
  colnames(bootstrap_1SE_mean_summary_df) <- paste0("perm_score_mean_bootstrap_1SE_", colnames(bootstrap_1SE_mean_summary_df))
  
  ## mean 1SE log10
  bootstrap_1SE_mean_log10_summary <- perm_bootstrap_CI(DR_obs = n_DR_obs, DR_div_vec = n_DR_div_vec, SE_obs = 1, 
                                                  bootstrap_replicates = 1000, seed = 1, log10_space = TRUE)
  
  bootstrap_1SE_mean_log10_summary_df <- data.frame(bootstrap_1SE_mean_log10_summary$df)
  colnames(bootstrap_1SE_mean_log10_summary_df) <- paste0("perm_score_mean_log10_bootstrap_1SE_", colnames(bootstrap_1SE_mean_log10_summary_df))
  
  
  
  # summaries + renaming (CIs by Anita)
  mean_summary <- summarizeStat(perm_scores_vec, "mean", conf_level)
  colnames(mean_summary) <- paste0("perm_score_mean_", colnames(mean_summary))
  
  median_summary <- summarizeStat(perm_scores_vec, "median", conf_level)
  colnames(median_summary) <- paste0("perm_score_median_", colnames(median_summary))
  
  log10_mean_summary <- summarizeStat(perm_log10_scores_vec, "mean", conf_level)
  colnames(log10_mean_summary) <- paste0("perm_log10_score_mean_", colnames(log10_mean_summary))
  
  # one-row tibble + list-cols with all raw points
  as_tibble(cbind(bootstrap_1DR_mean_summary_df, bootstrap_1SE_mean_summary_df, bootstrap_1SE_mean_log10_summary_df, mean_summary, median_summary, log10_mean_summary)) %>%
    mutate(
      perm_score_mean_scores_list = list(perm_scores_vec),
      perm_score_median_scores_list = list(perm_scores_vec),
      perm_log10_score_mean_scores_list = list(perm_log10_scores_vec),
      perm_score_mean_bootstrap_1SE_scores_list = list(unlist(bootstrap_1SE_mean_summary$scores_list)),
      perm_score_mean_bootstrap_1DR_scores_list = list(unlist(bootstrap_1DR_mean_summary$scores_list)),
      perm_score_mean_log10_bootstrap_1SE_scores_list = list(unlist(bootstrap_1SE_mean_log10_summary$scores_list))
    )
}
