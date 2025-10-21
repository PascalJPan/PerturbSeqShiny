get_DR_DT_model <- function (permutation_outputs_df) {
  # takes the df with all Regulons as input and fits a linear model to DR and DT genes
  
  permutations_df_summarized <- permutation_outputs_df %>% 
    mutate(permuted = ifelse(permutation > 0, "div", "obs")) %>% 
    group_by(permuted, target_TF) %>% 
    summarise(DR = mean(n_DR), DE_union = mean(n_DE_union), DT = mean(n_DT), DRT = mean(n_DRT), sm500 = mean(sm500), .groups = "drop") %>% ungroup()
  
  combined_permutations_df <- permutations_df_summarized %>% 
    pivot_wider(id_cols = c("target_TF"), names_from = c(permuted), values_from = c(DR, DE_union, DT, DRT, sm500))
  
  conservation_scores_df <- combined_permutations_df %>%
    mutate(DE_union_obs_log10 = log10(DE_union_obs + 1)) %>% 
    mutate(DR_obs_log10 = log10(DR_obs + 1)) %>% 
    mutate(DT_obs_log10 = log10(DT_obs + 1)) %>% 
    mutate(DRT_obs_log10 = log10(DRT_obs + 1))
  
  
  model_DR <- lm(DR_obs_log10 ~ DE_union_obs_log10, data = conservation_scores_df)
  model_DT <- lm(DT_obs_log10 ~ DE_union_obs_log10, data = conservation_scores_df)
  model_DRT <- lm(DRT_obs_log10 ~ DE_union_obs_log10, data = conservation_scores_df)
  
  model_DR_simple <- lm(DR_obs ~ DE_union_obs, data = conservation_scores_df)
  model_DT_simple <- lm(DT_obs ~ DE_union_obs, data = conservation_scores_df)
  
  model_DR_log10_0 <- lm(DR_obs_log10 ~ 0 + DE_union_obs_log10, data = conservation_scores_df)
  model_DT_log10_0 <- lm(DT_obs_log10 ~ 0 + DE_union_obs_log10, data = conservation_scores_df)
  
  conservation_scores_df$rank_sm500 <- rank(conservation_scores_df$sm500_obs)
  conservation_scores_df$rank_DE_union_log10 <- rank(conservation_scores_df$DE_union_obs_log10)
  
  model_sm500_DE_union <- lm(sm500_obs ~ DE_union_obs, data = conservation_scores_df)
  model_sm500_DE_union_log10 <- lm(sm500_obs ~ DE_union_obs_log10, data = conservation_scores_df)
  model_sm500_DE_union_log10_rank <- lm(rank_sm500 ~ rank_DE_union_log10, data = conservation_scores_df)
  
  return(list(model_DR = model_DR,
              model_DT = model_DT,
              model_DRT = model_DRT,
              model_DR_simple = model_DR_simple,
              model_DT_simple = model_DT_simple,
              model_DR_log10_0 = model_DR_log10_0,
              model_DT_log10_0 = model_DT_log10_0,
              model_sm500_DE_union = model_sm500_DE_union,
              model_sm500_DE_union_log10 = model_sm500_DE_union_log10,
              model_sm500_DE_union_log10_rank = model_sm500_DE_union_log10_rank))
}
