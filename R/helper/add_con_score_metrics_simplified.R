add_con_score_metrics_simplified <- function(permutation_outputs_df, downsampled, reference_TF_df = NULL) {
    
    permutations_df_summarized <- permutation_outputs_df %>% 
      mutate(permuted = ifelse(permutation > 0, "div", "obs")) %>% 
      group_by(permuted, target_TF, n_human_pert, n_cyno_pert) %>% 
      summarise(n_human_pert = mean(n_human_pert), n_cyno_pert = mean(n_cyno_pert), DR = mean(n_DR), sd_DR = sd(n_DR),  DE_union = mean(n_DE_union), sd_DE_union = sd(n_DE_union), DE_sum = mean(n_DE_sum), DE_human = mean(n_DE_human), DE_cyno = mean(n_DE_cyno), n_permutation = dplyr::n(), n_DR_not_DE = mean(n_DR_not_DE), DT = mean(n_DT), DR_not_DT = mean(n_DR_not_DT), DT_not_DR = mean(n_DT_not_DR), DRT = mean(n_DRT), DT_strict = mean(n_DT_strict), DT_open = mean(n_DT_open), sd_DT = sd(n_DT), sd_sm500 = sd(sm500), sm500 = mean(sm500), sm1000 = mean(sm1000), pe500 = mean(pe500), pe1000 = mean(pe1000), cor500_union_size = mean(cor500_union_size), cor1000_union_size = mean(cor1000_union_size),sm500_fix=mean(sm500_fix),sm1000_fix=mean(sm1000_fix),pe500_fix=mean(pe500_fix),pe1000_fix=mean(pe1000_fix), n_cor_fix_rank_500=mean(n_cor_fix_rank_500),n_cor_fix_rank_1000=mean(n_cor_fix_rank_1000),  .groups = "drop") #smDE = mean(smDE), peDE = mean(peDE),
    
    combined_permutations_df <- permutations_df_summarized %>% 
      pivot_wider(id_cols = c("target_TF", "n_human_pert", "n_cyno_pert"), names_from = c(permuted), values_from = c(DR,DE_union,DE_sum,DE_human,DE_cyno, sd_DR, sd_DE_union, n_DR_not_DE, DT, DRT, DT_strict,DT_open,sd_DT,sm500,sm1000,pe500,pe1000, cor500_union_size,cor1000_union_size,sm500_fix,sm1000_fix,pe500_fix,pe1000_fix, n_cor_fix_rank_500,n_cor_fix_rank_1000,sd_sm500, DT_not_DR, DR_not_DT)) #, smDE, peDE

  conservation_scores_df <- combined_permutations_df %>%
    #Interesting stats
    mutate(
      DE_ratio_between_species = ifelse(
        pmin(DE_human_obs, DE_cyno_obs) == 0,
        NA, 
        pmax(DE_human_obs, DE_cyno_obs) / pmin(DE_human_obs, DE_cyno_obs)
      )
    ) %>% 
    # Summarizations
    mutate(DE_union_obs_log10 = log10(DE_union_obs + 1)) %>% 
    mutate(DR_obs_log10 = log10(DR_obs + 1)) %>% 
    mutate(DT_obs_log10 = log10(DT_obs + 1)) %>% 
    mutate(DRT_obs_log10 = log10(DRT_obs +1)) %>% 
    mutate(DE_union_log10 = log10(DE_union_obs + 1)) %>% 
    mutate(DR_log10 = log10(DR_obs + 1)) %>% 
    mutate(DT_log10 = log10(DT_obs + 1)) %>% 
    mutate(DE_difference = abs(DE_human_obs - DE_cyno_obs)) %>%
    mutate(DRT_log10 = DRT_obs_log10) %>% 
    # Union Scores
    mutate(DE_union_score = (DE_union_obs - DR_obs) / DE_union_obs) %>% 
    mutate(DE_union_log10_score = (DE_union_obs_log10 - DR_obs_log10) / DE_union_obs_log10) %>%
    mutate(DE_union_log10_x_score = (DE_union_obs_log10 - DR_obs) / DE_union_obs_log10) %>%
    mutate(DT_DE_union_score = (DE_union_obs - DT_obs) / DE_union_obs) %>% 
    mutate(DT_DE_union_log10_score = (DE_union_obs_log10 - DT_obs_log10) / DE_union_obs_log10) %>% 
    mutate(DT_DE_union_log10_x_score = (DE_union_obs_log10 - DT_obs) / DE_union_obs_log10) %>% 
    # Union Perm Scores
    # Correlations
    mutate(sm500_score = sm500_obs) %>% 
    mutate(sm1000_score = sm1000_obs) %>% 
    mutate(pe500_score = pe500_obs) %>% 
    mutate(pe1000_score = pe1000_obs) %>% 
    # Correlations_fix
    # Correlations
    mutate(sm500_fix_score = sm500_fix_obs) %>% 
    mutate(sm1000_fix_score = sm1000_fix_obs) %>% 
    mutate(pe500_fix_score = pe500_fix_obs) %>% 
    mutate(pe1000_fix_score = pe1000_fix_obs)
    
    
    # Linear models
    if (downsampled) {
      
      DR_DT_model_fit <- get_DR_DT_model(reference_TF_df)
      
      # Log10 - not through 0
      conservation_scores_df$predicted_DR_log10 <- predict(DR_DT_model_fit$model_DR, newdata = conservation_scores_df)
      conservation_scores_df$DE_union_log10_lm_score <- conservation_scores_df$DR_obs_log10 - conservation_scores_df$predicted_DR_log10
      conservation_scores_df$predicted_DT_log10 <- predict(DR_DT_model_fit$model_DT, newdata = conservation_scores_df)
      conservation_scores_df$DT_DE_union_log10_lm_score <- conservation_scores_df$DT_obs_log10 - conservation_scores_df$predicted_DT_log10
      
      # simple - not through 0
      conservation_scores_df$predicted_DR <- predict(DR_DT_model_fit$model_DR_simple, newdata = conservation_scores_df)
      conservation_scores_df$DE_union_lm_score <- conservation_scores_df$DR_obs - conservation_scores_df$predicted_DR
      conservation_scores_df$predicted_DT <- predict(DR_DT_model_fit$model_DT_simple, newdata = conservation_scores_df)
      conservation_scores_df$DT_DE_union_lm_score <- conservation_scores_df$DT_obs - conservation_scores_df$predicted_DT
      
      # log10 - through 0
      conservation_scores_df$predicted_DR_log10_0 <- predict(DR_DT_model_fit$model_DR_log10_0, newdata = conservation_scores_df)
      conservation_scores_df$DE_union_log10_0_lm_score <- conservation_scores_df$DR_obs_log10 - conservation_scores_df$predicted_DR_log10_0
      conservation_scores_df$predicted_DT_log10_0 <- predict(DR_DT_model_fit$model_DT_log10_0, newdata = conservation_scores_df)
      conservation_scores_df$DT_DE_union_log10_0_lm_score <- conservation_scores_df$DT_obs_log10 - conservation_scores_df$predicted_DT_log10_0
      
      
    } else {
      
      # conservation_scores_df$predicted_DR_log10 <- predict(model_DR, newdata = conservation_scores_df)
      # conservation_scores_df$DE_union_log10_lm_score <- conservation_scores_df$DR_obs_log10 - conservation_scores_df$predicted_DR_log10
      # conservation_scores_df$predicted_DT_log10 <- predict(model_DT, newdata = conservation_scores_df)
      # conservation_scores_df$DT_DE_union_log10_lm_score <- conservation_scores_df$DT_obs_log10 - conservation_scores_df$predicted_DT_log10
      
      DR_DT_model_fit <- get_DR_DT_model(permutation_outputs_df)
      
      conservation_scores_df$predicted_DR_log10 <- predict(DR_DT_model_fit$model_DR, newdata = conservation_scores_df)
      conservation_scores_df$DE_union_log10_lm_score <- conservation_scores_df$DR_obs_log10 - conservation_scores_df$predicted_DR_log10
      conservation_scores_df$predicted_DT_log10 <- predict(DR_DT_model_fit$model_DT, newdata = conservation_scores_df)
      conservation_scores_df$DT_DE_union_log10_lm_score <- conservation_scores_df$DT_obs_log10 - conservation_scores_df$predicted_DT_log10
      
      conservation_scores_df$predicted_DRT_log10 <- predict(DR_DT_model_fit$model_DRT, newdata = conservation_scores_df)
      conservation_scores_df$DRT_DE_union_log10_lm_score <- conservation_scores_df$DRT_obs_log10 - conservation_scores_df$predicted_DRT_log10
      
      
      conservation_scores_df$predicted_DR <- predict(DR_DT_model_fit$model_DR_simple, newdata = conservation_scores_df)
      conservation_scores_df$DE_union_lm_score <- conservation_scores_df$DR_obs - conservation_scores_df$predicted_DR
      conservation_scores_df$predicted_DT <- predict(DR_DT_model_fit$model_DT_simple, newdata = conservation_scores_df)
      conservation_scores_df$DT_DE_union_lm_score <- conservation_scores_df$DT_obs - conservation_scores_df$predicted_DT
      
      conservation_scores_df$predicted_DR_log10_0 <- predict(DR_DT_model_fit$model_DR_log10_0, newdata = conservation_scores_df)
      conservation_scores_df$DE_union_log10_0_lm_score <- conservation_scores_df$DR_obs_log10 - conservation_scores_df$predicted_DR_log10_0
      conservation_scores_df$predicted_DT_log10_0 <- predict(DR_DT_model_fit$model_DT_log10_0, newdata = conservation_scores_df)
      conservation_scores_df$DT_DE_union_log10_0_lm_score <- conservation_scores_df$DT_obs_log10 - conservation_scores_df$predicted_DT_log10_0

      # Adding outlier assignment
      conservation_scores_df <- data.frame(conservation_scores_df) %>%
        dplyr::mutate(DE_union_log10_lm_outlier = case_when(DE_union_log10_lm_score >  mean(DE_union_log10_lm_score) + 2 * sd(DE_union_log10_lm_score) ~ "diverged",
                                                            DE_union_log10_lm_score < mean(DE_union_log10_lm_score) - 2 * sd(DE_union_log10_lm_score) ~ "conserved",
                                                            TRUE ~ "none")) %>%
        dplyr::mutate(DT_DE_union_log10_lm_outlier = case_when(DT_DE_union_log10_lm_score >  mean(DT_DE_union_log10_lm_score) + 2 * sd(DT_DE_union_log10_lm_score) ~ "diverged",
                                                               DT_DE_union_log10_lm_score < mean(DT_DE_union_log10_lm_score) - 2 * sd(DT_DE_union_log10_lm_score) ~ "conserved",
                                                               TRUE ~ "none")) %>% 
        dplyr::mutate(DRT_DE_union_log10_lm_outlier = case_when(DRT_DE_union_log10_lm_score >  mean(DRT_DE_union_log10_lm_score) + 2 * sd(DRT_DE_union_log10_lm_score) ~ "diverged",
                                                               DRT_DE_union_log10_lm_score < mean(DRT_DE_union_log10_lm_score) - 2 * sd(DRT_DE_union_log10_lm_score) ~ "conserved",
                                                               TRUE ~ "none"))
        
    }
  
  # Correlation modeled by DE_union
  conservation_scores_df$predicted_sm500_DE_union <- predict(DR_DT_model_fit$model_sm500_DE_union, newdata = conservation_scores_df)
  conservation_scores_df$sm500_DE_union_lm_score <- conservation_scores_df$sm500_obs - conservation_scores_df$predicted_sm500_DE_union
  
  conservation_scores_df$predicted_sm500_DE_union_log10 <- predict(DR_DT_model_fit$model_sm500_DE_union_log10, newdata = conservation_scores_df)
  conservation_scores_df$sm500_DE_union_log10_lm_score <- conservation_scores_df$sm500_obs - conservation_scores_df$predicted_sm500_DE_union_log10
  
  conservation_scores_df$rank_sm500 <- rank(conservation_scores_df$sm500_obs)
  conservation_scores_df$rank_DE_union_log10  <- rank(conservation_scores_df$DE_union_obs_log10)
  
  conservation_scores_df$predicted_sm500_DE_union_log10_rank <- predict(DR_DT_model_fit$model_sm500_DE_union_log10_rank, newdata = conservation_scores_df)
  conservation_scores_df$sm500_DE_union_log10_lm_rank_score  <- conservation_scores_df$sm500_obs - conservation_scores_df$predicted_sm500_DE_union_log10_rank

  # Add orthogonal residuals
  conservation_scores_df <- compute_tls_residuals(conservation_scores_df, id_col = "target_TF", x_col= "DE_union_log10", y_col="DRT_log10")
  
  
   return(conservation_scores_df)
  
}
  