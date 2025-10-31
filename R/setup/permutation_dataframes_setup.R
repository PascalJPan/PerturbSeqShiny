# We are saving permutation summaries so that it does not need to be loaded every time in the application as it is quite resource intensive

# Loading Libraries
library(tidyverse)
library(dplyr)

sapply(list.files(pattern="[.]R$", path="/data/share/htp/perturb-seq/pascal/functions", full.names=TRUE), function(i){
  print(i)
  source(i, echo = F, print.eval = F, verbose = F)
});

sapply(list.files(pattern="[.]R$", path="/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/R/helper", full.names=TRUE), function(i){
  print(i)
  source(i, echo = F, print.eval = F, verbose = F)
});

select <- dplyr::select

# Loading permutations

target_TF_id_list_TF68 = readLines("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_68.txt")
output_path_folder_TF68="/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/dream_output/500_2000/"
run_name_TF68="TF68_downsampl_dream"

# Specify the amount of available permutations in permutation_number_vector
permutation_outputs_list_TF68 <- pull_permutation_output_lists(output_path_folder_TF68, target_TF_id_list_TF68, run_name_TF68, permutation_number_vector = seq(0,50,by = 1), print=FALSE, warning=TRUE)

permutation_outputs_df_TF68 <- summarise_metrics(permutation_outputs_list_TF68, p_value_threshold = 0.05)

saveRDS(permutation_outputs_df_TF68,"/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/dream_output/TF_68_summarised_metrics_500_2000_perm_included.rds")

cs_TFs_unfiltered <- add_con_score_metrics(permutation_outputs_df_TF68, downsampled=FALSE, reference_TF_df = NULL)

columns_to_keep <- c(colnames(cs_TFs_unfiltered)[1:29],colnames(cs_TFs_unfiltered)[80:82]) 
# this is prone to errors a bit, recheck with the colnames which are saved already first
TF_ds_perm_score_metrics_68 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/dream_output/TF_ds_perm_score_metrics_68.rds")

if (!all(colnames(TF_ds_perm_score_metrics_68) == columns_to_keep)) {
  stop("The colnames of the prior saved object do not match the selected colnames")
}

saveRDS(cs_TFs_unfiltered[,columns_to_keep],"/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/dream_output/TF_ds_perm_score_metrics_68.rds")
