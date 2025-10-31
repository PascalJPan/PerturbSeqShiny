# knockdown_data        <- readRDS(here("data", "knockdown_efficiency", "tf_de.rds"))
# sig_knockdown_TFs_human      <- dplyr::filter(knockdown_data, species == "human", significant) %>% select(perturbed_TF) %>% pull() %>% unique()
# sig_knockdown_TFs_cynomolgus <- dplyr::filter(knockdown_data, species == "cynomolgus", significant) %>% select(perturbed_TF) %>% pull() %>% unique()
# 
# sig_knockdown_TFs <- intersect(sig_knockdown_TFs_human,sig_knockdown_TFs_cynomolgus)
# setdiff(sig_knockdown_TFs,TF_list_87.rds)
# TF_list_87 is all of the TFs which have a significant knockdown gRNA in both in of the species

# setdiff(TF_list_87,TF_list_81)
# setdiff(TF_list_81,TF_list_87)

# Start
TF_list_94 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_94.rds")
# Filtering Step 1
TF_list_91 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_91.rds")
# Filtering Step 2
# Remove ATF1
# Filtering Step 3
TF_list_87 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_87.rds") 

#TF_list_81 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_81.rds") 
# TF_list_79 <- TF_list_81[TF_list_81 != "CIC" & TF_list_81 != "KLF6"]
# length(TF_list_81)
# length(TF_list_79)

TF_list_79 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_79.rds") 

# Filtering Step 4
TF_list_68 <- readLines("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_68.txt")

TF_filter_information <- data.frame(TF_list_94)
colnames(TF_filter_information) <- "TF"

TF_filter_information <- TF_filter_information %>% 
  mutate(F0 = TRUE) %>% 
  mutate(F1 = TF %in% TF_list_91) %>% 
  mutate(F2 = (F1 & TF != "ATF1")) %>% 
  mutate(F2.5 = (F2 & (TF %in% TF_list_87))) %>% 
  mutate(F3 = (F2.5 & (TF %in% TF_list_79))) %>% # CIC and KLF6 was removed in TF 87 already
  mutate(F4 = (F3 & (TF %in% TF_list_68)))



#Test
colSums(TF_filter_information[2:7])

TF_filter_information <- TF_filter_information %>% 
  mutate(filter_message = "not filtered") %>% 
  mutate(filter_message = case_when(!F1 ~ "The selected TF was removed, due to a low expression in the control cells.",
                                    !F2 ~ "The selected TF was removed, due to too few cells for all gRNAs in at least one species.",
                                    !F2.5 ~ "The selected TF was removed, due to a lack of gRNAs with an efficient knockdown in at least one of the species.",
                                    !F3 ~ "The selected TF was removed, due to a lack of efficient gRNAs after filtering out non pluripotentent cells.",
                                    !F4 ~ "The selected TF was removed, due to too few cells in at least one species.",
                                    TRUE ~ "TF was not removed")) %>% 
  arrange(TF)


rownames(TF_filter_information) <- TF_filter_information$TF
library(here)
saveRDS(TF_filter_information, here("data", "TF_lists", "TF_filter_information.rds"))

