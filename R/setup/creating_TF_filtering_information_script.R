
# Start
TF_list_94 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_94.rds")
# Filtering Step 1
TF_list_91 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_91.rds")
# Filtering Step 2
# Remove ATF1
# Filtering Step 3
# TF_list_87 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_87.rds") #length is 88 but "NT_control" is still in there
# TF_list_87 <- TF_list_87[TF_list_87 != "SMARCC2" & TF_list_87 != "SREBF2" ] #& 

TF_list_81 <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_81.rds") 

# Filtering Step 4
TF_list_68 <- readLines("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/TF_lists/TF_list_68.txt")

TF_filter_information <- data.frame(TF_list_94)
colnames(TF_filter_information) <- "TF"

TF_filter_information <- TF_filter_information %>% 
  mutate(F0 = TRUE) %>% 
  mutate(F1 = TF %in% TF_list_91) %>% 
  mutate(F2 = (F1 & TF != "ATF1")) %>% 
  mutate(F3 = (F2 & (TF %in% TF_list_81))) %>% 
  mutate(F4 = (F3 & (TF %in% TF_list_68)))

#Test
colSums(TF_filter_information[2:6])

TF_filter_information <- TF_filter_information %>% 
  mutate(filter_message = "not filtered") %>% 
  mutate(filter_message = case_when(!F1 ~ "The selected TF was removed, due to a low expression in the control cells.",
                                    !F2 ~ "The selected TF was removed, due to no gRNAs left after filtering all gRNAs with too few cells.",
                                    !F3 ~ "The selected TF was removed, due to a lack of gRNAs with an efficient knockdown in at least one of the species.",
                                    !F4 ~ "The selected TF was removed, due to too few cells in at least one species.",
                                    TRUE ~ "TF was not removed")) %>% 
  arrange(TF)


rownames(TF_filter_information) <- TF_filter_information$TF
library(here)
saveRDS(TF_filter_information, here("data", "TF_lists", "TF_filter_information.rds"))

