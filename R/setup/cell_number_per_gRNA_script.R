seu_cell_numbers <- readRDS(here("data", "enrichment", "seu_QCfilt_gRNAfilt.rds"))

cell_numbers_per_gRNA <- seu_cell_numbers@meta.data %>%
  group_by(perturbed_TF, gRNA, species) %>%
  summarise(cell_count = n()) %>%
  arrange(desc(cell_count)) %>% 
  data.frame()

saveRDS(cell_numbers_per_gRNA,here("data", "enrichment", "cell_numbers_per_gRNA.rds"))

cell_numbers_per_gRNA

cell_numbers_per_gRNA <- cell_numbers_per_gRNA %>% 
  filter(perturbed_TF == "SOX4")

