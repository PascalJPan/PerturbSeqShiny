seu_cell_numbers <- readRDS(here("data", "enrichment", "seu_QCfilt_gRNAfilt.rds"))

library(dplyr)

cell_numbers_per_gRNA <- seu_cell_numbers@meta.data %>%
  as_tibble() %>%
  count(perturbed_TF, gRNA, species, name = "cell_count") %>%
  arrange(desc(cell_count))

saveRDS(cell_numbers_per_gRNA,here("data", "enrichment", "cell_numbers_per_gRNA.rds"))

cell_numbers_per_gRNA_and_individual <- seu_cell_numbers@meta.data %>%
  as_tibble() %>%
  count(perturbed_TF, gRNA, species, individual, name = "cell_count") %>%
  arrange(desc(cell_count))

saveRDS(cell_numbers_per_gRNA_and_individual,here("data", "enrichment", "cell_numbers_per_gRNA_and_individual.rds"))



