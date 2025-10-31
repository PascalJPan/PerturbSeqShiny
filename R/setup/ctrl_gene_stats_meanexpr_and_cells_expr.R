library(SingleCellExperiment)
library(Matrix)
library(DelayedArray)
library(DelayedMatrixStats)
library(dplyr)
library(tibble)

TF94_combined <- readRDS("/data/share/htp/perturb-seq/TF94_combined/RDS/sce.rds")

# --- helper functions ---
.rowMeans_any <- function(m) {
  if (methods::is(m, "DelayedMatrix")) {
    DelayedMatrixStats::rowMeans2(m)
  } else if (methods::is(m, "Matrix")) {
    Matrix::rowMeans(m)
  } else {
    base::rowMeans(m)
  }
}

.rowSums_any <- function(m) {
  if (methods::is(m, "DelayedMatrix")) {
    DelayedMatrixStats::rowSums2(m)
  } else if (methods::is(m, "Matrix")) {
    Matrix::rowSums(m)
  } else {
    base::rowSums(m)
  }
}

# --- main computation ---
sce <- TF94_combined
cnts <- assay(sce, "counts")

# get gene names
genes <- if ("gene_name" %in% colnames(rowData(sce))) {
  as.character(rowData(sce)$gene_name)
} else {
  rownames(sce)
}

# keep only control cells
ctrl_idx <- colData(sce)$perturbed_TF == "NT_control"
sce_ctrl <- sce[, ctrl_idx]
species_vec <- as.character(colData(sce_ctrl)$species)
species_levels <- unique(species_vec)

# compute stats per species
ctrl_gene_stats_by_species <- bind_rows(lapply(species_levels, function(sp) {
  message("Processing species: ", sp)
  sp_idx <- species_vec == sp
  m <- counts(sce_ctrl)[, sp_idx, drop = FALSE]
  
  tibble(
    gene       = genes,
    species    = sp,
    mean_count = as.numeric(.rowMeans_any(m)),
    frac_expr  = as.numeric(.rowSums_any(m > 0) / ncol(m))
  )
}))

# Optionally save
saveRDS(ctrl_gene_stats_by_species, here("data","expression","ctrl_gene_stats_by_species.rds"))
