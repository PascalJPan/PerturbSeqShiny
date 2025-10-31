library(GenomicRanges)
library(dplyr)
library(tibble)

gr_to_nanopore_like <- function(gr, keep_types = "exon") {
  # Optionally filter by feature type (e.g., exon)
  if (!is.null(keep_types) && "type" %in% names(mcols(gr))) {
    gr <- gr[mcols(gr)$type %in% keep_types]
  }
  
  df <- as_tibble(as.data.frame(gr))
  
  # safely choose existing columns
  transcript_col <- intersect(c("transcript_id", "transcript", "tx_id"), names(df))[1]
  gene_col       <- intersect(c("gene_name", "gene_symbol", "gene_id"), names(df))[1]
  
  df_out <- df %>%
    transmute(
      chromosome = as.character(seqnames),
      start      = start,
      end        = end,
      width      = width,
      strand     = as.character(strand),
      transcript = if (!is.na(transcript_col)) .data[[transcript_col]] else NA_character_,
      symbol     = if (!is.na(gene_col)) .data[[gene_col]] else NA_character_
    )
  
  df_out
}


hg38_iPSC_np_annot <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/grna_characteristics/hg38/gene_models_old/hg38_iPSC_np_annot.rds")
hg38_iPSC_np_annot_new_format <- gr_to_nanopore_like(hg38_iPSC_np_annot, keep_types = "exon")
saveRDS(hg38_iPSC_np_annot_new_format,"/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/grna_characteristics/hg38/gene_models/Nanopore.rds")

mf6_iPSC_np_annot <- readRDS("/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/grna_characteristics/macFas6/gene_models_old/mf6_iPSC_np_annot.rds")
mf6_iPSC_np_annot_new_format <- gr_to_nanopore_like(mf6_iPSC_np_annot, keep_types = "exon")
saveRDS(mf6_iPSC_np_annot_new_format,"/data/share/htp/perturb-seq/pascal/perturbSeq_shiny/data/grna_characteristics/macFas6/gene_models/Nanopore.rds")
