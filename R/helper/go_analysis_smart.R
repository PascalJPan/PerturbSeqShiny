go_analysis_smart <- function (
    gene_list_of_interest,
    gene_list_background,
    node_number = 10,
    pvalueCutoff = 0.05,
    qvalueCutoff = 0.2,
    min_background_genes = 10,   # min Annotated in background
    min_interest_hits   = 2      # min hits from interest set
) {
  # map SYMBOL -> ENTREZ
  #cat("Example gene list:", head(gene_list_of_interest), "\n")
  entrez_of_interest <- clusterProfiler::bitr(
    gene_list_of_interest, fromType = "SYMBOL", toType = "ENTREZID",
    OrgDb = org.Hs.eg.db::org.Hs.eg.db
  )
  
  #cat("Example gene list background:", head(gene_list_background), "\n")
  entrez_all <- clusterProfiler::bitr(
    gene_list_background, fromType = "SYMBOL", toType = "ENTREZID",
    OrgDb = org.Hs.eg.db::org.Hs.eg.db
  )
  
  entrez_of_interest <- unique(na.omit(entrez_of_interest$ENTREZID))
  entrez_all        <- unique(na.omit(entrez_all$ENTREZID))
  if (length(entrez_of_interest) == 0L || length(entrez_all) == 0L) return(NULL)
  
  # ---- Build custom gene2GO (BP) for the *background* only ----
  suppressPackageStartupMessages({
    library(AnnotationDbi)
    library(org.Hs.eg.db)
    library(GO.db)
    library(topGO)
  })
  
  ann <- AnnotationDbi::select(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = entrez_all,
    keytype = "ENTREZID",
    columns = c("GO", "ONTOLOGY")
  )
  ann <- ann[ann$ONTOLOGY == "BP" & !is.na(ann$GO), c("ENTREZID", "GO")]
  if (nrow(ann) == 0L) return(NULL)
  
  # gene2GO list for background
  gene2GO_bg <- tapply(ann$GO, ann$ENTREZID, unique)
  # convenience: reverse map GO -> genes (background universe)
  go2genes_bg <- split(ann$ENTREZID, ann$GO)
  
  # Count per-term background size and interest hits
  interest_set <- intersect(entrez_of_interest, entrez_all)
  annotated_counts <- vapply(go2genes_bg, length, integer(1))
  hit_counts <- vapply(go2genes_bg, function(v) sum(v %in% interest_set), integer(1))
  
  # Keep only GO terms meeting both thresholds
  keep_go <- names(which(annotated_counts >= min_background_genes & hit_counts >= min_interest_hits))
  if (length(keep_go) == 0L) {
    message("No GO terms satisfy Annotated ≥ ", min_background_genes,
            " and Hits ≥ ", min_interest_hits, " before testing.")
    return(NULL)
  }
  
  # Prune gene2GO to allowed terms only
  gene2GO_bg_pruned <- lapply(gene2GO_bg, function(goids) intersect(goids, keep_go))
  gene2GO_bg_pruned <- gene2GO_bg_pruned[lengths(gene2GO_bg_pruned) > 0L]
  
  # ---- Prepare topGOdata using the pruned map (true pre-test pruning) ----
  geneList <- factor(as.integer(names(gene2GO_bg_pruned) %in% interest_set))
  names(geneList) <- names(gene2GO_bg_pruned)
  
  message("start go output function")
  goOutput <- new("topGOdata",
                  ontology = "BP",
                  allGenes = geneList,
                  geneSelectionFun = function(x) x == 1,
                  annot = annFUN.gene2GO,
                  gene2GO = gene2GO_bg_pruned,
                  # nodeSize no longer required for background ≥ threshold (we already pruned),
                  # but harmless to leave at 0
                  nodeSize = 0)
  message("end go output function")
  
  message("start resultElim function")
  resultElim <- runTest(goOutput, algorithm = "elim", statistic = "fisher")
  message("end resultElim function")
  
  # significant terms?
  pvals <- score(resultElim)
  sig_terms <- sum(pvals < 0.05, na.rm = TRUE)
  if (sig_terms == 0) {
    message("No significant GO terms found after testing.")
    return(NULL)
  }
  
  topNodes <- min(node_number, sig_terms)
  df <- GenTable(goOutput, Fisher.elim = resultElim, topNodes = topNodes)
  
  # full term names & tidy columns
  df$Term <- AnnotationDbi::Term(GO.db::GOTERM[df$GO.ID])
  df$Fisher.elim <- as.numeric(gsub("<", "", df$Fisher.elim))
  df$Term <- factor(df$Term, levels = unique(df$Term[order(df$Fisher.elim)]))
  df$GeneRatio <- round(df$Significant / df$Annotated,2)
  
  df <- df[df$Annotated >= min_background_genes & df$Significant >= min_interest_hits, , drop = FALSE]
  if (!is.null(pvalueCutoff)) df <- df[df$Fisher.elim <= pvalueCutoff, , drop = FALSE]
  
  return(df)
}
