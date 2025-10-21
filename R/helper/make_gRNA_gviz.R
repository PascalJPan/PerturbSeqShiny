make_gRNA_gviz <- function(
    gn, gen, offset, gene_models, tss, grnas,
    atacBW, nanoporeBAM, gviz_focus, color = "#D62728"
) {
      
  # Set chromosome naming style
  options(ucscChromosomeNames = FALSE)
  
  # Filter and split gRNAs by library inclusion
  grnas_filt <- grnas %>%
    plyranges::filter(gene == gn) %>%
    dplyr::mutate(is_in_final_lib = factor(is_in_final_lib, levels = c(FALSE, TRUE)))
  seqlevelsStyle(grnas_filt) <- "NCBI"
  gRNA_data_split <- split(grnas_filt, as_tibble(grnas_filt)$is_in_final_lib)
  
  # Extract TSS for the selected gene
  tss_filt <- tss[["final"]] %>%
    plyranges::filter(gene_name == gn) %>%
    arrange(rnk)
  
  if (gen != "hg38") {
    rbb_tss_filt <- tss[["rbb"]] %>%
      plyranges::filter(gene_name == gn) %>%
      arrange(tss_id)
  }
  
  # Filter gene models for the selected gene
  gene_models_filt <- lapply(gene_models, function(gm) {
    gm %>%
      dplyr::filter(symbol == gn) %>%
      dplyr::mutate(chromosome = gsub("chr", "", chromosome))
  })
  names(gene_models_filt) <- names(gene_models)
  
  # Determine chromosome source
  if (nrow(as_tibble(gene_models_filt[["Nanopore"]])) > 0) {
    chr <- as.character(unique(as_tibble(gene_models_filt[["Nanopore"]])$chromosome))
  } else if (gen == "hg38") {
    chr <- as.character(unique(as_tibble(gene_models_filt[["GENCODE"]])$chromosome))
  } else {
    chr <- as.character(unique(as_tibble(gene_models_filt[["Liftoff"]])$chromosome))
  }
  
  # Define region boundaries
  get_start <- function(gr) as_tibble(gr)$start
  get_end <- function(gr) as_tibble(gr)$end
  
  if (gviz_focus == "tss" && nrow(as_tibble(tss_filt)) > 0) {
    min_pos <- min(as_tibble(tss_filt)$start)
    max_pos <- max(as_tibble(tss_filt)$end)
  } else {
    min_pos <- lapply(c(list(tss_filt), gene_models_filt), get_start) %>% unlist() %>% min(na.rm = TRUE)
    max_pos <- lapply(c(list(tss_filt), gene_models_filt), get_end) %>% unlist() %>% max(na.rm = TRUE)
  }
  
  x_from <- min_pos - offset
  x_to <- max_pos + offset
  
  # Genome axis
  axis <- Gviz::GenomeAxisTrack(genome = gen)
  
  # Gene annotation tracks
  gene_model_tracks <- lapply(names(gene_models_filt), function(name) {
    Gviz::GeneRegionTrack(
      gene_models_filt[[name]],
      chromosome = chr, genome = gen,
      showId = TRUE, geneSymbol = TRUE,
      name = paste0(name, "\ntranscripts"),
      fontsize = 10, cex.title = 0.8,
      fill = "darkblue", col = "darkblue",
      col.axis = "black", col.title = "black"
    )
  })
  names(gene_model_tracks) <- names(gene_models_filt)
  
  # Nanopore read track
  nanopore_track <- Gviz::AlignmentsTrack(
    nanoporeBAM, chromosome = chr, genome = gen,
    name = "Nanopore\nreads",
    height = 0.2, coverageHeight = 0.1, minCoverageHeight = 0,
    window = -1, windowSize = 100,
    cex.title = 0.8, col.axis = "black", col.title = "black"
  )
  
  # ATAC-seq tracks
  .atac_label <- function(nm) paste0("ATAC-seq\n", tools::file_path_sans_ext(basename(nm)))
  atac_files <- unname(atacBW)
  
  atac_tracks <- lapply(seq_along(atac_files), function(i) {
    f <- atac_files[[i]]
    Gviz::DataTrack(
      range = f, chromosome = chr, type = "h",
      name = .atac_label(f),
      col = "sienna3", col.axis = "black",
      col.title = "black", cex.title = 0.8
    )
  })
  
  # Highlight RBB TSS on Liftoff track
  if (gen != "hg38" && nrow(as_tibble(rbb_tss_filt)) > 0) {
    n_rbb <- nrow(as_tibble(rbb_tss_filt))
    gene_model_tracks[["Liftoff"]] <- Gviz::HighlightTrack(
      trackList = gene_model_tracks[["Liftoff"]],
      start = start(rbb_tss_filt) - 10,
      end = end(rbb_tss_filt) + 10,
      chromosome = chr,
      fill = rep(color, length.out = n_rbb),
      col = rep(color, length.out = n_rbb)
    )
  }
  
  # Highlight TSS regions on tracks
  if (nrow(as_tibble(tss_filt)) > 0) {
    n_tss <- nrow(as_tibble(tss_filt))
    tss_track <- Gviz::HighlightTrack(
      trackList = c(gene_model_tracks[names(gene_model_tracks) != "Liftoff"], nanopore_track, atac_tracks),
      start = start(tss_filt) - 10,
      end = end(tss_filt) + 10,
      chromosome = chr,
      fill = rep(color, length.out = n_tss),
      col = rep(color, length.out = n_tss)
    )
  } else {
    tss_track <- c(gene_model_tracks[names(gene_model_tracks) != "Liftoff"], nanopore_track, atac_tracks)
  }
  
  # gRNA annotation tracks
  if (nrow(as_tibble(grnas_filt)) > 0) {
    gRNA_ann_tracks <- lapply(names(gRNA_data_split), function(n) {
      gr <- gRNA_data_split[[n]]
      strand(gr) <- S4Vectors::Rle("+")
      col_base <- if (n == "TRUE") color else "grey70"
      col_alpha <- adjustcolor(col_base, alpha.f = 0.4)
      
      Gviz::AnnotationTrack(
        gr, chromosome = chr, genome = gen,
        name = if (n == "TRUE") "gRNAs (final)" else "gRNAs (other)",
        shape = "line", stacking = "dense",
        col = col_alpha, fill = col_alpha,
        cex.title = 0.8, col.axis = "black", col.title = "black"
      )
    })
    grna_track <- Gviz::OverlayTrack(gRNA_ann_tracks, name = "Designed gRNAs")
  } else {
    grna_track <- NULL
  }
  
  # Combine tracks
  track.list <- c(
    axis,
    if (!is.null(gene_model_tracks[["Liftoff"]])) gene_model_tracks[["Liftoff"]],
    tss_track,
    grna_track
  )
  
  # Adjust track sizes
  n_tracks <- length(track.list)
  sizes <- rep(1, n_tracks)
  sizes[1] <- 0.7
  sizes[which(sapply(track.list, inherits, "HighlightTrack"))] <- 3
  
  if (length(gene_model_tracks) == 2) {
    start_sizes <- c(2.4, 1.2)
  } else {
    start_sizes <- c(1.2, 1.2, 1.2)
  }
  
  sizes <- c(0.7, start_sizes, 3, rep(1.2, length(atac_tracks) + !is.null(grna_track)))
  
  # Plot
  Gviz::plotTracks(
    track.list,
    collapseTranscripts = FALSE, shape = "arrow",
    from = x_from, to = x_to,
    title.width = 1.1, col.grid = "grey",
    sizes = sizes, fontsize = 11,
    main = paste0("chromosome ", chr), cex.main = 1
  )

}
