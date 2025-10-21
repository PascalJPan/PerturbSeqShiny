get_IC <- function(gn, motif_IC) {
  motif_IC %>% 
    dplyr::filter(SYMBOL == gn) %>% 
    dplyr::mutate(text = paste0(motif_id, ": IC = ", format(IC, digits = 3))) %>% 
    pull(text) %>% 
    paste(collapse = "\n")
  
}
