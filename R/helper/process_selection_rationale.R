process_selection_rationale <- function(rationale) {
  # # Split the input by ", "
  # rationale_parts <- unlist(strsplit(rationale, ", "))
  # 
  # # Replace "someones_favorite" with "literature search"
  # rationale_parts <- gsub("someones_favorite", "literature search", rationale_parts)
  # 
  # # Replace "tf27", "repeated_tf27", "random" with "random"
  # rationale_parts <- gsub("tf27|repeated_tf27|random", "", rationale_parts)
  # 
  # # Remove underscores
  # rationale_parts <- gsub("_", " ", rationale_parts)
  # 
  # # Remove empty parts (if any)
  # rationale_parts <- rationale_parts[rationale_parts != ""]
  # 
  # # If no valid rationale left, return "random"
  # if (length(rationale_parts) == 0) {
  #   return("random")
  # }
  # 
  # # Join the remaining parts back into a single string
  # return(paste(rationale_parts, collapse = ", "))
  
  
  TF_selection <- rationale %>% 
    tidyr::separate_rows(selection_rationale, sep = ", ") %>% 
    ungroup() %>% 
    dplyr::transmute(TF = gene, selection_rationale = case_when(selection_rationale == "pluripotency_marker" ~ "pluripotency factor",
                                                                selection_rationale == "most_expressed" ~ "highly expressed in iPSCs",
                                                                selection_rationale == "conserved_sequence_AA_phastCons" ~ "conserved sequence", 
                                                                selection_rationale %in% c("diverged_sequence_AA_phastCons", "positive_selection_hominid_lrt","positive_selection_macaque_lrt") ~ "diverged sequence",
                                                                selection_rationale == "conserved_network" ~ "conserved network",
                                                                selection_rationale == "diverged_network" ~ "diverged network",
                                                                selection_rationale == "noKRAB_C2H2_ZnF" ~ "non-KRAB C2H2 zinc finger",
                                                                selection_rationale == "KRAB_C2H2_ZnF" ~ "KRAB C2H2 zinc finger",
                                                                selection_rationale == "universal_stripe_factor" ~ "universal stripe factor",
                                                                selection_rationale == "someones_favorite" ~ "literature search",
                                                                T ~ "other")) %>% 
    distinct(TF, selection_rationale) %>% 
    group_by(TF) %>% 
    dplyr::filter(length(selection_rationale) == 1 | selection_rationale != "other") %>% 
    dplyr::summarise(selection_rationale = paste(selection_rationale, collapse = ", ")) %>% 
    ungroup()
  
  return(TF_selection)
}