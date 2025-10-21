is_TF_not_filtered_out <- function(TF_filter_information, selected_TF, filtering_level) {
  # This tests against the TF_filter_information if a specific TF is still present for a specific filtering_level or was filtered out
  # under R/setup/creating_TF_filtering_information_script.R the script can be found how the TF_filter_information was build in case there are some changes or related errors
  
  # Ensure filtering_level is valid
  if (!filtering_level %in% c("F0","F1", "F2", "F3", "F4")) {
    stop('filtering_level must be one of "F0","F1", "F2", "F3", or "F4".')
  }
  
  if (!selected_TF %in% TF_filter_information$TF) {
    stop('selected TF was never present in the original 94 TFs.')
  }
  
  return(TF_filter_information[selected_TF,filtering_level])
}
