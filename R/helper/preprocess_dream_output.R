preprocess_dream_output <- function (dream_output, p_value_threshold) {

dream_output_concise <- dream_output[[1]] %>%
  as.data.frame() %>%
  filter(gene != dream_output[["target_TF"]]) %>%
  select(-AveExpr, -t, -P.Value, -B) %>%
  pivot_wider(
    id_cols = gene,
    names_from = contrast,
    values_from = c(adj.P.Val, CI.L, CI.R, logFC)
  ) %>%
  mutate(
    DE_human = adj.P.Val_human < p_value_threshold,
    DE_cyno = adj.P.Val_cynomolgus < p_value_threshold,
    DE_union = DE_human | DE_cyno,
    DR = DE_union & adj.P.Val_interaction < p_value_threshold,
    DE_in_both = DE_human & DE_cyno,
    logFC_cyno_in_CI_human = logFC_cynomolgus >= CI.L_human & logFC_cynomolgus <= CI.R_human,
    logFC_human_in_CI_cyno = logFC_human >= CI.L_cynomolgus & logFC_human <= CI.R_cynomolgus,
    logFC_human_includes_zero = CI.L_human <= 0 & CI.R_human >= 0,
    logFC_cyno_includes_zero = CI.L_cynomolgus <= 0 & CI.R_cynomolgus >= 0,
    same_direction = logFC_human * logFC_cynomolgus > 0
  ) %>%
  mutate(
    DT = case_when(
      (DE_cyno | DE_human) & !same_direction ~ TRUE,
      DE_cyno & DE_human & same_direction ~ FALSE,
      (DE_cyno & !logFC_human_in_CI_cyno) | (DE_human & !logFC_cyno_in_CI_human) ~ TRUE,
      TRUE ~ FALSE
    ),
    DT_strict = case_when(
      (DE_cyno | DE_human) & !same_direction ~ TRUE,
      (DE_cyno & !logFC_human_in_CI_cyno & !logFC_cyno_in_CI_human) |
        (DE_human & !logFC_human_in_CI_cyno & !logFC_cyno_in_CI_human) ~ TRUE,
      TRUE ~ FALSE
    ),
    DT_open = case_when(
      (DE_cyno | DE_human) & !same_direction ~ TRUE,
      (!logFC_human_in_CI_cyno & !logFC_cyno_in_CI_human &
         !logFC_human_includes_zero & !logFC_cyno_includes_zero) ~ TRUE,
      TRUE ~ FALSE
    ),
    DR_not_DT = DR & !DT,
    DT_not_DR = DT & !DR,
    DRT = DR & DT
  )

  return(dream_output_concise)

}
