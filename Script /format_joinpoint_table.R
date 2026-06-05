# ============================================================
# FORMAT JOINPOINT RESULTS INTO PUBLICATION-READY TABLE
# Supplementary Table 3
# ============================================================

library(tidyverse)

jp <- read_csv("Results/Table_Joinpoint_Summary.csv", show_col_types = FALSE)

# ── Clean labels ─────────────────────────────────────────────
jp_clean <- jp %>%
  mutate(
    disease = case_when(
      disease == "Enteric infections"                          ~ "Enteric Infections",
      disease == "Lower respiratory infections"                ~ "Lower Respiratory Infections",
      disease == "Upper respiratory infections"                ~ "Upper Respiratory Infections",
      disease == "Neglected tropical diseases and malaria"     ~ "NTD & Malaria",
      TRUE ~ disease
    ),
    measure = case_when(
      measure == "Incidence"                                   ~ "Incidence",
      measure == "Deaths"                                      ~ "Mortality",
      measure == "DALYs (Disability-Adjusted Life Years)"     ~ "DALYs",
      TRUE ~ measure
    ),
    APC_fmt = sprintf("%+.2f%%", APC),
    # Flag noteworthy segments in bold notation
    note = case_when(
      disease == "Enteric Infections" & period == "2015-2023" ~ "Recent upturn",
      disease == "NTD & Malaria"      & measure == "Incidence" & n_joinpoints == 0 ~ "No inflection — gradual signal",
      disease == "Lower Respiratory Infections" & period == "1998-1999" ~ "Sharp decline (PCV era)",
      TRUE ~ ""
    )
  ) %>%
  # Order diseases and measures logically
  mutate(
    disease = factor(disease, levels = c(
      "Enteric Infections", "Lower Respiratory Infections",
      "Upper Respiratory Infections", "NTD & Malaria"
    )),
    measure = factor(measure, levels = c("Incidence", "Mortality", "DALYs"))
  ) %>%
  arrange(disease, measure) %>%
  # For display: only show disease/measure/n_joinpoints on first row of each group
  group_by(disease, measure) %>%
  mutate(
    row_num       = row_number(),
    disease_disp  = ifelse(row_num == 1 & measure == first(measure[row_num == 1]),
                           as.character(disease), ""),
    measure_disp  = ifelse(row_num == 1, as.character(measure), ""),
    n_jp_disp     = ifelse(row_num == 1, as.character(n_joinpoints), "")
  ) %>%
  ungroup() %>%
  dplyr::select(
    "Disease"     = disease_disp,
    "Measure"     = measure_disp,
    "Joinpoints"  = n_jp_disp,
    "Period"      = period,
    "APC"         = APC_fmt,
    "Direction"   = direction,
    "Note"        = note
  )

# ── Print to console ─────────────────────────────────────────
cat("=======================================================\n")
cat("SUPPLEMENTARY TABLE 3: Joinpoint Regression Results\n")
cat("=======================================================\n\n")
print(as.data.frame(jp_clean), row.names = FALSE)

# ── Save formatted CSV ───────────────────────────────────────
write_csv(jp_clean, "Results/SupplementaryTable3_Joinpoint_Results.csv")
cat("\nSaved: Results/SupplementaryTable3_Joinpoint_Results.csv\n")

# ── Also save a compact version grouped by disease/measure ───
# This version is what goes directly into the manuscript appendix
cat("\n\n=======================================================\n")
cat("COMPACT VIEW (for manuscript)\n")
cat("=======================================================\n\n")

jp_compact <- jp %>%
  mutate(
    disease = case_when(
      disease == "Enteric infections"                       ~ "Enteric Infections",
      disease == "Lower respiratory infections"             ~ "Lower Respiratory Infections",
      disease == "Upper respiratory infections"             ~ "Upper Respiratory Infections",
      disease == "Neglected tropical diseases and malaria"  ~ "NTD & Malaria",
      TRUE ~ disease
    ),
    measure = case_when(
      measure == "Incidence"                               ~ "Incidence",
      measure == "Deaths"                                  ~ "Mortality",
      measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ measure
    )
  ) %>%
  mutate(
    disease = factor(disease, levels = c(
      "Enteric Infections","Lower Respiratory Infections",
      "Upper Respiratory Infections","NTD & Malaria")),
    measure = factor(measure, levels = c("Incidence","Mortality","DALYs"))
  ) %>%
  arrange(disease, measure) %>%
  group_by(disease, measure) %>%
  summarise(
    n_jp      = first(n_joinpoints),
    segments  = paste0(period, " (APC ", sprintf("%+.2f", APC), "%)", collapse = "; "),
    .groups   = "drop"
  ) %>%
  dplyr::select(Disease = disease, Measure = measure,
                `No. Joinpoints` = n_jp, `Segments (Period: APC)` = segments)

print(as.data.frame(jp_compact), row.names = FALSE)
write_csv(jp_compact, "Results/SupplementaryTable3_Joinpoint_Compact.csv")
cat("\nSaved: Results/SupplementaryTable3_Joinpoint_Compact.csv\n")
cat("\nDone.\n")
