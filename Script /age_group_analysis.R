library(tidyverse)

# Load data
data <- read_csv("Data/IHME-GBD_2023_DATA-c62f4bc4-1.csv", show_col_types = FALSE)

us_data <- data %>%
  filter(location_name == "United States of America",
         age_name %in% c("<5 years", "5-9 years", "10-14 years"),
         sex_name == "Both")

cat("===============================================================\n")
cat("AGE-SPECIFIC KEY FINDINGS (2023)\n")
cat("===============================================================\n\n")

# ==================== INCIDENCE ====================
cat("1. INCIDENCE RATES BY AGE GROUP\n")
cat("---------------------------------------------------------------\n\n")

incidence_2023 <- us_data %>%
  filter(measure_name == "Incidence", metric_name == "Rate", year == 2023) %>%
  select(cause_name, age_name, val) %>%
  pivot_wider(names_from = age_name, values_from = val)

total_inc <- incidence_2023 %>%
  summarise(under5 = sum(`<5 years`, na.rm = TRUE),
            age5_9 = sum(`5-9 years`, na.rm = TRUE),
            age10_14 = sum(`10-14 years`, na.rm = TRUE))

cat("OVERALL BURDEN:\n")
cat(sprintf("  <5 years:    %10.0f cases per 100,000\n", total_inc$under5))
cat(sprintf("  5-9 years:   %10.0f cases per 100,000\n", total_inc$age5_9))
cat(sprintf("  10-14 years: %10.0f cases per 100,000\n", total_inc$age10_14))
cat(sprintf("  → Younger children: %.1fx higher risk\n\n", total_inc$under5/total_inc$age10_14))

cat("TOP 3 DISEASES PER AGE:\n\n")
for(age in c("<5 years", "5-9 years", "10-14 years")) {
  cat(sprintf("%s:\n", age))
  top3 <- incidence_2023 %>% arrange(desc(.data[[age]])) %>% slice(1:3)
  for(i in 1:3) {
    cat(sprintf("  %d. %-40s %10.0f\n", i, top3$cause_name[i], top3[[age]][i]))
  }
  cat("\n")
}

# ==================== MORTALITY ====================
cat("\n2. MORTALITY RATES BY AGE GROUP\n")
cat("---------------------------------------------------------------\n\n")

mortality_2023 <- us_data %>%
  filter(measure_name == "Deaths", metric_name == "Rate", year == 2023) %>%
  select(cause_name, age_name, val) %>%
  pivot_wider(names_from = age_name, values_from = val)

total_mort <- mortality_2023 %>%
  summarise(under5 = sum(`<5 years`, na.rm = TRUE),
            age5_9 = sum(`5-9 years`, na.rm = TRUE),
            age10_14 = sum(`10-14 years`, na.rm = TRUE))

cat("OVERALL MORTALITY:\n")
cat(sprintf("  <5 years:    %.2f deaths per 100,000\n", total_mort$under5))
cat(sprintf("  5-9 years:   %.2f deaths per 100,000\n", total_mort$age5_9))
cat(sprintf("  10-14 years: %.2f deaths per 100,000\n", total_mort$age10_14))
cat(sprintf("  → Infants: %.1fx higher death risk\n\n", total_mort$under5/total_mort$age10_14))

cat("TOP 3 KILLERS PER AGE:\n\n")
for(age in c("<5 years", "5-9 years", "10-14 years")) {
  cat(sprintf("%s:\n", age))
  top3 <- mortality_2023 %>% arrange(desc(.data[[age]])) %>% slice(1:3)
  for(i in 1:3) {
    cat(sprintf("  %d. %-40s %8.3f\n", i, top3$cause_name[i], top3[[age]][i]))
  }
  cat("\n")
}

# ==================== DALYs ====================
cat("\n3. DALYs RATES BY AGE GROUP\n")
cat("---------------------------------------------------------------\n\n")

dalys_2023 <- us_data %>%
  filter(measure_name == "DALYs (Disability-Adjusted Life Years)", 
         metric_name == "Rate", year == 2023) %>%
  select(cause_name, age_name, val) %>%
  pivot_wider(names_from = age_name, values_from = val)

total_dalys <- dalys_2023 %>%
  summarise(under5 = sum(`<5 years`, na.rm = TRUE),
            age5_9 = sum(`5-9 years`, na.rm = TRUE),
            age10_14 = sum(`10-14 years`, na.rm = TRUE))

cat("OVERALL DISEASE BURDEN:\n")
cat(sprintf("  <5 years:    %.1f DALYs per 100,000\n", total_dalys$under5))
cat(sprintf("  5-9 years:   %.1f DALYs per 100,000\n", total_dalys$age5_9))
cat(sprintf("  10-14 years: %.1f DALYs per 100,000\n", total_dalys$age10_14))
cat(sprintf("  → Young children: %.1fx higher burden\n\n", total_dalys$under5/total_dalys$age10_14))

cat("TOP 3 BURDEN CONTRIBUTORS PER AGE:\n\n")
for(age in c("<5 years", "5-9 years", "10-14 years")) {
  cat(sprintf("%s:\n", age))
  top3 <- dalys_2023 %>% arrange(desc(.data[[age]])) %>% slice(1:3)
  total_age <- sum(dalys_2023[[age]], na.rm = TRUE)
  for(i in 1:3) {
    pct <- 100 * top3[[age]][i] / total_age
    cat(sprintf("  %d. %-40s %7.1f (%.1f%%)\n", i, top3$cause_name[i], top3[[age]][i], pct))
  }
  cat("\n")
}

# ==================== COMPARATIVE ANALYSIS ====================
cat("\n===============================================================\n")
cat("COMPARATIVE ANALYSIS\n")
cat("===============================================================\n\n")

cat("AGE GRADIENT (All Measures):\n")
cat(sprintf("  • Incidence:  %.1fx higher in <5 vs 10-14\n", total_inc$under5/total_inc$age10_14))
cat(sprintf("  • Mortality:  %.1fx higher in <5 vs 10-14\n", total_mort$under5/total_mort$age10_14))
cat(sprintf("  • DALYs:      %.1fx higher in <5 vs 10-14\n\n", total_dalys$under5/total_dalys$age10_14))

cat("DISEASES WITH STEEPEST AGE GRADIENTS:\n")
age_gradient <- incidence_2023 %>%
  mutate(ratio = `<5 years` / `10-14 years`) %>%
  filter(!is.infinite(ratio), ratio > 1) %>%
  arrange(desc(ratio)) %>%
  slice(1:5)

for(i in 1:nrow(age_gradient)) {
  cat(sprintf("  %d. %-40s %.1fx higher in <5\n", 
              i, age_gradient$cause_name[i], age_gradient$ratio[i]))
}

cat("\nDISEASES THAT INCREASE WITH AGE:\n")
age_increase <- incidence_2023 %>%
  filter(`10-14 years` > `<5 years`) %>%
  mutate(ratio = `10-14 years` / `<5 years`) %>%
  arrange(desc(ratio))

if(nrow(age_increase) > 0) {
  for(i in 1:nrow(age_increase)) {
    cat(sprintf("  • %-40s %.1fx higher in adolescents\n", 
                age_increase$cause_name[i], age_increase$ratio[i]))
  }
} else {
  cat("  • ALL diseases decline with age\n")
}

# ==================== KEY TAKEAWAYS ====================
cat("\n\n===============================================================\n")
cat("KEY TAKEAWAYS BY AGE GROUP\n")
cat("===============================================================\n\n")

cat("<5 YEARS (HIGHEST RISK):\n")
cat("   • Highest burden across all measures\n")
cat(sprintf("   • %.0f infections per year per child (average)\n", total_inc$under5/100000*100000))
cat("   • Immature immune system + environmental exposures\n")
cat("   • URIs dominate, LRIs most lethal\n\n")

cat("5-9 YEARS (INTERMEDIATE RISK):\n")
cat(sprintf("   • %.0f%% lower burden than <5 years\n", 100*(1 - total_inc$age5_9/total_inc$under5)))
cat("   • School exposure maintains high URI rates\n")
cat("   • Better immunity reduces severity\n\n")

cat("10-14 YEARS (LOWEST RISK):\n")
cat(sprintf("   • %.0f%% lower burden than <5 years\n", 100*(1 - total_inc$age10_14/total_inc$under5)))
cat("   • Mature immune system\n")
cat("   • STIs emerge as new concern\n")
cat("   • Transition period to adult disease patterns\n\n")

cat("===============================================================\n")
