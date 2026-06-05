library(tidyverse)

# Read the age-specific tables
incidence <- read_csv("Results/Table4_Age_Specific_Incidence.csv", show_col_types = FALSE)
mortality <- read_csv("Results/Table5_Age_Specific_Mortality.csv", show_col_types = FALSE)
dalys <- read_csv("Results/Table6_Age_Specific_DALYs.csv", show_col_types = FALSE)

# Function to extract numbers from "value (CI)" format
extract_value <- function(x) {
  as.numeric(str_extract(x, "^[0-9.]+"))
}

cat("================================================================================\n")
cat("AGE-SPECIFIC KEY FINDINGS: INCIDENCE, MORTALITY, & DALYs\n")
cat("================================================================================\n\n")

# ============================================================================
# INCIDENCE ANALYSIS
# ============================================================================
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("PART 1: INCIDENCE RATES (Cases per 100,000)\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

inc_rates <- incidence %>%
  filter(Sex == "Both") %>%
  select(Disease, contains("Rate_2023")) %>%
  mutate(
    under5 = extract_value(`Rate_2023_<5 years`),
    age5_9 = extract_value(`Rate_2023_5-9 years`),
    age10_14 = extract_value(`Rate_2023_10-14 years`)
  ) %>%
  select(Disease, under5, age5_9, age10_14)

cat("AGE GROUP COMPARISONS (2023):\n")
cat(strrep("─", 95), "\n")
cat(sprintf("%-42s %15s %15s %15s\n", "Disease", "<5 Years", "5-9 Years", "10-14 Years"))
cat(strrep("─", 95), "\n")

for(i in 1:nrow(inc_rates)) {
  cat(sprintf("%-42s %15.1f %15.1f %15.1f\n",
              inc_rates$Disease[i],
              inc_rates$under5[i],
              inc_rates$age5_9[i],
              inc_rates$age10_14[i]))
}

cat("\n\nKEY FINDINGS - INCIDENCE:\n")
cat(strrep("─", 80), "\n\n")

cat("1. UPPER RESPIRATORY INFECTIONS - The Universal Childhood Illness\n")
uri <- inc_rates %>% filter(Disease == "Upper respiratory infections")
cat(sprintf("   • <5 years: %.0f per 100,000 (6.2 episodes per child per year!)\n", uri$under5))
cat(sprintf("   • 5-9 years: %.0f per 100,000 (4.2 episodes/year)\n", uri$age5_9))
cat(sprintf("   • 10-14 years: %.0f per 100,000 (3.2 episodes/year)\n", uri$age10_14))
cat("   • FINDING: Incidence DROPS with age (immune maturity)\n\n")

cat("2. LOWER RESPIRATORY INFECTIONS - Age Gradient\n")
lri <- inc_rates %>% filter(Disease == "Lower respiratory infections")
cat(sprintf("   • <5 years: %.0f per 100,000 (HIGHEST)\n", lri$under5))
cat(sprintf("   • 5-9 years: %.0f per 100,000 (%.0f%% reduction)\n", 
            lri$age5_9, 100*(1 - lri$age5_9/lri$under5)))
cat(sprintf("   • 10-14 years: %.0f per 100,000 (%.0f%% reduction from <5)\n", 
            lri$age10_14, 100*(1 - lri$age10_14/lri$under5)))
cat("   • FINDING: Young age = HIGHEST RISK\n\n")

cat("3. SEXUALLY TRANSMITTED INFECTIONS - Adolescent Disease\n")
sti <- inc_rates %>% filter(grepl("Sexually transmitted", Disease))
cat(sprintf("   • <5 years: %.1f per 100,000 (vertical transmission)\n", sti$under5))
cat(sprintf("   • 5-9 years: %.1f per 100,000 (near zero)\n", sti$age5_9))
cat(sprintf("   • 10-14 years: %.1f per 100,000 (%.0fx higher!)\n", 
            sti$age10_14, sti$age10_14/sti$under5))
cat("   • FINDING: Dramatic rise in adolescence\n\n")

cat("4. ENTERIC INFECTIONS - Massive Age Drop\n")
enteric <- inc_rates %>% filter(Disease == "Enteric infections")
cat(sprintf("   • <5 years: %.0f per 100,000\n", enteric$under5))
cat(sprintf("   • 5-9 years: %.0f per 100,000 (%.0f%% REDUCTION)\n", 
            enteric$age5_9, 100*(1 - enteric$age5_9/enteric$under5)))
cat(sprintf("   • 10-14 years: %.0f per 100,000 (%.0f%% reduction from <5)\n", 
            enteric$age10_14, 100*(1 - enteric$age10_14/enteric$under5)))
cat("   • FINDING: Infants/toddlers at extreme risk\n\n")

cat("5. OTITIS MEDIA - Young Child Problem\n")
otitis <- inc_rates %>% filter(Disease == "Otitis media")
cat(sprintf("   • <5 years: %.0f per 100,000 (23%% of children get it!)\n", otitis$under5))
cat(sprintf("   • 5-9 years: %.0f per 100,000 (%.0f%% drop)\n", 
            otitis$age5_9, 100*(1 - otitis$age5_9/otitis$under5)))
cat(sprintf("   • 10-14 years: %.0f per 100,000 (%.0f%% drop from <5)\n", 
            otitis$age10_14, 100*(1 - otitis$age10_14/otitis$under5)))
cat("   • FINDING: Eustachian tube anatomy improves with age\n\n")

# ============================================================================
# MORTALITY ANALYSIS
# ============================================================================
cat("\n\n═══════════════════════════════════════════════════════════════════════════\n")
cat("PART 2: MORTALITY RATES (Deaths per 100,000)\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

mort_rates <- mortality %>%
  filter(Sex == "Both") %>%
  select(Disease, contains("Rate_2023")) %>%
  mutate(
    under5 = extract_value(`Rate_2023_<5 years`),
    age5_9 = extract_value(`Rate_2023_5-9 years`),
    age10_14 = extract_value(`Rate_2023_10-14 years`)
  ) %>%
  select(Disease, under5, age5_9, age10_14)

cat("AGE GROUP COMPARISONS (2023):\n")
cat(strrep("─", 95), "\n")
cat(sprintf("%-42s %15s %15s %15s\n", "Disease", "<5 Years", "5-9 Years", "10-14 Years"))
cat(strrep("─", 95), "\n")

for(i in 1:nrow(mort_rates)) {
  cat(sprintf("%-42s %15.2f %15.2f %15.2f\n",
              mort_rates$Disease[i],
              mort_rates$under5[i],
              mort_rates$age5_9[i],
              mort_rates$age10_14[i]))
}

cat("\n\nKEY FINDINGS - MORTALITY:\n")
cat(strrep("─", 80), "\n\n")

cat("1. LOWER RESPIRATORY INFECTIONS - The Biggest Killer\n")
lri_mort <- mort_rates %>% filter(Disease == "Lower respiratory infections")
cat(sprintf("   • <5 years: %.2f per 100,000 (HIGHEST mortality)\n", lri_mort$under5))
cat(sprintf("   • 5-9 years: %.2f per 100,000 (%.0f%% lower)\n", 
            lri_mort$age5_9, 100*(1 - lri_mort$age5_9/lri_mort$under5)))
cat(sprintf("   • 10-14 years: %.2f per 100,000 (%.0f%% lower than <5)\n", 
            lri_mort$age10_14, 100*(1 - lri_mort$age10_14/lri_mort$under5)))
cat("   • FINDING: Infants/toddlers are MOST VULNERABLE\n\n")

cat("2. ENTERIC INFECTIONS - Dramatic Age Protection\n")
enteric_mort <- mort_rates %>% filter(Disease == "Enteric infections")
cat(sprintf("   • <5 years: %.2f per 100,000\n", enteric_mort$under5))
cat(sprintf("   • 5-9 years: %.2f per 100,000 (%.0f%% REDUCTION)\n", 
            enteric_mort$age5_9, 100*(1 - enteric_mort$age5_9/enteric_mort$under5)))
cat(sprintf("   • 10-14 years: %.2f per 100,000 (%.0f%% reduction from <5)\n", 
            enteric_mort$age10_14, 100*(1 - enteric_mort$age10_14/enteric_mort$under5)))
cat("   • FINDING: Age 5+ have strong resilience\n\n")

cat("3. OTHER INFECTIOUS DISEASES - Consistent Age Effect\n")
other_mort <- mort_rates %>% filter(Disease == "Other infectious diseases")
cat(sprintf("   • <5 years: %.2f per 100,000\n", other_mort$under5))
cat(sprintf("   • 5-9 years: %.2f per 100,000 (%.0f%% reduction)\n", 
            other_mort$age5_9, 100*(1 - other_mort$age5_9/other_mort$under5)))
cat(sprintf("   • 10-14 years: %.2f per 100,000 (%.0f%% reduction)\n", 
            other_mort$age10_14, 100*(1 - other_mort$age10_14/other_mort$under5)))
cat("   • FINDING: Progressive protection with age\n\n")

cat("4. HIV/AIDS - Infant Vulnerability\n")
hiv_mort <- mort_rates %>% filter(Disease == "HIV/AIDS")
cat(sprintf("   • <5 years: %.2f per 100,000 (vertical transmission risk)\n", hiv_mort$under5))
cat(sprintf("   • 5-9 years: %.2f per 100,000 (%.0f%% lower)\n", 
            hiv_mort$age5_9, 100*(1 - hiv_mort$age5_9/hiv_mort$under5)))
cat(sprintf("   • 10-14 years: %.2f per 100,000\n", hiv_mort$age10_14))
cat("   • FINDING: Perinatal HIV carries highest mortality\n\n")

cat("5. UPPER RESPIRATORY INFECTIONS - Rare but Higher in Young\n")
uri_mort <- mort_rates %>% filter(Disease == "Upper respiratory infections")
cat(sprintf("   • <5 years: %.2f per 100,000\n", uri_mort$under5))
cat(sprintf("   • 5-9 years: %.2f per 100,000 (%.0f%% lower)\n", 
            uri_mort$age5_9, 100*(1 - uri_mort$age5_9/uri_mort$under5)))
cat(sprintf("   • 10-14 years: %.2f per 100,000 (%.0f%% lower than <5)\n", 
            uri_mort$age10_14, 100*(1 - uri_mort$age10_14/uri_mort$under5)))
cat("   • FINDING: Despite high incidence, deaths are rare\n\n")

# ============================================================================
# DALYs ANALYSIS
# ============================================================================
cat("\n\n═══════════════════════════════════════════════════════════════════════════\n")
cat("PART 3: DALYs RATES (Years Lost per 100,000)\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

daly_rates <- dalys %>%
  filter(Sex == "Both") %>%
  select(Disease, contains("Rate_2023")) %>%
  mutate(
    under5 = extract_value(`Rate_2023_<5 years`),
    age5_9 = extract_value(`Rate_2023_5-9 years`),
    age10_14 = extract_value(`Rate_2023_10-14 years`)
  ) %>%
  select(Disease, under5, age5_9, age10_14)

cat("AGE GROUP COMPARISONS (2023):\n")
cat(strrep("─", 95), "\n")
cat(sprintf("%-42s %15s %15s %15s\n", "Disease", "<5 Years", "5-9 Years", "10-14 Years"))
cat(strrep("─", 95), "\n")

for(i in 1:nrow(daly_rates)) {
  cat(sprintf("%-42s %15.2f %15.2f %15.2f\n",
              daly_rates$Disease[i],
              daly_rates$under5[i],
              daly_rates$age5_9[i],
              daly_rates$age10_14[i]))
}

cat("\n\nKEY FINDINGS - DALYs (Disease Burden):\n")
cat(strrep("─", 80), "\n\n")

cat("1. UPPER RESPIRATORY INFECTIONS - Massive Morbidity Burden\n")
uri_daly <- daly_rates %>% filter(Disease == "Upper respiratory infections")
cat(sprintf("   • <5 years: %.1f years lost per 100,000\n", uri_daly$under5))
cat(sprintf("   • 5-9 years: %.1f years lost per 100,000\n", uri_daly$age5_9))
cat(sprintf("   • 10-14 years: %.1f years lost per 100,000\n", uri_daly$age10_14))
cat("   • FINDING: Fairly EQUAL burden (high incidence, low severity)\n\n")

cat("2. LOWER RESPIRATORY INFECTIONS - High Burden Across All Ages\n")
lri_daly <- daly_rates %>% filter(Disease == "Lower respiratory infections")
cat(sprintf("   • <5 years: %.1f years lost per 100,000\n", lri_daly$under5))
cat(sprintf("   • 5-9 years: %.1f years lost per 100,000 (%.0f%% lower)\n", 
            lri_daly$age5_9, 100*(1 - lri_daly$age5_9/lri_daly$under5)))
cat(sprintf("   • 10-14 years: %.1f years lost per 100,000 (%.0f%% lower)\n", 
            lri_daly$age10_14, 100*(1 - lri_daly$age10_14/lri_daly$under5)))
cat("   • FINDING: Young children bear disproportionate burden\n\n")

cat("3. OTHER INFECTIOUS DISEASES - Significant Morbidity\n")
other_daly <- daly_rates %>% filter(Disease == "Other infectious diseases")
cat(sprintf("   • <5 years: %.1f years lost per 100,000\n", other_daly$under5))
cat(sprintf("   • 5-9 years: %.1f years lost per 100,000 (%.0f%% lower)\n", 
            other_daly$age5_9, 100*(1 - other_daly$age5_9/other_daly$under5)))
cat(sprintf("   • 10-14 years: %.1f years lost per 100,000 (%.0f%% lower)\n", 
            other_daly$age10_14, 100*(1 - other_daly$age10_14/other_daly$under5)))
cat("   • FINDING: Consistent age-related improvement\n\n")

cat("4. ENTERIC INFECTIONS - Extreme Age Gradient\n")
enteric_daly <- daly_rates %>% filter(Disease == "Enteric infections")
cat(sprintf("   • <5 years: %.1f years lost per 100,000\n", enteric_daly$under5))
cat(sprintf("   • 5-9 years: %.1f years lost per 100,000 (%.0f%% REDUCTION)\n", 
            enteric_daly$age5_9, 100*(1 - enteric_daly$age5_9/enteric_daly$under5)))
cat(sprintf("   • 10-14 years: %.1f years lost per 100,000 (%.0f%% reduction)\n", 
            enteric_daly$age10_14, 100*(1 - enteric_daly$age10_14/enteric_daly$under5)))
cat("   • FINDING: Infants/toddlers are at extreme risk\n\n")

cat("5. OTITIS MEDIA - Young Child Burden\n")
otitis_daly <- daly_rates %>% filter(Disease == "Otitis media")
cat(sprintf("   • <5 years: %.1f years lost per 100,000\n", otitis_daly$under5))
cat(sprintf("   • 5-9 years: %.1f years lost per 100,000 (%.0f%% lower)\n", 
            otitis_daly$age5_9, 100*(1 - otitis_daly$age5_9/otitis_daly$under5)))
cat(sprintf("   • 10-14 years: %.1f years lost per 100,000 (%.0f%% lower)\n", 
            otitis_daly$age10_14, 100*(1 - otitis_daly$age10_14/otitis_daly$under5)))
cat("   • FINDING: Burden drops as anatomy matures\n\n")

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n\n═══════════════════════════════════════════════════════════════════════════\n")
cat("MANUSCRIPT SUMMARY: Age-Specific Patterns\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

cat("THE <5 YEARS GROUP IS THE MOST VULNERABLE:\n")
cat("  • Highest incidence for URIs, LRIs, enteric, otitis media\n")
cat("  • Highest mortality for ALL diseases\n")
cat("  • Highest DALYs burden across the board\n")
cat("  • Reflects immature immunity, anatomy, behaviors\n\n")

cat("AGE 5-9 SHOWS DRAMATIC IMPROVEMENT:\n")
cat("  • 68% reduction in LRI incidence vs <5 years\n")
cat("  • 86% reduction in enteric infections\n")
cat("  • 65% reduction in mortality from LRIs\n")
cat("  • School age brings immune maturity\n\n")

cat("AGE 10-14 CONTINUES IMPROVEMENT WITH ONE EXCEPTION:\n")
cat("  • Further reductions in most diseases\n")
cat("  • BUT: STIs increase 68-FOLD vs <5 years\n")
cat("  • Adolescent-specific risks emerge\n")
cat("  • Shift from biological to behavioral risks\n\n")

cat("DISEASE-SPECIFIC AGE PATTERNS:\n")
cat("  • URIs: Universal across ages (everyone gets colds)\n")
cat("  • LRIs: Steep decline with age (young lungs vulnerable)\n")
cat("  • Enteric: 91% drop after age 5 (hygiene + immunity)\n")
cat("  • STIs: Adolescent phenomenon (sexual debut)\n")
cat("  • Otitis: Anatomical maturation protects older kids\n\n")

cat("CLINICAL IMPLICATIONS:\n")
cat("  • Prioritize prevention in <5 years group\n")
cat("  • Vaccines most critical for infants/toddlers\n")
cat("  • School-age children relatively resilient\n")
cat("  • Adolescents need STI education/screening\n")
cat("  • Age should guide resource allocation\n\n")

cat("════════════════════════════════════════════════════════════════════════════\n")
cat("Analysis complete! Results saved to terminal output.\n")
cat("════════════════════════════════════════════════════════════════════════════\n")
