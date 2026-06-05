#!/usr/bin/env Rscript
# Create 6 publication-quality tables for US pediatric infectious diseases (1990-2023)
# Tables follow the structure from reference PDF but adapted for US data

library(tidyverse)
library(flextable)
library(officer)

# Read the data
data <- read_csv("Data/IHME-GBD_2023_DATA-c62f4bc4-1.csv")

# Filter for US, children 0-14, years 1990 and 2023
us_data <- data %>%
  filter(location_name == "United States of America",
         age_name %in% c("<5 years", "5-9 years", "10-14 years"))

# Define disease categories (now including Acute hepatitis)
disease_categories <- c(
  "Acute hepatitis",
  "HIV/AIDS",
  "Sexually transmitted infections excluding HIV",
  "Tuberculosis",
  "Lower respiratory infections",
  "Upper respiratory infections",
  "Otitis media",
  "Enteric infections",
  "Neglected tropical diseases and malaria",
  "Other infectious diseases"
)

# Function to calculate EAPC
calculate_eapc <- function(year_data) {
  if(length(unique(year_data$year)) < 2) return(data.frame(EAPC = NA, CI_lower = NA, CI_upper = NA))
  
  year_data <- year_data %>%
    filter(!is.na(val), val > 0) %>%
    mutate(ln_val = log(val))
  
  if(nrow(year_data) < 2) return(data.frame(EAPC = NA, CI_lower = NA, CI_upper = NA))
  
  model <- lm(ln_val ~ year, data = year_data)
  beta <- coef(model)[2]
  conf_int <- confint(model, "year", level = 0.95)
  
  eapc <- 100 * (exp(beta) - 1)
  eapc_lower <- 100 * (exp(conf_int[1]) - 1)
  eapc_upper <- 100 * (exp(conf_int[2]) - 1)
  
  data.frame(EAPC = eapc, CI_lower = eapc_lower, CI_upper = eapc_upper)
}

# Function to aggregate data for both sexes and by sex
aggregate_data <- function(data, measure_name, metric_name) {
  # For rates: average across age groups
  # For numbers: sum across age groups
  
  # Handle different DALYs naming
  if(measure_name == "DALYs") {
    measure_name <- "DALYs (Disability-Adjusted Life Years)"
  }
  
  if(grepl("Rate", metric_name)) {
    agg_func <- mean
  } else {
    agg_func <- sum
  }
  
  data %>%
    filter(measure_name == !!measure_name, 
           metric_name == !!metric_name) %>%
    group_by(cause_name, sex_name, year) %>%
    summarise(
      val = agg_func(val, na.rm = TRUE),
      upper = agg_func(upper, na.rm = TRUE),
      lower = agg_func(lower, na.rm = TRUE),
      .groups = "drop"
    )
}

# ============================================================================
# TABLE 1: Incidence Rate Burden
# ============================================================================
create_table1 <- function(data) {
  cat("Creating Table 1: Incidence rate burden...\n")
  
  # Get incidence data
  inc_number <- aggregate_data(us_data, "Incidence", "Number")
  inc_rate <- aggregate_data(us_data, "Incidence", "Rate")
  
  # Function to create summary for one disease and sex
  summarize_disease <- function(disease, sex) {
    # Numbers
    num_1990 <- inc_number %>% 
      filter(cause_name == disease, sex_name == sex, year == 1990)
    num_2023 <- inc_number %>% 
      filter(cause_name == disease, sex_name == sex, year == 2023)
    
    # Rates
    rate_1990 <- inc_rate %>% 
      filter(cause_name == disease, sex_name == sex, year == 1990)
    rate_2023 <- inc_rate %>% 
      filter(cause_name == disease, sex_name == sex, year == 2023)
    
    # EAPC
    eapc_data <- inc_rate %>%
      filter(cause_name == disease, sex_name == sex)
    eapc_result <- calculate_eapc(eapc_data)
    
    data.frame(
      Disease = disease,
      Sex = sex,
      Cases_1990 = if(nrow(num_1990) > 0) num_1990$val[1] else NA,
      Cases_1990_lower = if(nrow(num_1990) > 0) num_1990$lower[1] else NA,
      Cases_1990_upper = if(nrow(num_1990) > 0) num_1990$upper[1] else NA,
      Cases_2023 = if(nrow(num_2023) > 0) num_2023$val[1] else NA,
      Cases_2023_lower = if(nrow(num_2023) > 0) num_2023$lower[1] else NA,
      Cases_2023_upper = if(nrow(num_2023) > 0) num_2023$upper[1] else NA,
      ASR_1990 = if(nrow(rate_1990) > 0) rate_1990$val[1] else NA,
      ASR_1990_lower = if(nrow(rate_1990) > 0) rate_1990$lower[1] else NA,
      ASR_1990_upper = if(nrow(rate_1990) > 0) rate_1990$upper[1] else NA,
      ASR_2023 = if(nrow(rate_2023) > 0) rate_2023$val[1] else NA,
      ASR_2023_lower = if(nrow(rate_2023) > 0) rate_2023$lower[1] else NA,
      ASR_2023_upper = if(nrow(rate_2023) > 0) rate_2023$upper[1] else NA,
      EAPC = eapc_result$EAPC[1],
      EAPC_lower = eapc_result$CI_lower[1],
      EAPC_upper = eapc_result$CI_upper[1]
    )
  }
  
  # Create table for all diseases and sexes
  table1_data <- expand_grid(
    disease = disease_categories,
    sex = c("Both", "Male", "Female")
  ) %>%
    rowwise() %>%
    do({
      summarize_disease(.$disease, .$sex)
    }) %>%
    ungroup()
  
  # Format for display
  table1_formatted <- table1_data %>%
    mutate(
      Cases_1990_fmt = sprintf("%.2f (%.2f, %.2f)", 
                          Cases_1990, Cases_1990_lower, Cases_1990_upper),
      Cases_2023_fmt = sprintf("%.2f (%.2f, %.2f)", 
                          Cases_2023, Cases_2023_lower, Cases_2023_upper),
      ASR_1990_fmt = sprintf("%.2f (%.2f, %.2f)", ASR_1990, ASR_1990_lower, ASR_1990_upper),
      ASR_2023_fmt = sprintf("%.2f (%.2f, %.2f)", ASR_2023, ASR_2023_lower, ASR_2023_upper),
      EAPC_fmt = sprintf("%.2f (%.2f, %.2f)", EAPC, EAPC_lower, EAPC_upper)
    ) %>%
    select(Disease, Sex, Cases_1990_fmt, Cases_2023_fmt, ASR_1990_fmt, ASR_2023_fmt, EAPC_fmt) %>%
    rename(
      `Cases in 1990` = Cases_1990_fmt,
      `Cases in 2023` = Cases_2023_fmt,
      `ASR in 1990 (per 100,000)` = ASR_1990_fmt,
      `ASR in 2023 (per 100,000)` = ASR_2023_fmt,
      `EAPC (95% CI)` = EAPC_fmt
    )
  
  return(table1_formatted)
}

# ============================================================================
# TABLE 2: Mortality Burden
# ============================================================================
create_table2 <- function(data) {
  cat("Creating Table 2: Mortality burden...\n")
  
  # Get death data
  death_number <- aggregate_data(us_data, "Deaths", "Number")
  death_rate <- aggregate_data(us_data, "Deaths", "Rate")
  
  # Function to create summary for one disease and sex
  summarize_disease <- function(disease, sex) {
    # Numbers
    num_1990 <- death_number %>% 
      filter(cause_name == disease, sex_name == sex, year == 1990)
    num_2023 <- death_number %>% 
      filter(cause_name == disease, sex_name == sex, year == 2023)
    
    # Rates
    rate_1990 <- death_rate %>% 
      filter(cause_name == disease, sex_name == sex, year == 1990)
    rate_2023 <- death_rate %>% 
      filter(cause_name == disease, sex_name == sex, year == 2023)
    
    # EAPC
    eapc_data <- death_rate %>%
      filter(cause_name == disease, sex_name == sex)
    eapc_result <- calculate_eapc(eapc_data)
    
    data.frame(
      Disease = disease,
      Sex = sex,
      Deaths_1990 = if(nrow(num_1990) > 0) num_1990$val[1] else NA,
      Deaths_1990_lower = if(nrow(num_1990) > 0) num_1990$lower[1] else NA,
      Deaths_1990_upper = if(nrow(num_1990) > 0) num_1990$upper[1] else NA,
      Deaths_2023 = if(nrow(num_2023) > 0) num_2023$val[1] else NA,
      Deaths_2023_lower = if(nrow(num_2023) > 0) num_2023$lower[1] else NA,
      Deaths_2023_upper = if(nrow(num_2023) > 0) num_2023$upper[1] else NA,
      ASMR_1990 = if(nrow(rate_1990) > 0) rate_1990$val[1] else NA,
      ASMR_1990_lower = if(nrow(rate_1990) > 0) rate_1990$lower[1] else NA,
      ASMR_1990_upper = if(nrow(rate_1990) > 0) rate_1990$upper[1] else NA,
      ASMR_2023 = if(nrow(rate_2023) > 0) rate_2023$val[1] else NA,
      ASMR_2023_lower = if(nrow(rate_2023) > 0) rate_2023$lower[1] else NA,
      ASMR_2023_upper = if(nrow(rate_2023) > 0) rate_2023$upper[1] else NA,
      EAPC = eapc_result$EAPC[1],
      EAPC_lower = eapc_result$CI_lower[1],
      EAPC_upper = eapc_result$CI_upper[1]
    )
  }
  
  # Create table for all diseases and sexes
  table2_data <- expand_grid(
    disease = disease_categories,
    sex = c("Both", "Male", "Female")
  ) %>%
    rowwise() %>%
    do({
      summarize_disease(.$disease, .$sex)
    }) %>%
    ungroup()
  
  # Format for display
  table2_formatted <- table2_data %>%
    mutate(
      Deaths_1990_fmt = sprintf("%.2f (%.2f, %.2f)", Deaths_1990, Deaths_1990_lower, Deaths_1990_upper),
      Deaths_2023_fmt = sprintf("%.2f (%.2f, %.2f)", Deaths_2023, Deaths_2023_lower, Deaths_2023_upper),
      ASMR_1990_fmt = sprintf("%.2f (%.2f, %.2f)", ASMR_1990, ASMR_1990_lower, ASMR_1990_upper),
      ASMR_2023_fmt = sprintf("%.2f (%.2f, %.2f)", ASMR_2023, ASMR_2023_lower, ASMR_2023_upper),
      EAPC_fmt = sprintf("%.2f (%.2f, %.2f)", EAPC, EAPC_lower, EAPC_upper)
    ) %>%
    select(Disease, Sex, Deaths_1990_fmt, Deaths_2023_fmt, ASMR_1990_fmt, ASMR_2023_fmt, EAPC_fmt) %>%
    rename(
      `Deaths in 1990` = Deaths_1990_fmt,
      `Deaths in 2023` = Deaths_2023_fmt,
      `ASMR in 1990 (per 100,000)` = ASMR_1990_fmt,
      `ASMR in 2023 (per 100,000)` = ASMR_2023_fmt,
      `EAPC (95% CI)` = EAPC_fmt
    )
  
  return(table2_formatted)
}

# ============================================================================
# TABLE 3: DALYs Burden
# ============================================================================
create_table3 <- function(data) {
  cat("Creating Table 3: DALYs burden...\n")
  
  # Get DALYs data
  daly_number <- aggregate_data(us_data, "DALYs", "Number")
  daly_rate <- aggregate_data(us_data, "DALYs", "Rate")
  
  # Function to create summary for one disease and sex
  summarize_disease <- function(disease, sex) {
    # Numbers
    num_1990 <- daly_number %>% 
      filter(cause_name == disease, sex_name == sex, year == 1990)
    num_2023 <- daly_number %>% 
      filter(cause_name == disease, sex_name == sex, year == 2023)
    
    # Rates
    rate_1990 <- daly_rate %>% 
      filter(cause_name == disease, sex_name == sex, year == 1990)
    rate_2023 <- daly_rate %>% 
      filter(cause_name == disease, sex_name == sex, year == 2023)
    
    # EAPC
    eapc_data <- daly_rate %>%
      filter(cause_name == disease, sex_name == sex)
    eapc_result <- calculate_eapc(eapc_data)
    
    data.frame(
      Disease = disease,
      Sex = sex,
      DALYs_1990 = if(nrow(num_1990) > 0) num_1990$val[1] else NA,
      DALYs_1990_lower = if(nrow(num_1990) > 0) num_1990$lower[1] else NA,
      DALYs_1990_upper = if(nrow(num_1990) > 0) num_1990$upper[1] else NA,
      DALYs_2023 = if(nrow(num_2023) > 0) num_2023$val[1] else NA,
      DALYs_2023_lower = if(nrow(num_2023) > 0) num_2023$lower[1] else NA,
      DALYs_2023_upper = if(nrow(num_2023) > 0) num_2023$upper[1] else NA,
      ASDR_1990 = if(nrow(rate_1990) > 0) rate_1990$val[1] else NA,
      ASDR_1990_lower = if(nrow(rate_1990) > 0) rate_1990$lower[1] else NA,
      ASDR_1990_upper = if(nrow(rate_1990) > 0) rate_1990$upper[1] else NA,
      ASDR_2023 = if(nrow(rate_2023) > 0) rate_2023$val[1] else NA,
      ASDR_2023_lower = if(nrow(rate_2023) > 0) rate_2023$lower[1] else NA,
      ASDR_2023_upper = if(nrow(rate_2023) > 0) rate_2023$upper[1] else NA,
      EAPC = eapc_result$EAPC[1],
      EAPC_lower = eapc_result$CI_lower[1],
      EAPC_upper = eapc_result$CI_upper[1]
    )
  }
  
  # Create table for all diseases and sexes
  table3_data <- expand_grid(
    disease = disease_categories,
    sex = c("Both", "Male", "Female")
  ) %>%
    rowwise() %>%
    do({
      summarize_disease(.$disease, .$sex)
    }) %>%
    ungroup()
  
  # Format for display
  table3_formatted <- table3_data %>%
    mutate(
      DALYs_1990_fmt = sprintf("%.2f (%.2f, %.2f)", DALYs_1990, DALYs_1990_lower, DALYs_1990_upper),
      DALYs_2023_fmt = sprintf("%.2f (%.2f, %.2f)", DALYs_2023, DALYs_2023_lower, DALYs_2023_upper),
      ASDR_1990_fmt = sprintf("%.2f (%.2f, %.2f)", ASDR_1990, ASDR_1990_lower, ASDR_1990_upper),
      ASDR_2023_fmt = sprintf("%.2f (%.2f, %.2f)", ASDR_2023, ASDR_2023_lower, ASDR_2023_upper),
      EAPC_fmt = sprintf("%.2f (%.2f, %.2f)", EAPC, EAPC_lower, EAPC_upper)
    ) %>%
    select(Disease, Sex, DALYs_1990_fmt, DALYs_2023_fmt, ASDR_1990_fmt, ASDR_2023_fmt, EAPC_fmt) %>%
    rename(
      `DALYs in 1990` = DALYs_1990_fmt,
      `DALYs in 2023` = DALYs_2023_fmt,
      `ASDR in 1990 (per 100,000)` = ASDR_1990_fmt,
      `ASDR in 2023 (per 100,000)` = ASDR_2023_fmt,
      `EAPC (95% CI)` = EAPC_fmt
    )
  
  return(table3_formatted)
}

# ============================================================================
# TABLES 4-6: Age-specific burden tables
# ============================================================================
create_age_specific_table <- function(measure_name, table_num) {
  cat(sprintf("Creating Table %d: Age-specific %s burden...\n", table_num, measure_name))
  
  # Handle DALYs naming
  measure_filter <- if(measure_name == "DALYs") {
    "DALYs (Disability-Adjusted Life Years)"
  } else {
    measure_name
  }
  
  # Get data for this measure
  number_data <- us_data %>%
    filter(measure_name == !!measure_filter, 
           metric_name == "Number") %>%
    group_by(cause_name, sex_name, age_name, year) %>%
    summarise(val = sum(val, na.rm = TRUE),
              lower = sum(lower, na.rm = TRUE),
              upper = sum(upper, na.rm = TRUE),
              .groups = "drop")
  
  rate_data <- us_data %>%
    filter(measure_name == !!measure_filter, 
           metric_name == "Rate") %>%
    group_by(cause_name, sex_name, age_name, year) %>%
    summarise(val = mean(val, na.rm = TRUE),
              lower = mean(lower, na.rm = TRUE),
              upper = mean(upper, na.rm = TRUE),
              .groups = "drop")
  
  # Create summary table
  age_table <- expand_grid(
    disease = disease_categories,
    sex = c("Male", "Female", "Both"),
    age = c("<5 years", "5-9 years", "10-14 years"),
    year = c(1990, 2023)
  ) %>%
    left_join(
      number_data %>% rename(num_val = val, num_lower = lower, num_upper = upper),
      by = c("disease" = "cause_name", "sex" = "sex_name", "age" = "age_name", "year")
    ) %>%
    left_join(
      rate_data %>% rename(rate_val = val, rate_lower = lower, rate_upper = upper),
      by = c("disease" = "cause_name", "sex" = "sex_name", "age" = "age_name", "year")
    ) %>%
    mutate(
      Number = sprintf("%.2f (%.2f, %.2f)", num_val, num_lower, num_upper),
      Rate = sprintf("%.2f (%.2f, %.2f)", rate_val, rate_lower, rate_upper)
    ) %>%
    select(disease, sex, age, year, Number, Rate) %>%
    pivot_wider(
      names_from = c(year, age),
      values_from = c(Number, Rate),
      names_sep = "_"
    ) %>%
    rename(Disease = disease, Sex = sex)
  
  return(age_table)
}

# ============================================================================
# Main execution
# ============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("Creating Publication Tables for US Pediatric Infectious Diseases\n")
cat("Period: 1990-2023, Ages: 0-14 years\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Create all tables
table1 <- create_table1(us_data)
table2 <- create_table2(us_data)
table3 <- create_table3(us_data)
table4 <- create_age_specific_table("Incidence", 4)
table5 <- create_age_specific_table("Deaths", 5)
table6 <- create_age_specific_table("DALYs", 6)

# Save as CSV files first
write_csv(table1, "Results/Table1_Incidence_Burden.csv")
write_csv(table2, "Results/Table2_Mortality_Burden.csv")
write_csv(table3, "Results/Table3_DALYs_Burden.csv")
write_csv(table4, "Results/Table4_Age_Specific_Incidence.csv")
write_csv(table5, "Results/Table5_Age_Specific_Mortality.csv")
write_csv(table6, "Results/Table6_Age_Specific_DALYs.csv")

cat("\nCSV tables saved successfully!\n\n")

# Now create Word document with formatted tables
cat("Creating Word document with formatted tables...\n")

# Initialize Word document
doc <- read_docx()

# Add Table 1
doc <- doc %>%
  body_add_par("Table 1: Incidence rate burden of infectious diseases among children in the United States", 
               style = "heading 1") %>%
  body_add_flextable(flextable(table1) %>% 
                      theme_vanilla() %>%
                      autofit() %>%
                      font(fontname = "Times New Roman", part = "all") %>%
                      fontsize(size = 9, part = "all")) %>%
  body_add_break()

# Add Table 2
doc <- doc %>%
  body_add_par("Table 2: Burden of mortality from infectious diseases among children in the United States", 
               style = "heading 1") %>%
  body_add_flextable(flextable(table2) %>% 
                      theme_vanilla() %>%
                      autofit() %>%
                      font(fontname = "Times New Roman", part = "all") %>%
                      fontsize(size = 9, part = "all")) %>%
  body_add_break()

# Add Table 3
doc <- doc %>%
  body_add_par("Table 3: Burden of DALYs caused by infectious diseases among children in the United States", 
               style = "heading 1") %>%
  body_add_flextable(flextable(table3) %>% 
                      theme_vanilla() %>%
                      autofit() %>%
                      font(fontname = "Times New Roman", part = "all") %>%
                      fontsize(size = 9, part = "all")) %>%
  body_add_break()

# Add Table 4
doc <- doc %>%
  body_add_par("Table 4: Age-specific burden of infectious diseases incidence among children in the United States", 
               style = "heading 1") %>%
  body_add_flextable(flextable(table4) %>% 
                      theme_vanilla() %>%
                      autofit() %>%
                      font(fontname = "Times New Roman", part = "all") %>%
                      fontsize(size = 8, part = "all")) %>%
  body_add_break()

# Add Table 5
doc <- doc %>%
  body_add_par("Table 5: Age-specific burden of infectious disease mortality among children in the United States", 
               style = "heading 1") %>%
  body_add_flextable(flextable(table5) %>% 
                      theme_vanilla() %>%
                      autofit() %>%
                      font(fontname = "Times New Roman", part = "all") %>%
                      fontsize(size = 8, part = "all")) %>%
  body_add_break()

# Add Table 6
doc <- doc %>%
  body_add_par("Table 6: Age-specific burden of DALYs among children in the United States", 
               style = "heading 1") %>%
  body_add_flextable(flextable(table6) %>% 
                      theme_vanilla() %>%
                      autofit() %>%
                      font(fontname = "Times New Roman", part = "all") %>%
                      fontsize(size = 8, part = "all"))

# Save Word document
print(doc, target = "Results/Publication_Tables_US_Pediatric_1990_2023.docx")

cat("\n" , rep("=", 72), "\n", sep = "")
cat("SUCCESS! Word document created:\n")
cat("Results/Publication_Tables_US_Pediatric_1990_2023.docx\n")
cat(rep("=", 72), "\n", sep = "")
