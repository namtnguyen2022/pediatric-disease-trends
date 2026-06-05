#!/usr/bin/env Rscript
# Climate-Associated Diseases Analysis: Figure 1, Figure 2, and EAPC
# Focus on 4 climate-related diseases in US children (1990-2023)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)

# Read the data
data <- read_csv("Data/IHME-GBD_2023_DATA-c62f4bc4-1.csv", show_col_types = FALSE)

# Filter for US, children 0-14
us_data <- data %>%
  filter(location_name == "United States of America",
         age_name %in% c("<5 years", "5-9 years", "10-14 years"))

# Define CLIMATE-ASSOCIATED diseases only
climate_diseases <- c(
  "Enteric infections",
  "Lower respiratory infections",
  "Neglected tropical diseases and malaria",
  "Upper respiratory infections"
)

# Define color palette for the 4 climate diseases
disease_colors <- c(
  "Enteric infections" = "#FFA500",      # Orange
  "Lower respiratory infections" = "#87CEEB",  # Sky Blue
  "Neglected tropical diseases and malaria" = "#00CED1",  # Turquoise
  "Upper respiratory infections" = "#DDA0DD"   # Plum
)

# Filter data for climate diseases only
climate_data <- us_data %>%
  filter(cause_name %in% climate_diseases)

cat("════════════════════════════════════════════════════════════════════════════\n")
cat("CLIMATE-ASSOCIATED DISEASES ANALYSIS\n")
cat("US Children Ages 0-14, 1990-2023\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

cat("Diseases included:\n")
for(i in 1:length(climate_diseases)) {
  cat(sprintf("  %d. %s\n", i, climate_diseases[i]))
}
cat("\n")

# ============================================================================
# FUNCTION: Calculate EAPC
# ============================================================================
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

# ============================================================================
# FIGURE 1: TEMPORAL TRENDS (1990-2023) - DEATHS AND DALYs
# 4 Panels matching original format
# ============================================================================
cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("GENERATING FIGURE 1: Deaths and DALYs Trends (1990-2023)\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

# Prepare rate data (average across child age groups)
child_rates <- climate_data %>%
  filter(sex_name == "Both",
         metric_name == "Rate") %>%
  group_by(cause_name, measure_name, year) %>%
  summarise(avg_rate = mean(val, na.rm = TRUE), .groups = "drop")

# Prepare number data (sum across child age groups)  
child_numbers <- climate_data %>%
  filter(sex_name == "Both",
         metric_name == "Number") %>%
  group_by(cause_name, measure_name, year) %>%
  summarise(total_number = sum(val, na.rm = TRUE), .groups = "drop")

# Function to create trend plots matching original style
create_trend_plot <- function(data, measure, y_label, title, use_numbers = FALSE) {
  
  if (use_numbers) {
    plot_data <- data %>% filter(measure_name == measure)
    y_var <- "total_number"
  } else {
    plot_data <- data %>% filter(measure_name == measure)
    y_var <- "avg_rate"
  }
  
  p <- ggplot(plot_data, aes(x = year, y = .data[[y_var]], color = cause_name)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = disease_colors, name = "") +
    labs(x = "Year", y = y_label, title = title) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    scale_x_continuous(breaks = seq(1990, 2023, by = 10)) +
    guides(color = guide_legend(ncol = 2))
  
  if (use_numbers) {
    p <- p + scale_y_continuous(labels = label_number())
  }
  
  return(p)
}

# Panel A: Deaths - Number
p_deaths_num <- create_trend_plot(child_numbers, "Deaths", "Deaths (Number)", "A", use_numbers = TRUE)

# Panel B: Deaths - Rate (ASDR)
p_deaths_rate <- create_trend_plot(child_rates, "Deaths", "ASDR (per 100,000)", "B", use_numbers = FALSE)

# Panel C: DALYs - Number
p_dalys_num <- create_trend_plot(child_numbers, "DALYs (Disability-Adjusted Life Years)", 
                                  "DALYs (Number)", "C", use_numbers = TRUE)

# Panel D: DALYs - Rate
p_dalys_rate <- create_trend_plot(child_rates, "DALYs (Disability-Adjusted Life Years)",
                                   "DALYs Rate (per 100,000)", "D", use_numbers = FALSE)

# Combine into Figure 1
figure1 <- grid.arrange(
  p_deaths_num, p_deaths_rate,
  p_dalys_num, p_dalys_rate,
  ncol = 2,
  top = "Deaths and DALYs for Climate-Associated Diseases in United States (1990-2023)"
)

# Save Figure 1
ggsave("Results/Figure1_Climate_Diseases_Trends.png", 
       plot = figure1, 
       width = 12, 
       height = 10, 
       dpi = 300)

cat("✅ Figure 1 saved: Results/Figure1_Climate_Diseases_Trends.png\n")

# ============================================================================
# FIGURE 2: INCIDENCE WITH LOG SCALE (2 Panels)
# ============================================================================
cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("GENERATING FIGURE 2: Incidence Trends with Log Scale\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

# Panel A: Incidence Number (log scale)
p_inc_num <- create_trend_plot(child_numbers, "Incidence", 
                                "Incidence (Number, log10 scale)", "A", use_numbers = TRUE) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale()))

# Panel B: Incidence Rate (log scale)
p_inc_rate <- create_trend_plot(child_rates, "Incidence",
                                 "Incidence Rate (per 100,000, log10 scale)", "B", use_numbers = FALSE) +
  scale_y_log10(labels = label_number())

# Combine into Figure 2
figure2 <- grid.arrange(
  p_inc_num, p_inc_rate,
  ncol = 2,
  top = "Incidence of Climate-Associated Diseases in United States (1990-2023) - Log Scale"
)

# Save Figure 2
ggsave("Results/Figure2_Climate_Diseases_Incidence_Log.png",
       plot = figure2,
       width = 12,
       height = 5,
       dpi = 300)

cat("✅ Figure 2 saved: Results/Figure2_Climate_Diseases_Incidence_Log.png\n")

# ============================================================================
# EAPC ANALYSIS - FOR 4 CLIMATE-ASSOCIATED DISEASES
# ============================================================================
cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("GENERATING EAPC ANALYSIS FOR CLIMATE-ASSOCIATED DISEASES\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

# Calculate EAPC for CLIMATE diseases only (4 diseases)
# Use rates and sum across age groups first (aggregate data)
eapc_input <- climate_data %>%
  filter(metric_name == "Rate") %>%
  group_by(cause_name, measure_name, sex_name, year) %>%
  summarise(val = sum(val, na.rm = TRUE), .groups = "drop")

# Now calculate EAPC for each disease/measure/sex combination
eapc_results <- eapc_input %>%
  group_by(cause_name, measure_name, sex_name) %>%
  filter(n() >= 10) %>%  # At least 10 years of data
  do(calculate_eapc(.)) %>%
  ungroup()

# Format EAPC results
eapc_table <- eapc_results %>%
  mutate(
    EAPC_formatted = sprintf("%.2f (%.2f, %.2f)", EAPC, CI_lower, CI_upper),
    trend = case_when(
      is.na(EAPC) ~ "No data",
      EAPC > 0 & CI_lower > 0 ~ "Increasing ↑",
      EAPC < 0 & CI_upper < 0 ~ "Decreasing ↓",
      TRUE ~ "Stable →"
    )
  )

# Save EAPC table
write_csv(eapc_table, "Results/Table_EAPC_Climate_Diseases.csv")

# Print EAPC results
cat("\nEAPC RESULTS (1990-2023):\n")
cat(strrep("=", 100), "\n")
cat(sprintf("%-35s %-30s %-10s %20s %15s\n", 
            "Disease", "Measure", "Sex", "EAPC (95% CI)", "Trend"))
cat(strrep("=", 100), "\n")

for(i in 1:nrow(eapc_table)) {
  cat(sprintf("%-35s %-30s %-10s %20s %15s\n",
              eapc_table$cause_name[i],
              eapc_table$measure_name[i],
              eapc_table$sex_name[i],
              eapc_table$EAPC_formatted[i],
              eapc_table$trend[i]))
}

cat("\n✅ EAPC table saved: Results/Table_EAPC_Climate_Diseases.csv\n")

# ============================================================================
# CREATE EAPC VISUALIZATION - Matching original format EXACTLY
# ============================================================================
cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("GENERATING EAPC VISUALIZATION\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

# Function to create EAPC bar plot matching original format
create_eapc_plot <- function(data, measure, panel_label) {
  
  plot_data <- data %>%
    filter(measure_name == measure) %>%
    arrange(EAPC)  # Sort by EAPC value
  
  # Reorder factor levels for plotting
  plot_data$cause_name <- factor(plot_data$cause_name, 
                                  levels = plot_data$cause_name)
  
  # Determine appropriate x-axis limits based on data range
  x_min <- floor(min(plot_data$CI_lower, na.rm = TRUE))
  x_max <- ceiling(max(plot_data$CI_upper, na.rm = TRUE))
  
  # Create horizontal bar chart with gradient fill
  p <- ggplot(plot_data, aes(x = EAPC, y = cause_name)) +
    geom_col(aes(fill = EAPC), color = "black", linewidth = 0.3) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), 
                   height = 0.3, linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#4575B4",      # Blue for negative (decrease)
      mid = "#F7F7F7",      # Light gray for near zero
      high = "#D73027",     # Red for positive (increase)
      midpoint = 0,
      name = "EAPC",
      limits = c(-15, 15),
      oob = scales::squish
    ) +
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = seq(x_min, x_max, by = 2),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      x = "EAPC (%)",
      y = "",
      title = panel_label
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 9, color = "black"),
      axis.title.x = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(p)
}

# Filter for "Both" sex only
eapc_viz_data <- eapc_table %>%
  filter(sex_name == "Both")

# Determine overall x-axis limits across all measures
x_min <- floor(min(eapc_viz_data$CI_lower, na.rm = TRUE))
x_max <- ceiling(max(eapc_viz_data$CI_upper, na.rm = TRUE))

# Update the create_eapc_plot function to use fixed limits
create_eapc_plot_fixed <- function(data, measure, panel_label, x_min, x_max) {
  plot_data <- data %>%
    filter(measure_name == measure) %>%
    arrange(EAPC)
  
  plot_data$cause_name <- factor(plot_data$cause_name, 
                                  levels = plot_data$cause_name)
  
  p <- ggplot(plot_data, aes(x = EAPC, y = cause_name)) +
    geom_col(aes(fill = EAPC), color = "black", linewidth = 0.3) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), 
                   height = 0.3, linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#4575B4",
      mid = "#F7F7F7",
      high = "#D73027",
      midpoint = 0,
      name = "EAPC",
      limits = c(-15, 15),
      oob = scales::squish
    ) +
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = seq(x_min, x_max, by = 2),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      x = "EAPC (%)",
      y = "",
      title = panel_label
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 9, color = "black"),
      axis.title.x = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(p)
}

# Create plots for each measure (A, B, C) with same x-axis
plot_a <- create_eapc_plot_fixed(eapc_viz_data, "Incidence", "A", x_min, x_max)
plot_b <- create_eapc_plot_fixed(eapc_viz_data, "Deaths", "B", x_min, x_max)
plot_c <- create_eapc_plot_fixed(eapc_viz_data, "DALYs (Disability-Adjusted Life Years)", "C", x_min, x_max)

# Combine all three plots vertically
figure3_eapc <- grid.arrange(
  plot_a, plot_b, plot_c,
  ncol = 1,
  top = "Estimated Annual Percentage Change (EAPC) of Infectious Diseases\nUnited States (1990-2023)"
)

# Save the combined plot
ggsave("Results/Figure3_EAPC_Climate_Diseases.png",
       plot = figure3_eapc,
       width = 8,
       height = 12,
       dpi = 300)

cat("✅ EAPC visualization saved: Results/Figure3_EAPC_Climate_Diseases.png\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================
cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("SUMMARY STATISTICS\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

summary_stats <- climate_data %>%
  filter(sex_name == "Both",
         metric_name == "Rate",
         year %in% c(1990, 2023)) %>%
  group_by(cause_name, measure_name, year) %>%
  summarise(rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = rate) %>%
  mutate(
    change = `2023` - `1990`,
    pct_change = 100 * (`2023` - `1990`) / `1990`
  )

cat("DISEASE BURDEN CHANGES (1990 → 2023):\n")
cat(strrep("-", 120), "\n")
cat(sprintf("%-35s %-25s %12s %12s %12s %12s\n",
            "Disease", "Measure", "1990", "2023", "Change", "% Change"))
cat(strrep("-", 120), "\n")

for(i in 1:nrow(summary_stats)) {
  cat(sprintf("%-35s %-25s %12.2f %12.2f %12.2f %11.1f%%\n",
              summary_stats$cause_name[i],
              summary_stats$measure_name[i],
              summary_stats$`1990`[i],
              summary_stats$`2023`[i],
              summary_stats$change[i],
              summary_stats$pct_change[i]))
}

cat("\n════════════════════════════════════════════════════════════════════════════\n")
cat("✅ CLIMATE-ASSOCIATED DISEASES ANALYSIS COMPLETE!\n")
cat("════════════════════════════════════════════════════════════════════════════\n\n")

cat("Generated files:\n")
cat("  1. Results/Figure1_Climate_Diseases_Trends.png (Deaths and DALYs)\n")
cat("  2. Results/Figure2_Climate_Diseases_Incidence_Log.png (Incidence with log scale)\n")
cat("  3. Results/Figure3_EAPC_Climate_Diseases.png (EAPC bar charts)\n")
cat("  4. Results/Table_EAPC_Climate_Diseases.csv (EAPC data table)\n\n")
