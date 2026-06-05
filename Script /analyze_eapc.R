# EAPC Analysis for Infectious Diseases in United States
# Estimated Annual Percentage Change (EAPC) Calculation
# Author: Script generated for KIDS_DISEASES project
# Date: 2025-10-16

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(gridExtra)

# Set working directory and load data
setwd("/Users/nam.tnguyen2022/Documents/KIDS_DISEASES ")

# Read the data
data <- read_csv("Data/IHME-GBD_2023_DATA-c62f4bc4-1.csv")

# Filter for United States, Both sexes, CHILDREN ONLY (ages 0-14)
# Years 1990-2023
# Calculate average rates across pediatric age groups
rate_data <- data %>%
  filter(location_name == "United States of America",
         sex_name == "Both",
         age_name %in% c("<5 years", "5-9 years", "10-14 years"),
         metric_name == "Rate",
         year >= 1990,
         year <= 2023) %>%
  group_by(measure_name, cause_name, year) %>%
  summarise(val = mean(val, na.rm = TRUE),  # Average rate across child age groups
            .groups = "drop")

# List of causes to analyze
causes <- c(
  "Acute hepatitis",
  "Enteric infections", 
  "HIV/AIDS",
  "Lower respiratory infections",
  "Neglected tropical diseases and malaria",
  "Other infectious diseases",
  "Sexually transmitted infections excluding HIV",
  "Tuberculosis",
  "Upper respiratory infections",
  "Otitis media"
)

# Function to calculate EAPC (Estimated Annual Percentage Change)
# EAPC is calculated by fitting a log-linear regression: ln(y) = a + b*x
# EAPC = 100 * (exp(b) - 1)
calculate_eapc <- function(data_subset) {
  if (nrow(data_subset) < 2) {
    return(data.frame(eapc = NA, lower_ci = NA, upper_ci = NA))
  }
  
  # Natural log transformation of rates
  data_subset <- data_subset %>%
    filter(val > 0) %>%  # Remove zero or negative values
    mutate(ln_val = log(val))
  
  if (nrow(data_subset) < 2) {
    return(data.frame(eapc = NA, lower_ci = NA, upper_ci = NA))
  }
  
  # Fit linear regression model: ln(rate) ~ year
  model <- lm(ln_val ~ year, data = data_subset)
  
  # Extract coefficient and confidence interval
  coef_summary <- summary(model)
  beta <- coef(model)[2]  # Slope coefficient
  
  # Calculate EAPC
  eapc <- 100 * (exp(beta) - 1)
  
  # Calculate 95% confidence interval
  ci <- confint(model, "year", level = 0.95)
  lower_ci <- 100 * (exp(ci[1]) - 1)
  upper_ci <- 100 * (exp(ci[2]) - 1)
  
  return(data.frame(
    eapc = as.numeric(eapc),
    lower_ci = as.numeric(lower_ci),
    upper_ci = as.numeric(upper_ci)
  ))
}

# Calculate EAPC for each measure and cause
eapc_results <- rate_data %>%
  filter(cause_name %in% causes) %>%
  group_by(measure_name, cause_name) %>%
  do(calculate_eapc(.)) %>%
  ungroup() %>%
  filter(!is.na(eapc))  # Remove any NAs

# Add significance indicator (if CI doesn't cross zero)
eapc_results <- eapc_results %>%
  mutate(
    significant = (lower_ci > 0 & upper_ci > 0) | (lower_ci < 0 & upper_ci < 0),
    direction = case_when(
      eapc > 0 ~ "Increase",
      eapc < 0 ~ "Decrease",
      TRUE ~ "No change"
    )
  )

# Function to create EAPC bar plot
create_eapc_plot <- function(data, measure, panel_label) {
  
  plot_data <- data %>%
    filter(measure_name == measure) %>%
    arrange(eapc)  # Sort by EAPC value
  
  # Reorder factor levels for plotting
  plot_data$cause_name <- factor(plot_data$cause_name, 
                                  levels = plot_data$cause_name)
  
  # Determine axis label
  if (measure == "Deaths") {
    y_label <- "Mortality rate"
  } else if (measure == "Incidence") {
    y_label <- "Incidence rate"
  } else if (measure == "DALYs (Disability-Adjusted Life Years)") {
    y_label <- "DALYs rate"
  }
  
  # Create color based on EAPC value (gradient from blue to red)
  p <- ggplot(plot_data, aes(x = eapc, y = cause_name)) +
    geom_col(aes(fill = eapc), color = "black", linewidth = 0.3) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
                   height = 0.3, linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#4575B4",      # Blue for negative (decrease)
      mid = "#F7F7F7",      # Light gray for near zero
      high = "#D73027",     # Red for positive (increase)
      midpoint = 0,
      name = "EAPC",
      limits = c(-15, 15),
      oob = scales::squish  # Squish values outside limits
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
    ) +
    scale_x_continuous(breaks = seq(-15, 15, by = 5))
  
  return(p)
}

# Create plots for each measure
plot_a <- create_eapc_plot(eapc_results, "Incidence", "A")
plot_b <- create_eapc_plot(eapc_results, "Deaths", "B")
plot_c <- create_eapc_plot(eapc_results, "DALYs (Disability-Adjusted Life Years)", "C")

# Combine all three plots vertically
combined_eapc <- grid.arrange(
  plot_a, plot_b, plot_c,
  ncol = 1,
  top = "Estimated Annual Percentage Change (EAPC) of Infectious Diseases\nUnited States (1990-2023)"
)

# Save the combined plot
ggsave("Results/figure3_eapc_analysis.png", 
       combined_eapc, 
       width = 8, 
       height = 12, 
       dpi = 300)

# Save EAPC results to CSV
eapc_results_wide <- eapc_results %>%
  select(measure_name, cause_name, eapc, lower_ci, upper_ci) %>%
  mutate(
    eapc_95ci = sprintf("%.2f (%.2f to %.2f)", eapc, lower_ci, upper_ci)
  ) %>%
  select(measure_name, cause_name, eapc, eapc_95ci) %>%
  arrange(measure_name, desc(eapc))

write_csv(eapc_results_wide, "Results/eapc_results.csv")

# Print summary
cat("\n=== EAPC Analysis Complete ===\n")
cat("Plot saved: Results/figure3_eapc_analysis.png\n")
cat("Data saved: Results/eapc_results.csv\n\n")

cat("Summary of EAPC Results:\n\n")

for (measure in unique(eapc_results$measure_name)) {
  cat(paste0("\n", measure, ":\n"))
  cat(paste0(rep("-", 60), collapse = ""), "\n")
  
  measure_data <- eapc_results_wide %>%
    filter(measure_name == measure)
  
  for (i in 1:nrow(measure_data)) {
    cat(sprintf("  %-50s %s\n", 
                measure_data$cause_name[i], 
                measure_data$eapc_95ci[i]))
  }
}

cat("\nNote: EAPC = Estimated Annual Percentage Change\n")
cat("Positive values indicate increasing trends\n")
cat("Negative values indicate decreasing trends\n")
cat("95% confidence intervals are shown in parentheses\n")
