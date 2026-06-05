# Disease Trends Analysis for United States (1990-2023)
# Analyzing Mortality, Incidence, and DALYs by cause
# Author: Script generated for KIDS_DISEASES project
# Date: 2025-10-16

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(gridExtra)
library(scales)

# Set working directory and load data
setwd("/Users/nam.tnguyen2022/Documents/KIDS_DISEASES ")

# Read the data
data <- read_csv("Data/IHME-GBD_2023_DATA-c62f4bc4-1.csv")

# Filter for United States, Both sexes
# Years 1990-2023
# For Numbers: sum all age groups (not age-standardized)
# For Rates: use age-standardized

# Get Rate data (age-standardized)
rate_data <- data %>%
  filter(location_name == "United States of America",
         sex_name == "Both",
         age_name == "Age-standardized",
         metric_name == "Rate",
         year >= 1990,
         year <= 2023)

# Get Number data (sum across all age groups)
number_data <- data %>%
  filter(location_name == "United States of America",
         sex_name == "Both",
         age_name != "Age-standardized",  # Exclude age-standardized
         metric_name == "Number",
         year >= 1990,
         year <= 2023) %>%
  group_by(measure_name, cause_name, year) %>%
  summarise(val = sum(val, na.rm = TRUE),
            upper = sum(upper, na.rm = TRUE),
            lower = sum(lower, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(metric_name = "Number",
         location_name = "United States of America",
         sex_name = "Both")

# Combine both datasets
filtered_data <- bind_rows(rate_data, number_data)

# List of causes to analyze (all available causes)
causes <- c(
  "Acute hepatitis",
  "Enteric infections", 
  "HIV/AIDS",
  "Lower respiratory infections",
  "Neglected tropical diseases and malaria",
  "Other infectious diseases",
  "Sexually transmitted infections excluding HIV",
  "Tuberculosis",
  "Upper respiratory infections"
)

# Define color palette for diseases (matching the reference image style)
disease_colors <- c(
  "Acute hepatitis" = "#FF69B4",
  "Enteric infections" = "#FFA500",
  "HIV/AIDS" = "#32CD32",
  "Lower respiratory infections" = "#87CEEB",
  "Neglected tropical diseases and malaria" = "#00CED1",
  "Other infectious diseases" = "#9370DB",
  "Sexually transmitted infections excluding HIV" = "#FFB6C1",
  "Tuberculosis" = "#FF6347",
  "Upper respiratory infections" = "#DDA0DD"
)

# Function to create individual plots
create_trend_plot <- function(data, measure, metric, title_suffix) {
  
  plot_data <- data %>%
    filter(measure_name == measure,
           metric_name == metric,
           cause_name %in% causes)
  
  # Determine y-axis label based on measure and metric
  if (measure == "Deaths" && metric == "Number") {
    y_label <- "Deaths"
  } else if (measure == "Deaths" && metric == "Rate") {
    y_label <- "ASMR"
  } else if (measure == "Incidence" && metric == "Number") {
    y_label <- "Incidence"
  } else if (measure == "Incidence" && metric == "Rate") {
    y_label <- "ASIR"
  } else if (measure == "DALYs (Disability-Adjusted Life Years)" && metric == "Number") {
    y_label <- "DALYs (Disability-Adjusted Life Years)"
  } else if (measure == "DALYs (Disability-Adjusted Life Years)" && metric == "Rate") {
    y_label <- "ASDR"
  } else {
    y_label <- paste(measure, metric)
  }
  
  # Create plot with panel label
  p <- ggplot(plot_data, aes(x = year, y = val, color = cause_name)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = disease_colors,
                       name = "") +
    labs(x = "Year",
         y = y_label,
         title = title_suffix) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    scale_x_continuous(breaks = seq(1990, 2023, by = 10)) +
    guides(color = guide_legend(ncol = 2))
  
  # Format y-axis based on scale
  max_val <- max(plot_data$val, na.rm = TRUE)
  if (max_val > 1000000) {
    p <- p + scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))
  } else if (max_val > 1000) {
    p <- p + scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))
  } else {
    p <- p + scale_y_continuous(labels = label_number())
  }
  
  return(p)
}

# Create all plots
# Deaths
deaths_number <- create_trend_plot(filtered_data, "Deaths", "Number", "A")
deaths_rate <- create_trend_plot(filtered_data, "Deaths", "Rate", "B")

# DALYs
dalys_number <- create_trend_plot(filtered_data, "DALYs (Disability-Adjusted Life Years)", "Number", "C")
dalys_rate <- create_trend_plot(filtered_data, "DALYs (Disability-Adjusted Life Years)", "Rate", "D")

# Incidence (with log scale for better visualization)
incidence_number <- create_trend_plot(filtered_data, "Incidence", "Number", "A") +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  labs(y = "Incidence (log10 scale)")

incidence_rate <- create_trend_plot(filtered_data, "Incidence", "Rate", "B") +
  scale_y_log10(labels = label_number()) +
  labs(y = "ASIR (log10 scale)")

# FIGURE 1: Deaths and DALYs (2 columns x 2 rows)
deaths_dalys_plot <- grid.arrange(
  deaths_number, deaths_rate,
  dalys_number, dalys_rate,
  ncol = 2,
  top = "Deaths and DALYs Trends in United States (1990-2023)"
)

# Save Figure 1
ggsave("Results/figure1_deaths_dalys.png", 
       deaths_dalys_plot, 
       width = 12, 
       height = 10, 
       dpi = 300)

# FIGURE 2: Incidence with log scale (2 columns x 1 row)
incidence_plot <- grid.arrange(
  incidence_number, incidence_rate,
  ncol = 2,
  top = "Incidence Trends in United States (1990-2023) - Log Scale"
)

# Save Figure 2
ggsave("Results/figure2_incidence_log.png", 
       incidence_plot, 
       width = 12, 
       height = 5, 
       dpi = 300)

# Create summary statistics table
summary_stats <- filtered_data %>%
  filter(cause_name %in% causes,
         metric_name == "Rate") %>%
  group_by(measure_name, cause_name) %>%
  summarise(
    mean_rate = mean(val, na.rm = TRUE),
    min_year = year[which.min(val)],
    min_rate = min(val, na.rm = TRUE),
    max_year = year[which.max(val)],
    max_rate = max(val, na.rm = TRUE),
    percent_change = ((val[year == max(year)] - val[year == min(year)]) / 
                       val[year == min(year)]) * 100,
    .groups = "drop"
  )

# Save summary statistics
write_csv(summary_stats, "Results/summary_statistics.csv")

# Print summary
cat("\n=== Analysis Complete ===\n")
cat("Plots saved in Results/ directory:\n")
cat("  - figure1_deaths_dalys.png (Deaths and DALYs - 4 panels)\n")
cat("  - figure2_incidence_log.png (Incidence with log scale - 2 panels)\n")
cat("  - summary_statistics.csv\n\n")

cat("Diseases analyzed:\n")
for (cause in causes) {
  cat(paste0("  - ", cause, "\n"))
}

cat("\nMeasures analyzed:\n")
cat("  - Deaths (Number and Rate)\n")
cat("  - Incidence (Number and Rate)\n")
cat("  - DALYs (Number and Rate)\n")
cat("\nTime period: 1990-2023\n")
cat("Location: United States of America\n")
cat("Population: Both sexes, Age-standardized\n")
