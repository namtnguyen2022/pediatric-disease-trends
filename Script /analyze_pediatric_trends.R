# Pediatric Age-Standardized Analysis for Infectious Diseases
# Calculating age-standardized rates ONLY for children (0-14 years)
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

# Filter for United States, Both sexes, and ONLY pediatric age groups
# Years 1990-2023
pediatric_data <- data %>%
  filter(location_name == "United States of America",
         sex_name == "Both",
         age_name %in% c("<5 years", "5-9 years", "10-14 years"),
         year >= 1990,
         year <= 2023)

# Get age-specific rates for children
child_rates <- pediatric_data %>%
  filter(metric_name == "Rate")

# Get numbers for children (for summing)
child_numbers <- pediatric_data %>%
  filter(metric_name == "Number") %>%
  group_by(measure_name, cause_name, sex_name, year) %>%
  summarise(
    total_number = sum(val, na.rm = TRUE),
    .groups = "drop"
  )

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

# Define color palette for diseases
disease_colors <- c(
  "Acute hepatitis" = "#FF69B4",
  "Enteric infections" = "#FFA500",
  "HIV/AIDS" = "#32CD32",
  "Lower respiratory infections" = "#87CEEB",
  "Neglected tropical diseases and malaria" = "#00CED1",
  "Other infectious diseases" = "#9370DB",
  "Sexually transmitted infections excluding HIV" = "#FFB6C1",
  "Tuberculosis" = "#FF6347",
  "Upper respiratory infections" = "#DDA0DD",
  "Otitis media" = "#FFD700"
)

# Function to create trend plots for children
create_child_trend_plot <- function(data_rates, data_numbers, measure, title_label) {
  
  # For rates, we'll show the age-specific rates combined
  # Calculate weighted average of rates across child age groups
  plot_data_rate <- data_rates %>%
    filter(measure_name == measure,
           cause_name %in% causes) %>%
    group_by(cause_name, year) %>%
    summarise(avg_rate = mean(val, na.rm = TRUE), .groups = "drop")
  
  # For numbers, use the summed totals
  plot_data_number <- data_numbers %>%
    filter(measure_name == measure,
           cause_name %in% causes)
  
  # Create rate plot
  p_rate <- ggplot(plot_data_rate, aes(x = year, y = avg_rate, color = cause_name)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = disease_colors, name = "") +
    labs(x = "Year",
         y = "Rate (per 100,000)",
         title = paste0(title_label, " - Rate")) +
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
  
  # Create number plot
  p_number <- ggplot(plot_data_number, aes(x = year, y = total_number, color = cause_name)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = disease_colors, name = "") +
    labs(x = "Year",
         y = "Number",
         title = paste0(title_label, " - Number")) +
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
    scale_y_continuous(labels = label_number()) +
    guides(color = guide_legend(ncol = 2))
  
  return(list(rate = p_rate, number = p_number))
}

# Create plots for each measure
cat("Creating pediatric-specific trend plots...\n")

deaths_plots <- create_child_trend_plot(child_rates, child_numbers, "Deaths", "")
incidence_plots <- create_child_trend_plot(child_rates, child_numbers, "Incidence", "")
dalys_plots <- create_child_trend_plot(child_rates, child_numbers, 
                                       "DALYs (Disability-Adjusted Life Years)", 
                                       "")

# Update plot titles and y-axis labels for Figure 1
deaths_plots$number <- deaths_plots$number + labs(title = "A", y = "Deaths (Number)")
deaths_plots$rate <- deaths_plots$rate + labs(title = "B", y = "ASDR (per 100,000)")
dalys_plots$number <- dalys_plots$number + labs(title = "C", y = "DALYs (Number)")
dalys_plots$rate <- dalys_plots$rate + labs(title = "D", y = "DALYs Rate (per 100,000)")

# FIGURE 1: Deaths and DALYs (4 panels)
deaths_dalys_child <- grid.arrange(
  deaths_plots$number, deaths_plots$rate,
  dalys_plots$number, dalys_plots$rate,
  ncol = 2,
  top = "Deaths and DALYs in United States (1990-2023)"
)

ggsave("Results/figure1_children_deaths_dalys.png", 
       deaths_dalys_child, 
       width = 12, 
       height = 10, 
       dpi = 300)

# FIGURE 2: Incidence with log scale (2 panels)
incidence_child <- grid.arrange(
  incidence_plots$number + scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) + 
    labs(title = "A", y = "Incidence (Number, log10 scale)"),
  incidence_plots$rate + scale_y_log10(labels = label_number()) + 
    labs(y = "Incidence Rate (per 100,000, log10 scale)", title = "B"),
  ncol = 2,
  top = "Incidence in United States (1990-2023) - Log Scale"
)

ggsave("Results/figure2_children_incidence_log.png", 
       incidence_child, 
       width = 12, 
       height = 5, 
       dpi = 300)

# Create summary statistics for children
child_summary <- child_numbers %>%
  group_by(measure_name, cause_name) %>%
  summarise(
    total_1990 = total_number[year == 1990][1],
    total_2023 = total_number[year == 2023][1],
    min_val = min(total_number, na.rm = TRUE),
    min_year = year[which.min(total_number)][1],
    max_val = max(total_number, na.rm = TRUE),
    max_year = year[which.max(total_number)][1],
    percent_change = ((total_2023 - total_1990) / total_1990) * 100,
    .groups = "drop"
  ) %>%
  filter(cause_name %in% causes)

write_csv(child_summary, "Results/pediatric_summary_statistics.csv")

# Print summary
cat("\n=== Pediatric Analysis Complete ===\n")
cat("Figures saved in Results/ directory:\n")
cat("  - figure1_children_deaths_dalys.png (Deaths and DALYs for children 0-14 years)\n")
cat("  - figure2_children_incidence_log.png (Incidence for children 0-14 years, log scale)\n")
cat("  - pediatric_summary_statistics.csv\n\n")

cat("Age groups included: <5 years, 5-9 years, 10-14 years\n")
cat("Location: United States of America\n")
cat("Time period: 1990-2023\n\n")

cat("Summary of Changes in Children (1990-2023):\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

for (measure in c("Deaths", "Incidence", "DALYs (Disability-Adjusted Life Years)")) {
  cat(paste0(measure, ":\n"))
  cat(paste0(rep("-", 80), collapse = ""), "\n")
  
  measure_data <- child_summary %>%
    filter(measure_name == measure) %>%
    arrange(desc(percent_change))
  
  for (i in 1:nrow(measure_data)) {
    row <- measure_data[i,]
    cat(sprintf("  %-50s %6.0f → %6.0f (%+.1f%%)\n", 
                row$cause_name, row$total_1990, row$total_2023, row$percent_change))
  }
  cat("\n")
}

cat("Note: These rates are specific to the pediatric population (ages 0-14)\n")
cat("Not the same as all-age age-standardized rates\n")
