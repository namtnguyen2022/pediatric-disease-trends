# Disease Burden Trends by Sex - United States
# Analyzing trends separated by sex (Male, Female, Both)
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

# SPECIFY THE DISEASE TO ANALYZE
# Change this to analyze different diseases
# Check if DISEASE_NAME is already defined (e.g., from command line)
if (!exists("DISEASE_NAME")) {
  DISEASE_NAME <- "Neglected tropical diseases and malaria"  # Default if not specified
}
# Options: "HIV/AIDS", "Tuberculosis", "Lower respiratory infections", etc.

# Filter for United States, selected disease, and CHILDREN ONLY (ages 0-14)
# Years 1990-2023

# Get Rate data for children (average of pediatric age groups)
rate_data <- data %>%
  filter(location_name == "United States of America",
         cause_name == DISEASE_NAME,
         age_name %in% c("<5 years", "5-9 years", "10-14 years"),
         metric_name == "Rate",
         year >= 1990,
         year <= 2023) %>%
  group_by(measure_name, sex_name, year) %>%
  summarise(val = mean(val, na.rm = TRUE),  # Average rate across child age groups
            .groups = "drop") %>%
  mutate(metric_name = "Rate",
         location_name = "United States of America",
         cause_name = DISEASE_NAME)

# Get Number data for children (sum across pediatric age groups)
number_data <- data %>%
  filter(location_name == "United States of America",
         cause_name == DISEASE_NAME,
         age_name %in% c("<5 years", "5-9 years", "10-14 years"),
         metric_name == "Number",
         year >= 1990,
         year <= 2023) %>%
  group_by(measure_name, sex_name, year) %>%
  summarise(val = sum(val, na.rm = TRUE),  # Sum numbers across child age groups
            upper = sum(upper, na.rm = TRUE),
            lower = sum(lower, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(metric_name = "Number",
         location_name = "United States of America",
         cause_name = DISEASE_NAME)

# Combine both datasets
filtered_data <- bind_rows(rate_data, number_data)

# Define sex colors (matching reference image style)
sex_colors <- c(
  "Both" = "#E74C3C",    # Red/coral
  "Female" = "#2ECC71",  # Green
  "Male" = "#3498DB"     # Blue
)

# Function to create trend plots by sex
create_sex_trend_plot <- function(data, measure, metric, title_label, y_label_text) {
  
  plot_data <- data %>%
    filter(measure_name == measure,
           metric_name == metric)
  
  # Create plot
  p <- ggplot(plot_data, aes(x = year, y = val, color = sex_name, group = sex_name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5, alpha = 0.7) +
    scale_color_manual(values = sex_colors, name = "Sex") +
    labs(x = "Year",
         y = y_label_text,
         title = title_label) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"),
      legend.key.size = unit(0.5, "cm"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 11, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    scale_x_continuous(breaks = seq(1990, 2025, by = 10))
  
  # Format y-axis based on metric
  if (metric == "Number") {
    max_val <- max(plot_data$val, na.rm = TRUE)
    if (max_val > 10000) {
      p <- p + scale_y_continuous(labels = label_number())
    } else {
      p <- p + scale_y_continuous(labels = label_number())
    }
  } else {
    p <- p + scale_y_continuous(labels = label_number())
  }
  
  return(p)
}

# Create all six plots
plot_a <- create_sex_trend_plot(filtered_data, "Incidence", "Number", "A", "Incidence")
plot_b <- create_sex_trend_plot(filtered_data, "Incidence", "Rate", "B", "ASIR")
plot_c <- create_sex_trend_plot(filtered_data, "Deaths", "Number", "C", "Deaths")
plot_d <- create_sex_trend_plot(filtered_data, "Deaths", "Rate", "D", "ASMR")
plot_e <- create_sex_trend_plot(filtered_data, "DALYs (Disability-Adjusted Life Years)", "Number", "E", "DALYs")
plot_f <- create_sex_trend_plot(filtered_data, "DALYs (Disability-Adjusted Life Years)", "Rate", "F", "ASDR")

# Combine all plots in a 3x2 grid
combined_plot <- grid.arrange(
  plot_a, plot_b,
  plot_c, plot_d,
  plot_e, plot_f,
  ncol = 2,
  top = paste0("Burden of ", DISEASE_NAME, 
               " in United States (1990-2023)")
)

# Save the combined plot
output_filename <- paste0("Results/figure3_children_sex_trends_", 
                          gsub("/", "_", gsub(" ", "_", tolower(DISEASE_NAME))), 
                          ".png")

ggsave(output_filename, 
       combined_plot, 
       width = 12, 
       height = 14, 
       dpi = 300)

# Create summary statistics by sex
summary_by_sex <- filtered_data %>%
  group_by(measure_name, metric_name, sex_name) %>%
  summarise(
    year_1990 = val[year == 1990][1],
    year_2023 = val[year == 2023][1],
    min_val = min(val, na.rm = TRUE),
    min_year = year[which.min(val)][1],
    max_val = max(val, na.rm = TRUE),
    max_year = year[which.max(val)][1],
    percent_change = ((year_2023 - year_1990) / year_1990) * 100,
    .groups = "drop"
  )

# Save summary statistics
summary_filename <- paste0("Results/children_sex_trends_summary_", 
                           gsub("/", "_", gsub(" ", "_", tolower(DISEASE_NAME))), 
                           ".csv")
write_csv(summary_by_sex, summary_filename)

# Print summary
cat("\n=== Sex-Specific Trend Analysis Complete ===\n")
cat(paste0("Disease analyzed: ", DISEASE_NAME, "\n\n"))
cat(paste0("Plot saved: ", output_filename, "\n"))
cat(paste0("Summary saved: ", summary_filename, "\n\n"))

cat("Summary of Changes (1990-2023):\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

for (measure in c("Deaths", "Incidence", "DALYs (Disability-Adjusted Life Years)")) {
  cat(paste0("\n", measure, ":\n"))
  cat(paste0(rep("-", 70), collapse = ""), "\n")
  
  measure_summary <- summary_by_sex %>%
    filter(measure_name == measure)
  
  for (i in 1:nrow(measure_summary)) {
    row <- measure_summary[i,]
    cat(sprintf("  %s - %s:\n", row$sex_name, row$metric_name))
    cat(sprintf("    1990: %.2f  →  2023: %.2f  (%.1f%% change)\n", 
                row$year_1990, row$year_2023, row$percent_change))
    cat(sprintf("    Peak: %.2f (year %d)\n", row$max_val, row$max_year))
  }
}

cat("\n\nNote: You can change the DISEASE_NAME variable in the script to analyze different diseases.\n")
cat("Available diseases:\n")
cat("  - HIV/AIDS\n")
cat("  - Tuberculosis\n")
cat("  - Lower respiratory infections\n")
cat("  - Upper respiratory infections\n")
cat("  - Acute hepatitis\n")
cat("  - Enteric infections\n")
cat("  - And others in your dataset\n")
