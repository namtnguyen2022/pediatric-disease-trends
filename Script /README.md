# Disease Trends Analysis - README

## Overview
This R script analyzes mortality, incidence, and DALYs (Disability-Adjusted Life Years) trends for infectious diseases in the United States from 1990 to 2023 using IHME Global Burden of Disease (GBD) 2023 data.

## Requirements

### R Packages
Install the required packages by running:
```r
install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "gridExtra", "scales"))
```

### Data
- The script expects the data file at: `Data/IHME-GBD_2023_DATA-c62f4bc4-1.csv`
- Results will be saved to: `Results/` directory

## Diseases Analyzed
The script analyzes the following infectious diseases:
1. Acute hepatitis
2. Enteric infections
3. HIV/AIDS
4. Lower respiratory infections
5. Neglected tropical diseases and malaria
6. Other infectious diseases
7. Sexually transmitted infections excluding HIV
8. Tuberculosis
9. Upper respiratory infections

## Measures Analyzed
For each disease, the script analyzes:
- **Deaths**: Number and age-standardized rate
- **Incidence**: Number and age-standardized rate  
- **DALYs**: Number and age-standardized rate

## How to Run

### Option 1: From RStudio
1. Open `Script/analyze_disease_trends.R` in RStudio
2. Click "Source" or press Cmd+Shift+S (Mac) / Ctrl+Shift+S (Windows)

### Option 2: From R Console
```r
source("Script/analyze_disease_trends.R")
```

### Option 3: From Terminal
```bash
cd "/Users/nam.tnguyen2022/Documents/KIDS_DISEASES "
Rscript "Script /analyze_disease_trends.R"
```

## Output Files

The script generates the following files in the `Results/` directory:

1. **disease_trends_combined.png** - Combined 6-panel plot showing all measures (12" x 14")
   - Panel A: Deaths (Number)
   - Panel B: Deaths (Rate)
   - Panel C: Incidence (Number)
   - Panel D: Incidence (Rate)
   - Panel E: DALYs (Number)
   - Panel F: DALYs (Rate)

2. **deaths_trends.png** - Deaths trends (Number and Rate)
3. **incidence_trends.png** - Incidence trends (Number and Rate)
4. **dalys_trends.png** - DALYs trends (Number and Rate)
5. **summary_statistics.csv** - Summary statistics including:
   - Mean rates
   - Minimum and maximum values with years
   - Percent change from 1990 to 2023

## Plot Features
- Time series line plots (1990-2023)
- Color-coded by disease category
- Left column: Absolute numbers
- Right column: Age-standardized rates per 100,000
- Formatted axis labels with appropriate scaling (K for thousands, M for millions)
- Legend positioned at top with disease names

## Customization

### To modify the color scheme:
Edit the `disease_colors` vector in the script (lines 37-47)

### To analyze different age groups:
Change the filter on line 23:
```r
age_name == "Age-standardized"  # Change to specific age group like "<5 years"
```

### To analyze specific sex:
Change the filter on line 22:
```r
sex_name == "Both"  # Change to "Male" or "Female"
```

### To modify time period:
Change the year filters on lines 24-25

## Notes
- The script uses age-standardized rates for better comparison across time
- Both sexes combined are analyzed by default
- Data source: IHME Global Burden of Disease Study 2023
- All plots are saved at 300 DPI for publication quality

## Troubleshooting

**Error: Cannot find data file**
- Ensure the CSV file is in the `Data/` directory
- Check that the filename matches exactly

**Error: Package not found**
- Install missing packages using `install.packages()`

**Warning messages about "no visible binding"**
- These are normal warnings from R CMD check and can be safely ignored
- They relate to non-standard evaluation in dplyr and ggplot2
