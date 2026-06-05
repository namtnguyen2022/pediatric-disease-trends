# Pediatric Infectious Disease Trends — United States (1990–2023)

Analysis of infectious disease burden among U.S. children aged 0–14 years using Global Burden of Disease (GBD) 2023 data.

## Overview

This project examines trends in incidence, mortality, and disability-adjusted life years (DALYs) for 10 infectious disease categories from 1990 to 2023. Key analyses include estimated annual percentage change (EAPC), joinpoint regression to identify inflection points, and age- and sex-stratified comparisons.

## Data

**Source:** IHME Global Burden of Disease 2023  
**Population:** U.S. children, ages 0–14 years  
**Period:** 1990–2023  
**Diseases:** Enteric infections, lower/upper respiratory infections, neglected tropical diseases & malaria, HIV/AIDS, tuberculosis, sexually transmitted infections, acute hepatitis, otitis media, and other infectious diseases

> Raw data (`Data/`) is not included in this repository.

## Scripts

| Script | Description |
|--------|-------------|
| `Script/joinpoint_analysis.R` | Joinpoint regression with breakpoint detection (segmented package, BIC selection) |
| `Script/analyze_eapc.R` | EAPC calculation via log-linear regression |
| `Script/analyze_disease_trends.R` | Overall disease trend plots |
| `Script/analyze_pediatric_trends.R` | Pediatric-specific trend figures |
| `Script/analyze_sex_trends.R` | Sex-stratified analysis by disease |
| `Script/age_group_analysis.R` | Age-specific burden comparisons (<5, 5–9, 10–14 years) |
| `Script/create_publication_tables.R` | Publication-ready tables (Word + CSV) |
| `Script/format_joinpoint_table.R` | Supplementary Table 7 formatting |
| `Script/create_revision_markup.R` | Manuscript revision tracked-changes document |
| `analyze_age_groups.R` | Age-specific key findings summary |

## Results

- **Figures 1–3:** Publication-ready trend and EAPC figures
- **Supplementary Figures 1–3:** Joinpoint regression plots (incidence, mortality, DALYs)
- **Tables 1–3:** Age-standardized incidence, mortality, and DALY burden with EAPC
- **Supplementary Table 7:** Joinpoint results with segment-specific APCs and policy milestone annotations

## Requirements

```r
install.packages(c("tidyverse", "ggplot2", "segmented", "gridExtra",
                   "cowplot", "ggpubr", "flextable", "officer", "scales"))
```

## Key Findings

- Overall decline in pediatric infectious disease burden from 1990–2023
- Enteric infection incidence reversed after 2015 (APC: +20.71%/year)
- LRI mortality showed a sharp decline around 1998–1999, coinciding with PCV7 introduction
- Children under 5 carry disproportionately higher burden across all measures
