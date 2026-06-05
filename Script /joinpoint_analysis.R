# ============================================================
# JOINPOINT REGRESSION ANALYSIS
# U.S. Pediatric Infectious Diseases 1990-2023
# Addresses Reviewer #1 comments on trend modeling
# ============================================================

library(tidyverse)
library(segmented)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

# ── 1. LOAD DATA ─────────────────────────────────────────────
data <- read_csv("Data/IHME-GBD_2023_DATA-c62f4bc4-1.csv", show_col_types = FALSE)

climate_diseases <- c(
  "Enteric infections",
  "Lower respiratory infections",
  "Upper respiratory infections",
  "Neglected tropical diseases and malaria"
)

disease_colors <- c(
  "Enteric infections"                          = "#E41A1C",
  "Lower respiratory infections"                = "#377EB8",
  "Upper respiratory infections"                = "#4DAF4A",
  "Neglected tropical diseases and malaria"     = "#FF7F00"
)

disease_labels <- c(
  "Enteric infections"                          = "Enteric Infections",
  "Lower respiratory infections"                = "Lower Respiratory Infections",
  "Upper respiratory infections"                = "Upper Respiratory Infections",
  "Neglected tropical diseases and malaria"     = "NTD & Malaria"
)

# Policy milestone years for annotation
policy_milestones <- data.frame(
  year        = c(1994, 2000, 2006, 2020),
  label       = c("VFC (1994)", "PCV7 (2000)", "RV Vaccine (2006)", "COVID-19 (2020)"),
  label_short = c("VFC\n1994",  "PCV7\n2000",  "RV\n2006",          "COVID\n2020")
)

# ── 2. AGGREGATE: average rate across age groups ──────────────
agg_rates <- data %>%
  filter(
    cause_name    %in% climate_diseases,
    location_name == "United States of America",
    sex_name      == "Both",
    metric_name   == "Rate"
  ) %>%
  group_by(cause_name, measure_name, year) %>%
  summarise(rate = mean(val), .groups = "drop")

# ── 3. JOINPOINT FUNCTION ─────────────────────────────────────
# Uses the `segmented` package to fit 0–3 joinpoints
# Returns the best model by BIC, fitted values, breakpoints,
# and annual percentage changes (APC) per segment.

run_joinpoint <- function(df, max_breaks = 3) {
  years <- df$year
  rates <- df$rate

  # log-transform rate for EAPC-style modeling
  log_rate <- log(rates)

  # base linear model
  base_lm <- lm(log_rate ~ years)

  best_bic  <- BIC(base_lm)
  best_mod  <- base_lm
  best_k    <- 0

  # try adding 1 … max_breaks joinpoints
  for (k in seq_len(max_breaks)) {
    psi_start <- seq(min(years) + 5, max(years) - 5, length.out = k)
    tryCatch({
      seg_mod <- segmented(base_lm,
                           seg.Z   = ~years,
                           psi     = psi_start,
                           control = seg.control(it.max = 100, tol = 1e-6))
      b <- BIC(seg_mod)
      if (b < best_bic) {
        best_bic <- b
        best_mod <- seg_mod
        best_k   <- k
      }
    }, error = function(e) NULL)
  }

  # ── fitted values on original scale
  fitted_log  <- fitted(best_mod)
  fitted_vals <- exp(fitted_log)

  # ── breakpoints
  breakpoints <- if (best_k > 0) round(best_mod$psi[, "Est."]) else NULL

  # ── APC per segment
  seg_years <- sort(c(min(years), breakpoints, max(years)))

  # Use slope() safely; fall back to manual calculation if it fails
  slopes <- tryCatch({
    slope(best_mod, .coef = coef(best_mod))$years[, "Est."]
  }, error = function(e) {
    # Manual fallback: extract slopes from fitted log values at segment boundaries
    sapply(seq_len(length(seg_years) - 1), function(i) {
      idx1 <- which(years == seg_years[i])
      idx2 <- which(years == seg_years[i + 1])
      if (length(idx1) == 0) idx1 <- which.min(abs(years - seg_years[i]))
      if (length(idx2) == 0) idx2 <- which.min(abs(years - seg_years[i + 1]))
      (fitted_log[idx2] - fitted_log[idx1]) / (years[idx2] - years[idx1])
    })
  })

  # Ensure slopes length matches number of segments
  n_segs <- length(seg_years) - 1
  if (length(slopes) != n_segs) {
    slopes <- sapply(seq_len(n_segs), function(i) {
      idx1 <- which.min(abs(years - seg_years[i]))
      idx2 <- which.min(abs(years - seg_years[i + 1]))
      (fitted_log[idx2] - fitted_log[idx1]) / (years[idx2] - years[idx1])
    })
  }

  segments_df <- map2_dfr(
    seq_len(n_segs),
    slopes,
    ~ data.frame(
        start = seg_years[.x],
        end   = seg_years[.x + 1],
        APC   = round(100 * (exp(.y) - 1), 2)
      )
  )

  list(
    model       = best_mod,
    n_breaks    = best_k,
    breakpoints = breakpoints,
    fitted      = data.frame(year = years, fitted = fitted_vals, log_rate = log_rate),
    segments    = segments_df,
    bic         = best_bic
  )
}

# ── 4. RUN FOR ALL DISEASE × MEASURE COMBINATIONS ────────────
measures <- c("Incidence", "Deaths", "DALYs (Disability-Adjusted Life Years)")

results_list <- list()

for (disease in climate_diseases) {
  for (measure in measures) {
    df_sub <- agg_rates %>%
      filter(cause_name == disease, measure_name == measure) %>%
      arrange(year)

    if (nrow(df_sub) < 10) next

    cat(sprintf("\n--- %s | %s ---\n", disease, measure))

    jp <- run_joinpoint(df_sub)

    cat(sprintf("  Best model: %d joinpoint(s)\n", jp$n_breaks))
    if (!is.null(jp$breakpoints))
      cat(sprintf("  Breakpoints: %s\n", paste(jp$breakpoints, collapse = ", ")))

    print(jp$segments)

    results_list[[paste(disease, measure, sep = " | ")]] <- list(
      disease     = disease,
      measure     = measure,
      joinpoint   = jp,
      data        = df_sub
    )
  }
}

# ── 5. SUMMARY TABLE ──────────────────────────────────────────
cat("\n\n====================================================\n")
cat("JOINPOINT SUMMARY TABLE\n")
cat("====================================================\n")

summary_rows <- map_dfr(results_list, function(x) {
  segs <- x$joinpoint$segments
  segs$disease     <- x$disease
  segs$measure     <- x$measure
  segs$n_joinpoints <- x$joinpoint$n_breaks
  segs
}) %>%
  mutate(
    period    = paste0(start, "-", end),
    direction = ifelse(APC > 0, "Increasing", "Decreasing")
  ) %>%
  dplyr::select(disease, measure, n_joinpoints, period, APC, direction)

print(as.data.frame(summary_rows), row.names = FALSE)

# Save summary table
write_csv(summary_rows, "Results/Table_Joinpoint_Summary.csv")
cat("\nSaved: Results/Table_Joinpoint_Summary.csv\n")

# ── 6. FIGURES ────────────────────────────────────────────────
make_jp_plot <- function(res_item, measure_label, use_log = FALSE) {
  disease  <- res_item$disease
  jp       <- res_item$joinpoint
  df_raw   <- res_item$data
  df_fit   <- jp$fitted
  segs     <- jp$segments
  bps      <- jp$breakpoints

  # build APC label string for subtitle
  apc_labels <- paste0(
    segs$start, "\u2013", segs$end, ": APC=", sprintf("%+.2f%%", segs$APC),
    collapse = "  |  "
  )

  # y-axis: log scale makes recent upticks visible even when early values dominate
  y_scale <- if (use_log) {
    scale_y_log10(labels = scales::comma)
  } else {
    scale_y_continuous(labels = scales::comma)
  }

  y_label <- if (use_log) {
    paste0(measure_label, " per 100,000 population, log scale")
  } else {
    paste0(measure_label, " per 100,000 population")
  }

  # Build a small data frame for breakpoints (empty if none) — always register both legend levels
  bp_df <- if (!is.null(bps)) data.frame(year = bps) else data.frame(year = numeric(0))

  p <- ggplot() +
    # raw data points
    geom_point(data = df_raw,
               aes(x = year, y = rate),
               color = disease_colors[disease], alpha = 0.7, size = 2) +
    # fitted segmented line
    geom_line(data = df_fit,
              aes(x = year, y = fitted),
              color = disease_colors[disease], linewidth = 1.3) +
    # ── Policy milestones: light grey long-dashed, very subtle ──
    geom_vline(data = policy_milestones,
               aes(xintercept = year, linetype = "Policy milestone"),
               color = "grey70", linewidth = 0.45) +
    geom_text(data = policy_milestones,
              aes(x = year, y = Inf, label = label_short),
              vjust = 1.4, hjust = -0.08, size = 2.5, color = "grey55",
              fontface = "italic") +
    # ── Model breakpoints: dark solid, clearly distinct ──
    # Always drawn (empty data = no lines) so both legend keys always render
    geom_vline(data = bp_df,
               aes(xintercept = year, linetype = "Estimated breakpoint"),
               color = "#333333", linewidth = 0.85) +
    # Legend for the two line types
    scale_linetype_manual(
      name   = NULL,
      values = c("Policy milestone"     = "dashed",
                 "Estimated breakpoint" = "solid")
    ) +
    y_scale +
    scale_x_continuous(breaks = seq(1990, 2023, 5)) +
    labs(
      title    = disease_labels[disease],
      subtitle = apc_labels,
      x        = "Year",
      y        = y_label
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8.5, color = "grey25"),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y      = element_text(size = 10),
      axis.title       = element_text(size = 11),
      panel.grid.minor = element_blank(),
      legend.position      = "none",   # removed — shared legend added at figure level
      legend.text          = element_text(size = 8),
      legend.key.width     = unit(1.2, "cm"),
      legend.background    = element_blank()
    )

  p
}

# ── Figure for each measure
for (measure in measures) {
  measure_short <- case_when(
    measure == "Incidence"                                ~ "Incidence Rate",
    measure == "Deaths"                                   ~ "Mortality Rate",
    measure == "DALYs (Disability-Adjusted Life Years)"  ~ "DALYs Rate",
    TRUE                                                  ~ measure
  )

  # Use log scale for all measures — large between-disease spread on all outcomes
  # (e.g., URI incidence ~6M x NTD; LRI mortality ~65x NTD) makes linear scale
  # compress smaller diseases into a flat line, hiding meaningful trends.
  use_log <- TRUE

  plots <- map(
    climate_diseases,
    ~ make_jp_plot(
        results_list[[paste(.x, measure, sep = " | ")]],
        measure_short,
        use_log = use_log
      )
  )

  fname <- paste0(
    "Results/Figure_Joinpoint_",
    gsub("[^A-Za-z]", "_", measure_short),
    ".png"
  )

  # ── Build a dummy plot that carries the legend, then extract it ──
  legend_plot <- plots[[1]] +
    theme(
      legend.position   = "bottom",
      legend.text       = element_text(size = 10),
      legend.key.width  = unit(1.6, "cm"),
      legend.background = element_blank()
    )
  shared_legend <- cowplot::get_legend(legend_plot)

  # ── Strip legend from all panels (already none, but be explicit) ──
  panels_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

  # ── Compose: 2×2 panels + shared legend at bottom ──
  figure_body <- plot_grid(
    plotlist = panels_no_legend,
    ncol     = 2,
    align    = "hv"
  )

  title_grob <- ggdraw() +
    draw_label(
      paste("Segmented Regression \u2013", measure_short, "\nU.S. Children 0\u201314 Years"),
      fontface = "bold",
      size     = 14,
      x = 0.5, hjust = 0.5
    )

  final_fig <- plot_grid(
    title_grob,
    figure_body,
    shared_legend,
    ncol    = 1,
    rel_heights = c(0.08, 1, 0.07)
  )

  png(fname, width = 14, height = 10.5, units = "in", res = 300)
  print(final_fig)
  dev.off()
  cat(sprintf("Saved: %s\n", fname))
}

# ── 7. COMBINED 3-PANEL FIGURE (for manuscript) ───────────────
# One row per measure, showing all 4 diseases on same axes

make_combined_plot <- function(measure, measure_label, show_legend = FALSE,
                               show_milestone_labels = FALSE) {
  all_data   <- map_dfr(climate_diseases, function(d) {
    key <- paste(d, measure, sep = " | ")
    if (is.null(results_list[[key]])) return(NULL)
    results_list[[key]]$data %>% mutate(disease = d)
  })
  all_fitted <- map_dfr(climate_diseases, function(d) {
    key <- paste(d, measure, sep = " | ")
    if (is.null(results_list[[key]])) return(NULL)
    results_list[[key]]$joinpoint$fitted %>% mutate(disease = d)
  })

  # y-axis max for placing milestone labels near top
  y_max <- max(all_data$rate, na.rm = TRUE)

  # milestone label data — only show on first panel
  milestone_label_df <- policy_milestones %>%
    mutate(
      label_short = c("VFC\n(1994)", "PCV7\n(2000)", "RV\n(2006)", "COVID-19\n(2020)")
    )

  p <- ggplot() +
    geom_point(data = all_data,
               aes(x = year, y = rate, color = disease),
               alpha = 0.4, size = 1.2) +
    geom_line(data = all_fitted,
              aes(x = year, y = fitted, color = disease),
              linewidth = 1.1) +
    # policy milestone dashed lines — all panels, bold and dark
    geom_vline(data = policy_milestones,
               aes(xintercept = year),
               linetype = "dashed", color = "#222222", linewidth = 1.1) +
    # milestone text labels — only on first panel
    { if (show_milestone_labels)
        geom_text(data = milestone_label_df,
                  aes(x = year, y = y_max, label = label_short),
                  vjust = 1.1, hjust = -0.1, size = 3.2, color = "#111111",
                  fontface = "bold", lineheight = 0.85)
      else list() } +
    scale_color_manual(values = disease_colors, labels = disease_labels,
                       name = NULL) +
    scale_x_continuous(breaks = seq(1990, 2023, 5)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = measure_label,
      x     = "Year",
      y     = paste(measure_label, "(per 100,000)")
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y      = element_text(size = 9),
      axis.title       = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position  = if (show_legend) "bottom" else "none",
      legend.text      = element_text(size = 10),
      legend.key.width = unit(1.5, "cm")
    )
  p
}

# Panel 1: Incidence — show milestone labels here
p1 <- make_combined_plot("Incidence",
                         "Age-Standardized Incidence Rate",
                         show_legend         = FALSE,
                         show_milestone_labels = TRUE)

# Panel 2: Mortality — no labels, no legend
p2 <- make_combined_plot("Deaths",
                         "Age-Standardized Mortality Rate",
                         show_legend         = FALSE,
                         show_milestone_labels = FALSE)

# Panel 3: DALYs — legend only here
p3 <- make_combined_plot("DALYs (Disability-Adjusted Life Years)",
                         "Age-Standardized DALY Rate",
                         show_legend         = TRUE,
                         show_milestone_labels = FALSE)

# Extract the shared legend from p3
library(ggpubr)
shared_legend <- get_legend(p3)
p3_no_legend  <- p3 + theme(legend.position = "none")

png("Results/Supplementary_Figure2_Joinpoint_Trends.png",
    width = 18, height = 7.5, units = "in", res = 300)
grid.arrange(
  arrangeGrob(p1, p2, p3_no_legend, ncol = 3),
  shared_legend,
  nrow    = 2,
  heights = c(6, 1.2),
  top     = textGrob(
    "Supplementary Figure 2. Joinpoint Regression Analysis of Age-Standardized Infectious Disease Rates\nU.S. Children 0\u201314 Years, 1990\u20132023",
    gp = gpar(fontsize = 13, fontface = "bold")
  )
)
dev.off()
cat("Saved: Results/Supplementary_Figure2_Joinpoint_Trends.png\n")

cat("\nJoinpoint analysis complete.\n")
cat("Output files:\n")
cat("  Results/Table_Joinpoint_Summary.csv\n")
cat("  Results/Figure_Joinpoint_Incidence_Rate.png\n")
cat("  Results/Figure_Joinpoint_Mortality_Rate.png\n")
cat("  Results/Figure_Joinpoint_DALY_Rate.png\n")
cat("  Results/Figure_Joinpoint_Combined.png\n")
