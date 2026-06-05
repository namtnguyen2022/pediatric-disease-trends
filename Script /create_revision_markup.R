library(officer)
library(dplyr)

# ============================================================
# Manuscript Revision Markup Document
# Bold = NEW text to ADD
# Strikethrough = text to REMOVE or REPLACE
# Regular = keep as-is
# ============================================================

doc <- read_docx()

# Helper: add a section heading
add_heading <- function(doc, text, level = 1) {
  doc <- doc %>%
    body_add_par(text, style = if (level == 1) "heading 1" else "heading 2")
  doc
}

# Helper: build a paragraph with mixed formatting chunks
# Each chunk is list(text, bold, strike, italic, color)
add_mixed_par <- function(doc, chunks, style = "Normal") {
  p <- fpar()
  runs <- lapply(chunks, function(ch) {
    fp <- fp_text(
      bold       = isTRUE(ch$bold),
      strike     = isTRUE(ch$strike),
      italic     = isTRUE(ch$italic),
      color      = if (!is.null(ch$color)) ch$color else "black",
      shading.color = if (isTRUE(ch$bold) && !isTRUE(ch$strike)) "#FFFF99" else "transparent",
      font.size  = 11,
      font.family = "Times New Roman"
    )
    ftext(ch$text, fp)
  })
  p <- do.call(fpar, runs)
  doc <- body_add_fpar(doc, p, style = style)
  doc
}

# Shortcut constructors
keep   <- function(t) list(text = t, bold = FALSE, strike = FALSE)
add    <- function(t) list(text = t, bold = TRUE,  strike = FALSE, color = "#1a1a1a")
del    <- function(t) list(text = t, bold = FALSE,  strike = TRUE,  color = "#CC0000")
sp     <- function()  list(text = " ", bold = FALSE, strike = FALSE)

# ============================================================
# LEGEND PAGE
# ============================================================
doc <- add_heading(doc, "MANUSCRIPT REVISION — TRACKED CHANGES GUIDE", 1)
doc <- body_add_par(doc, "How to read this document:", style = "Normal")
doc <- add_mixed_par(doc, list(add("  BOLD + YELLOW HIGHLIGHT = New text to ADD")))
doc <- add_mixed_par(doc, list(del("  RED STRIKETHROUGH = Text to DELETE or REPLACE")))
doc <- add_mixed_par(doc, list(keep("  Plain text = Keep as-is")))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# AFFILIATION FIX
# ============================================================
doc <- add_heading(doc, "AFFILIATION FIX (Author #4)", 2)
doc <- add_mixed_par(doc, list(
  keep("Washington University in St. Louis, "),
  del("MI"),
  add("MO"),
  keep(", United States")
))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# ABSTRACT
# ============================================================
doc <- add_heading(doc, "ABSTRACT", 1)

doc <- add_heading(doc, "Background", 2)
doc <- add_mixed_par(doc, list(
  keep("Infectious diseases remain a leading cause of morbidity and mortality among children aged 0–14 years in the United States. "),
  add("However, the long-term temporal dynamics and policy-relevant inflection points in this burden have not been comprehensively characterized. "),
  keep("This study aimed to analyze trends in the incidence, mortality, and disability-adjusted life years (DALYs) of infectious diseases in this population using Global Burden of Disease (GBD) 2023 data.")
))

doc <- add_heading(doc, "Methods", 2)
doc <- add_mixed_par(doc, list(
  keep("We analyzed GBD 2023 data for 10 infectious disease categories from 1990 to 2023. Age-standardized rates and estimated annual percentage changes (EAPCs) were calculated for incidence, mortality, and DALYs. "),
  add("Joinpoint regression was additionally applied to identify statistically significant temporal inflection points for selected disease categories.")
))

doc <- add_heading(doc, "Conclusions (Abstract)", 2)
doc <- add_mixed_par(doc, list(
  del("The findings highlight the need for continued surveillance and targeted interventions."),
  add("These findings underscore the need for continued surveillance, vaccine program reinforcement, and targeted interventions—particularly to address the post-2015 resurgence in enteric infection incidence and the persistently high burden of neglected tropical diseases.")
))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# KEYWORDS
# ============================================================
doc <- add_heading(doc, "KEYWORDS", 1)
doc <- add_mixed_par(doc, list(
  keep("infectious diseases; children; Global Burden of Disease; age-standardized rates; EAPC"),
  add("; joinpoint regression; pediatric epidemiology")
))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# INTRODUCTION
# ============================================================
doc <- add_heading(doc, "INTRODUCTION", 1)
doc <- body_add_par(doc, "[Keep Introduction as-is — no changes required]", style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# METHODS
# ============================================================
doc <- add_heading(doc, "METHODS", 1)

doc <- add_heading(doc, "Data Source (keep as-is)", 2)
doc <- body_add_par(doc, "[No changes to Data Source paragraph]", style = "Normal")

doc <- add_heading(doc, "Statistical Analysis — EAPC paragraph (existing, with edits)", 2)
doc <- add_mixed_par(doc, list(
  keep("The estimated annual percentage change (EAPC) was calculated to quantify trends in age-standardized rates. A linear regression model was fitted to the natural logarithm of the age-standardized rate against calendar year: ln(rate) = α + β × year + ε. The EAPC was then derived as: EAPC = (e"),
  keep("β − 1) × 100. An increasing trend was defined as EAPC > 0 with its 95% confidence interval (CI) entirely above zero, whereas a decreasing trend required the 95% CI to be entirely below zero; otherwise, the trend was considered stable. This study "),
  del("does not also"),
  add("does not"),
  keep(" involve the collection of personal data, and "),
  del("significantly"),
  keep(" reduced the risk of bias.")
))

doc <- add_heading(doc, "*** NEW PARAGRAPH — ADD after EAPC paragraph ***", 2)
doc <- add_mixed_par(doc, list(
  add("To identify statistically significant temporal inflection points in long-term trends, joinpoint regression was applied to four disease categories with notable trend heterogeneity: enteric infections, lower respiratory infections (LRI), upper respiratory infections (URI), and neglected tropical diseases and malaria (NTD). For each disease–outcome combination (incidence, mortality, DALYs), a log-linear joinpoint model was fitted allowing up to three breakpoints, with the optimal number of joinpoints selected by the Bayesian Information Criterion (BIC). Analyses were conducted using the segmented R package (v2.1-3). Annual percentage changes (APCs) were estimated for each segment. For contextual interpretation, key policy milestones were annotated: introduction of the Vaccines for Children (VFC) program (1994), pneumococcal conjugate vaccine PCV7 licensure (2000), rotavirus vaccine introduction (2006), and the COVID-19 pandemic onset (2020). Results are presented in Supplementary Table 7 and Supplementary Figures 1–3.")
))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# RESULTS
# ============================================================
doc <- add_heading(doc, "RESULTS", 1)

doc <- add_heading(doc, "Overall burden sentence", 2)
doc <- add_mixed_par(doc, list(
  keep("From 1990 to 2023, the burden of pediatric infectious diseases in the United States "),
  del("illustrated a declining trend"),
  add("showed a declining trend across most categories"),
  keep(", with notable heterogeneity across disease groups.")
))

doc <- add_heading(doc, "Enteric infections — EAPC sentence (keep) + ADD joinpoint finding after it", 2)
doc <- add_mixed_par(doc, list(
  keep("[Keep existing EAPC sentence for enteric infections as written.] "),
  add("Joinpoint regression identified three significant inflection points in enteric infection incidence (1995, 2002, 2015), revealing a complex non-linear trajectory. After a prolonged decline, a trend reversal was detected beginning in 2015 (APC: +20.71% per year), which persisted through 2023. This post-2015 resurgence warrants close monitoring (Supplementary Table 7, Supplementary Figure 1).")
))

doc <- add_heading(doc, "NTD sentence — ADD qualifier", 2)
doc <- add_mixed_par(doc, list(
  keep("[Keep existing NTD EAPC sentence.] "),
  add("Notably, year-to-year incidence estimates for NTD and malaria showed substantial variability, and joinpoint regression did not identify statistically significant breakpoints for incidence (Supplementary Table 7), suggesting that observed fluctuations may reflect data estimation uncertainty rather than true epidemiological transitions.")
))

doc <- add_heading(doc, "LRI mortality sentence — ADD joinpoint finding after it", 2)
doc <- add_mixed_par(doc, list(
  keep("[Keep existing LRI mortality EAPC sentence.] "),
  add("Joinpoint regression of LRI mortality identified a particularly sharp decline during 1998–1999 (APC: −15.94%), temporally coinciding with the licensure of PCV7 in 2000, and a sustained gradual decline thereafter (APC: −1.94% per year through 2023) (Supplementary Table 7, Supplementary Figure 2).")
))

doc <- add_heading(doc, "\"For concern\" phrase — REMOVE", 2)
doc <- add_mixed_par(doc, list(
  del("These trends are of concern and"),
  add("These trends"),
  keep(" highlight the need for continued surveillance.")
))

doc <- add_heading(doc, "\"Strongly declined\" — SOFTEN", 2)
doc <- add_mixed_par(doc, list(
  keep("URI mortality "),
  del("strongly"),
  keep(" declined over the study period.")
))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# DISCUSSION
# ============================================================
doc <- add_heading(doc, "DISCUSSION", 1)

doc <- add_heading(doc, "Opening paragraph — REWRITE first sentence", 2)
doc <- add_mixed_par(doc, list(
  del("This study found that infectious disease burden in US children declined overall from 1990 to 2023."),
  add("This study demonstrates that while the overall burden of pediatric infectious diseases in the United States declined substantially from 1990 to 2023, this trajectory was neither uniform nor uninterrupted—joinpoint regression revealed multiple inflection points, including a significant post-2015 resurgence in enteric infection incidence that has not been previously characterized.")
))

doc <- add_heading(doc, "*** NEW PARAGRAPH — ADD: Post-2015 enteric uptick (currently MISSING entirely) ***", 2)
doc <- add_mixed_par(doc, list(
  add("A notable finding of this study was the reversal of the long-term decline in enteric infection incidence beginning in 2015, with an APC of +20.71% per year through 2023. This trend predates the COVID-19 pandemic and therefore cannot be attributed solely to pandemic-related disruptions. Potential explanations include increasing antimicrobial resistance in enteric pathogens, shifts in food production and distribution systems, and changes in healthcare-seeking behavior affecting case ascertainment. This finding warrants corroboration with CDC FoodNet surveillance data and merits further investigation into its drivers and clinical implications.")
))

doc <- add_heading(doc, "NTD paragraph — REWRITE with joinpoint-informed caution", 2)
doc <- add_mixed_par(doc, list(
  del("[Remove overconfident interpretation of NTD trends]"),
  add("The burden of neglected tropical diseases and malaria showed a modest overall increasing trend (EAPC: +3.25%), though interpretation requires caution. Joinpoint regression did not identify statistically significant breakpoints in NTD incidence, and year-to-year estimates exhibited substantial variability (e.g., >70% swings between adjacent years). This pattern likely reflects the inherent difficulty in estimating NTD burden in a low-endemic setting such as the United States, where GBD modeled estimates may be sensitive to sparse data. Despite this uncertainty, the persistently elevated and potentially increasing NTD burden underscores the need for enhanced surveillance and equity-focused interventions in at-risk communities.")
))

doc <- add_heading(doc, "LRI paragraph — ADD PCV7 joinpoint interpretation", 2)
doc <- add_mixed_par(doc, list(
  keep("[Keep existing LRI discussion paragraph.] "),
  add("The joinpoint-identified sharp decline in LRI mortality during 1998–1999 (APC: −15.94%) is temporally consistent with the introduction of PCV7 in 2000, providing indirect epidemiological evidence for vaccine impact. This finding is consistent with published literature documenting rapid reductions in pediatric pneumococcal disease following PCV7 introduction (Grijalva et al., 2006; Whitney et al., 2003).")
))

doc <- add_heading(doc, "Conclusion paragraph — REWRITE with specific actions", 2)
doc <- add_mixed_par(doc, list(
  del("In conclusion, this study highlights the importance of continued surveillance and public health interventions to reduce the burden of infectious diseases in children."),
  add("In conclusion, this study provides a comprehensive, 34-year characterization of pediatric infectious disease burden in the United States, identifying both long-term improvements and emerging threats. Key priorities include: (1) investigation and reversal of the post-2015 enteric infection resurgence; (2) reinforcement of pneumococcal and rotavirus immunization programs; (3) enhanced NTD surveillance in at-risk populations; and (4) preparedness planning for pandemic-related disruptions to infection control. These findings provide an evidence base for targeted pediatric infectious disease policy in the United States.")
))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# SUPPLEMENTARY MATERIALS SECTION
# ============================================================
doc <- add_heading(doc, "SUPPLEMENTARY MATERIALS SECTION — REWRITE", 1)
doc <- add_mixed_par(doc, list(
  del("The supplementary materials include six tables and two supplementary figures."),
  add("The supplementary materials include seven tables and three supplementary figures. Supplementary Tables 1–3 present full age-standardized incidence, mortality, and DALY rates with EAPC estimates by disease and sex. Supplementary Tables 4–6 present age-specific rates (<5, 5–9, and 10–14 years) by sex for incidence, mortality, and DALYs, respectively. Supplementary Table 7 presents joinpoint regression results including the number of joinpoints, segment-specific annual percentage changes, and trend directions for enteric infections, lower respiratory infections, upper respiratory infections, and neglected tropical diseases and malaria. Supplementary Figures 1–3 present joinpoint regression trend plots for incidence, mortality, and DALYs, respectively, with annotated policy milestones.")
))
doc <- body_add_par(doc, "", style = "Normal")

# ============================================================
# SAVE
# ============================================================
out_path <- "/Users/nam.tnguyen2022/Documents/KIDS_DISEASES /Results/Manuscript_Revision_Markup.docx"
print(doc, target = out_path)
cat("Saved:", out_path, "\n")
