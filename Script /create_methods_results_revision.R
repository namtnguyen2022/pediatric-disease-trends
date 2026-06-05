library(officer)

doc <- read_docx()

# ── Styling helpers ──────────────────────────────────────────
normal_fp  <- fp_text(font.size = 11, font.family = "Times New Roman", color = "black")
add_fp     <- fp_text(font.size = 11, font.family = "Times New Roman",
                      bold = TRUE, color = "#000000", shading.color = "#FFFF99")
del_fp     <- fp_text(font.size = 11, font.family = "Times New Roman",
                      strike = TRUE, color = "#CC0000")
head1_fp   <- fp_text(font.size = 13, font.family = "Times New Roman", bold = TRUE)
head2_fp   <- fp_text(font.size = 11, font.family = "Times New Roman",
                      bold = TRUE, color = "#1F4E79")
note_fp    <- fp_text(font.size = 10, font.family = "Times New Roman",
                      italic = TRUE, color = "#555555")

k <- function(t) ftext(t, normal_fp)
a <- function(t) ftext(t, add_fp)
d <- function(t) ftext(t, del_fp)
h1 <- function(t) ftext(t, head1_fp)
h2 <- function(t) ftext(t, head2_fp)
n  <- function(t) ftext(t, note_fp)

add_line <- function(doc, ...) {
  body_add_fpar(doc, fpar(...), style = "Normal")
}

# ═══════════════════════════════════════════════════════════════
# LEGEND
# ═══════════════════════════════════════════════════════════════
doc <- add_line(doc, h1("METHODS & RESULTS — REVISION GUIDE"))
doc <- add_line(doc, n("Legend:  "),
                     ftext("Bold + yellow  ", add_fp), n("= ADD this text     "),
                     ftext("Red strikethrough  ", del_fp), n("= DELETE this text     "),
                     ftext("Plain text  ", normal_fp), n("= keep as-is"))
doc <- body_add_par(doc, "", style = "Normal")

# ═══════════════════════════════════════════════════════════════
# OVERALL VERDICT
# ═══════════════════════════════════════════════════════════════
doc <- add_line(doc, h1("OVERALL ASSESSMENT"))
doc <- add_line(doc,
  n("Your Methods and Results are in great shape. The joinpoint paragraph, enteric inflection point, "),
  n("LRI/PCV7 finding, and NTD variability note are all already incorporated correctly. "),
  n("Only 4 small fixes remain — see below."))
doc <- body_add_par(doc, "", style = "Normal")

# ═══════════════════════════════════════════════════════════════
# METHODS — CHANGE 1
# ═══════════════════════════════════════════════════════════════
doc <- add_line(doc, h1("METHODS"))
doc <- body_add_par(doc, "", style = "Normal")

doc <- add_line(doc, h2("FIX 1 of 2 — Remove 'also' (IRB sentence)"))
doc <- add_line(doc, n("Current sentence:"))
doc <- add_line(doc,
  k("Since the database is publicly available online, the data downloaded does not contain any personal information and does "),
  d("not also"),
  k(" require ethical IRB approval."))
doc <- body_add_par(doc, "", style = "Normal")
doc <- add_line(doc, n("Change to:"))
doc <- add_line(doc,
  k("Since the database is publicly available online, the data downloaded does not contain any personal information and does "),
  a("not"),
  k(" require ethical IRB approval."))
doc <- body_add_par(doc, "", style = "Normal")

doc <- add_line(doc, h2("FIX 2 of 2 — Joinpoint paragraph is already correct"))
doc <- add_line(doc, n("Your joinpoint paragraph (segmented package, BIC, up to 3 breakpoints, policy milestones, Supplementary Table 7 / Figures 1–3) is complete and well-written. No changes needed."))
doc <- body_add_par(doc, "", style = "Normal")

# ═══════════════════════════════════════════════════════════════
# RESULTS — CHANGES
# ═══════════════════════════════════════════════════════════════
doc <- add_line(doc, h1("RESULTS"))
doc <- body_add_par(doc, "", style = "Normal")

doc <- add_line(doc, h2("FIX 1 of 2 — Remove 'significantly' (Age-Specific Burden section)"))
doc <- add_line(doc, n("Locate this sentence in the Age-Specific Burden subsection:"))
doc <- add_line(doc,
  k("However, the ASMR for lower respiratory infections in children under 5 years old was approximately 2.81/100,000, "),
  d("significantly"),
  k(" higher than other infectious diseases (Supplementary Table 5)."))
doc <- body_add_par(doc, "", style = "Normal")
doc <- add_line(doc, n("Change to:"))
doc <- add_line(doc,
  k("However, the ASMR for lower respiratory infections in children under 5 years old was approximately 2.81/100,000, "),
  a("notably"),
  k(" higher than other infectious diseases (Supplementary Table 5)."))
doc <- body_add_par(doc, "", style = "Normal")

doc <- add_line(doc, h2("FIX 2 of 2 — Enteric joinpoint: small wording improvement"))
doc <- add_line(doc, n("Current (end of your enteric joinpoint sentence):"))
doc <- add_line(doc,
  k("...temporally coinciding with "),
  d("shifts in rotavirus vaccine uptake patterns"),
  k(" (Supplementary Table 7, Supplementary Figure 1)."))
doc <- body_add_par(doc, "", style = "Normal")
doc <- add_line(doc, n("Change to (more precise — rotavirus vaccine was introduced in 2006, not shifts in uptake):"))
doc <- add_line(doc,
  k("...temporally coinciding with "),
  a("changes in preventive infrastructure including rotavirus vaccine introduction in 2006"),
  k(" (Supplementary Table 7, Supplementary Figure 1)."))
doc <- body_add_par(doc, "", style = "Normal")

doc <- add_line(doc, n("NOTE: All other joinpoint findings (enteric trend reversal +20.71%, LRI/PCV7 −15.94%, NTD variability) are already correctly written in your current draft."))
doc <- body_add_par(doc, "", style = "Normal")

# ═══════════════════════════════════════════════════════════════
# SUPPLEMENTARY MATERIALS SECTION — NEEDS UPDATE
# ═══════════════════════════════════════════════════════════════
doc <- add_line(doc, h1("SUPPLEMENTARY MATERIALS SECTION — needs update"))
doc <- body_add_par(doc, "", style = "Normal")
doc <- add_line(doc, n("Current text (bottom of manuscript):"))
doc <- add_line(doc,
  k("The Supplemental.docx contains "),
  d("six comprehensive tables"),
  k(" supporting the manuscript's statistical findings. (1) age-standardized incidence rates (ASIRs) and estimated annual percentage changes (EAPCs); (2) age-standardized mortality rates (ASMRs); (3) age-standardized DALY rates (ASDRs); (4) age-specific incidence by sex; (5) age-specific mortality by sex; and (6) age-specific DALYs by sex across 1990–2023."))
doc <- body_add_par(doc, "", style = "Normal")
doc <- add_line(doc, n("Change to:"))
doc <- add_line(doc,
  k("The supplementary materials contain "),
  a("seven tables and three supplementary figures"),
  k(" supporting the manuscript's statistical findings. (1) age-standardized incidence rates (ASIRs) and estimated annual percentage changes (EAPCs); (2) age-standardized mortality rates (ASMRs); (3) age-standardized DALY rates (ASDRs); (4) age-specific incidence by sex; (5) age-specific mortality by sex; (6) age-specific DALYs by sex across 1990–2023"),
  a("; and (7) joinpoint regression results including segment-specific annual percentage changes for enteric infections, lower respiratory infections, upper respiratory infections, and neglected tropical diseases and malaria. Supplementary Figures 1–3 present joinpoint regression trend plots for incidence, mortality, and DALYs, respectively, with annotated policy milestones."))
doc <- body_add_par(doc, "", style = "Normal")

# ═══════════════════════════════════════════════════════════════
# SUMMARY CHECKLIST
# ═══════════════════════════════════════════════════════════════
doc <- add_line(doc, h1("SUMMARY CHECKLIST"))
doc <- add_line(doc, k("☐  Methods: Remove 'also' from IRB sentence"))
doc <- add_line(doc, k("☐  Results (Age-Specific): 'significantly' → 'notably'"))
doc <- add_line(doc, k("☐  Results (Enteric joinpoint): tighten 'shifts in rotavirus vaccine uptake patterns' wording"))
doc <- add_line(doc, k("☐  Supplementary Materials: update from 6 tables → 7 tables + 3 figures"))
doc <- add_line(doc, k("☑  Methods joinpoint paragraph — already complete"))
doc <- add_line(doc, k("☑  Results enteric +20.71% trend reversal — already in"))
doc <- add_line(doc, k("☑  Results LRI/PCV7 −15.94% APC — already in"))
doc <- add_line(doc, k("☑  Results NTD variability qualifier — already in"))

# ═══════════════════════════════════════════════════════════════
# SAVE
# ═══════════════════════════════════════════════════════════════
out <- "/Users/nam.tnguyen2022/Documents/KIDS_DISEASES /Results/Methods_Results_Revision.docx"
print(doc, target = out)
cat("Saved:", out, "\n")
