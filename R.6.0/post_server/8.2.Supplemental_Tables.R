# =============================================================================
# Supplemental_Tables.R
# Generate Supplementary_Tables.xlsx from saved model outputs
#
# Run from anywhere — auto-detects paper/ folder
# =============================================================================

library(openxlsx)
library(dplyr)

# ── Find the paper/ directory ------------------------------------------------
script_dir <- getwd()
if (file.exists(file.path(script_dir, "paper", "tabs", "Forecast_Results.RData"))) {
  paper_dir <- file.path(script_dir, "paper")
} else if (file.exists(file.path(script_dir, "tabs", "Forecast_Results.RData"))) {
  paper_dir <- script_dir
} else {
  stop("Cannot find tabs/ directory. Run from the paper/ folder or its parent.")
}

tabs_dir <- file.path(paper_dir, "tabs")
out_path <- file.path(paper_dir, "Supplementary_Tables.xlsx")

message("Paper dir: ", paper_dir)
message("Tabs dir:  ", tabs_dir)
message("Output:    ", out_path)
message("")

# ── Label maps ───────────────────────────────────────────────────────────────
outcome_labels <- c("binge_current" = "Binge Eating", "vomit_current" = "Vomiting",
                    "fowg_current" = "Fear of Weight Gain", "other_current" = "Other Weight Control")
predictor_labels <- c("bmiz_below_lower" = "Below Forecast CI",
                      "bmiz_above_upper" = "Above Forecast CI",
                      "ow" = "Overweight (BMIz > 1.04)",
                      "uw" = "Underweight (BMIz < -1.65)")
forecast_labels <- c("mean" = "Mean", "max" = "Maximum",
                     "most_recent" = "Most Recent",
                     "mean+most_recent" = "Mean + Most Recent",
                     "pop_median" = "Population Median")
sex_labels <- c("1" = "Male", "2" = "Female",
                "Female" = "Female", "Male" = "Male")
reporter_labels <- c("parent" = "Parent", "youth" = "Youth",
                     "Parent" = "Parent", "Youth" = "Youth")
ci_labels <- c("bmiz +/- 0.5" = "+/- 0.5 SD",
               "bmiz +/- 1.0" = "+/- 1.0 SD",
               "bmiz +/- 1" = "+/- 1.0 SD",
               "95" = "95% CI", "99" = "99% CI")
term_labels <- c("(Intercept)" = "Intercept",
                 "sex2" = "Sex (Female)",
                 "age_c" = "Age (centered)",
                 "bmiz_diff_raw" = "BMIz Deviation",
                 "bmiz" = "BMIz",
                 "pds_mean_c" = "PDS (centered)",
                 "bmiz_diff_raw:bmiz" = "BMIz Deviation x BMIz",
                 "bmiz_diff_raw:pds_mean_c" = "BMIz Deviation x PDS")

relabel <- function(x, map) {
  out <- map[as.character(x)]
  ifelse(is.na(out), as.character(x), out)
}

# ── Rounding helpers ─────────────────────────────────────────────────────────
round_or <- function(x) round(x, 2)   # ORs, CIs, betas, slopes
round_p  <- function(x) round(x, 3)   # p-values, FDR, pd

# ── Workbook setup ───────────────────────────────────────────────────────────
wb <- createWorkbook()

header_style <- createStyle(
  textDecoration = "bold", halign = "center",
  border = "Bottom", borderStyle = "thin", fgFill = "#D9E2F3"
)
note_style <- createStyle(fontName = "Arial", fontSize = 10, textDecoration = "italic",
                          wrapText = TRUE)

add_sheet <- function(wb, name, data, note = NULL) {
  addWorksheet(wb, name)
  start_row <- 1
  if (!is.null(note)) {
    writeData(wb, name, x = note, startRow = 1, startCol = 1)
    addStyle(wb, name, note_style, rows = 1, cols = 1)
    mergeCells(wb, name, cols = 1:min(ncol(data), 8), rows = 1)
    # Excel does not auto-fit merged cells: size row 1 so the full caption shows
    setRowHeights(wb, name, rows = 1, heights = 15 * ceiling(nchar(note) / 75))
    start_row <- 3
  }
  writeData(wb, name, data, startRow = start_row, headerStyle = header_style)
  addStyle(wb, name, header_style,
           rows = start_row, cols = 1:ncol(data), gridExpand = TRUE)
  # Constrain the wide first column (long row labels) and auto-size the rest
  # (Reviewer 1, Comment 12).
  # Size column 1 to its contents (longest label + 3), capped at 38 with wrapping
  # (R&R 2026-09-26: a fixed width of 38 was still far wider than the short labels).
  w1 <- min(38, max(nchar(c(names(data)[1], as.character(data[[1]]))), na.rm = TRUE) + 3)
  setColWidths(wb, name, cols = 1, widths = w1)
  if (ncol(data) > 1) setColWidths(wb, name, cols = 2:ncol(data), widths = "auto")
  addStyle(wb, name, createStyle(wrapText = TRUE, valign = "top"),
           rows = start_row:(start_row + nrow(data)), cols = 1, gridExpand = TRUE, stack = TRUE)
  freezePane(wb, name, firstActiveRow = start_row + 1)
  message("  OK: ", name, " -> ", nrow(data), " rows x ", ncol(data), " cols")
}

# =============================================================================
# S1: GLMM Coefficients (Aim 3) — Frequentist with Bayesian where available
# (formerly S6 — moved to S1 to match first citation order in manuscript)
# =============================================================================
message("S1: GLMM Coefficients")
f_freq  <- file.path(tabs_dir, "aim3_glmm_coefficients.csv")
f_bayes <- file.path(tabs_dir, "aim3_bayesian_sensitivity.RData")

if (file.exists(f_freq)) {
  s1 <- read.csv(f_freq)
  
  # Flag separation artifacts: SE < 0.01 on non-intercept terms, or |z| > 100
  s1$separation_flag <- ""
  is_sep <- (abs(s1$std.error) < 0.01 & s1$term != "(Intercept)") |
    (abs(s1$statistic) > 100 & s1$term != "(Intercept)")
  s1$separation_flag[is_sep] <- "Possible separation artifact"
  
  # If Bayesian results exist, merge them in
  if (file.exists(f_bayes)) {
    message("  Merging Bayesian sensitivity estimates")
    e_bayes <- new.env()
    load(f_bayes, envir = e_bayes)
    if (exists("bayes_all", envir = e_bayes)) {
      ba <- e_bayes$bayes_all
      if (all(abs(ba$estimate) < 20, na.rm = TRUE)) {
        ba_merge <- ba %>%
          select(reporter, outcome, term, estimate, conf.low, conf.high, pd) %>%
          rename(Bayes_OR = estimate, Bayes_CI_Low = conf.low,
                 Bayes_CI_High = conf.high, Bayes_pd = pd) %>%
          mutate(across(c(Bayes_OR, Bayes_CI_Low, Bayes_CI_High), ~ round_or(exp(.x))),
                 Bayes_pd = round_p(Bayes_pd))
      } else {
        ba_merge <- ba %>%
          select(reporter, outcome, term, estimate, conf.low, conf.high, pd) %>%
          rename(Bayes_OR = estimate, Bayes_CI_Low = conf.low,
                 Bayes_CI_High = conf.high, Bayes_pd = pd) %>%
          mutate(across(c(Bayes_OR, Bayes_CI_Low, Bayes_CI_High), ~ round_or(.x)),
                 Bayes_pd = round_p(Bayes_pd))
      }
      s1$reporter_lc   <- tolower(s1$reporter)
      ba_merge$reporter_lc <- tolower(ba_merge$reporter)
      s1 <- merge(s1, ba_merge, by = c("reporter_lc", "outcome", "term"),
                  all.x = TRUE, suffixes = c("", ".bayes"))
      s1$reporter_lc <- NULL
    }
  }
  
  # Apply human-readable labels
  s1$outcome  <- relabel(s1$outcome,  outcome_labels)
  s1$reporter <- relabel(s1$reporter, reporter_labels)
  s1$term     <- relabel(s1$term,     term_labels)
  
  # Round frequentist columns
  if ("effect" %in% names(s1)) s1 <- s1 %>% filter(effect == "fixed")
  if ("OR"      %in% names(s1)) s1$OR     <- round_or(s1$OR)
  if ("OR_lo"   %in% names(s1)) s1$OR_lo  <- round_or(s1$OR_lo)
  if ("OR_hi"   %in% names(s1)) s1$OR_hi  <- round_or(s1$OR_hi)
  if ("p.value" %in% names(s1)) s1$p.value <- round_p(s1$p.value)
  
  # Select and rename columns
  cols_out  <- c("reporter", "outcome", "term", "OR", "OR_lo", "OR_hi", "p.value")
  col_names <- c("Reporter", "Outcome", "Term", "Freq OR", "Freq 95% CI Low",
                 "Freq 95% CI High", "Freq p")
  if ("Bayes_OR" %in% names(s1)) {
    cols_out  <- c(cols_out,  "Bayes_OR", "Bayes_CI_Low", "Bayes_CI_High", "Bayes_pd")
    col_names <- c(col_names, "Bayes OR", "Bayes 95% CrI Low", "Bayes 95% CrI High",
                   "Prob of Direction")
  }
  cols_out  <- c(cols_out,  "separation_flag")
  col_names <- c(col_names, "Note")
  s1_out <- s1[, intersect(cols_out, names(s1))]
  names(s1_out) <- col_names[seq_along(names(s1_out))]
  
  add_sheet(wb, "S1-GLMM Coefficients", s1_out,
            note = "Table S1. Aim 3 GLMM results and multiple imputation (MI) sensitivity analyses (mice, 20 imputations, predictive mean matching). Frequentist estimates shown for all models; Bayesian estimates (weakly informative priors) shown where available. 'Possible separation artifact' flags estimates where quasi-complete separation produced pathologically small SEs in the frequentist model.")
} else { message("  FILE NOT FOUND: ", f_freq) }

# =============================================================================
# S2: Forecast Accuracy
# (formerly S1)
# =============================================================================
message("S2: Forecast Accuracy")
f <- file.path(tabs_dir, "Forecast_Results.RData")
if (file.exists(f)) {
  e2 <- new.env()
  load(f, envir = e2)
  s2 <- e2$fcast_results
  if ("forecast_point" %in% names(s2))
    s2$forecast_point <- relabel(s2$forecast_point, forecast_labels)
  if ("ci" %in% names(s2))
    s2$ci <- relabel(s2$ci, ci_labels)
  names(s2) <- gsub("^forecast_point$",    "Forecast Method",     names(s2))
  names(s2) <- gsub("^ci$",                "Threshold",           names(s2))
  names(s2) <- gsub("^timepoint$",         "Timepoint",           names(s2))
  names(s2) <- gsub("^bmiz_below_lower$",  "N Below Forecast CI", names(s2))
  names(s2) <- gsub("^bmiz_above_upper$",  "N Above Forecast CI", names(s2))
  names(s2) <- gsub("^bmiz_within_ci$",    "N Within Forecast CI",names(s2))
  names(s2) <- gsub("^mean_eBMIz_diff$",   "Mean BMIz Error",     names(s2))
  names(s2) <- gsub("^median_eBMIz_diff$", "Median BMIz Error",   names(s2))
  names(s2) <- gsub("^mean_eBMI_diff$",    "Mean BMI Error",      names(s2))
  names(s2) <- gsub("^median_eBMI_diff$",  "Median BMI Error",    names(s2))
  s2[sapply(s2, is.numeric)] <- lapply(s2[sapply(s2, is.numeric)], round_or)
  add_sheet(wb, "S2-Forecast Accuracy", s2,
            note = "Table S2. Forecast accuracy metrics by method, CI threshold, and target year. Each row summarizes the forecast evaluated at its target year only (the Y1-trained forecast evaluated at Year 2, Y2-trained at Year 3, ..., Y5-trained at Year 6), matching the values shown in Figure 2A and used throughout the main text and downstream analyses. BMI/BMIz error = absolute deviation of observed from forecasted value.")
} else { message("  FILE NOT FOUND: ", f) }

# =============================================================================
# S3: Classification Accuracy
# (formerly S2)
# =============================================================================
message("S3: Classification Accuracy")
f <- file.path(tabs_dir, "accuracy_metrics.RData")
if (file.exists(f)) {
  e3 <- new.env()
  load(f, envir = e3)
  message("  Loaded objects: ", paste(ls(e3), collapse = ", "))
  to_df <- function(x) {
    as.data.frame(lapply(x, function(col) if (is.factor(col)) as.character(col) else col),
                  stringsAsFactors = FALSE)
  }
  uw <- to_df(e3$uw_summary); uw$category <- "Underweight"
  ow <- to_df(e3$ow_summary); ow$category <- "Overweight"
  ob <- to_df(e3$ob_summary); ob$category <- "Obesity"
  all_cols <- unique(c(names(uw), names(ow), names(ob)))
  pad <- function(df) { for (col in setdiff(all_cols, names(df))) df[[col]] <- NA; df[, all_cols] }
  s3 <- rbind(pad(uw), pad(ow), pad(ob))
  if ("forecast_point" %in% names(s3))
    s3$forecast_point <- relabel(s3$forecast_point, forecast_labels)
  if ("ci" %in% names(s3))
    s3$ci <- relabel(s3$ci, ci_labels)
  metric_cols <- intersect(names(s3), c("sensitivity","specificity","ppv","npv","balanced_accuracy","f1"))
  s3[metric_cols] <- lapply(s3[metric_cols], round_or)
  names(s3) <- gsub("^forecast_point$",    "Forecast Method",     names(s3))
  names(s3) <- gsub("^ci$",                "Threshold",           names(s3))
  names(s3) <- gsub("^timepoint$",         "Timepoint",           names(s3))
  names(s3) <- gsub("^category$",          "Weight Category",     names(s3))
  names(s3) <- gsub("^sensitivity$",       "Sensitivity",         names(s3))
  names(s3) <- gsub("^specificity$",       "Specificity",         names(s3))
  names(s3) <- gsub("^ppv$",               "PPV",                 names(s3))
  names(s3) <- gsub("^npv$",               "NPV",                 names(s3))
  names(s3) <- gsub("^balanced_accuracy$", "Balanced Accuracy",   names(s3))
  names(s3) <- gsub("^f1$",                "F1",                  names(s3))
  names(s3) <- gsub("^n$",                 "N",                   names(s3))
  names(s3) <- gsub("^n_uw$",              "N Underweight (pop)", names(s3))
  names(s3) <- gsub("^n_ow$",              "N Overweight (pop)",  names(s3))
  names(s3) <- gsub("^n_ob$",              "N Obese (pop)",       names(s3))
  names(s3) <- gsub("^n_below_lower$",     "N Below Forecast CI", names(s3))
  names(s3) <- gsub("^n_above_upper$",     "N Above Forecast CI", names(s3))
  add_sheet(wb, "S3-Classification Accuracy", s3,
            note = "Table S3. Classification accuracy metrics for population-based and forecast-derived weight indicators by method, threshold, and timepoint.")
} else { message("  FILE NOT FOUND: ", f) }

# =============================================================================
# S4: Continuous Logistic (Aim 2)
# (formerly S3)
# =============================================================================
f <- file.path(tabs_dir, "ED_behavior_prediction_results.RData")
if (file.exists(f)) {
  e4 <- new.env()
  load(f, envir = e4)
  message("  Loaded objects: ", paste(ls(e4), collapse = ", "))
  
  message("S4: Continuous Logistic")
  if (!is.null(e4$logistic_results)) {
    s4 <- e4$logistic_results %>%
      mutate(outcome        = relabel(outcome, outcome_labels),
             forecast_point = relabel(forecast_point, forecast_labels),
             reporter       = relabel(reporter, reporter_labels),
             sex            = relabel(as.character(sex), sex_labels),
             ci             = relabel(ci, ci_labels),
             across(c(odds_ratio, conf.low, conf.high), ~ round_or(.x)),
             across(c(p.value, p.value.fdr),            ~ round_p(.x))) %>%
      select(forecast_point, ci, reporter, timepoint, sex, outcome,
             odds_ratio, conf.low, conf.high, p.value, p.value.fdr)
    names(s4) <- c("Forecast Method", "Threshold", "Reporter", "Timepoint", "Sex",
                   "Outcome", "OR", "95% CI Low", "95% CI High", "p", "p (FDR)")
    add_sheet(wb, "S4-Continuous Logistic", s4,
              note = "Table S4. Continuous logistic regression: BMIz deviation predicting ED symptoms, stratified by forecast method, threshold, timepoint, reporter, and sex.")
  }
  
  # S5: Binary Combined (formerly S4)
  # Mark cells as not estimable when frequentist logistic regression suffers
  # quasi-complete separation (predictor never co-occurs with outcome, or
  # always co-occurs). Symptoms: OR exactly 0 or Inf, NaN/Inf CI bound, or
  # implausibly extreme CI (high > 1000 or low < 0.001 with span > 100).
  # Such estimates are uninterpretable and should not be presented as
  # point estimates.
  message("S5: Binary Combined")
  if (!is.null(e4$binary_combined_results)) {
    s5 <- e4$binary_combined_results %>%
      mutate(
        .unest = is.na(odds_ratio) | odds_ratio == 0 | is.infinite(odds_ratio) |
                 is.na(conf.high)  | is.infinite(conf.high) | conf.high > 1000 |
                 is.na(conf.low)   | (conf.low < 0.001 & conf.high > 100),
        odds_ratio  = ifelse(.unest, NA_real_, odds_ratio),
        conf.low    = ifelse(.unest, NA_real_, conf.low),
        conf.high   = ifelse(.unest, NA_real_, conf.high),
        p.value     = ifelse(.unest, NA_real_, p.value),
        p.value.fdr = ifelse(.unest, NA_real_, p.value.fdr)
      ) %>%
      mutate(outcome        = relabel(outcome, outcome_labels),
             term           = relabel(term, predictor_labels),
             forecast_point = relabel(forecast_point, forecast_labels),
             reporter       = relabel(reporter, reporter_labels),
             sex            = relabel(as.character(sex), sex_labels),
             ci             = relabel(ci, ci_labels),
             across(c(odds_ratio, conf.low, conf.high), ~ round_or(.x)),
             across(c(p.value, p.value.fdr),            ~ round_p(.x))) %>%
      select(forecast_point, ci, reporter, timepoint, sex, outcome, term,
             odds_ratio, conf.low, conf.high, p.value, p.value.fdr)
    names(s5) <- c("Forecast Method", "Threshold", "Reporter", "Timepoint", "Sex",
                   "Outcome", "Predictor", "OR", "95% CI Low", "95% CI High", "p", "p (FDR)")
    add_sheet(wb, "S5-Binary Combined", s5,
              note = "Table S5. Combined binary model: weight status & forecast deviation predicting ED symptoms. Empty cells (NA) indicate quasi-complete separation \u2014 the predictor and outcome never (or always) co-occurred in that subgroup, so the frequentist logistic regression estimate is unbounded and not interpretable. See Bayesian sensitivity analyses (Aim 3 GLMMs) for shrinkage-stabilized estimates of the same effects.")
  }

  # S6: Classification Metrics (formerly S5)
  # F1 is undefined when both Sensitivity and PPV are 0 (predictor catches
  # zero true positives in this subgroup). Set those cells to NA so readers
  # don't mistake "0.00" for a meaningful estimate.
  message("S6: Classification Metrics")
  if (!is.null(e4$classification_results)) {
    s6 <- e4$classification_results %>%
      mutate(
        .f1_unest = (!is.na(sensitivity) & sensitivity == 0 &
                     !is.na(ppv)         & ppv         == 0),
        f1 = ifelse(.f1_unest, NA_real_, f1)
      ) %>%
      mutate(outcome        = relabel(outcome, outcome_labels),
             predictor      = relabel(predictor, predictor_labels),
             forecast_point = relabel(forecast_point, forecast_labels),
             reporter       = relabel(reporter, reporter_labels),
             sex            = relabel(as.character(sex), sex_labels),
             ci             = relabel(ci, ci_labels),
             across(c(accuracy, balanced_accuracy, sensitivity, specificity,
                      ppv, npv, f1, prevalence), ~ round_or(.x))) %>%
      select(forecast_point, ci, reporter, timepoint, sex, outcome, predictor,
             sensitivity, specificity, ppv, npv, balanced_accuracy, f1, prevalence)
    names(s6) <- c("Forecast Method", "Threshold", "Reporter", "Timepoint", "Sex",
                   "Outcome", "Predictor", "Sensitivity", "Specificity", "PPV", "NPV",
                   "Balanced Accuracy", "F1", "Prevalence")
    add_sheet(wb, "S6-Classification Metrics", s6,
              note = "Table S6. Classification performance: binary weight status/deviation indicators predicting ED symptoms. F1 is reported as NA when both Sensitivity and PPV are 0 (predictor caught no true positives in that subgroup), as F1 is mathematically undefined in that case. Most NA F1 cells involve the Underweight predictor paired with low-prevalence outcomes (vomiting, FOWG) in stratified subgroups with insufficient co-occurrence to estimate.")
  }
} else { message("  FILE NOT FOUND: ", f) }

# =============================================================================
# S7: Marginal Slopes
# (formerly S7 — unchanged)
# =============================================================================
message("S7: Marginal Slopes")
f <- file.path(tabs_dir, "aim3_marginal_slopes.csv")
if (file.exists(f)) {
  s7 <- read.csv(f)
  s7$outcome  <- relabel(s7$outcome,  outcome_labels)
  s7$reporter <- relabel(s7$reporter, reporter_labels)
  s7 <- s7 %>%
    mutate(across(c(beta_main, beta_interaction, marginal_slope), ~ round_or(.x)))
  names(s7) <- c("Reporter", "Outcome", "Moderator", "Level", "Level Value",
                 "Beta (Main)", "Beta (Interaction)", "Marginal Slope", "Direction")
  add_sheet(wb, "S7-Marginal Slopes", s7,
            note = "Table S7. Marginal slopes of BMIz deviation at different levels of each moderator (BMIz, PDS). Slopes are on the log-odds scale; negative slopes indicate below-forecast = higher risk.")
} else { message("  FILE NOT FOUND: ", f) }

# =============================================================================
# S8: Stratified Models
# (formerly S8 — unchanged)
# =============================================================================
message("S8: Stratified Models")
f <- file.path(tabs_dir, "model_results_stratified.RData")
if (file.exists(f)) {
  e8 <- new.env()
  load(f, envir = e8)
  s8 <- e8$final_results
  
  if ("effect" %in% names(s8)) s8 <- s8 %>% filter(effect == "fixed")
  
  s8$separation_flag <- ""
  is_sep <- (!is.na(s8$std.error) & s8$std.error < 0.01 & s8$term != "(Intercept)") |
    (!is.na(s8$statistic) & abs(s8$statistic) > 100 & s8$term != "(Intercept)")
  s8$separation_flag[is_sep] <- "Possible separation artifact"
  
  s8$outcome  <- relabel(s8$outcome,  outcome_labels)
  s8$reporter <- relabel(s8$reporter, reporter_labels)
  s8$term     <- relabel(s8$term,     term_labels)
  
  s8 <- s8 %>%
    mutate(across(c(estimate, conf.low, conf.high),    ~ round_or(.x)),
           across(any_of(c("p.value", "p.value.fdr")), ~ round_p(.x)))
  
  cols <- c("reporter", "outcome", "term", "estimate", "conf.low", "conf.high",
            "p.value", "separation_flag", "p.value.fdr")
  cols <- intersect(cols, names(s8))
  s8_out <- s8[, cols]
  col_names <- c("Reporter", "Outcome", "Term", "OR", "95% CI Low", "95% CI High",
                 "p", "Note", "p (FDR)")
  names(s8_out) <- col_names[seq_along(cols)]
  
  add_sheet(wb, "S8-Stratified Models", s8_out,
            note = "Table S8. Stratified GLMM results by reporter and outcome. Estimates are odds ratios. 'Possible separation artifact' flags estimates where quasi-complete separation produced pathologically small SEs.")
} else { message("  FILE NOT FOUND: ", f) }

# =============================================================================
# S9: Pre-registered Aim 3 correlations (R&R 2026-09-26)
# Pearson r (point-biserial for binary ED outcomes), primary forecast method
# (mean + most recent, ±0.5 SD), by timepoint and reporter. From 5.1.
# =============================================================================
message("S9: Pre-registered correlations")
f <- file.path(tabs_dir, "model_results_stratified.RData")
if (file.exists(f)) {
  e9 <- new.env()
  load(f, envir = e9)
  s9 <- e9$aim3_correlations

  pair_labels <- c(
    r_bmiz_diff_pds   = "BMIz deviation \u2013 Pubertal development (PDS)",
    r_bmiz_diff_bmiz  = "BMIz deviation \u2013 BMIz",
    r_pds_vomit       = "PDS \u2013 Vomiting",
    r_pds_binge       = "PDS \u2013 Binge eating",
    r_pds_other       = "PDS \u2013 Other weight control behaviors",
    r_pds_fowg        = "PDS \u2013 Fear of weight gain",
    r_bmiz_diff_vomit = "BMIz deviation \u2013 Vomiting",
    r_bmiz_diff_binge = "BMIz deviation \u2013 Binge eating",
    r_bmiz_diff_other = "BMIz deviation \u2013 Other weight control behaviors",
    r_bmiz_diff_fowg  = "BMIz deviation \u2013 Fear of weight gain"
  )

  s9_out <- s9 %>%
    tidyr::pivot_longer(cols = starts_with("r_"), names_to = "pair", values_to = "r") %>%
    mutate(pair = factor(pair, levels = names(pair_labels), labels = pair_labels),
           reporter = relabel(reporter, reporter_labels),
           r = round(r, 3)) %>%
    arrange(pair, reporter, timepoint) %>%
    select(pair, reporter, timepoint, r, n)
  names(s9_out) <- c("Correlation", "Reporter", "Timepoint", "r", "N")

  add_sheet(wb, "S9-Correlations", s9_out,
            note = "Table S9. Pre-registered Aim 3 correlations between continuous BMIz deviation, pubertal development (PDS, mean-centered within sex), BMIz, and each ED symptom, by timepoint and reporter (primary forecast method: mean with most recent, \u00b10.5 SD). Pearson r; correlations with binary ED symptoms are point-biserial.")
} else { message("  FILE NOT FOUND: ", f) }

# =============================================================================
# Save
# =============================================================================
saveWorkbook(wb, out_path, overwrite = TRUE)
message("\nSaved: ", out_path)
message("Sheets: ", paste(names(wb), collapse = ", "))