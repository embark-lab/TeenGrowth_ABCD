# =============================================================================
# 2.1c CI Width Comparison & Threshold Analysis
#
# Purpose:
#   1. Compute median (IQR) CI width in BMIz units for the 95% and 99%
#      prediction intervals at each timepoint, for each forecast method
#   2. Compare ED prediction performance of 95%/99% CI vs ±0.5 SD at
#      early (Y2) vs late (Y4, Y6) timepoints
#   3. Fix the ±1.0 SD Aim 1 accuracy metrics bug (currently duplicates ±0.5)
#
# Requires: full forecast data with upper/lower bounds per individual
# =============================================================================

library(dplyr)
library(tidyr)

# =============================================================================
# 0. LOAD DATA
# =============================================================================

# Full-sample forecasts (Aim 1 uses all BMIz data including post-ED-onset)
load("data/6.0/Forecast_Results.RData")  # full_data with eBMIz, upper/lower bounds

# Check what variables we have
message("=== Available columns ===")
cat(paste(names(full_data), collapse = "\n"), "\n")

# =============================================================================
# PART 1: MEDIAN CI WIDTH BY TIMEPOINT
# =============================================================================

message("\n========== PART 1: CI WIDTH ANALYSIS ==========\n")

# The CI bounds should be: upper_eBMIz, lower_eBMIz (or similar)
# Identify the relevant columns
ci_cols <- grep("upper|lower|eBMIz|ci_", names(full_data), value = TRUE, ignore.case = TRUE)
message("CI-related columns: ", paste(ci_cols, collapse = ", "))

# Compute CI width = upper - lower for each observation
# This will vary by forecast method and CI level

ci_width_summary <- full_data %>%
  filter(!is.na(upper_eBMIz), !is.na(lower_eBMIz)) %>%
  mutate(ci_width = upper_eBMIz - lower_eBMIz) %>%
  group_by(forecast_point, ci, timepoint) %>%
  summarise(
    n = n(),
    median_width = median(ci_width, na.rm = TRUE),
    q25_width = quantile(ci_width, 0.25, na.rm = TRUE),
    q75_width = quantile(ci_width, 0.75, na.rm = TRUE),
    mean_width = mean(ci_width, na.rm = TRUE),
    sd_width = sd(ci_width, na.rm = TRUE),
    min_width = min(ci_width, na.rm = TRUE),
    max_width = max(ci_width, na.rm = TRUE),
    # Also compute the half-width (comparable to ±0.5 or ±1.0)
    median_half_width = median(ci_width / 2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(forecast_point, ci, timepoint)

message("\n--- CI Width Summary (all methods) ---")
print(as.data.frame(ci_width_summary))

# Focus on mean+most_recent for the key comparison
message("\n--- CI Width: mean+most_recent only ---")
mpr_widths <- ci_width_summary %>%
  filter(forecast_point == "mean+most_recent") %>%
  select(ci, timepoint, n, median_width, median_half_width, q25_width, q75_width)
print(as.data.frame(mpr_widths))

# Compare: how does median half-width of 95%/99% CI compare to the fixed 0.5/1.0?
message("\n--- Key comparison: median half-width vs fixed thresholds ---")
message("  ±0.5 SD: fixed half-width = 0.50 BMIz")
message("  ±1.0 SD: fixed half-width = 1.00 BMIz")
mpr_widths %>%
  filter(ci %in% c("95", "99")) %>%
  mutate(
    vs_05 = sprintf("%.2f (%.1f%% of ±0.5)", median_half_width,
                     100 * median_half_width / 0.5),
    vs_10 = sprintf("%.2f (%.1f%% of ±1.0)", median_half_width,
                     100 * median_half_width / 1.0)
  ) %>%
  select(ci, timepoint, median_half_width, vs_05, vs_10) %>%
  as.data.frame() %>%
  print()

# =============================================================================
# PART 2: ED PREDICTION BY CI LEVEL × TIMEPOINT
# =============================================================================

message("\n========== PART 2: ED PREDICTION BY CI × TIMEPOINT ==========\n")

# Load ED prediction results
load("tabs/ED_behavior_prediction_results.RData")

# Focus on binary combined model (where CI matters)
# Compare early (Y2) vs late (Y4, Y6) for each CI level
bin_mpr <- binary_combined_results %>%
  filter(forecast_point == "mean+most_recent")

# Below-predicted associations by CI and timepoint
message("--- below_lower: OR and significance by CI × timepoint ---")
below_results <- bin_mpr %>%
  filter(term == "bmiz_below_lower") %>%
  group_by(ci, timepoint) %>%
  summarise(
    n_tests = n(),
    n_sig_raw = sum(p.value < 0.05),
    n_sig_fdr = sum(p.value.fdr < 0.05),
    median_OR = median(odds_ratio, na.rm = TRUE),
    mean_OR = mean(odds_ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ci, timepoint)
print(as.data.frame(below_results))

# Above-predicted associations
message("\n--- above_upper: OR and significance by CI × timepoint ---")
above_results <- bin_mpr %>%
  filter(term == "bmiz_above_upper") %>%
  group_by(ci, timepoint) %>%
  summarise(
    n_tests = n(),
    n_sig_raw = sum(p.value < 0.05),
    n_sig_fdr = sum(p.value.fdr < 0.05),
    median_OR = median(odds_ratio, na.rm = TRUE),
    mean_OR = mean(odds_ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ci, timepoint)
print(as.data.frame(above_results))

# Direct comparison: does 95% CI outperform ±0.5 at early timepoints?
message("\n--- Direct comparison: 95% CI vs ±0.5 SD at each timepoint ---")
for (tp in c("Year 2", "Year 4", "Year 6")) {
  message(sprintf("\n  Timepoint: %s", tp))
  for (ci_val in c("bmiz +/- 0.5", "95", "99")) {
    sub <- bin_mpr %>%
      filter(term == "bmiz_below_lower", ci == ci_val, timepoint == tp)
    n_sig <- sum(sub$p.value.fdr < 0.05)
    med_or <- median(sub$odds_ratio, na.rm = TRUE)
    message(sprintf("    %15s: %d/%d sig (FDR), median OR = %.2f",
                    ci_val, n_sig, nrow(sub), med_or))
  }
}

# =============================================================================
# PART 3: FIX ±1.0 SD AIM 1 ACCURACY METRICS
# =============================================================================

message("\n========== PART 3: FIX ±1.0 SD ACCURACY METRICS ==========\n")

# The ±1.0 SD accuracy metrics are currently copies of ±0.5 due to a
# forecasting bug. Recompute from the raw forecast data.

# Load the full-sample data (Aim 1 uses all timepoints regardless of ED onset)
# We need: observed bmiz, eBMIz, and population weight status (uw, ow, ob)
load("data/6.0/Forecast_Results.RData")  # full_data

# For Aim 1, we also need the full-sample version
if (file.exists("data/6.0/Forecast_Results_FullSample.RData")) {
  load("data/6.0/Forecast_Results_FullSample.RData")
  aim1_data <- full_data_fullsample
  message("Using full-sample forecast data")
} else {
  aim1_data <- full_data
  message("Using standard forecast data (full-sample not found)")
}

# Recompute ±1.0 SD classification
aim1_data <- aim1_data %>%
  mutate(
    bmiz_below_lower_1sd = as.integer(bmiz < (eBMIz - 1.0)),
    bmiz_above_upper_1sd = as.integer(bmiz > (eBMIz + 1.0))
  )

# Define weight status variables (should already exist)
# uw: BMIz < -1.65; ow: BMIz > 1.04; ob: BMIz > 1.65
if (!"uw" %in% names(aim1_data)) {
  aim1_data <- aim1_data %>%
    mutate(
      uw = as.integer(bmiz < -1.65),
      ow = as.integer(bmiz > 1.04),
      ob = as.integer(bmiz > 1.65)
    )
}

# Compute accuracy metrics for ±1.0 SD
compute_metrics <- function(truth, pred) {
  # truth and pred are binary vectors
  tp <- sum(truth == 1 & pred == 1, na.rm = TRUE)
  fp <- sum(truth == 0 & pred == 1, na.rm = TRUE)
  tn <- sum(truth == 0 & pred == 0, na.rm = TRUE)
  fn <- sum(truth == 1 & pred == 0, na.rm = TRUE)

  sens <- ifelse((tp + fn) > 0, tp / (tp + fn), NA)
  spec <- ifelse((tn + fp) > 0, tn / (tn + fp), NA)
  ppv  <- ifelse((tp + fp) > 0, tp / (tp + fp), NA)
  npv  <- ifelse((tn + fn) > 0, tn / (tn + fn), NA)
  bal_acc <- ifelse(!is.na(sens) & !is.na(spec), (sens + spec) / 2, NA)
  f1  <- ifelse(!is.na(sens) & !is.na(ppv) & (sens + ppv) > 0,
                2 * sens * ppv / (sens + ppv), NA)

  data.frame(sensitivity = sens, specificity = spec, ppv = ppv,
             npv = npv, balanced_accuracy = bal_acc, f1 = f1)
}

# Filter to where we have forecasts and filter out ±0.5/95/99 CI levels
# We only need the mean+most_recent rows (or whichever has eBMIz)
# Note: eBMIz is the same across CI levels for a given forecast_point

# Use one CI level to avoid duplication
aim1_1sd <- aim1_data %>%
  filter(ci == "bmiz +/- 0.5")  # Use any CI since eBMIz doesn't depend on CI

# Compute UW metrics (truth = uw, pred = bmiz_below_lower_1sd)
uw_1sd <- aim1_1sd %>%
  filter(!is.na(uw), !is.na(bmiz_below_lower_1sd)) %>%
  group_by(forecast_point, timepoint) %>%
  summarise(
    n = n(),
    n_uw = sum(uw),
    n_below_lower = sum(bmiz_below_lower_1sd),
    {compute_metrics(uw, bmiz_below_lower_1sd)},
    .groups = "drop"
  ) %>%
  mutate(ci = "bmiz +/- 1.0")

# Compute OW metrics (truth = ow, pred = bmiz_above_upper_1sd)
ow_1sd <- aim1_1sd %>%
  filter(!is.na(ow), !is.na(bmiz_above_upper_1sd)) %>%
  group_by(forecast_point, timepoint) %>%
  summarise(
    n = n(),
    n_ow = sum(ow),
    n_above_upper = sum(bmiz_above_upper_1sd),
    {compute_metrics(ow, bmiz_above_upper_1sd)},
    .groups = "drop"
  ) %>%
  mutate(ci = "bmiz +/- 1.0")

# Compute OB metrics
ob_1sd <- aim1_1sd %>%
  filter(!is.na(ob), !is.na(bmiz_above_upper_1sd)) %>%
  group_by(forecast_point, timepoint) %>%
  summarise(
    n = n(),
    n_ob = sum(ob),
    n_above_upper = sum(bmiz_above_upper_1sd),
    {compute_metrics(ob, bmiz_above_upper_1sd)},
    .groups = "drop"
  ) %>%
  mutate(ci = "bmiz +/- 1.0")

message("--- Corrected ±1.0 SD UW accuracy (mean+most_recent) ---")
print(as.data.frame(uw_1sd %>% filter(forecast_point == "mean+most_recent")))

message("\n--- Corrected ±1.0 SD OW accuracy (mean+most_recent) ---")
print(as.data.frame(ow_1sd %>% filter(forecast_point == "mean+most_recent")))

# Load existing accuracy metrics and replace the ±1.0 rows
load("tabs/accuracy_metrics.RData")

# Remove old ±1.0 rows and add corrected ones
uw_summary <- bind_rows(
  uw_summary %>% filter(ci != "bmiz +/- 1.0"),
  uw_1sd
) %>% arrange(forecast_point, ci, timepoint)

ow_summary <- bind_rows(
  ow_summary %>% filter(ci != "bmiz +/- 1.0"),
  ow_1sd
) %>% arrange(forecast_point, ci, timepoint)

ob_summary <- bind_rows(
  ob_summary %>% filter(ci != "bmiz +/- 1.0"),
  ob_1sd
) %>% arrange(forecast_point, ci, timepoint)

# Verify fix
message("\n--- Verification: ±0.5 vs ±1.0 now different? ---")
check_05 <- uw_summary %>%
  filter(forecast_point == "mean+most_recent", ci == "bmiz +/- 0.5") %>%
  pull(sensitivity)
check_10 <- uw_summary %>%
  filter(forecast_point == "mean+most_recent", ci == "bmiz +/- 1.0") %>%
  pull(sensitivity)
message(sprintf("  ±0.5 sensitivities: %s", paste(round(check_05, 3), collapse = ", ")))
message(sprintf("  ±1.0 sensitivities: %s", paste(round(check_10, 3), collapse = ", ")))
message(sprintf("  Different? %s", !identical(check_05, check_10)))

# Save corrected metrics
save(uw_summary, ow_summary, ob_summary,
     file = "tabs/accuracy_metrics.RData")
message("\nCorrected accuracy_metrics.RData saved.")

# Also save CI width analysis
save(ci_width_summary, below_results, above_results,
     file = "tabs/ci_width_analysis.RData")
message("CI width analysis saved to tabs/ci_width_analysis.RData")

# =============================================================================
# PART 4: SUMMARY TABLE FOR PAPER
# =============================================================================

message("\n========== SUMMARY FOR PAPER ==========\n")

message("--- Median CI half-width (BMIz units) for mean+most_recent ---")
message("  (Compare to fixed ±0.5 SD = 0.50 and ±1.0 SD = 1.00)")
mpr_widths %>%
  filter(ci %in% c("95", "99")) %>%
  select(ci, timepoint, median_half_width) %>%
  tidyr::pivot_wider(names_from = ci, values_from = median_half_width,
                     names_prefix = "CI_") %>%
  as.data.frame() %>%
  print()

message("\n--- Classification: % below predicted by CI × timepoint ---")
full_data %>%
  filter(forecast_point == "mean+most_recent") %>%
  group_by(ci, timepoint) %>%
  summarise(pct_below = round(100 * mean(bmiz_below_lower, na.rm = TRUE), 1),
            pct_above = round(100 * mean(bmiz_above_upper, na.rm = TRUE), 1),
            .groups = "drop") %>%
  filter(ci %in% c("bmiz +/- 0.5", "95", "99")) %>%
  arrange(timepoint, ci) %>%
  as.data.frame() %>%
  print()

message("\nDone.")
