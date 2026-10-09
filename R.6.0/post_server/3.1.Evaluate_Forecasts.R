library(haven)
library(labelled)
library(tibble)
library(dplyr)
library(zscorer)
library(sjmisc)
library(wesanderson)
library(TeenGrowth)
library(cgwtools)
library(moments)
library(purrr)
library(tidyr)



load("data/6.0/bmi_pred_data.RData")  # robust load

results_list <- list()
full_data <- vector("list", length(bmi_pred_data$forecasts))
names(full_data) <- names(bmi_pred_data$forecasts)
names(bmi_pred_data$testing_list) <- names(bmi_pred_data$forecasts)

for (timepoint in names(bmi_pred_data$forecasts)) {
  forecasts <- bmi_pred_data$forecasts[[timepoint]]
  training <- bmi_pred_data$training_list[[timepoint]]
  test <- bmi_pred_data$testing_list[[timepoint]]
  
  for (forecast_name in names(forecasts)) {
    forecast_data <- forecasts[[forecast_name]]
    
    forecast_merge <- right_join(forecast_data, test, by = c("id", "agemos", "sex", "adult_height_in", "agemos_adult_ht"))
    
    forecast_merge <- forecast_merge %>%
      filter(!is.na(eBMIz)) %>%
      mutate(
        eBMIz_diff = abs(eBMIz - bmiz),
        eBMI_diff = abs(eBMI - bmi),
        bmiz_in_ci = ifelse(bmiz >= lower_eBMIz & bmiz <= upper_eBMIz, 1, 0),
        bmiz_below_lower = ifelse(bmiz <= lower_eBMIz, 1, 0),
        bmiz_above_upper = ifelse(bmiz >= upper_eBMIz, 1, 0)
      )
    
    summary <- forecast_merge %>%
      summarise(
        bmiz_below_lower = mean(bmiz_below_lower, na.rm = TRUE) * 100,
        bmiz_above_upper = mean(bmiz_above_upper, na.rm = TRUE) * 100,
        bmiz_within_ci = mean(bmiz_in_ci, na.rm = TRUE) * 100,
        mean_eBMIz_diff = mean(eBMIz_diff, na.rm = TRUE),
        median_eBMIz_diff = median(eBMIz_diff, na.rm = TRUE),
        mean_eBMI_diff = mean(eBMI_diff, na.rm = TRUE),
        median_eBMI_diff = median(eBMI_diff, na.rm = TRUE)
      )
    
    summary$forecast <- forecast_name
    summary$timepoint <- timepoint
    
    # Save forecast-merged data
    if (is.null(full_data[[timepoint]])) full_data[[timepoint]] <- list()
    full_data[[timepoint]][[forecast_name]] <- forecast_merge
    
    results_list[[length(results_list) + 1]] <- summary
  }
}

# Combine and clean summary
fcast_results <- bind_rows(results_list) %>%
  separate(forecast, c("forecast_point", "ci"), sep = "_ci_", remove = FALSE) %>%
  mutate(
    ci = case_when(
      ci == "User-Defined-0.5" ~ "bmiz +/- 0.5",
      ci == "User-Defined-1.0" ~ "bmiz +/- 1.0",
      ci == "User-Defined"     ~ "bmiz +/- 0.5",
      is.na(ci) ~ "bmiz +/- 1",
      TRUE ~ ci
    )
  ) %>%
  select(-forecast)

save(fcast_results, file = "tabs/Forecast_Results.RData")


# Combine all forecasted full data
# Each training window (Y1-Y5) produces forecasts for all future timepoints,
# but we only want the NEXT timepoint's forecast from each window:
#   Y1 training → keep only Year 2 observations
#   Y2 training → keep only Year 3 observations
#   Y3 training → keep only Year 4 observations
#   Y4 training → keep only Year 5 observations
#   Y5 training → keep only Year 6 observations
# This ensures each forecast uses all available prior data (per pre-registration)

training_window_to_target <- c(
  "Y1" = "Year 2",
  "Y2" = "Year 3",
  "Y3" = "Year 4",
  "Y4" = "Year 5",
  "Y5" = "Year 6"
)

full_data_flat <- imap_dfr(full_data, function(forecast_list, training_window) {
  target_tp <- training_window_to_target[training_window]
  imap_dfr(forecast_list, function(df, forecast_name) {
    df_out <- df %>%
      mutate(
        training_window = training_window,
        forecast = forecast_name
      )
    # Filter to only the target timepoint if timepoint column exists
    if ("timepoint" %in% names(df_out) && !is.null(target_tp)) {
      df_out <- df_out %>% filter(timepoint == target_tp)
    }
    df_out
  })
})

full_data_flat <- full_data_flat %>%
  separate(forecast, c("forecast_point", "ci"), sep = "_ci_", remove = FALSE) %>%
  mutate(
    ci = case_when(
      ci == "User-Defined-0.5" ~ "bmiz +/- 0.5",
      ci == "User-Defined-1.0" ~ "bmiz +/- 1.0",
      ci == "User-Defined"     ~ "bmiz +/- 0.5",
      is.na(ci) ~ "bmiz +/- 1",
      TRUE ~ ci
    )
  ) %>%
  select(-forecast)

# ─────────────────────────────────────────────────────────────────────────────
# Regenerate fcast_results (source of Supplementary Table S2) from
# target-year-only observations. The per-training-window summary computed
# earlier in this script pooled each training window's forecast across ALL
# post-training waves (e.g., Y1-trained forecast evaluated at Year 2 AND
# Year 3 AND Year 4 AND Year 5 AND Year 6), which doesn't match how
# forecasts are used elsewhere in the paper. Figure 2A, the main-text
# accuracy numbers, and all Aim 2/3 analyses use each forecast at its
# target year only. This block overwrites fcast_results so S2 reports the
# same quantity.
# ─────────────────────────────────────────────────────────────────────────────
fcast_results <- full_data_flat %>%
  group_by(forecast_point, ci, timepoint) %>%
  summarise(
    bmiz_below_lower  = mean(bmiz_below_lower, na.rm = TRUE) * 100,
    bmiz_above_upper  = mean(bmiz_above_upper, na.rm = TRUE) * 100,
    bmiz_within_ci    = mean(bmiz_in_ci, na.rm = TRUE) * 100,
    mean_eBMIz_diff   = mean(eBMIz_diff, na.rm = TRUE),
    median_eBMIz_diff = median(eBMIz_diff, na.rm = TRUE),
    mean_eBMI_diff    = mean(eBMI_diff, na.rm = TRUE),
    median_eBMI_diff  = median(eBMI_diff, na.rm = TRUE),
    n                 = n(),
    .groups = "drop"
  )

# Overwrite the earlier saves (tabs/ and fallback tabs/) so
# 8.2.Supplemental_Tables.R picks up the target-year-only version.
if (!dir.exists("tabs")) dir.create("tabs", recursive = TRUE, showWarnings = FALSE)
if (!dir.exists("tabs"))       dir.create("tabs",       recursive = TRUE, showWarnings = FALSE)
save(fcast_results, file = "tabs/Forecast_Results.RData")
save(fcast_results, file = "tabs/Forecast_Results.RData")

# Add OW/UW/OB flags (pre-registered thresholds)
# uw: BMIz < -1.65 (~5th percentile)
# ow: BMIz > +1.04 (~85th percentile)
# ob: BMIz > +1.65 (~95th percentile)
full_data <- full_data_flat %>%
  mutate(
    uw = ifelse(bmiz < -1.65, 1, 0),
    ow = ifelse(bmiz > 1.04, 1, 0),
    ob = ifelse(bmiz > 1.65, 1, 0),
    # ±1.0 SD deviation indicators (computed post-hoc from existing eBMIz)
    # Same central value as ±0.5 SD — just wider bounds
    bmiz_below_lower_1sd = ifelse(bmiz <= (eBMIz - 1.0), 1, 0),
    bmiz_above_upper_1sd = ifelse(bmiz >= (eBMIz + 1.0), 1, 0)
  )

# Save full_data NOW, before the (non-essential) diagnostics below. Previously a
# crash in the diagnostic block (e.g. pivot_wider on the combined-value check)
# prevented this save, leaving data/6.0/Forecast_Results.RData stale while the
# summary above was fresh — which silently starved every downstream script.
if (!dir.exists("data/6.0")) dir.create("data/6.0", recursive = TRUE, showWarnings = FALSE)
save(full_data, file = "data/6.0/Forecast_Results.RData")

# ── Diagnostic: check sample sizes across methods per timepoint ──────────────
# If mean+most_recent has different N than mean or most_recent after the
# !is.na(eBMIz) filter, the error comparison is not apples-to-apples.
tryCatch({

n_diagnostic <- full_data %>%
  filter(ci == "bmiz +/- 0.5" | grepl("pop_median", forecast_point)) %>%
  filter(forecast_point %in% c("mean", "most_recent", "mean+most_recent")) %>%
  group_by(forecast_point, timepoint, training_window) %>%
  summarise(
    n_total    = n(),
    n_valid    = sum(!is.na(eBMIz)),
    n_na_eBMIz = sum(is.na(eBMIz)),
    n_na_eBMI  = sum(is.na(eBMI)),
    .groups = "drop"
  ) %>%
  arrange(timepoint, training_window, forecast_point)

# Flag any mismatches: for each timepoint × training_window, all methods
# should have the same n_valid
n_mismatches <- n_diagnostic %>%
  group_by(timepoint, training_window) %>%
  summarise(
    methods     = paste(forecast_point, collapse = ", "),
    n_valid_min = min(n_valid),
    n_valid_max = max(n_valid),
    mismatch    = n_valid_min != n_valid_max,
    .groups = "drop"
  ) %>%
  filter(mismatch)

cat("\n═══ METHOD SAMPLE SIZE DIAGNOSTIC ═══\n")
if (nrow(n_mismatches) == 0) {

  cat("✓ All methods have identical sample sizes at every timepoint × training window.\n")
  cat("  → Differential missingness is NOT the cause of combined > max(mean, recent) in BMI error.\n")

} else {

  cat("✗ MISMATCH DETECTED — methods have different Ns at some timepoint × training windows:\n\n")
  print(as.data.frame(n_mismatches), row.names = FALSE)
  cat("\nFull breakdown:\n")
  mismatched_keys <- n_mismatches %>% select(timepoint, training_window)
  print(
    as.data.frame(semi_join(n_diagnostic, mismatched_keys,
                            by = c("timepoint", "training_window"))),
    row.names = FALSE
  )
  cat("\n  → Differential missingness may contribute to combined showing higher BMI error.\n")
  cat("    Consider restricting the comparison to the intersection of valid IDs across methods.\n")

}

# Also check: individual-level bound violation
# For each person, |obs - combined| should be <= max(|obs - mean|, |obs - recent|) in BMIz space
# If not, the TeenGrowth package may not be computing (mean + recent) / 2
if (all(c("mean", "most_recent", "mean+most_recent") %in% unique(full_data$forecast_point))) {
  bound_check <- full_data %>%
    filter(ci == "bmiz +/- 0.5",
           forecast_point %in% c("mean", "most_recent", "mean+most_recent")) %>%
    select(id, timepoint, training_window, forecast_point, eBMIz, bmiz) %>%
    pivot_wider(id_cols = c(id, timepoint, training_window),
                names_from = forecast_point, values_from = eBMIz) %>%
    filter(!is.na(mean), !is.na(most_recent), !is.na(`mean+most_recent`)) %>%
    mutate(
      expected_combined = (mean + most_recent) / 2,
      actual_combined   = `mean+most_recent`,
      diff_from_expected = actual_combined - expected_combined
    )

  cat("\n═══ COMBINED = (MEAN + RECENT) / 2 VERIFICATION ═══\n")
  cat(sprintf("  N individuals × timepoints checked: %d\n", nrow(bound_check)))
  cat(sprintf("  Mean |actual - expected|:  %.6f\n", mean(abs(bound_check$diff_from_expected), na.rm = TRUE)))
  cat(sprintf("  Max  |actual - expected|:  %.6f\n", max(abs(bound_check$diff_from_expected), na.rm = TRUE)))

  if (max(abs(bound_check$diff_from_expected), na.rm = TRUE) > 0.01) {
    cat("  ✗ WARNING: mean+most_recent does NOT equal (mean + most_recent) / 2 for some observations.\n")
    cat("    This may indicate the TeenGrowth package computes the combined value differently.\n")
  } else {
    cat("  ✓ Confirmed: mean+most_recent ≈ (mean + most_recent) / 2 for all observations.\n")
  }
}

cat("\n")

}, error = function(e) message("[3.1] diagnostics skipped (non-fatal): ", conditionMessage(e)))

# Re-save (harmless; full_data already saved above before the diagnostics)
save(full_data, file = "data/6.0/Forecast_Results.RData")
