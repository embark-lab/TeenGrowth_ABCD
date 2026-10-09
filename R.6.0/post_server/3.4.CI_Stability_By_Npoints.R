# =============================================================================
# 2.1d CI Stability by Number of Available Datapoints
#
# Purpose:
#   Examine whether model-based prediction interval widths (95%, 99%) stabilize
#   after a certain number of prior BMIz observations, or whether they remain
#   sensitive to data availability. This is critical for evaluating whether
#   CI-based thresholds are appropriate for clinical use, where youth may have
#   varying numbers of growth chart entries.
#
# Key question: At what n_points does CI width stabilize, and does the
#   instability at low n_points undermine the utility of CI-based thresholds?
# =============================================================================

library(dplyr)
library(tidyr)

# =============================================================================
# 0. LOAD DATA
# =============================================================================

load("data/6.0/Forecast_Results.RData")  # full_data

message("=== Data dimensions ===")
message(sprintf("  Rows: %d, Participants: %d", nrow(full_data),
                length(unique(full_data$id))))

# =============================================================================
# 1. COMPUTE CI WIDTH AND n_points AT TIME OF FORECAST
# =============================================================================

# n_points = number of BMIz observations available AT THE TIME of forecast
# This is the visit_num (sequential observation number), NOT total observations
# The forecast at visit_num = 3 uses the first 2 observations to predict the 3rd

# visit_num should already be in the data; if not, compute it
if (!"visit_num" %in% names(full_data)) {
  full_data <- full_data %>%
    group_by(id, forecast_point, ci) %>%
    arrange(timepoint, .by_group = TRUE) %>%
    mutate(visit_num = row_number() + 2) %>%  # +2 because forecasts start after >=2 training points
    ungroup()
  message("Computed visit_num from row order")
} else {
  message(sprintf("visit_num range: %d to %d", min(full_data$visit_num, na.rm = TRUE),
                  max(full_data$visit_num, na.rm = TRUE)))
}

# Compute CI width for model-based CIs
ci_data <- full_data %>%
  filter(ci %in% c("95", "99"),
         !is.na(upper_eBMIz), !is.na(lower_eBMIz)) %>%
  mutate(
    ci_width = upper_eBMIz - lower_eBMIz,
    ci_half_width = ci_width / 2
  )

message(sprintf("\n  Rows with CI data: %d", nrow(ci_data)))
message(sprintf("  CI width range: %.3f to %.3f", min(ci_data$ci_width), max(ci_data$ci_width)))

# =============================================================================
# 2. CI WIDTH BY n_points (visit_num) — CONTROLLING FOR TIMEPOINT
# =============================================================================

message("\n========== CI WIDTH BY n_points ==========\n")

# First: what is the distribution of visit_num (n prior points)?
message("--- Distribution of visit_num ---")
print(table(ci_data$visit_num))

# CI width by visit_num, pooled across timepoints
width_by_npoints <- ci_data %>%
  filter(forecast_point == "mean+most_recent") %>%
  group_by(ci, visit_num) %>%
  summarise(
    n = n(),
    median_half_width = median(ci_half_width, na.rm = TRUE),
    q25 = quantile(ci_half_width, 0.25, na.rm = TRUE),
    q75 = quantile(ci_half_width, 0.75, na.rm = TRUE),
    iqr = q75 - q25,
    sd_half_width = sd(ci_half_width, na.rm = TRUE),
    cv = sd_half_width / median_half_width,  # coefficient of variation
    .groups = "drop"
  )

message("--- CI half-width by n_points (mean+most_recent) ---")
print(as.data.frame(width_by_npoints))

# =============================================================================
# 3. CI WIDTH BY n_points × TIMEPOINT (to separate n_points from age effects)
# =============================================================================

message("\n--- CI half-width by n_points × timepoint (mean+most_recent, 99% CI) ---")
width_by_npoints_tp <- ci_data %>%
  filter(forecast_point == "mean+most_recent", ci == "99") %>%
  group_by(timepoint, visit_num) %>%
  summarise(
    n = n(),
    median_half_width = median(ci_half_width, na.rm = TRUE),
    sd_half_width = sd(ci_half_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 20)  # Only show cells with enough data

print(as.data.frame(width_by_npoints_tp))

# =============================================================================
# 4. WITHIN-TIMEPOINT VARIABILITY: How much does CI width vary across
#    individuals at the SAME timepoint?
# =============================================================================

message("\n========== WITHIN-TIMEPOINT CI WIDTH VARIABILITY ==========\n")

within_tp_var <- ci_data %>%
  filter(forecast_point == "mean+most_recent") %>%
  group_by(ci, timepoint) %>%
  summarise(
    n = n(),
    median_half_width = median(ci_half_width, na.rm = TRUE),
    sd_half_width = sd(ci_half_width, na.rm = TRUE),
    cv = sd_half_width / median_half_width,
    min_half_width = min(ci_half_width, na.rm = TRUE),
    max_half_width = max(ci_half_width, na.rm = TRUE),
    range_ratio = max_half_width / min_half_width,
    .groups = "drop"
  )

message("--- Within-timepoint variability in CI half-width ---")
print(as.data.frame(within_tp_var))

# =============================================================================
# 5. DOES n_points PREDICT CI WIDTH AFTER CONTROLLING FOR TIMEPOINT?
# =============================================================================

message("\n========== REGRESSION: CI width ~ n_points + timepoint ==========\n")

for (ci_val in c("95", "99")) {
  mod_data <- ci_data %>%
    filter(forecast_point == "mean+most_recent", ci == ci_val)

  mod <- lm(ci_half_width ~ factor(visit_num) + factor(timepoint), data = mod_data)

  message(sprintf("--- %s%% CI: half-width ~ visit_num + timepoint ---", ci_val))
  message(sprintf("  R-squared: %.4f", summary(mod)$r.squared))
  message(sprintf("  Adj R-squared: %.4f", summary(mod)$adj.r.squared))

  # Partial effect of visit_num (controlling for timepoint)
  mod_tp_only <- lm(ci_half_width ~ factor(timepoint), data = mod_data)
  delta_r2 <- summary(mod)$r.squared - summary(mod_tp_only)$r.squared
  message(sprintf("  R² from timepoint alone: %.4f", summary(mod_tp_only)$r.squared))
  message(sprintf("  Additional R² from visit_num: %.4f", delta_r2))

  # Coefficients for visit_num
  coefs <- summary(mod)$coefficients
  visit_coefs <- coefs[grep("visit_num", rownames(coefs)), ]
  message("  visit_num coefficients (relative to visit_num=3):")
  print(round(visit_coefs, 4))
}

# =============================================================================
# 6. STABILIZATION TEST: At what n_points does the CI width coefficient
#    become non-significant (i.e., adding more points doesn't narrow the CI)?
# =============================================================================

message("\n========== STABILIZATION ANALYSIS ==========\n")

for (ci_val in c("95", "99")) {
  mod_data <- ci_data %>%
    filter(forecast_point == "mean+most_recent", ci == ci_val)

  # Pairwise comparisons: is CI width at visit_num k different from visit_num k+1?
  visit_nums <- sort(unique(mod_data$visit_num))

  message(sprintf("--- %s%% CI: pairwise width comparisons ---", ci_val))
  for (i in 1:(length(visit_nums) - 1)) {
    v1 <- visit_nums[i]
    v2 <- visit_nums[i + 1]
    w1 <- mod_data %>% filter(visit_num == v1) %>% pull(ci_half_width)
    w2 <- mod_data %>% filter(visit_num == v2) %>% pull(ci_half_width)

    if (length(w1) >= 20 & length(w2) >= 20) {
      test <- wilcox.test(w1, w2)
      effect <- median(w2) - median(w1)
      message(sprintf("  visit %d→%d: median change = %+.4f, p = %.4f, n1=%d, n2=%d",
                      v1, v2, effect, test$p.value, length(w1), length(w2)))
    }
  }
}

# =============================================================================
# 7. PRACTICAL IMPACT: Does CI-width variability affect ED prediction?
# =============================================================================

message("\n========== IMPACT ON ED PREDICTION ==========\n")

# Split into narrow vs wide CI groups (median split within timepoint)
# and compare ED prediction performance

load("data/6.0/ed_prediction_data.RData")

# Compute visit_num for ed_prediction_data (not present by default)
ed_prediction_data <- ed_prediction_data %>%
  group_by(id, forecast_point, ci) %>%
  arrange(timepoint, .by_group = TRUE) %>%
  mutate(visit_num_ed = row_number() + 2) %>%
  ungroup()

# Join CI width info
# Need to compute ci_half_width for the ed_prediction_data
ed_ci <- ed_prediction_data %>%
  filter(!is.na(upper_eBMIz), !is.na(lower_eBMIz),
         ci %in% c("95", "99"),
         forecast_point == "mean+most_recent") %>%
  mutate(ci_half_width = (upper_eBMIz - lower_eBMIz) / 2)

# Median split within timepoint
ed_ci <- ed_ci %>%
  group_by(ci, timepoint) %>%
  mutate(
    width_median = median(ci_half_width, na.rm = TRUE),
    ci_width_group = ifelse(ci_half_width <= width_median, "narrow", "wide")
  ) %>%
  ungroup()

message("--- CI width groups ---")
ed_ci %>%
  group_by(ci, timepoint, ci_width_group) %>%
  summarise(
    n = n(),
    median_half_width = round(median(ci_half_width), 3),
    median_visit_num = median(visit_num_ed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print()

# Compare below-predicted → ED associations in narrow vs wide CI groups
message("\n--- ED associations by CI width group (99% CI, mean+most_recent) ---")
behaviors <- c("vomit_current", "binge_current", "other_current", "fowg_current")

for (outcome in behaviors) {
  for (width_grp in c("narrow", "wide")) {
    sub <- ed_ci %>%
      filter(ci == "99", ci_width_group == width_grp,
             !is.na(.data[[outcome]]), !is.na(bmiz_below_lower))

    if (nrow(sub) < 50) next
    if (length(unique(sub[[outcome]])) < 2) next

    tryCatch({
      mod <- glm(as.formula(paste0(outcome, " ~ bmiz_below_lower")),
                 data = sub, family = binomial)
      or <- exp(coef(mod)["bmiz_below_lower"])
      p <- summary(mod)$coefficients["bmiz_below_lower", "Pr(>|z|)"]
      message(sprintf("  %s | %s CI width: OR = %.2f, p = %.4f, n = %d",
                      outcome, width_grp, or, p, nrow(sub)))
    }, error = function(e) NULL)
  }
}

message("\nDone.")
