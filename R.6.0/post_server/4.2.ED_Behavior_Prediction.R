# =============================================================================
# 3.2 ED Behavior Prediction (Aim 2)
# Updated to align with pre-registration:
#   - Combined logistic model: outcome ~ bmiz_below_lower + bmiz_above_upper + ow + uw
#   - Continuous logistic model: outcome ~ bmiz_diff_raw
#   - Four outcomes: vomit_current, binge_current, other_current, fowg_current
#     (restrict_current and exercise_current not available in 6.0 — collapsed into other_current)
#   - FDR correction across outcomes
#   - Stratified by forecast_point, ci, reporter, timepoint, sex
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(caret)

# Load data
load("data/6.0/ed_prediction_data.RData")

# Sort and compute deviation scores
ed_prediction_data <- ed_prediction_data %>%
  arrange(id, timepoint) %>%
  mutate(
    bmi_diff_raw  = bmi - eBMI,
    bmiz_diff_raw = bmiz - eBMIz
  )

# =============================================================================
# QUICK-FIX: For ±1.0 SD rows, the forecast chunks used ±0.5 margins
# (run_forecasts bug — margins not passed to forecast_bmi). The columns
# bmiz_below_lower / bmiz_above_upper are therefore identical to ±0.5.
# Swap in the post-hoc _1sd columns (computed correctly from eBMIz ± 1.0
# in 2.1.Evaluate_Forecasts.R) so the logistic models use the right values.
# This patch can be removed once forecasts are re-run with the fixed scripts.
# =============================================================================
if ("bmiz_below_lower_1sd" %in% names(ed_prediction_data)) {
  ed_prediction_data <- ed_prediction_data %>%
    mutate(
      bmiz_below_lower = ifelse(ci %in% c("bmiz +/- 1", "bmiz +/- 1.0"),
                                bmiz_below_lower_1sd, bmiz_below_lower),
      bmiz_above_upper = ifelse(ci %in% c("bmiz +/- 1", "bmiz +/- 1.0"),
                                bmiz_above_upper_1sd, bmiz_above_upper)
    )
  message("Patched: ±1.0 SD rows now use bmiz_below_lower_1sd / bmiz_above_upper_1sd")
}

# =============================================================================
# DEFINE OUTCOMES AND PREDICTORS (per pre-registration)
# =============================================================================

# Pre-registered outcomes: restrict_current, vomit_current, fowg_current_half,
#   binge_current, exercise_current
# Available in 6.0: vomit_current, binge_current, other_current, fowg_current
# NOTE: restrict_current and exercise_current collapsed into other_current in 6.0
# NOTE: fowg_current_half not available in 6.0; using fowg_current instead

outcomes <- c("fowg_current", "binge_current", "vomit_current", "other_current")

# Continuous predictors for logistic regression
continuous_predictors <- c("bmiz_diff_raw")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# 1. Logistic regression with continuous predictor
run_logistic_model <- function(data, outcome, predictor) {
  tryCatch({
    model <- glm(
      formula = as.formula(paste0(outcome, " ~ ", predictor)),
      data = data,
      family = binomial()
    )

    broom::tidy(model) %>%
      filter(term == predictor) %>%
      mutate(
        odds_ratio = exp(estimate),
        conf.low   = exp(estimate - 1.96 * std.error),
        conf.high  = exp(estimate + 1.96 * std.error),
        outcome    = outcome,
        predictor  = predictor,
        model_type = "logistic_continuous"
      )
  }, error = function(e) NULL)
}

# 2. Combined binary classification logistic model (pre-registered)
# Model: outcome ~ bmiz_below_lower + bmiz_above_upper + ow + uw
run_combined_binary_model <- function(data, outcome) {
  tryCatch({
    # Check that we have variability in outcome
    outcome_vals <- na.omit(data[[outcome]])
    if (length(unique(outcome_vals)) < 2) return(NULL)

    model <- glm(
      formula = as.formula(paste0(outcome,
        " ~ bmiz_below_lower + bmiz_above_upper + ow + uw")),
      data = data,
      family = binomial()
    )

    broom::tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        odds_ratio = exp(estimate),
        conf.low   = exp(estimate - 1.96 * std.error),
        conf.high  = exp(estimate + 1.96 * std.error),
        outcome    = outcome,
        model_type = "logistic_binary_combined"
      )
  }, error = function(e) NULL)
}

# 3. Classification metrics for individual binary predictors (supplementary)
compute_classification_metrics <- function(data, outcome, predictor) {
  df <- data %>%
    select(all_of(c(outcome, predictor))) %>%
    drop_na()

  colnames(df) <- c("truth", "pred")

  df <- df %>%
    mutate(
      truth = factor(truth, levels = c(0, 1)),
      pred  = factor(pred, levels = c(0, 1))
    )

  if (n_distinct(df$truth) < 2 | n_distinct(df$pred) < 2) return(NULL)

  cm <- caret::confusionMatrix(df$pred, df$truth, positive = "1")
  prevalence <- mean(as.numeric(as.character(df$truth)))

  data.frame(
    outcome            = outcome,
    predictor          = predictor,
    model_type         = "classification",
    accuracy           = cm$overall["Accuracy"],
    balanced_accuracy  = cm$byClass["Balanced Accuracy"],
    sensitivity        = cm$byClass["Sensitivity"],
    specificity        = cm$byClass["Specificity"],
    ppv                = cm$byClass["Pos Pred Value"],
    npv                = cm$byClass["Neg Pred Value"],
    f1 = 2 * ((cm$byClass["Sensitivity"] * cm$byClass["Pos Pred Value"]) /
              (cm$byClass["Sensitivity"] + cm$byClass["Pos Pred Value"])),
    prevalence         = prevalence
  )
}

# =============================================================================
# RUN MODELS
# =============================================================================

# Filter to analysis timepoints (Y2, Y4, Y6 — note: pre-reg says all timepoints,
# but per decision we keep Y2, Y4, Y6 and note simplification in paper)
grouped_data <- ed_prediction_data %>%
  filter(timepoint %in% c("Year 2", "Year 4", "Year 6")) %>%
  group_by(forecast_point, ci, reporter, timepoint, sex) %>%
  group_split()

# --- 1. Continuous logistic regression: outcome ~ bmiz_diff_raw ---
logistic_results <- map_dfr(grouped_data, function(df) {
  map_dfr(continuous_predictors, function(predictor) {
    map_dfr(outcomes, function(outcome) {
      result <- run_logistic_model(df, outcome, predictor)
      if (is.null(result)) return(NULL)
      result %>%
        mutate(
          forecast_point = df$forecast_point[1],
          ci       = df$ci[1],
          reporter = df$reporter[1],
          timepoint = df$timepoint[1],
          sex      = df$sex[1]
        )
    })
  })
})

# --- 2. Combined binary model: outcome ~ bmiz_below_lower + bmiz_above_upper + ow + uw ---
binary_combined_results <- map_dfr(grouped_data, function(df) {
  map_dfr(outcomes, function(outcome) {
    result <- run_combined_binary_model(df, outcome)
    if (is.null(result)) return(NULL)
    result %>%
      mutate(
        forecast_point = df$forecast_point[1],
        ci       = df$ci[1],
        reporter = df$reporter[1],
        timepoint = df$timepoint[1],
        sex      = df$sex[1]
      )
  })
})

# --- 3. Individual classification metrics (supplementary) ---
binary_predictors <- c("bmiz_below_lower", "bmiz_above_upper", "ow", "uw")

classification_results <- map_dfr(grouped_data, function(df) {
  map_dfr(binary_predictors, function(predictor) {
    map_dfr(outcomes, function(outcome) {
      result <- compute_classification_metrics(df, outcome, predictor)
      if (is.null(result)) return(NULL)
      result %>%
        mutate(
          forecast_point = df$forecast_point[1],
          ci       = df$ci[1],
          reporter = df$reporter[1],
          timepoint = df$timepoint[1],
          sex      = df$sex[1]
        )
    })
  })
})

# =============================================================================
# FDR CORRECTION (across outcomes, per pre-registration)
# =============================================================================

# Apply FDR to continuous logistic results
logistic_results <- logistic_results %>%
  group_by(forecast_point, ci, reporter, timepoint, sex, predictor) %>%
  mutate(p.value.fdr = p.adjust(p.value, method = "fdr")) %>%
  ungroup()

# Apply FDR to combined binary model results
binary_combined_results <- binary_combined_results %>%
  group_by(forecast_point, ci, reporter, timepoint, sex, term) %>%
  mutate(p.value.fdr = p.adjust(p.value, method = "fdr")) %>%
  ungroup()

# =============================================================================
# SAVE
# =============================================================================

save(classification_results, logistic_results, binary_combined_results,
     file = "tabs/ED_behavior_prediction_results.RData")

message("Aim 2 models complete. Results saved to tabs/ED_behavior_prediction_results.RData")
