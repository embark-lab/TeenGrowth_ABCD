# =============================================================================
# 5.0 Exploratory: Do ED Behaviors Predict Growth Deviations?
#
# Flips the Aim 2 direction: instead of testing whether BMIz deviations
# predict ED symptoms, we test whether ED symptoms predict concurrent
# and prospective growth deviations.
#
# Concurrent:  ED behavior at time T → bmiz_diff_raw at time T
# Prospective: ED behavior at time T → bmiz_diff_raw at time T+1
#
# Models use linear regression (bmiz_diff_raw is continuous).
# Forecasts are already blind to ED status (TeenGrowth uses only BMIz data).
#
# NOTE: This is an EXPLORATORY analysis not in the pre-registration.
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(lme4)
library(broom.mixed)

# ── Load data ────────────────────────────────────────────────────────────────
# Use FULL-SAMPLE forecasts (not noedsx) so we have predictions for
# participants with ED behaviors — needed to test the reverse direction.
#
# Pipeline: bmi_pred_data_fullsample.RData → evaluate → join with ED data
# If the full-sample evaluated data exists, use it; otherwise fall back to noedsx.

if (file.exists("data/6.0/Forecast_Results_fullsample.RData")) {
  message("Loading full-sample forecast results...")
  load("data/6.0/Forecast_Results_fullsample.RData")  # loads full_data
  load("data/6.0/cleaned_data.RData")                  # loads ant_ed_threepoints

  # Join forecast results with ED data from full sample
  ed_join <- ant_ed_threepoints %>%
    rename(id = participant_id) %>%
    mutate(agemos = round(assess_ageyrs * 12)) %>%
    select(id, agemos, timepoint, reporter:last_col())

  ed_prediction_data <- full_data %>%
    left_join(ed_join, by = c("id", "agemos")) %>%
    select(id, timepoint, agemos, everything())

  rm(full_data, ed_join, ant_ed_threepoints); gc()
  message("Using full-sample data for exploratory analysis.")

} else if (file.exists("data/6.0/ed_prediction_data.RData")) {
  message("Full-sample forecast not found; falling back to noedsx data.")
  message("NOTE: This limits the analysis to participants without early ED symptoms.")
  load("data/6.0/ed_prediction_data.RData")

} else {
  stop("No prediction data found. Run the post-server pipeline first.")
}

# Compute deviation scores
ed_prediction_data <- ed_prediction_data %>%
  arrange(id, timepoint) %>%
  mutate(
    bmiz_diff_raw = bmiz - eBMIz,
    bmi_diff_raw  = bmi - eBMI
  )

# Define ED behavior predictors (available in 6.0)
ed_behaviors <- c("binge_current", "vomit_current", "fowg_current", "other_current")

# Define timepoint ordering for prospective models
tp_order <- c("Year 2" = 2, "Year 3" = 3, "Year 4" = 4,
              "Year 5" = 5, "Year 6" = 6)

# =============================================================================
# 1. CONCURRENT MODELS: ED at time T → bmiz_diff_raw at time T
# =============================================================================
# Model: bmiz_diff_raw ~ ed_behavior
# Stratified by forecast_point, ci, reporter, timepoint, sex

message("── Running concurrent models ──")

# Filter to timepoints with ED data (Y2, Y4, Y6)
concurrent_data <- ed_prediction_data %>%
  filter(timepoint %in% c("Year 2", "Year 4", "Year 6"))

grouped_concurrent <- concurrent_data %>%
  group_by(forecast_point, ci, reporter, timepoint, sex) %>%
  group_split()

concurrent_results <- map_dfr(grouped_concurrent, function(df) {
  map_dfr(ed_behaviors, function(ed_var) {
    tryCatch({
      # Need variability in predictor
      pred_vals <- na.omit(df[[ed_var]])
      if (length(unique(pred_vals)) < 2) return(NULL)
      if (sum(pred_vals == 1) < 5) return(NULL)  # need at least 5 cases

      model <- lm(
        formula = as.formula(paste0("bmiz_diff_raw ~ ", ed_var)),
        data = df
      )

      tidy(model) %>%
        filter(term == ed_var) %>%
        mutate(
          conf.low  = estimate - 1.96 * std.error,
          conf.high = estimate + 1.96 * std.error,
          ed_predictor   = ed_var,
          forecast_point = df$forecast_point[1],
          ci             = df$ci[1],
          reporter       = df$reporter[1],
          timepoint      = df$timepoint[1],
          sex            = df$sex[1],
          model_type     = "concurrent",
          n              = nrow(df),
          n_ed           = sum(df[[ed_var]] == 1, na.rm = TRUE)
        )
    }, error = function(e) NULL)
  })
})

# FDR correction across ED behaviors within each stratum
concurrent_results <- concurrent_results %>%
  group_by(forecast_point, ci, reporter, timepoint, sex) %>%
  mutate(p.value.fdr = p.adjust(p.value, method = "fdr")) %>%
  ungroup()

message(sprintf("  Concurrent models: %d results", nrow(concurrent_results)))

# =============================================================================
# 2. PROSPECTIVE MODELS: ED at time T → bmiz_diff_raw at time T+1
# =============================================================================
# Create lagged outcome: bmiz_diff_raw at next timepoint

message("── Running prospective models ──")

# Create timepoint numeric for ordering
ed_prediction_data <- ed_prediction_data %>%
  mutate(tp_num = tp_order[as.character(timepoint)])

# For each participant × forecast combo, get the NEXT timepoint's bmiz_diff_raw
prospective_data <- ed_prediction_data %>%
  filter(!is.na(tp_num)) %>%
  arrange(id, forecast_point, ci, tp_num) %>%
  group_by(id, forecast_point, ci, reporter, sex) %>%
  mutate(
    bmiz_diff_raw_next = lead(bmiz_diff_raw, order_by = tp_num),
    timepoint_next     = lead(timepoint, order_by = tp_num)
  ) %>%
  ungroup() %>%
  filter(!is.na(bmiz_diff_raw_next))

# Filter to timepoints where we have ED data AND a next-timepoint deviation
# ED data available at Y2, Y4, Y6; next-timepoint deviation at Y3, Y5, Y7 (but Y7 doesn't exist)
# So usable pairs: Y2→Y3, Y4→Y5 (Y6 has no Y7)
# But Y3 and Y5 have sparse/missing ED data, so the test observations come from BMIz only
prospective_data <- prospective_data %>%
  filter(timepoint %in% c("Year 2", "Year 4"))

grouped_prospective <- prospective_data %>%
  group_by(forecast_point, ci, reporter, timepoint, sex) %>%
  group_split()

prospective_results <- map_dfr(grouped_prospective, function(df) {
  map_dfr(ed_behaviors, function(ed_var) {
    tryCatch({
      pred_vals <- na.omit(df[[ed_var]])
      if (length(unique(pred_vals)) < 2) return(NULL)
      if (sum(pred_vals == 1) < 5) return(NULL)

      model <- lm(
        formula = as.formula(paste0("bmiz_diff_raw_next ~ ", ed_var)),
        data = df
      )

      tidy(model) %>%
        filter(term == ed_var) %>%
        mutate(
          conf.low  = estimate - 1.96 * std.error,
          conf.high = estimate + 1.96 * std.error,
          ed_predictor   = ed_var,
          forecast_point = df$forecast_point[1],
          ci             = df$ci[1],
          reporter       = df$reporter[1],
          timepoint      = df$timepoint[1],
          timepoint_next = df$timepoint_next[1],
          sex            = df$sex[1],
          model_type     = "prospective",
          n              = nrow(df),
          n_ed           = sum(df[[ed_var]] == 1, na.rm = TRUE)
        )
    }, error = function(e) NULL)
  })
})

# FDR correction
prospective_results <- prospective_results %>%
  group_by(forecast_point, ci, reporter, timepoint, sex) %>%
  mutate(p.value.fdr = p.adjust(p.value, method = "fdr")) %>%
  ungroup()

message(sprintf("  Prospective models: %d results", nrow(prospective_results)))

# =============================================================================
# 3. MIXED-EFFECTS MODELS (pooling across timepoints)
# =============================================================================
# Concurrent LMEM: bmiz_diff_raw ~ ed_behavior + age_c + sex + (1 | id)
# Prospective LMEM: bmiz_diff_raw_next ~ ed_behavior + age_c + sex + (1 | id)

message("── Running mixed-effects models ──")

lmem_results <- list()

for (reporter_val in unique(na.omit(concurrent_data$reporter))) {
  for (fp in unique(concurrent_data$forecast_point)) {
    for (ci_val in unique(concurrent_data$ci)) {

      df_sub <- concurrent_data %>%
        filter(reporter == reporter_val,
               forecast_point == fp,
               ci == ci_val,
               !is.na(bmiz_diff_raw))

      # Mean-center age within timepoint
      df_sub <- df_sub %>%
        group_by(timepoint) %>%
        mutate(age_c = (agemos / 12) - mean(agemos / 12, na.rm = TRUE)) %>%
        ungroup()

      for (ed_var in ed_behaviors) {
        tryCatch({
          df_model <- df_sub %>% filter(!is.na(.data[[ed_var]]))
          if (sum(df_model[[ed_var]] == 1, na.rm = TRUE) < 10) next
          if (n_distinct(df_model$id) < 50) next

          fit <- lmer(
            formula = as.formula(paste0("bmiz_diff_raw ~ ", ed_var, " + age_c + sex + (1 | id)")),
            data = df_model
          )

          result <- tidy(fit, conf.int = TRUE) %>%
            filter(term == ed_var) %>%
            mutate(
              ed_predictor   = ed_var,
              forecast_point = fp,
              ci             = ci_val,
              reporter       = reporter_val,
              model_type     = "concurrent_lmem",
              n              = nrow(df_model),
              n_ids          = n_distinct(df_model$id),
              n_ed           = sum(df_model[[ed_var]] == 1, na.rm = TRUE)
            )

          lmem_results[[length(lmem_results) + 1]] <- result
        }, error = function(e) NULL)
      }
    }
  }
}

lmem_combined <- bind_rows(lmem_results)

# FDR correction across ED behaviors
if (nrow(lmem_combined) > 0) {
  lmem_combined <- lmem_combined %>%
    group_by(forecast_point, ci, reporter) %>%
    mutate(p.value.fdr = p.adjust(p.value, method = "fdr")) %>%
    ungroup()
}

message(sprintf("  LMEM models: %d results", nrow(lmem_combined)))

# =============================================================================
# 4. SAVE
# =============================================================================

save(concurrent_results, prospective_results, lmem_combined,
     file = "tabs/ED_predicts_growth_deviation.RData")

message("\n✅ Exploratory analysis complete. Results saved to tabs/ED_predicts_growth_deviation.RData")
