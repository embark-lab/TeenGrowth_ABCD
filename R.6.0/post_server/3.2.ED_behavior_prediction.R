# load file = "data/ed_prediction_data.RData"


load("data/6.0/ed_prediction_data.RData")
# sort ascending by id
ed_prediction_data <- ed_prediction_data %>%
  arrange(id, timepoint) %>% 
  mutate(bmi_diff_raw = bmi - eBMI, 
         bmiz_diff_raw = bmiz - eBMIz)


library(dplyr)
library(purrr)
library(broom)
library(caret)

compute_classification_metrics_caret <- function(data, outcome, predictor) {
  df <- data %>%
    select(all_of(c(outcome, predictor))) %>%
    drop_na()
  
  colnames(df) <- c("truth", "pred")
  
  df <- df %>%
    mutate(
      truth = factor(truth, levels = c(0, 1)),
      pred = factor(pred, levels = c(0, 1))
    )
  
  if (n_distinct(df$truth) < 2 | n_distinct(df$pred) < 2) return(NULL)
  
  cm <- caret::confusionMatrix(df$pred, df$truth, positive = "1")
  prevalence <- mean(as.numeric(as.character(df$truth)))
  
  data.frame(
    outcome = outcome,
    predictor = predictor,
    model_type = "classification",
    accuracy = cm$overall["Accuracy"],
    balanced_accuracy = cm$byClass["Balanced Accuracy"],
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    ppv = cm$byClass["Pos Pred Value"],
    npv = cm$byClass["Neg Pred Value"],
    f1 = 2 * ((cm$byClass["Sensitivity"] * cm$byClass["Pos Pred Value"]) /
                (cm$byClass["Sensitivity"] + cm$byClass["Pos Pred Value"])),
    prevalence = prevalence
  )
}


run_logistic_model <- function(data, outcome, predictor) {
  model <- glm(
    formula = as.formula(paste0(outcome, " ~ ", predictor)),
    data = data,
    family = binomial()
  )
  
  broom::tidy(model) %>%
    filter(term == predictor) %>%
    mutate(
      odds_ratio = exp(estimate),
      conf.low = exp(estimate - 1.96 * std.error),
      conf.high = exp(estimate + 1.96 * std.error),
      outcome = outcome,
      predictor = predictor,
      model_type = "logistic"
    )
}

# Define predictors and outcomes
outcomes <- c("fowg_current", "binge_current", "vomit_current", "other_current")
binary_predictors <- c("bmiz_below_lower", "bmiz_above_upper")
continuous_predictors <- c("bmiz_diff_raw", "bmi_diff_raw")

# Prepare empty lists for storage
classification_results <- list()
logistic_results <- list()

# Loop through groups
grouped_data <- ed_prediction_data %>%
  filter(timepoint %in% c("Year 2", "Year 4", "Year 6")) %>%
  group_by(forecast_point, ci, reporter, timepoint, sex) %>%
  group_split()

# Run classification models (binary predictors)
classification_results <- map_dfr(grouped_data, function(df) {
  map_dfr(binary_predictors, function(predictor) {
    map_dfr(outcomes, function(outcome) {
      result <- compute_classification_metrics_caret(df, outcome, predictor)
      if (is.null(result)) return(NULL)
      result %>%
        mutate(
          forecast_point = df$forecast_point[1],
          ci = df$ci[1],
          reporter = df$reporter[1],
          timepoint = df$timepoint[1],
          sex = df$sex[1]
        )
    })
  })
})

# Run logistic regressions (continuous predictors)
logistic_results <- map_dfr(grouped_data, function(df) {
  map_dfr(continuous_predictors, function(predictor) {
    map_dfr(outcomes, function(outcome) {
      if (length(unique(na.omit(df[[outcome]]))) < 2) return(NULL)
      
      run_logistic_model(df, outcome, predictor) %>%
        mutate(
          forecast_point = df$forecast_point[1],
          ci = df$ci[1],
          reporter = df$reporter[1],
          timepoint = df$timepoint[1],
          sex = df$sex[1]
        )
    })
  })
})


# save the results
save(classification_results, logistic_results, file = "tabs/6.0/ED_behavior_prediction_results.RData")
