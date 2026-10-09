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



load("data/6.0/bmi_pred_data_noED.RData")

results <- data.frame()
results_list <- list()
full_data <- vector("list", length(bmi_pred_data_noEDinfo$forecasts))
names(full_data) <- names(bmi_pred_data_noEDinfo$forecasts)
names(bmi_pred_data_noEDinfo$testing_list) <- names(bmi_pred_data_noEDinfo$forecasts)

for (timepoint in names(bmi_pred_data_noEDinfo$forecasts)) {
  forecasts <- bmi_pred_data_noEDinfo$forecasts[[timepoint]]
  training <- bmi_pred_data_noEDinfo$training_list[[timepoint]]
  test <- bmi_pred_data_noEDinfo$testing_list[[timepoint]]
  
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
results_noEDinfo <- bind_rows(results_list) %>%
  separate(forecast, c("forecast_point", "ci"), sep = "_ci_", remove = FALSE) %>%
  mutate(
    ci = case_when(
      ci == "User-Defined" ~ "bmiz +/- 0.5",
      is.na(ci) ~ "bmiz +/- 1",
      TRUE ~ ci
    )
  ) %>%
  select(-forecast)

# Combine all forecasted full data
full_data_flat <- imap_dfr(full_data, function(forecast_list, timepoint) {
  imap_dfr(forecast_list, function(df, forecast_name) {
    df %>%
      mutate(
        timepoint = timepoint,
        forecast = forecast_name
      )
  })
})

full_data_flat <- full_data_flat %>%
  separate(forecast, c("forecast_point", "ci"), sep = "_ci_", remove = FALSE) %>%
  mutate(
    ci = case_when(
      ci == "User-Defined" ~ "bmiz +/- 0.5",
      is.na(ci) ~ "bmiz +/- 1",
      TRUE ~ ci
    )
  ) %>%
  select(-forecast)

# Add OW/UW flags
full_data_noEDinfo <- full_data_flat %>%
  mutate(
    ow = ifelse(bmiz > 1, 1, 0),
    uw = ifelse(bmiz < -1, 1, 0)
  )

# Save everything
save(full_data_noEDinfo, results, file = "data/6.0/Forecast_Results_noEDinfo.RData")
