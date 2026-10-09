library(dplyr)
library(broom)
library(purrr)
library(lme4)

load("data/ed_prediction_data.RData")
# load pubertal development data
load("data/pds_scored.RData")

# right join with ed_prediction_data
ed_prediction_data <- ed_prediction_data %>%
  left_join(pds_scored, by = c("id", "timepoint"))



# Define behavior outcomes
behaviors <- c("restrict_current", 
               "vomit_current", 
               "binge_current", 
               "exercise_current", 
               "fowg_current_half")

# Ensure categorical variables are properly coded
ed_prediction_data <- ed_prediction_data %>%
  mutate(
    sex = factor(sex),
    timepoint = factor(timepoint, levels = c("Year 2", "Year 3", "Year 4"))
  )


ed_prediction_data <- ed_prediction_data %>%
  arrange(id, timepoint) %>% 
  mutate(bmi_diff_raw = bmi - eBMI, 
         bmiz_diff_raw = bmiz - eBMIz)

# mean center pds within sex
ed_prediction_data <- ed_prediction_data %>%
  group_by(sex) %>%
  mutate(pds_mean_c = pds_mean - mean(pds_mean, na.rm = TRUE)) %>%
  ungroup()

ed_prediction_data <- ed_prediction_data %>%
  group_by(timepoint) %>%
  mutate(age_c = (agemos / 12) - mean(agemos / 12, na.rm = TRUE)) %>%
  ungroup()


# Split by reporter
reporter_groups <- ed_prediction_data %>%
  filter(!is.na(reporter)) %>%
  group_by(reporter) %>%
  group_split()

# Initialize results list
model_results <- list()

# Loop over reporter groups
for (grp in reporter_groups) {
  rep_label <- unique(grp$reporter)
  
  for (outcome in behaviors) {
    message("Fitting model for outcome: ", outcome, " | reporter: ", rep_label)
    
    # Prepare data subset
    df_sub <- grp %>%
      filter(!is.na(.data[[outcome]]),
             !is.na(sex),
             !is.na(bmiz_below_lower),
             !is.na(bmiz_diff_raw),
             !is.na(timepoint))
    
    if (nrow(df_sub) < 50) {
      message("Skipping (too few rows): ", outcome, " | ", rep_label)
      next
    }
    
    # Fit logistic model
    model <- glmer(
      formula = as.formula(paste0(outcome, " ~ sex + 
                              age_c + 
                              bmiz_diff_raw + 
                              bmiz * bmiz_diff_raw + 
                              pds_mean_c * bmiz_diff_raw + 
                              (1 | id)")),
      data = df_sub,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa")
    )
    # Store results
    model_results[[paste0(rep_label, "_", outcome)]] <-
      tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
      mutate(reporter = rep_label, outcome = outcome)
  }
}

# Combine all results
final_results <- bind_rows(model_results)

# View or save
print(final_results)
save(final_results, "model_results_stratified.RData")


# save ed_prediction_data 

save(ed_prediction_data, file = "data/ed_prediction_data_with_pds.RData")
