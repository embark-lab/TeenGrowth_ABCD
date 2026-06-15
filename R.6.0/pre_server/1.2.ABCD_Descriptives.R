
# =============================================================================
# ABCD 6.0 - Descriptives & ED-BMI Merge
# Updated from 5.0 script
# =============================================================================

library(haven)
library(labelled)
library(tibble)
library(dplyr)
library(tidyr)
library(zscorer)
library(sjmisc)
library(wesanderson)
library(TeenGrowth)
library(weights) # For weighted correlation
library(cgwtools)
library(moments)

dir.create("data/6.0", recursive = TRUE, showWarnings = FALSE)
dir.create("tabs/6.0", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 0. Load anthropometric data
# =============================================================================

load('data/6.0/ant.RData')

# =============================================================================
# 1. N individuals with > 2 datapoints
# =============================================================================

# remove rows where bmiz is NA
ant <- ant |> filter(!is.na(bmiz))

# count rows per participant
ant <- ant |> 
  group_by(participant_id) |> 
  mutate(n_points = n()) |> 
  ungroup()

ant <- subset(ant, bmiz >= -10 & bmiz <= 10)

# count individuals with > 2 datapoints
N_ValidBMI <- ant |> 
  filter(n_points > 2) |> 
  group_by(participant_id) |> 
  summarise(n_points = n()) |> 
  nrow()

save(N_ValidBMI, file = 'tabs/6.0/descriptives_ABCD.RData')

# =============================================================================
# Timepoint mapping (6.0)
# =============================================================================

timepoint_map <- c(
  "ses-00A" = 0, "ses-01A" = 1, "ses-02A" = 2,
  "ses-03A" = 3, "ses-04A" = 4, "ses-05A" = 5, "ses-06A" = 6
)

# =============================================================================
# 2. Mean and SD of BMIz by session
# =============================================================================

# ph_y_anthr_age is already in years
BMIz_meantable <- tapply(ant$bmiz, ant$session_id, mean, na.rm = TRUE)
BMIz_sdtable <- tapply(ant$bmiz, ant$session_id, sd, na.rm = TRUE)
BMIz_agetable <- tapply(ant$ph_y_anthr_age, ant$session_id, mean, na.rm = TRUE)
Age_sdtable <- tapply(ant$ph_y_anthr_age, ant$session_id, sd, na.rm = TRUE)

BMIz_table <- data.frame(BMIz_meantable, BMIz_sdtable, BMIz_agetable, Age_sdtable)
colnames(BMIz_table) <- c('BMIz Mean', 'BMIz SD', 'Age Mean', 'Age SD')

BMIz_table$timepoint <- rownames(BMIz_table)
BMIz_table$timepoint_num <- timepoint_map[BMIz_table$timepoint]

BMIz_table$timepoint_factor <- factor(
  BMIz_table$timepoint_num,
  levels = 0:6,
  labels = c("Baseline", "Year 1", "Year 2", "Year 3", "Year 4", "Year 5", "Year 6")
)

BMIz_table <- BMIz_table %>%
  mutate(
    `Age` = sprintf("%.2f (%.2f)", `Age Mean`, `Age SD`),
    `BMIz` = sprintf("%.2f (%.2f)", `BMIz Mean`, `BMIz SD`)
  ) %>%
  select(timepoint_factor, Age, `BMIz`)

colnames(BMIz_table) <- c('Timepoint', 'Age Mean (SD)', 'BMIz Mean (SD)')
BMIz_table <- BMIz_table[order(BMIz_table$Timepoint), ]
rownames(BMIz_table) <- NULL

resave(BMIz_table, file = 'tabs/6.0/descriptives_ABCD.RData')

# =============================================================================
# 3. Year-over-year autocorrelation
# =============================================================================

ant$timepoint <- factor(ant$session_id,
                        levels = names(timepoint_map),
                        labels = c("Baseline", "Year 1", "Year 2", "Year 3",
                                   "Year 4", "Year 5", "Year 6"))

ant$assess_ageyrs <- ant$ph_y_anthr_age

df_lagged <- ant %>%
  select(participant_id, assess_ageyrs, bmiz) %>%
  group_by(participant_id) %>%
  arrange(assess_ageyrs, .by_group = TRUE) %>%
  mutate(
    bmiz_next = lead(bmiz, 1),
    age_diff = lead(assess_ageyrs, 1) - assess_ageyrs
  ) %>%
  ungroup()

df_pairs <- df_lagged %>%
  filter(!is.na(bmiz), !is.na(bmiz_next), !is.na(age_diff), age_diff > 0)

cor_weighted <- wtd.cor(df_pairs$bmiz, df_pairs$bmiz_next, weight = 1 / df_pairs$age_diff)

resave(cor_weighted, file = 'tabs/6.0/descriptives_ABCD.RData')

# =============================================================================
# 4. N datapoints per individual (mean, SD)
# =============================================================================

N_datapoints <- mean(ant$n_points)
SD_datapoints <- sd(ant$n_points)

resave(N_datapoints, SD_datapoints, file = 'tabs/6.0/descriptives_ABCD.RData')

# =============================================================================
# 5. Within-subject BMIz Mean, SD
# =============================================================================

BMIz_mean <- ant %>%
  group_by(participant_id) %>%
  summarise(mean_bmiz = mean(bmiz, na.rm = TRUE),
            sd_bmiz = sd(bmiz, na.rm = TRUE)) %>%
  ungroup()

mean_BMIz_within <- mean(BMIz_mean$mean_bmiz)
sd_BMIz_within <- mean(BMIz_mean$sd_bmiz, na.rm = TRUE)
sd_BMIz_within_sd <- sd(BMIz_mean$sd_bmiz, na.rm = TRUE)

resave(mean_BMIz_within, sd_BMIz_within, sd_BMIz_within_sd, file = 'tabs/6.0/descriptives_ABCD.RData')

# =============================================================================
# 6. Visit ordering & ED merge
# =============================================================================

ant$timepoint_numeric <- as.numeric(ant$timepoint)

ant_ordered <- ant %>%
  group_by(participant_id) %>%
  arrange(timepoint_numeric, .by_group = TRUE) %>%
  mutate(visit_num = row_number()) %>%
  ungroup()

# Load ED datasets (6.0 paths)
load("data/6.0/binge_keyvars.RData")
load("data/6.0/wt_control.RData")
load("data/6.0/fowg.RData")

# Combine ED datasets
all_ed <- full_join(binge_keyvars, wt_control,
                    by = c("participant_id", "session_id", "reporter", "timepoint_num"))

all_ed <- full_join(all_ed, fowg,
                    by = c("participant_id", "session_id", "reporter", "timepoint_num"))

# Merge ED with anthropometric data
ant_ed <- full_join(ant_ordered, all_ed, by = c("participant_id", "session_id"))

# =============================================================================
# ED symptom rolling variable
# In 5.0 this was: binge OR pills OR restrict OR exercise OR vomit OR laxative
# In 6.0, pills/restrict/exercise/laxative are all collapsed into "other"
# So equivalent is: binge OR vomit OR other
# =============================================================================

ant_ed$ed_sx_rolling <- case_when(
  is.na(ant_ed$binge_rolling) & is.na(ant_ed$vomit_rolling) & is.na(ant_ed$other_rolling) ~ NA,
  ant_ed$binge_rolling == 0 &
    ant_ed$vomit_rolling == 0 &
    ant_ed$other_rolling == 0 ~ 0,
  TRUE ~ 1
)

save(ant_ed, file = 'data/6.0/ant_ed.RData')

table(ant_ed$ed_sx_rolling, ant_ed$visit_num, ant_ed$reporter)

# =============================================================================
# 7. Subsample selection
# =============================================================================

ant_ed_threepoints <- ant_ed %>%
  group_by(participant_id) %>%
  filter(n_points >= 3) %>%
  ungroup()

N_threepoints <- length(unique(ant_ed_threepoints$participant_id))

# IDs with ed_sx_rolling == 1 or NA at visit_num <= 2
ed_bl <- ant_ed_threepoints %>%
  group_by(participant_id) %>%
  filter(visit_num <= 2) %>%
  filter(ed_sx_rolling == 1 | is.na(ed_sx_rolling)) %>%
  pull(participant_id)

# Remove those IDs
ant_ed_threepoints_noedsx <- ant_ed_threepoints %>%
  filter(!participant_id %in% ed_bl)

N_subsample <- length(unique(ant_ed_threepoints_noedsx$participant_id))
N_ed <- length(unique(ed_bl))

resave(N_threepoints, N_subsample, N_ed, file = 'tabs/6.0/descriptives_ABCD.RData')

# =============================================================================
# 8. Final sample & timepoint tables
# =============================================================================

final_sample <- ant_ed_threepoints_noedsx |> filter(n_points > 2)
N_final <- length(unique(final_sample$participant_id))

# Complete missing timepoints
# NOTE: 6.0 has 7 timepoints (1:7 as factor levels)
ant_ed_threepoints_noedsx_full <- final_sample %>%
  group_by(participant_id, sex) %>%
  tidyr::complete(timepoint_numeric = 1:7, reporter = c('parent', 'youth')) %>%
  ungroup()

ant_ed_threepoints <- ant_ed_threepoints %>%
  group_by(participant_id, sex) %>%
  tidyr::complete(timepoint_numeric = 1:7, reporter = c('parent', 'youth')) %>%
  ungroup()

save(ant_ed_threepoints, file = 'data/6.0/ant_ed_threepoints.RData')

# =============================================================================
# 9. Create frequency/percentage tables
# =============================================================================

create_tables <- function(df, outcome) {
  x <- table(
    df[[outcome]],
    df$timepoint_numeric,
    df$reporter,
    df$sex,
    useNA = "always"
  )
  
  df_table <- as.data.frame(x)
  names(df_table)[1:4] <- c(outcome, "timepoint_numeric", "reporter", "sex")
  names(df_table)[5] <- "frequency"
  
  df_table <- df_table %>%
    group_by(timepoint_numeric, sex, reporter) %>%
    mutate(percentage_within_timepoint = (frequency / sum(frequency)) * 100) %>%
    ungroup() %>%
    filter(!is.na(percentage_within_timepoint) & !is.na(reporter) & !is.na(sex))
  
  df_table_wide_percentage <- df_table %>%
    select(-frequency) %>%
    pivot_wider(
      names_from = !!sym(outcome),
      values_from = percentage_within_timepoint
    ) %>%
    mutate(outcome = outcome)
  
  df_table_wide_frequency <- df_table %>%
    select(-percentage_within_timepoint) %>%
    pivot_wider(
      names_from = !!sym(outcome),
      values_from = frequency
    ) %>%
    mutate(outcome = outcome)
  
  list(percentage = df_table_wide_percentage,
       frequency = df_table_wide_frequency)
}

# --- Updated outcome lists for 6.0 ---
# 5.0 had: restrict, binge, pills, exercise, vomit, laxative, fowg, fowg_half
# 6.0 has: binge, vomit, other, fowg (no individual subtypes, no fowg_half)

outcomes_current <- c("binge_current", "vomit_current", "other_current", "fowg_current")
outcomes_rolling <- c("binge_rolling", "vomit_rolling", "other_rolling", "fowg_rolling")

# For ant_ed_threepoints_noedsx_full:
tables_noedsx_current <- lapply(outcomes_current, function(outcome) {
  create_tables(ant_ed_threepoints_noedsx_full, outcome)
})
names(tables_noedsx_current) <- outcomes_current

tables_noedsx_rolling <- lapply(outcomes_rolling, function(outcome) {
  create_tables(ant_ed_threepoints_noedsx_full, outcome)
})
names(tables_noedsx_rolling) <- outcomes_rolling

combined_noedsx_percentage_current <- bind_rows(lapply(tables_noedsx_current, `[[`, "percentage"))
combined_noedsx_frequency_current  <- bind_rows(lapply(tables_noedsx_current, `[[`, "frequency"))

combined_noedsx_percentage_rolling <- bind_rows(lapply(tables_noedsx_rolling, `[[`, "percentage"))
combined_noedsx_frequency_rolling  <- bind_rows(lapply(tables_noedsx_rolling, `[[`, "frequency"))

# For ant_ed_threepoints:
tables_threepoints_current <- lapply(outcomes_current, function(outcome) {
  create_tables(ant_ed_threepoints, outcome)
})
names(tables_threepoints_current) <- outcomes_current

tables_threepoints_rolling <- lapply(outcomes_rolling, function(outcome) {
  create_tables(ant_ed_threepoints, outcome)
})
names(tables_threepoints_rolling) <- outcomes_rolling

combined_threepoints_percentage_current <- bind_rows(lapply(tables_threepoints_current, `[[`, "percentage"))
combined_threepoints_frequency_current  <- bind_rows(lapply(tables_threepoints_current, `[[`, "frequency"))

combined_threepoints_percentage_rolling <- bind_rows(lapply(tables_threepoints_rolling, `[[`, "percentage"))
combined_threepoints_frequency_rolling  <- bind_rows(lapply(tables_threepoints_rolling, `[[`, "frequency"))

# =============================================================================
# 10. Recode and filter
# =============================================================================

recode_and_filter <- function(df) {
  df %>%
    mutate(
      timepoint_numeric = factor(timepoint_numeric,
                                 levels = 1:7,
                                 labels = c("baseline", "1 year", "2 year", "3 year",
                                            "4 year", "5 year", "6 year")),
      sex = factor(sex,
                   levels = c(1, 2),
                   labels = c("Male", "Female"))
    ) %>%
    filter(`0` != 0, `1` != 0)
}

combined_noedsx_percentage_current <- recode_and_filter(combined_noedsx_percentage_current)
combined_noedsx_frequency_current  <- recode_and_filter(combined_noedsx_frequency_current)
combined_noedsx_percentage_rolling <- recode_and_filter(combined_noedsx_percentage_rolling)
combined_noedsx_frequency_rolling  <- recode_and_filter(combined_noedsx_frequency_rolling)

combined_threepoints_percentage_current <- recode_and_filter(combined_threepoints_percentage_current)
combined_threepoints_frequency_current  <- recode_and_filter(combined_threepoints_frequency_current)
combined_threepoints_percentage_rolling <- recode_and_filter(combined_threepoints_percentage_rolling)
combined_threepoints_frequency_rolling  <- recode_and_filter(combined_threepoints_frequency_rolling)

# =============================================================================
# 11. Restructure outputs
# =============================================================================

# Process current percentages:
noedsx_current_pct <- combined_noedsx_percentage_current %>%
  mutate(behavior = sub("_current", "", outcome)) %>%
  rename(current_endorsed = `1`, current_missing = `NA`) %>%
  select(-`0`) %>%
  mutate(current_responded_rate = if_else((100 - current_missing) > 0,
                                          (current_endorsed / (100 - current_missing)) * 100,
                                          NA_real_))

# Process rolling percentages:
noedsx_rolling_pct <- combined_noedsx_percentage_rolling %>%
  mutate(behavior = sub("_rolling", "", outcome)) %>%
  rename(rolling_endorsed = `1`, rolling_missing = `NA`) %>%
  select(-`0`) %>%
  mutate(rolling_responded_rate = if_else((100 - rolling_missing) > 0,
                                          (rolling_endorsed / (100 - rolling_missing)) * 100,
                                          NA_real_))

# Merge current and rolling percentage tables:
ant_ed_noedsx_percentage_combined <- full_join(
  noedsx_current_pct %>% select(behavior, timepoint_numeric, reporter, sex,
                                current_endorsed, current_missing, current_responded_rate),
  noedsx_rolling_pct  %>% select(behavior, timepoint_numeric, reporter, sex,
                                 rolling_endorsed, rolling_missing, rolling_responded_rate),
  by = c("behavior", "timepoint_numeric", "reporter", "sex")
)

# Process current frequencies:
noedsx_current_freq <- combined_noedsx_frequency_current %>%
  mutate(behavior = sub("_current", "", outcome)) %>%
  rename_with(~ paste0("current_", .), .cols = c("0", "1", "NA"))

# Process rolling frequencies:
noedsx_rolling_freq <- combined_noedsx_frequency_rolling %>%
  mutate(behavior = sub("_rolling", "", outcome)) %>%
  rename_with(~ paste0("rolling_", .), .cols = c("0", "1", "NA"))

# Merge current and rolling frequency tables:
ant_ed_noedsx_frequency_combined <- full_join(
  noedsx_current_freq %>% select(behavior, timepoint_numeric, reporter, sex, starts_with("current_")),
  noedsx_rolling_freq  %>% select(behavior, timepoint_numeric, reporter, sex, starts_with("rolling_")),
  by = c("behavior", "timepoint_numeric", "reporter", "sex")
)

# --- Same for threepoints ---
threepoints_current_pct <- combined_threepoints_percentage_current %>%
  mutate(behavior = sub("_current", "", outcome)) %>%
  rename(current_endorsed = `1`, current_missing = `NA`) %>%
  select(-`0`) %>%
  mutate(current_responded_rate = if_else((100 - current_missing) > 0,
                                          (current_endorsed / (100 - current_missing)) * 100,
                                          NA_real_))

threepoints_rolling_pct <- combined_threepoints_percentage_rolling %>%
  mutate(behavior = sub("_rolling", "", outcome)) %>%
  rename(rolling_endorsed = `1`, rolling_missing = `NA`) %>%
  select(-`0`) %>%
  mutate(rolling_responded_rate = if_else((100 - rolling_missing) > 0,
                                          (rolling_endorsed / (100 - rolling_missing)) * 100,
                                          NA_real_))

ant_ed_threepoints_percentage_combined <- full_join(
  threepoints_current_pct %>% select(behavior, timepoint_numeric, reporter, sex,
                                     current_endorsed, current_missing, current_responded_rate),
  threepoints_rolling_pct  %>% select(behavior, timepoint_numeric, reporter, sex,
                                      rolling_endorsed, rolling_missing, rolling_responded_rate),
  by = c("behavior", "timepoint_numeric", "reporter", "sex")
)

threepoints_current_freq <- combined_threepoints_frequency_current %>%
  mutate(behavior = sub("_current", "", outcome)) %>%
  rename_with(~ paste0("current_", .), .cols = c("0", "1", "NA"))

threepoints_rolling_freq <- combined_threepoints_frequency_rolling %>%
  mutate(behavior = sub("_rolling", "", outcome)) %>%
  rename_with(~ paste0("rolling_", .), .cols = c("0", "1", "NA"))

ant_ed_threepoints_frequency_combined <- full_join(
  threepoints_current_freq %>% select(behavior, timepoint_numeric, reporter, sex, starts_with("current_")),
  threepoints_rolling_freq  %>% select(behavior, timepoint_numeric, reporter, sex, starts_with("rolling_")),
  by = c("behavior", "timepoint_numeric", "reporter", "sex")
)

# =============================================================================
# 12. Final lists & save
# =============================================================================

ant_ed_noedsx_list <- list(
  percentage = ant_ed_noedsx_percentage_combined,
  frequency  = ant_ed_noedsx_frequency_combined
)

ant_ed_noedsx_list$percentage <- ant_ed_noedsx_list$percentage %>%
  mutate(across(contains("current") | contains("rolling"), ~ round(., 2)))

ant_ed_noedsx_list$percentage <- ant_ed_noedsx_list$percentage %>%
  select(-contains("rate"))

ant_ed_threepoints_list <- list(
  percentage = ant_ed_threepoints_percentage_combined,
  frequency  = ant_ed_threepoints_frequency_combined
)

ant_ed_threepoints_list$percentage <- ant_ed_threepoints_list$percentage %>%
  mutate(across(contains("current") | contains("rolling"), ~ round(., 2)))

ant_ed_threepoints_list$percentage <- ant_ed_threepoints_list$percentage %>%
  select(-contains("rate"))

resave(ant_ed_noedsx_list, ant_ed_threepoints_list, file = 'tabs/6.0/descriptives_ABCD.RData')

save(ant_ed_threepoints_noedsx_full, ant_ed_threepoints_noedsx, ant_ed, ant_ed_threepoints,
     file = 'data/6.0/cleaned_data.RData')