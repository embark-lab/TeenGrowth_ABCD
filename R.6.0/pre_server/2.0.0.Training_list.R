# =============================================================================
# R/2.0.0.prepare_inputs.R  (run ONCE)
# Prepares cleaned data and training lists for both:
#   1. Full sample (ant_ed_threepoints)
#   2. No-ED-at-baseline subsample (ant_ed_threepoints_noedsx)
# =============================================================================

library(dplyr)
library(tidyr)
library(TeenGrowth)

load("data/6.0/cleaned_data.RData")

timepoints_ordered <- c("Baseline", "Year 1", "Year 2", "Year 3",
                        "Year 4", "Year 5", "Year 6")

# =============================================================================
# 1. FULL SAMPLE (ant_ed_threepoints)
# =============================================================================

cleaned_all <- clean_data(
  ant_ed_threepoints,
  id_col_name  = "participant_id",
  age_col_name = "ph_y_anthr_age",
  age_unit     = "years",
  sex_col_name = "sex_1M2F",
  ht_col_name  = "ht",
  wt_col_name  = "wt",
  ht_unit = "cm", wt_unit = "kg"
)

# Dedup after clean_data (rounding age to integer months creates ties)
cleaned_all <- cleaned_all %>%
  group_by(id, agemos) %>%
  slice(1) %>%
  ungroup()

# Timepoints map — also dedup so join doesn't reintroduce duplicates
timepoints_map_all <- ant_ed_threepoints %>%
  select(participant_id, ph_y_anthr_age, timepoint) %>%
  rename(id = participant_id, agemos = ph_y_anthr_age) %>%
  mutate(agemos = round(agemos * 12)) %>%
  group_by(id, agemos) %>%
  slice(1) %>%
  ungroup()

cleaned_all <- cleaned_all %>% left_join(timepoints_map_all, by = c("id", "agemos"))

# Final safety dedup
cleaned_all <- cleaned_all %>%
  group_by(id, agemos) %>%
  slice(1) %>%
  ungroup()

# Training lists
training_list_all <- lapply(2:6, function(i) cleaned_all %>% filter(timepoint %in% timepoints_ordered[1:i]))
names(training_list_all) <- c("Y1", "Y2", "Y3", "Y4", "Y5")

# Verify no dupes
stopifnot(all(sapply(training_list_all, function(x) x %>% group_by(id, agemos) %>% filter(n() > 1) %>% nrow()) == 0))

saveRDS(cleaned_all,       "data/6.0/cleaned_ant_threepoints.rds")
saveRDS(training_list_all, "data/6.0/training_list_ant_threepoints.rds")

rm(cleaned_all, timepoints_map_all, training_list_all)

# =============================================================================
# 2. NO-ED SUBSAMPLE (ant_ed_threepoints_noedsx)
# =============================================================================

cleaned_noedsx <- clean_data(
  ant_ed_threepoints_noedsx,
  id_col_name  = "participant_id",
  age_col_name = "ph_y_anthr_age",
  age_unit     = "years",
  sex_col_name = "sex_1M2F",
  ht_col_name  = "ht",
  wt_col_name  = "wt",
  ht_unit = "cm", wt_unit = "kg"
)

# Dedup after clean_data
cleaned_noedsx <- cleaned_noedsx %>%
  group_by(id, agemos) %>%
  slice(1) %>%
  ungroup()

# Timepoints map — dedup to prevent join from reintroducing duplicates
timepoints_map_noedsx <- ant_ed_threepoints_noedsx %>%
  select(participant_id, ph_y_anthr_age, timepoint) %>%
  rename(id = participant_id, agemos = ph_y_anthr_age) %>%
  mutate(agemos = round(agemos * 12)) %>%
  group_by(id, agemos) %>%
  slice(1) %>%
  ungroup()

cleaned_noedsx <- cleaned_noedsx %>%
  left_join(timepoints_map_noedsx, by = c("id", "agemos"))

# Final safety dedup
cleaned_noedsx <- cleaned_noedsx %>%
  group_by(id, agemos) %>%
  slice(1) %>%
  ungroup()

# --- ED onset age lookup ---
ant_ed_noedsx_renamed <- ant_ed_threepoints_noedsx %>%
  rename(id = participant_id) %>%
  mutate(agemos = round(ph_y_anthr_age * 12))

ed_subset <- ant_ed_noedsx_renamed %>%
  select(id, agemos, timepoint, ed_sx_rolling)

ed_long <- ed_subset %>%
  mutate(
    timepoint = as.character(timepoint),
    timepoint_num = case_when(
      timepoint == "Year 1" ~ 1,
      timepoint == "Year 2" ~ 2,
      timepoint == "Year 3" ~ 3,
      timepoint == "Year 4" ~ 4,
      timepoint == "Year 5" ~ 5,
      timepoint == "Year 6" ~ 6,
      TRUE ~ NA_real_
    )
  )

ed_lookup <- ed_long %>%
  arrange(id, timepoint_num) %>%
  group_by(id) %>%
  mutate(last_clean_timepoint = ifelse(ed_sx_rolling == 0, timepoint, NA_character_)) %>%
  tidyr::fill(last_clean_timepoint, .direction = "down") %>%
  ungroup()

agemos_onset <- ed_lookup %>%
  filter(ed_sx_rolling == 1) %>%
  group_by(id) %>%
  summarise(agemos_ed_onset = min(agemos, na.rm = TRUE), .groups = "drop")

if ("agemos_ed_onset" %in% names(cleaned_noedsx)) {
  cleaned_noedsx <- cleaned_noedsx %>% select(-agemos_ed_onset)
}
cleaned_noedsx <- cleaned_noedsx %>% left_join(agemos_onset, by = "id")

# Training lists
training_list_noedsx <- lapply(2:6, function(i) cleaned_noedsx %>% filter(timepoint %in% timepoints_ordered[1:i]))
names(training_list_noedsx) <- c("Y1", "Y2", "Y3", "Y4", "Y5")

# Verify no dupes
stopifnot(all(sapply(training_list_noedsx, function(x) x %>% group_by(id, agemos) %>% filter(n() > 1) %>% nrow()) == 0))

saveRDS(cleaned_noedsx,       "data/6.0/cleaned_ant_threepoints_noedsx.rds")
saveRDS(training_list_noedsx, "data/6.0/training_list_ant_threepoints_noedsx.rds")
