# R/2.0.0.prepare_inputs_noedsx.R  (run ONCE)
library(dplyr)
library(TeenGrowth)

load("data/6.0/cleaned_data.RData")  # contains ant_ed_threepoints_noedsx

cleaned <- clean_data(
  ant_ed_threepoints_noedsx,
  id_col_name  = "participant_id",
  age_col_name = "ph_y_anthr_age",
  age_unit     = "years",
  sex_col_name = "sex_1M2F",
  ht_col_name  = "ht",
  wt_col_name  = "wt",
  ht_unit = "cm", wt_unit = "kg"
)

# Remove duplicate id + agemos rows (caused by rounding age to integer months)
cleaned <- cleaned %>%
  group_by(id, agemos) %>%
  slice(1) %>%
  ungroup()

# Create timepoints map (agemos rounded to match clean_data output)
timepoints_map <- ant_ed_threepoints_noedsx %>%
  select(participant_id, ph_y_anthr_age, timepoint) %>%
  rename(id = participant_id, agemos = ph_y_anthr_age) %>%
  mutate(agemos = round(agemos * 12)) %>%
  distinct()

# Merge timepoints with cleaned data
cleaned <- cleaned %>%
  left_join(timepoints_map, by = c("id", "agemos"))

# Rename for downstream use
ant_ed_threepoints_noedsx <- ant_ed_threepoints_noedsx %>%
  rename(id = participant_id) %>%
  mutate(agemos = round(ph_y_anthr_age * 12))

# Extract ED subset
ed_subset <- ant_ed_threepoints_noedsx %>%
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

# Remove existing agemos_ed_onset if present, then join updated version
if ("agemos_ed_onset" %in% names(cleaned)) {
  cleaned <- cleaned %>% select(-agemos_ed_onset)
}
cleaned <- cleaned %>% left_join(agemos_onset, by = "id")

timepoints_ordered <- c("Baseline", "Year 1", "Year 2", "Year 3",
                        "Year 4", "Year 5", "Year 6")

training_list <- lapply(2:6, function(i) cleaned %>% filter(timepoint %in% timepoints_ordered[1:i]))
names(training_list) <- c("Y1", "Y2", "Y3", "Y4", "Y5")

saveRDS(cleaned,       "data/6.0/cleaned_ant_threepoints_noedsx.rds")
saveRDS(training_list, "data/6.0/training_list_ant_threepoints_noedsx.rds")