# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(caret)
library(purrr)

# Load results
load("data/6.0/Forecast_Results.RData")
load("data/6.0/cleaned_data.RData")


# rename ant_ed_threepoints src_subject_id to id and interview_age to agemos
ant_ed_threepoints_noedsx <- ant_ed_threepoints_noedsx %>%
  rename(id = participant_id) %>% 
  mutate(agemos = round(assess_ageyrs*12))

ed_subset <- ant_ed_threepoints_noedsx %>%
  select(
    id,
    agemos,
    reporter:last_col()
  )


# join full_data with ed_subset
ed_prediction_data <- full_data %>%
  left_join(ed_subset, by = c("id", "agemos")) %>% 
  select(
    id,
    timepoint,
    agemos,
    everything()
  )

# save joined data as ed_prediction_data
save(ed_prediction_data, file = "data/6.0/ed_prediction_data.RData")
