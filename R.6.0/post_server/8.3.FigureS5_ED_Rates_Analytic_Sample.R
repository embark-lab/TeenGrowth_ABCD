# =============================================================================
# 8.3 Supplementary Figure S5 (R&R, Reviewer 1 Comment 1; 2026-09-26)
# ED symptom endorsement rates in the Aims 2-3 analytic subsample (N = 9,250),
# i.e., after excluding youth who endorsed binge eating, vomiting, or other
# weight control behaviors at or before their second BMIz observation.
# Same construction as manuscript Figure 1 (Results_Draft.Rmd, fig-ed-rates),
# which uses ant_ed_threepoints_list (N = 10,917, pre-exclusion).
#
# NOTE: 1.2.ABCD_Descriptives.R drops rows with zero endorsers
# (filter(`1` != 0)). In the analytic subsample, behavioral endorsement at
# baseline/Year 1 is zero by construction, so those rows are missing. Zeros
# are restored below ONLY for reporter x wave x measure cells that were
# assessed (i.e., have at least one row for some behavior/sex).
# Run from ~/ABCD.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

load("tabs/6.0/descriptives_ABCD.RData")   # ant_ed_noedsx_list, ant_ed_threepoints_list

has_embark <- requireNamespace("embarktools", quietly = TRUE)
paper_theme <- function() {
  if (has_embark) embarktools::embark_theme_a + theme(text = element_text(size = 16))
  else theme_minimal(base_size = 16) + theme(panel.grid.minor = element_blank())
}

tp_map   <- c("baseline" = "BL", "1 year" = "Y1", "2 year" = "Y2",
              "3 year" = "Y3", "4 year" = "Y4", "5 year" = "Y5", "6 year" = "Y6")
year_map <- c("baseline" = 0, "1 year" = 1, "2 year" = 2,
              "3 year" = 3, "4 year" = 4, "6 year" = 6)
behavior_labels_ed <- c("binge" = "Binge Eating", "other" = "Other Wt Ctrl",
                        "fowg" = "Fear of Wt Gain", "vomit" = "Vomiting")

pct_df <- ant_ed_noedsx_list$percentage

rates <- bind_rows(
  pct_df %>%
    filter(!is.na(current_endorsed), !is.na(current_missing)) %>%
    mutate(rate = ifelse((100 - current_missing) > 0,
                         current_endorsed / (100 - current_missing) * 100, NA_real_),
           measure = "A: Current Endorsement"),
  pct_df %>%
    filter(!is.na(rolling_endorsed), !is.na(rolling_missing)) %>%
    mutate(rate = ifelse((100 - rolling_missing) > 0,
                         rolling_endorsed / (100 - rolling_missing) * 100, NA_real_),
           measure = "B: Cumulative Endorsement")
) %>%
  mutate(timepoint_numeric = as.character(timepoint_numeric),
         sex = as.character(sex)) %>%
  select(behavior, timepoint_numeric, reporter, sex, measure, rate)

# Cells (reporter x wave x measure) that were assessed at all
assessed <- rates %>% distinct(reporter, timepoint_numeric, measure)

# Restore zero-endorsement rows within assessed cells
rates_full <- assessed %>%
  tidyr::crossing(behavior = unique(rates$behavior),
                  sex = unique(rates$sex)) %>%
  left_join(rates, by = c("reporter", "timepoint_numeric", "measure", "behavior", "sex")) %>%
  mutate(rate = ifelse(is.na(rate), 0, rate))

plot_df <- rates_full %>%
  mutate(
    tp_label = factor(tp_map[timepoint_numeric], levels = c("BL","Y1","Y2","Y3","Y4","Y5","Y6")),
    year_num = year_map[timepoint_numeric],
    behavior_label = behavior_labels_ed[behavior],
    reporter_label = ifelse(reporter == "parent", "Parent", "Youth")
  ) %>%
  filter(!is.na(tp_label), !is.na(year_num)) %>%
  filter(!(reporter_label == "Youth" & tp_label %in% c("Y3", "Y5")),
         !(reporter_label == "Parent" & tp_label == "Y5")) %>%
  group_by(behavior, behavior_label, tp_label, year_num, reporter_label, measure) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop")

fig_s5 <- ggplot(plot_df,
                 aes(x = year_num, y = rate, color = behavior_label,
                     group = interaction(behavior, reporter_label),
                     linetype = reporter_label, shape = reporter_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, stroke = 1.2) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 6),
                     labels = c("BL", "Y1", "Y2", "Y3", "Y4", "Y6")) +
  scale_color_manual(values = c("Binge Eating" = "#1A4F66", "Other Wt Ctrl" = "#EF6C45",
                                "Fear of Wt Gain" = "#F5A623", "Vomiting" = "#9B8EC1"),
                     name = "Behavior") +
  scale_linetype_manual(values = c("Parent" = "dashed", "Youth" = "solid"), name = "Reporter") +
  scale_shape_manual(values = c("Parent" = 1, "Youth" = 16), name = "Reporter") +
  labs(x = "Assessment Wave", y = "% of Responders") +
  paper_theme() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 16))

ggsave("figs/figS5_ed_rates_analytic_sample.png", fig_s5,
       width = 12, height = 6, dpi = 300, bg = "white", device = ragg::agg_png)
message("Saved figs/figS5_ed_rates_analytic_sample.png")
