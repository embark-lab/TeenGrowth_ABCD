# is raw bmiz difference associated with pubertal status, sex, timepoint, bmiz, 
# slope of puberty, or slope of bmiz 

# Descriptives - sex, bmiz, bmiz_diff_raw, bmiz_below_lower, bmiz_above_upper, pds_mean and pds_category at each timepoint 

load(file = "data/ed_prediction_data_with_pds.RData")

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(broom)
library(purrr)
library(forcats)

# 0. Recode PDS Category as an ordered factor
ed_prediction_data <- ed_prediction_data %>%
  mutate(
    pds_category = factor(pds_category,
                          levels = c("Pre", "Early", "Mid", "Late", "Post"),
                          ordered = TRUE)
  )

# 1. Summary Table by Timepoint, Forecast Point, and CI
library(dplyr)

# Step 1: Calculate participant-level means (across forecast_point/ci) for each timepoint
individual_means <- ed_prediction_data %>%
  group_by(id, timepoint, sex) %>%
  summarise(
    agemos = mean(agemos, na.rm = TRUE),
    bmiz = mean(bmiz, na.rm = TRUE),
    bmiz_diff_raw = mean(bmiz_diff_raw, na.rm = TRUE),
    bmiz_below_lower = mean(bmiz_below_lower, na.rm = TRUE),
    bmiz_above_upper = mean(bmiz_above_upper, na.rm = TRUE),
    pds_mean = mean(pds_mean, na.rm = TRUE),
    .groups = "drop"
  )

library(dplyr)
library(gt)

# Step 2: Group-level summary with mean and SD formatting
summary_by_timepoint_sex <- individual_means %>%
  group_by(timepoint, sex) %>%
  summarise(
    n = n(),
    age_m = mean(agemos / 12, na.rm = TRUE),
    age_sd = sd(agemos / 12, na.rm = TRUE),
    bmiz_m = mean(bmiz, na.rm = TRUE),
    bmiz_sd = sd(bmiz, na.rm = TRUE),
    bmiz_diff_raw_m = mean(bmiz_diff_raw, na.rm = TRUE),
    bmiz_diff_raw_sd = sd(bmiz_diff_raw, na.rm = TRUE),
    pds_mean_m = mean(pds_mean, na.rm = TRUE),
    pds_mean_sd = sd(pds_mean, na.rm = TRUE),
    bmiz_below_lower_n = sum(bmiz_below_lower, na.rm = TRUE),
    bmiz_below_lower_pct = mean(bmiz_below_lower, na.rm = TRUE) * 100,
    bmiz_above_upper_n = sum(bmiz_above_upper, na.rm = TRUE),
    bmiz_above_upper_pct = mean(bmiz_above_upper, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    age = sprintf("%.2f (%.2f)", age_m, age_sd),
    bmiz = sprintf("%.2f (%.2f)", bmiz_m, bmiz_sd),
    bmiz_diff_raw = sprintf("%.2f (%.2f)", bmiz_diff_raw_m, bmiz_diff_raw_sd),
    pds_mean = sprintf("%.2f (%.2f)", pds_mean_m, pds_mean_sd),
    bmiz_below_lower = sprintf("%d (%.1f%%)", as.integer(bmiz_below_lower_n), bmiz_below_lower_pct),
    bmiz_above_upper = sprintf("%d (%.1f%%)", as.integer(bmiz_above_upper_n), bmiz_above_upper_pct),
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  ) %>%
  select(timepoint, sex, n, age, bmiz, bmiz_diff_raw, pds_mean, bmiz_below_lower, bmiz_above_upper)

# Format with gt
descriptives_gt <- summary_by_timepoint_sex %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics by Timepoint and Sex",
    subtitle = "Means (SD) by individual; proportions for thresholds"
  ) %>%
  cols_label(
    timepoint = "Timepoint",
    sex = "Sex",
    n = "N",
    age = "Age (Years)",
    pds_mean = "Pubertal Score",
    bmiz = "BMIZ",
    bmiz_diff_raw = "BMIZ Difference (Across Models)",
    bmiz_below_lower = "Below Lower Cutoff (Across Models)",
    bmiz_above_upper = "Above Upper Cutoff (Across Models)"
  ) %>%
  cols_align(align = "center", columns = everything())


# save the summary stats
save(descriptives_gt, file = 'tabs/bmiz_dev_descriptives_gt.RData')

descriptive_data <- ed_prediction_data %>%
  mutate(
    age_years = agemos / 12,
    pds_category = factor(pds_category, levels = c("Pre", "Early", "Mid", "Late", "Post"), ordered = TRUE)
  ) %>%
  group_by(id, timepoint) %>%
  summarise(
    bmiz_diff_raw = mean(bmiz_diff_raw, na.rm = TRUE),
    bmiz = mean(bmiz, na.rm = TRUE),
    pds_mean = mean(pds_mean, na.rm = TRUE),
    age_years = mean(age_years, na.rm = TRUE),
    pds_category = first(pds_category),  # assuming PDS category doesn't change within timepoint
    sex = first(sex),
    .groups = "drop"
  )

# Load required packages
library(dplyr)
library(ggplot2)
library(reshape2)

# Step 1: Prepare dataset with averaged values per id + timepoint
heatmap_data <- ed_prediction_data %>%
  mutate(
    age_years = agemos / 12,
    pds_category = factor(pds_category, levels = c("Pre", "Early", "Mid", "Late", "Post"), ordered = TRUE)
  ) %>%
  group_by(id, timepoint) %>%
  summarise(
    bmiz_diff_raw = mean(bmiz_diff_raw, na.rm = TRUE),
    bmiz = mean(bmiz, na.rm = TRUE),
    pds_mean = mean(pds_mean, na.rm = TRUE),
    age_years = mean(age_years, na.rm = TRUE),
    pds_category = first(pds_category),
    sex = first(sex),
    .groups = "drop"
  ) 



library(reshape2)
# Step 2: Define readable labels and rename
vars <- c("BMIZ Difference (Avg)" = "bmiz_diff_raw",
          "BMIZ" = "bmiz",
          "Pubertal Score" = "pds_mean",
          "Age (years)" = "age_years")

renamed_data <- heatmap_data %>%
  rename(!!!vars)

# Step 3: Compute sex-specific correlation matrices
boys_cor <- renamed_data %>%
  filter(sex == 1) %>%
  select(names(vars)) %>%
  cor(use = "pairwise.complete.obs")

girls_cor <- renamed_data %>%
  filter(sex == 2) %>%
  select(names(vars)) %>%
  cor(use = "pairwise.complete.obs")

# Step 4: Reshape to long format and tag triangles
boys_long <- melt(boys_cor) %>%
  mutate(sex = "Male",
         i = match(Var1, rownames(boys_cor)),
         j = match(Var2, colnames(boys_cor))) %>%
  filter(i <= j)  # upper triangle only

girls_long <- melt(girls_cor) %>%
  mutate(sex = "Female",
         i = match(Var1, rownames(girls_cor)),
         j = match(Var2, colnames(girls_cor))) %>%
  filter(i >= j)  # lower triangle only

# Step 5: Combine into one dataframe
cor_combined <- bind_rows(boys_long, girls_long)

# Step 6: Plot with ggplot2
ggplot(cor_combined, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(
    data = cor_combined %>% filter(i == j),
    fill = "grey80", color = "white"
  ) +
  # Heatmap tiles for off-diagonal values
  geom_tile(
    data = cor_combined %>% filter(i != j),
    aes(fill = value), color = "white"
  ) +
  geom_text(data = cor_combined %>% filter(i != j),
    aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low = "#1A4F66" , mid = "white", high = "#EF6C45", midpoint = 0,
    name = "Correlation (r)"
  ) +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  labs(
    title = "Correlation Heatmap",
    subtitle = "Boys (Upper), Girls (Lower)",
    x = "", y = ""
  ) +
  embarktools::embark_theme_a +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank()
  ) 

# save the data table and the heatmap 
save(heatmap_data, file = "tabs/heatmap_data.RData")
# save the heatmap
ggsave(file = "figs/correlation_heatmap.png", 
       width = 8, height = 6, dpi = 300)

ed_prediction_data <- ed_prediction_data %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  ) 

collapsed_data <- ed_prediction_data %>%
  arrange(id, timepoint, forecast_point) %>%  # ensures order is consistent
  group_by(id, timepoint, forecast_point) %>%
  slice(1) %>%
  ungroup() %>% 
  filter(.model %in% c("mean+most_recent", "pop_median")) %>% 
  # recode .model to be more interpretable
  mutate(
    forecast_point = recode(forecast_point,
                    "mean+most_recent" = "Individualized",
                    "pop_median" = "Population Median")
  ) %>% 
  filter(sex == 'Female')

# 5. Visualization by Forecast Point and CI
ggplot(collapsed_data, aes(x = pds_mean, y = bmiz_diff_raw, color = timepoint, fill = timepoint)) +
  geom_smooth(method = "lm") + 
  facet_grid(~forecast_point) +
  # add custom colors for color and fill
  scale_color_manual(values = c( "#1A4F66", "#EF6C45", "#C2B824") )+
  scale_fill_manual(values = c( "#1A4F66", "#EF6C45", "#C2B824")) +
  labs(
    title = "BMIZ Difference by Pubertal Category (Girls)",
    x = "Pubertal Development Score",
    y = "BMIz Deviation"
  ) +
  embarktools::embark_theme_a + 
  ylim(-1, 1) 

# 6. Save the plot
ggsave(file = "figs/BMIzbyPDSandSex.png", 
       width = 8, height = 8, dpi = 300)
