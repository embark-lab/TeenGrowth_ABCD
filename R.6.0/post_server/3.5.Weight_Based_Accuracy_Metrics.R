# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(caret)
library(purrr)

# Load results
load("data/6.0/Forecast_Results.RData")

# Define weight categories (pre-registered thresholds)
# uw: BMIz < -1.65 (~5th percentile)
# ow: BMIz > +1.04 (~85th percentile)
# ob: BMIz > +1.65 (~95th percentile)
full_data <- full_data %>%
  mutate(weight_category = case_when(
    bmiz < -1.65 ~ "Underweight",     # ~5th percentile
    bmiz > 1.04  ~ "Overweight",      # ~85th percentile
    TRUE         ~ "Normal Weight"
  ))

# Remove population median forecasts
full_data <- full_data %>%
  filter(!grepl("pop_median", forecast_point))

# Summarize % below forecast CI by group
tab_summary_uw <- full_data %>%
  group_by(timepoint, forecast_point, ci, weight_category) %>%
  summarise(
    n = n(),
    below_ci = sum(bmiz_below_lower == 1, na.rm = TRUE),
    pct_below_ci = round(100 * below_ci / n, 1),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(below_ci, "/", n),
    weight_category = factor(weight_category, levels = c("Underweight", "Normal Weight", "Overweight"))
  )


# Overall (All weight categories combined)
tab_overall <- full_data %>%
  group_by(timepoint, forecast_point, ci) %>%
  summarise(
    n = n(),
    below_ci = sum(bmiz_below_lower == 1, na.rm = TRUE),
    pct_below_ci = round(100 * below_ci / n, 1),
    weight_category = "All",
    .groups = "drop"
  ) %>%
  mutate(label = paste0(below_ci, "/", n))


tab_combined_uw <- bind_rows(tab_summary_uw, tab_overall) %>%
  mutate(weight_category = factor(
    weight_category,
    levels = c("All", "Underweight", "Normal Weight", "Overweight")
  ))

# (Optional) Wide version for table output
tab_wide <- tab_combined_uw %>%
  select(timepoint, forecast_point, ci, weight_category, pct_below_ci) %>%
  pivot_wider(names_from = weight_category, values_from = pct_below_ci)

# Final plot
uw_plot <- ggplot(tab_combined_uw, aes(x = pct_below_ci, y = forecast_point, fill = weight_category)) +
  geom_col(position = position_dodge2(preserve = "single"), color = "black", size = 0.3) +
  facet_grid(timepoint ~ ci) +
  scale_fill_manual(values = c("Underweight" = "#1A4F66", "Normal Weight" = "#EF6C45", "Overweight" = "#C2B824", "All" = "#A481C7")) +
  scale_color_manual(values = c("Underweight" = "#1A4F66", "Normal Weight" = "#EF6C45", "Overweight" = "#C2B824", "All" = "#A481C7" )) +
  labs(
    title = "% Below Forecast CI by \n Forecast Method, CI Level, and Weight Category",
    x = "% Below Forecast", y = "Forecast Method",
    fill = "Weight Category", color = "Label Text Color"
  ) +
  theme_minimal(base_size = 13) +
  coord_cartesian(xlim = c(0, 110)) +
  coord_flip() +
  guides(color = "none") + 
  embarktools::embark_theme_a +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 13)
  )

# Summarize % below forecast CI by group
tab_summary_ow <- full_data %>%
  group_by(timepoint, forecast_point, ci, weight_category) %>%
  summarise(
    n = n(),
    above_ci = sum(bmiz_above_upper == 1, na.rm = TRUE),
    pct_above_ci = round(100 * above_ci / n, 1),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(above_ci, "/", n),
    weight_category = factor(weight_category, levels = c("Underweight", "Normal Weight", "Overweight"))
  )

# Overall (All weight categories combined)
tab_overall_ow <- full_data %>%
  group_by(timepoint, forecast_point, ci) %>%
  summarise(
    n = n(),
    above_ci = sum(bmiz_above_upper == 1, na.rm = TRUE),
    pct_above_ci = round(100 * above_ci / n, 1),
    weight_category = "All",
    .groups = "drop"
  ) %>%
  mutate(label = paste0(above_ci, "/", n))


tab_combined_ow <- bind_rows(tab_summary_ow, tab_overall_ow) %>%
  mutate(weight_category = factor(
    weight_category,
    levels = c("All", "Underweight", "Normal Weight", "Overweight")
  ))
# Final plot
ow_plot <- ggplot(tab_combined_ow, aes(x = pct_above_ci, y = forecast_point, fill = weight_category)) +
  geom_col(position = position_dodge2(preserve = "single"), color = "black", size = 0.3) +
  facet_grid(timepoint ~ ci) +
  scale_fill_manual(values = c("Underweight" = "#1A4F66", "Normal Weight" = "#EF6C45", "Overweight" = "#C2B824", "All" = "#A481C7")) +
  scale_color_manual(values = c("Underweight" = "#1A4F66", "Normal Weight" = "#EF6C45", "Overweight" = "#C2B824", "All" = "#A481C7" )) +
  labs(
    title = "% Above Forecast CI by \n Forecast Method, CI Level, and Weight Category",
    x = "% Above Forecast", y = "Forecast Method",
    fill = "Weight Category", color = "Label Text Color"
  ) +
  theme_minimal(base_size = 13) +
  coord_cartesian(xlim = c(0, 110)) +
  coord_flip() +
  guides(color = "none") + 
  embarktools::embark_theme_a +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 13)
  )

# ---- R&R (Reviewer 1, Comment 10), 2026-09-26: readable S1 Panels B/C ----
# Fewer tick labels + extra row spacing so top/bottom ticks of adjacent Year rows
# no longer stack ("40" over "0"); human-readable facet and method labels.
ci_lab <- c("95" = "95% CI", "99" = "99% CI",
            "bmiz +/- 0.5" = "\u00b10.5 SD", "bmiz +/- 1.0" = "\u00b11.0 SD", "bmiz +/- 1" = "\u00b11.0 SD")
method_lab <- c("max" = "Maximum", "mean" = "Mean",
                "mean+most_recent" = "Mean + Most Recent", "most_recent" = "Most Recent")
fix_s1_panel <- function(p) {
  p +
    facet_grid(timepoint ~ ci, labeller = labeller(ci = ci_lab)) +
    scale_y_discrete(labels = method_lab) +
    scale_x_continuous(breaks = function(l) pretty(c(0, l[2]), n = 3)) +
    theme(panel.spacing.y = unit(1.2, "lines"),
          panel.spacing.x = unit(0.8, "lines"))
}
uw_plot <- fix_s1_panel(uw_plot)
ow_plot <- fix_s1_panel(ow_plot)

# uw_plot   # removed stray auto-print (renders to default PDF device; crashes on Avenir under Rscript)
# save results and graphs
save(tab_combined_uw, tab_combined_ow, file = "tabs/Forecast_CI_Summary.RData")
# save plots
ggsave(uw_plot, file = "figs/uw_forecast_ci_plot.png", width = 10, height = 7, dpi = 300, device = ragg::agg_png)
ggsave(ow_plot, file = "figs/ow_forecast_ci_plot.png", width = 10, height = 7, dpi = 300, device = ragg::agg_png)

# Filter out rows with missing values
full_data_f1 <- full_data %>%
  filter(!is.na(bmiz_below_lower) & !is.na(uw))

# Function to compute metrics (includes NPV per pre-registration)
compute_f1_metrics <- function(truth, prediction) {
  cm <- confusionMatrix(
    factor(prediction, levels = c(0, 1)),
    factor(truth, levels = c(0, 1)),
    positive = "1"
  )

  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  ppv <- cm$byClass["Pos Pred Value"]
  npv <- cm$byClass["Neg Pred Value"]
  balanced_accuracy <- cm$byClass["Balanced Accuracy"]

  f1 <- if (is.na(ppv) | is.na(sens) | (ppv + sens) == 0) NA else 2 * (ppv * sens) / (ppv + sens)

  tibble(sensitivity = sens, specificity = spec, ppv = ppv, npv = npv,
         balanced_accuracy = balanced_accuracy, f1 = f1)
}

# Group and summarize
uw_summary <- full_data_f1 %>%
  group_by(forecast_point, ci, timepoint) %>%
  summarise(
    n = n(),
    n_uw = sum(uw == 1, na.rm = TRUE),
    n_below_lower = sum(bmiz_below_lower == 1, na.rm = TRUE),
    compute_f1_metrics(uw, bmiz_below_lower),
    .groups = "drop"
  )

uw_summary

ow_summary <- full_data_f1 %>%
  group_by(forecast_point, ci, timepoint) %>%
  summarise(
    n = n(),
    n_ow = sum(ow == 1, na.rm = TRUE),
    n_above_upper = sum(bmiz_above_upper == 1, na.rm = TRUE),
    compute_f1_metrics(ow, bmiz_above_upper),
    .groups = "drop"
  )
ow_summary

# Obesity metrics (pre-registered: ob = BMIz > 1.65, ~95th percentile)
ob_summary <- full_data_f1 %>%
  filter(!is.na(ob)) %>%
  group_by(forecast_point, ci, timepoint) %>%
  summarise(
    n = n(),
    n_ob = sum(ob == 1, na.rm = TRUE),
    n_above_upper = sum(bmiz_above_upper == 1, na.rm = TRUE),
    compute_f1_metrics(ob, bmiz_above_upper),
    .groups = "drop"
  )
ob_summary

# Ensure factor levels for consistent ordering
ow_summary <- ow_summary %>%
  mutate(
    ci = as.factor(ci),
    forecast_point = factor(forecast_point, levels = unique(forecast_point))
  )

uw_summary <- uw_summary %>%
  mutate(
    ci = as.factor(ci),
    forecast_point = factor(forecast_point, levels = unique(forecast_point))
  )

save(uw_summary, ow_summary, ob_summary, file = "tabs/accuracy_metrics.RData")

uw_summary <- uw_summary %>% 
  # recode forecast_point for better readability
  mutate(
    forecast_point = recode(forecast_point,
                            "max" = "Max",
                            "mean" = "Mean",
                            "most_recent" = "Recent",
                            "mean+most_recent" = "Mean/Recent",)
  )

ow_summary <- ow_summary %>% 
  # recode forecast_point for better readability
  mutate(
    forecast_point = recode(forecast_point,
                            "max" = "Max",
                            "mean" = "Mean",
                            "most_recent" = "Recent",
                            "mean+most_recent" = "Mean/Recent",)
  )

f1_ow_plot <- ggplot(ow_summary, aes(x = f1, y = forecast_point, color = ci, shape = timepoint)) +
  geom_point(position = position_dodge(width = 0.6), size = 4, alpha = 0.8) +
  labs(
    title = "F1 by Forecast Method, \n CI Level, and Timepoint: Overweight",
    x = "F1 Score",
    y = "Forecast Method",
    color = "CI Level",
    shape = "Timepoint"
  ) +
  coord_flip() +
  xlim(0, 0.45) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8)
  ) +
  embarktools::embark_theme_a +
  scale_color_manual(values = embarktools::embark_palette(6)) +
  guides(shape = guide_legend(nrow = 1, order = 1),
         color = guide_legend(nrow = 1, order = 2)) +
  theme(legend.position = "top", legend.box = "vertical", legend.box.just = "left")

f1_uw_plot <- ggplot(uw_summary, aes(x = f1, y = forecast_point, color = ci, shape = timepoint)) +
  geom_point(position = position_dodge(width = 0.6), size = 4, alpha = 0.8) +
  labs(
    title = "F1 by Forecast Method, \n CI Level, and Timepoint: Underweight",
    x = "F1 Score",
    y = "Forecast Method",
    color = "CI Level",
    shape = "Timepoint"
  ) +
  coord_flip() +
  xlim(0, 0.45) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8)
  ) +
  embarktools::embark_theme_a +
  scale_color_manual(values = embarktools::embark_palette(6)) +
  guides(shape = guide_legend(nrow = 1, order = 1),
         color = guide_legend(nrow = 1, order = 2)) +
  theme(legend.position = "top", legend.box = "vertical", legend.box.just = "left")

# save results and graphs


# f1_uw_plot   # removed stray auto-print (renders to default PDF device; crashes on Avenir under Rscript)
# f1_ow_plot   # removed stray auto-print (renders to default PDF device; crashes on Avenir under Rscript)


# Save the plots
ggsave("figs/uw_f1_plot.png", f1_uw_plot, width = 11, height = 6.5, dpi = 300, device = ragg::agg_png)
ggsave("figs/ow_f1_plot.png", f1_ow_plot, width = 11, height = 6.5, dpi = 300, device = ragg::agg_png)
