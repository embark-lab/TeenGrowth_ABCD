#!/usr/bin/env Rscript
# ============================================================
# Figure 2: Forecast Method Comparison — IJED 2025-Matched Colors
#
# Run this script from the project root (where data/6.0/ lives).
# It regenerates Figure 2 with:
#   1. Colors matched to the IJED 2025 simulation paper (Schaumberg, 2025)
#   2. Optional sex-stratified Panel A (set SHOW_SEX <- TRUE)
#
# Output: figs/fig2_forecast_comparison_ijed.png
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(gridExtra)

# ---- CONFIGURATION ----
SHOW_SEX <- FALSE   # Set TRUE to see boys vs girls in Panel A
OUTPUT_FILE <- "figs/fig2_forecast_comparison_ijed.png"

# ---- IJED 2025-MATCHED COLORS ----
# These match the color assignments in Figure 2 of:
# Schaumberg (2025). TeenGrowth: Individualized Estimations of
# Weight-Related Risk and Recovery Metrics. IJED.
ijed_colors <- c(
  "Mean"              = "#EF6C45",   # Coral
  "Most Recent"       = "#A481C7",   # Lavender
  "Mean + Most Recent"= "#C2B824",   # Chartreuse
  "Maximum"           = "#1A4F66",   # Navy
  "Population Median" = "#E1AD01"    # Sunrise
)

# ---- LOAD DATA ----
load("data/6.0/Forecast_Results.RData")  # loads full_data

forecast_labels <- c(
  "mean" = "Mean",
  "most_recent" = "Most Recent",
  "mean+most_recent" = "Mean + Most Recent",
  "max" = "Maximum",
  "pop_median" = "Population Median"
)

# ---- PREPARE DATA ----
method_comp <- full_data %>%
  filter(grepl("0\\.5", ci) | grepl("pop_median", forecast_point)) %>%
  filter(!is.na(eBMI_diff), !is.na(timepoint)) %>%
  mutate(
    forecast_label = recode(forecast_point, !!!forecast_labels),
    forecast_label = factor(forecast_label,
                            levels = c("Mean", "Most Recent", "Mean + Most Recent",
                                       "Maximum", "Population Median"))
  )

# Deduplicate if training_window column exists
if ("training_window" %in% names(method_comp)) {
  method_comp <- method_comp %>%
    group_by(id, timepoint, forecast_label) %>%
    slice_max(training_window, n = 1, with_ties = FALSE) %>%
    ungroup()
} else {
  method_comp <- method_comp %>%
    group_by(id, timepoint, forecast_label) %>%
    summarise(
      bmiz = first(bmiz),
      bmi  = first(bmi),
      sex  = first(sex),
      eBMIz = mean(eBMIz, na.rm = TRUE),
      eBMI  = mean(eBMI, na.rm = TRUE),
      across(any_of(c("ow", "uw", "ob", "bmiz_below_lower", "bmiz_above_upper")), first),
      .groups = "drop"
    ) %>%
    mutate(
      eBMI_diff = abs(eBMI - bmi),
      eBMIz_diff = abs(eBMIz - bmiz)
    )
}

# ---- PANEL A: Median |BMI error| by method and timepoint ----
if (SHOW_SEX) {
  # Sex-stratified version
  method_comp <- method_comp %>%
    mutate(sex_label = ifelse(sex == 1, "Male", "Female"))

  panel_a_data <- method_comp %>%
    group_by(forecast_label, timepoint, sex_label) %>%
    summarise(
      median_bmi_error = median(eBMI_diff, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  p_a <- ggplot(panel_a_data,
                aes(x = timepoint, y = median_bmi_error,
                    color = forecast_label, group = interaction(forecast_label, sex_label),
                    linetype = sex_label)) +
    geom_line(linewidth = 1) +
    geom_point(aes(shape = sex_label), size = 3) +
    scale_color_manual(values = ijed_colors, name = "Forecast Method") +
    scale_linetype_manual(values = c("Female" = "solid", "Male" = "dashed"),
                          name = "Sex") +
    scale_shape_manual(values = c("Female" = 16, "Male" = 1),
                       name = "Sex") +
    labs(x = "Timepoint", y = "Median |BMI Error|",
         subtitle = "A: Prediction Accuracy by Method and Sex") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(face = "bold"),
      text = element_text(family = "sans")
    ) +
    guides(color = guide_legend(nrow = 1, order = 1, title.position = "top", title.hjust = 0.5),
           linetype = guide_legend(order = 2),
           shape = guide_legend(order = 2))

} else {
  # Non-sex-stratified version (matches IJED layout)
  panel_a_data <- method_comp %>%
    group_by(forecast_label, timepoint) %>%
    summarise(
      median_bmi_error = median(eBMI_diff, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  p_a <- ggplot(panel_a_data,
                aes(x = timepoint, y = median_bmi_error,
                    color = forecast_label, group = forecast_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_color_manual(values = ijed_colors, name = "Forecast Method") +
    labs(x = "Timepoint", y = "Median |BMI Error|",
         subtitle = "A: Prediction Accuracy by Method") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(face = "bold"),
      text = element_text(family = "sans"),
      plot.margin = margin(5.5, 5.5, 5.5, 14, "pt")
    ) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))
}

# ---- PANEL B: Violin/boxplot of signed BMIz deviation ----
panel_b_data <- method_comp %>%
  mutate(bmiz_diff_signed = bmiz - eBMIz) %>%
  filter(!is.na(bmiz_diff_signed))

p_b <- ggplot(panel_b_data,
              aes(x = forecast_label, y = bmiz_diff_signed, fill = forecast_label)) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  geom_violin(alpha = 0.6, scale = "width", width = 0.7) +
  geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.8) +
  scale_fill_manual(values = ijed_colors, guide = "none") +
  labs(x = "", y = "BMIz Deviation (observed \u2212 forecasted)",
       subtitle = "B: Distribution of BMIz Deviation") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(face = "bold"),
    text = element_text(family = "sans")
  ) +
  coord_cartesian(ylim = c(-3, 3))

# ---- COMBINE AND SAVE ----
width <- ifelse(SHOW_SEX, 14, 12)
height <- 7

png(OUTPUT_FILE, width = width, height = height, units = "in", res = 300)
gridExtra::grid.arrange(p_a, p_b, ncol = 2, widths = c(1.2, 1))
dev.off()

cat(sprintf("Saved: %s (%d x %d in @ 300 dpi)\n", OUTPUT_FILE, width, height))
cat(sprintf("Sex stratification: %s\n", ifelse(SHOW_SEX, "ON", "OFF")))

# ---- ALSO PRINT SEX DIFFERENCES SUMMARY ----
if (SHOW_SEX) {
  cat("\n--- Sex differences in median BMI error ---\n")
  sex_summary <- method_comp %>%
    mutate(sex_label = ifelse(sex == 1, "Male", "Female")) %>%
    group_by(forecast_label, sex_label) %>%
    summarise(
      median_bmi_error = median(eBMI_diff, na.rm = TRUE),
      mean_bmi_error = mean(eBMI_diff, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = sex_label, values_from = c(median_bmi_error, mean_bmi_error, n))

  print(sex_summary)

  cat("\n--- Sex differences by timepoint (Mean + Most Recent method) ---\n")
  mmr_sex <- method_comp %>%
    filter(forecast_label == "Mean + Most Recent") %>%
    mutate(sex_label = ifelse(sex == 1, "Male", "Female")) %>%
    group_by(timepoint, sex_label) %>%
    summarise(
      median_bmi_error = median(eBMI_diff, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = sex_label, values_from = c(median_bmi_error, n))

  print(mmr_sex)
}
