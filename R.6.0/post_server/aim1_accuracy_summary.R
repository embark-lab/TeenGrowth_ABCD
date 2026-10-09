#!/usr/bin/env Rscript
# =============================================================================
# aim1_accuracy_summary.R
#
# Reproduces the manuscript's Aim 1 pooled accuracy numbers (the median BMI
# error per method: currently 1.04 / 1.09 / 1.18 / 1.26 / 2.19 in the paper)
# using the EXACT recipe from Results_Draft.Rmd (chunk `forecast-comparison-text`),
# and saves the results as AGGREGATE tables to tabs/ so they can be pulled into
# the manuscript. Also regenerates the paper's Figure 2 to figs/.
#
# RUN FROM THE PROJECT ROOT (the dir that contains data/6.0/), e.g.:
#   cd ~/ABCD
#   Rscript aim1_accuracy_summary.R
#
# Outputs (aggregate only — no participant rows):
#   tabs/aim1_accuracy_pooled.csv        <- the 5 headline numbers
#   tabs/aim1_accuracy_by_timepoint.csv  <- Figure 2A values
#   tabs/aim1_accuracy_year56.csv        <- the "Year 5-6" sentence values
#   tabs/aim1_accuracy_summary.RData
#   figs/fig2_forecast_comparison.png    <- paper Figure 2 (non-sex version)
# =============================================================================

suppressMessages({ library(dplyr); library(ggplot2); library(tidyr); library(gridExtra) })

# ---- 1. Load the fresh person-level forecast data (full_data) ---------------
# 3.1 saves full_data here. (The Rmd currently looks in tabs/Forecast_Results.RData,
# which now holds only the summary — see the note in the accompanying message.)
src <- c("data/6.0/Forecast_Results.RData", "tabs/Forecast_Results.RData")
src <- src[file.exists(src)]
if (length(src) == 0) stop("Can't find Forecast_Results.RData (need full_data). Run from the project root.")
load(src[1])                      # loads full_data
if (!exists("full_data")) stop(src[1], " did not contain 'full_data'. Use the data/6.0 copy from 3.1.")
message("Loaded full_data from: ", src[1],
        "   (file mtime: ", format(file.info(src[1])$mtime), ")")

forecast_labels <- c("mean" = "Mean", "max" = "Maximum",
                     "most_recent" = "Most Recent",
                     "mean+most_recent" = "Mean + Most Recent",
                     "pop_median" = "Population Median")
relabel <- function(x, map) { out <- map[as.character(x)]; ifelse(is.na(out), as.character(x), out) }

# ---- 2. POOLED per-method accuracy — EXACT recipe from the Rmd --------------
comp_stats <- full_data %>%
  filter(grepl("0\\.5", ci) | grepl("pop_median", forecast_point)) %>%
  filter(!is.na(eBMI_diff)) %>%
  mutate(forecast_label = relabel(forecast_point, forecast_labels)) %>%
  group_by(forecast_label) %>%
  summarise(n = n(),
            mean_err   = mean(eBMI_diff,   na.rm = TRUE),
            median_err = median(eBMI_diff, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(median_err)

# ---- 3. Per-timepoint medians (Figure 2A) + Year 5-6 means ------------------
tp_comp <- full_data %>%
  filter(grepl("0\\.5", ci) | grepl("pop_median", forecast_point)) %>%
  filter(!is.na(eBMI_diff)) %>%
  mutate(forecast_label = relabel(forecast_point, forecast_labels)) %>%
  group_by(forecast_label, timepoint) %>%
  summarise(median_err = median(eBMI_diff, na.rm = TRUE), n = n(), .groups = "drop")

late <- tp_comp %>%
  filter(timepoint %in% c("Year 5", "Year 6")) %>%
  group_by(forecast_label) %>%
  summarise(year56_mean_of_medians = mean(median_err), .groups = "drop")

cat("\n================ POOLED median BMI error (the 5 headline numbers) ================\n")
print(as.data.frame(comp_stats), row.names = FALSE)
cat("\n================ Median BMI error by timepoint (Figure 2A) =======================\n")
print(as.data.frame(tp_comp %>% arrange(forecast_label, timepoint)), row.names = FALSE)
cat("\n================ Year 5-6 (mean of the two yearly medians) =======================\n")
print(as.data.frame(late), row.names = FALSE)

# ---- 4. Save AGGREGATE tables (safe to share; no participant rows) ----------
if (!dir.exists("tabs")) dir.create("tabs", recursive = TRUE, showWarnings = FALSE)
write.csv(comp_stats, "tabs/aim1_accuracy_pooled.csv",       row.names = FALSE)
write.csv(tp_comp,    "tabs/aim1_accuracy_by_timepoint.csv", row.names = FALSE)
write.csv(late,       "tabs/aim1_accuracy_year56.csv",       row.names = FALSE)
save(comp_stats, tp_comp, late, file = "tabs/aim1_accuracy_summary.RData")
cat("\nSaved: tabs/aim1_accuracy_pooled.csv, aim1_accuracy_by_timepoint.csv, aim1_accuracy_year56.csv, aim1_accuracy_summary.RData\n")

# ---- 5. Regenerate paper Figure 2 (non-sex version, matches the Rmd) --------
ijed_colors <- c("Mean" = "#EF6C45", "Most Recent" = "#A481C7",
                 "Mean + Most Recent" = "#C2B824", "Maximum" = "#1A4F66",
                 "Population Median" = "#E1AD01")
lvl <- c("Mean", "Most Recent", "Mean + Most Recent", "Maximum", "Population Median")

method_comp <- full_data %>%
  filter(grepl("0\\.5", ci) | grepl("pop_median", forecast_point)) %>%
  filter(!is.na(eBMI_diff), !is.na(timepoint)) %>%
  mutate(forecast_label = factor(relabel(forecast_point, forecast_labels), levels = lvl))
if ("training_window" %in% names(method_comp)) {
  method_comp <- method_comp %>% group_by(id, timepoint, forecast_label) %>%
    slice_max(training_window, n = 1, with_ties = FALSE) %>% ungroup()
}

pa <- method_comp %>% group_by(forecast_label, timepoint) %>%
  summarise(median_bmi_error = median(eBMI_diff, na.rm = TRUE), .groups = "drop")
p_a <- ggplot(pa, aes(timepoint, median_bmi_error, color = forecast_label, group = forecast_label)) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  scale_color_manual(values = ijed_colors, name = "Forecast Method") +
  labs(x = "Timepoint", y = "Median |BMI Error|", subtitle = "A: Prediction Accuracy by Method") +
  # Comment 7 (reviewer): full, untruncated y-axis anchored at 0 (values span ~0.8-2.3).
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.subtitle = element_text(face = "bold"))

p_b <- method_comp %>% mutate(d = bmiz - eBMIz) %>% filter(!is.na(d)) %>%
  ggplot(aes(forecast_label, d, fill = forecast_label)) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  geom_violin(alpha = 0.6, scale = "width", width = 0.7) +
  geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.8) +
  scale_fill_manual(values = ijed_colors, guide = "none") +
  labs(x = "", y = "BMIz Deviation (observed - forecasted)", subtitle = "B: Distribution of BMIz Deviation") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.minor = element_blank(),
        plot.subtitle = element_text(face = "bold")) +
  coord_cartesian(ylim = c(-3, 3))

if (!dir.exists("figs")) dir.create("figs", recursive = TRUE, showWarnings = FALSE)
png("figs/fig2_forecast_comparison.png", width = 12, height = 7, units = "in", res = 300)
gridExtra::grid.arrange(p_a, p_b, ncol = 2, widths = c(1.2, 1))
invisible(dev.off())
cat("Saved: figs/fig2_forecast_comparison.png\n\n")
