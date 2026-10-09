# =============================================================================
# 4.3 ED Behavior Prediction Graphs (Aim 2)
# Generates forest plots for continuous and binary ED prediction models
# =============================================================================

load("tabs/ED_behavior_prediction_results.RData")

library(dplyr)
library(ggplot2)
library(forcats)

pal <- c("#1A4F66", "#EF6C45", "#C2B824", "#6C7A89")

# =============================================================================
# 1. CONTINUOUS LOGISTIC MODEL FOREST PLOT (bmiz_diff_raw)
#    All forecast methods, all timepoints
# =============================================================================

df <- logistic_results %>%
  filter(predictor == "bmiz_diff_raw",
         ci %in% c("bmiz +/- 0.5", "95"),
         is.finite(conf.low), is.finite(conf.high)) %>%
  mutate(
    outcome = recode(outcome,
                     "binge_current" = "Binge eating",
                     "other_current" = "Other WCB",
                     "vomit_current" = "Vomiting",
                     "fowg_current"  = "Fear of wt gain"),
    forecast_point = recode(forecast_point,
                            "mean"             = "Mean",
                            "max"              = "Max",
                            "most_recent"      = "Recent",
                            "mean+most_recent" = "Mean + Recent",
                            "pop_median"       = "Population Median"),
    reporter = recode(reporter, parent = "Parent", youth = "Youth"),
    # ABCD sex coding: 1 = Male, 2 = Female (CDC standard)
    sex_lab = dplyr::recode(as.character(sex),
                            "1" = "Male", "2" = "Female",
                            "Male" = "Male", "Female" = "Female",
                            .default = as.character(sex)),
    sig_label = ifelse(p.value.fdr < 0.05, "*", ""),
    outcome   = factor(outcome),
    timepoint = factor(timepoint)
  ) %>%
  droplevels() %>%
  mutate(significant = ifelse(p.value.fdr < 0.05, "sig", "ns"))

cat("Rows:", nrow(df), "\n")
cat("Timepoints:", paste(levels(df$timepoint), collapse = ", "), "\n")
cat("Outcomes:", paste(levels(df$outcome), collapse = ", "), "\n")
stopifnot(nrow(df) > 0)

n_tp  <- n_distinct(df$timepoint)
n_out <- n_distinct(df$outcome)

# --- All-methods supplementary figure ---
# Significant (FDR p < .05) = opaque; non-significant = faded
final_plot <- ggplot(
  df,
  aes(x = odds_ratio,
      y = forecast_point,
      color = reporter,
      linetype = sex_lab,
      shape = sex_lab,
      alpha = significant,
      group = interaction(reporter, forecast_point, sex_lab))
) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey65") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge2(width = 0.3, padding = 0.05,
                                            preserve = "single")) +
  geom_point(position = position_dodge2(width = 0.3, padding = 0.05,
                                        preserve = "single"),
             size = 2.5, stroke = 1.0) +
  facet_grid(timepoint ~ outcome, scales = "free_x") +
  scale_color_manual(values = c(Parent = "#1A4F66", Youth = "#EF6C45"),
                     name = "Reporter") +
  scale_linetype_manual(values = c(Female = "solid", Male = "longdash"),
                        name = "Sex") +
  scale_shape_manual(values = c(Female = 16, Male = 17),
                     name = "Sex") +
  scale_alpha_manual(values = c(sig = 1.0, ns = 0.25), guide = "none") +
  labs(x = "Odds ratio (FDR-corrected)",
       y = "Forecast method",
       title = "BMIz deviations predicting ED symptoms across timepoints") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold", size = 14),
    strip.background  = element_blank()
  )

plot_w <- max(8, n_out * 3)
plot_h <- max(4, n_tp * 2)

ggsave("figs/ED_behavior_BMIz_diff_graph_allyears.png",
       final_plot, width = plot_w, height = plot_h, dpi = 300, bg = "white")

# --- Figure 3: Mean + Most Recent only (manuscript figure) ---
# Significant results (FDR p < .05) are opaque; non-significant are faded
df_main <- df %>%
  filter(forecast_point == "Mean + Recent") %>%
  mutate(significant = ifelse(p.value.fdr < 0.05, "sig", "ns"))

fig3 <- ggplot(
  df_main,
  aes(x = odds_ratio,
      y = timepoint,
      color = reporter,
      linetype = sex_lab,
      shape = sex_lab,
      alpha = significant,
      group = interaction(reporter, sex_lab))
) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey65") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.4)) +
  geom_point(position = position_dodge(width = 0.4),
             size = 2.5, stroke = 1.0) +
  facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c(Parent = "#1A4F66", Youth = "#EF6C45"),
                     name = "Reporter") +
  scale_linetype_manual(values = c(Female = "solid", Male = "longdash"),
                        name = "Sex") +
  scale_shape_manual(values = c(Female = 16, Male = 17),
                     name = "Sex") +
  scale_alpha_manual(values = c(sig = 1.0, ns = 0.25), guide = "none") +
  scale_x_continuous(n.breaks = 4) +   # R&R: fewer ticks so adjacent panels' labels don't collide
  labs(x = "Odds Ratio (95% CI)",
       y = "Timepoint") +
  theme_minimal(base_size = 15) +
  theme(
    panel.spacing.x   = unit(1.5, "lines"),
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold", size = 14),
    strip.background  = element_blank()
  )

ggsave("figs/fig3_continuous_forest_plot.tiff",
       fig3, width = 9, height = 5.5, dpi = 300, compression = "lzw", bg = "white")
# PNG copy for embedding (R&R 2026-09-26: smaller canvas -> larger effective font at 6.5 in)
ggsave("figs/fig3_continuous_forest_plot.png",
       fig3, width = 9, height = 5.5, dpi = 300, bg = "white", device = ragg::agg_png)

# =============================================================================
# 2. COMBINED BINARY MODEL FOREST PLOTS
#    Figure 4: ±0.5 SD threshold (manuscript)
#    Figure S2: ±1.0 SD threshold (supplementary)
# =============================================================================

# Helper: prepare binary data for a given CI threshold
prep_binary <- function(data, ci_filter) {
  data %>%
    filter(ci %in% ci_filter,
           forecast_point == "mean+most_recent",
           is.finite(conf.low), is.finite(conf.high)) %>%
    mutate(
      outcome = recode(outcome,
                       "binge_current" = "Binge eating",
                       "other_current" = "Other WCB",
                       "vomit_current" = "Vomiting",
                       "fowg_current"  = "Fear of wt gain"),
      term = recode(term,
                    "bmiz_below_lower" = "Below Forecast",
                    "bmiz_above_upper" = "Above Forecast",
                    "ow" = "Overweight (BMIz > 1.04)",
                    "uw" = "Underweight (BMIz < -1.65)"),
      reporter = recode(reporter, parent = "Parent", youth = "Youth"),
      # ABCD sex coding: 1 = Male, 2 = Female
      sex_lab = dplyr::recode(as.character(sex),
                              "1" = "Male", "2" = "Female",
                              "Male" = "Male", "Female" = "Female",
                              .default = as.character(sex)),
      significant = ifelse(p.value.fdr < 0.05, "sig", "ns"),
      outcome   = factor(outcome),
      timepoint = factor(timepoint)
    ) %>%
    droplevels()
}

# Helper: build binary forest plot
# - Log-scaled x-axis for readable ORs
# - Faceted by timepoint (rows) × outcome (cols)
# - Color = sex, shape = reporter, alpha = significance
# - Sex dodged within each row for clean separation
make_binary_plot <- function(data, title_suffix) {
  # Clip extreme CIs to keep axis readable (cap at 0.05–50 on OR scale)
  data <- data %>%
    mutate(conf.low  = pmax(conf.low, 0.05),
           conf.high = pmin(conf.high, 50))

  ggplot(data,
         aes(x = odds_ratio, y = term,
             color = sex_lab, shape = reporter,
             alpha = significant,
             group = interaction(reporter, sex_lab))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey65") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2,
                   position = position_dodge(width = 0.55)) +
    geom_point(position = position_dodge(width = 0.55),
               size = 2.5, stroke = 1.0) +
    facet_grid(timepoint ~ outcome) +
    scale_x_log10(breaks = c(0.1, 1, 10),
                  labels = c("0.1", "1", "10"),
                  minor_breaks = c(0.3, 3, 30)) +
    coord_cartesian(xlim = c(0.05, 50)) +
    scale_color_manual(values = c(Female = "#EF6C45", Male = "#1A4F66"),
                       name = "Sex") +
    scale_shape_manual(values = c(Parent = 17, Youth = 16),
                       name = "Reporter") +
    scale_alpha_manual(values = c(sig = 1.0, ns = 0.25), guide = "none") +
    labs(x = "Odds Ratio (log scale, FDR-corrected)",
         y = "Predictor") +
    theme_minimal(base_size = 15) +
    theme(
      panel.spacing.x   = unit(1.2, "lines"),
      legend.position   = "bottom",
      panel.grid.minor  = element_blank(),
      strip.text        = element_text(face = "bold", size = 14),
      strip.background  = element_blank()
    )
}

# Figure 4: ±0.5 SD (manuscript)
df_binary_05 <- prep_binary(binary_combined_results, c("bmiz +/- 0.5"))
binary_plot_05 <- make_binary_plot(df_binary_05, "\u00b10.5 SD")

ggsave("figs/fig4_binary_forest_plot.tiff",
       binary_plot_05, width = 13, height = 9, dpi = 300, bg = "white")

# Figure S2: ±1.0 SD (supplementary)
df_binary_10 <- prep_binary(binary_combined_results, c("bmiz +/- 1", "bmiz +/- 1.0"))
binary_plot_10 <- make_binary_plot(df_binary_10, "\u00b11.0 SD")

ggsave("figs/fig_s2_binary_forest_1sd.png",
       binary_plot_10, width = 13, height = 9, dpi = 300, bg = "white")

# =============================================================================
# 3. FIGURE 4 (R&R): ±0.5 SD (Panel A, pre-registered primary) and
#    ±1.0 SD (Panel B) given equal space in the main text
#    (Reviewer response / Option A, 2026-09-26). Replaces the separate
#    ±1.0 SD supplementary forest plot in the manuscript.
# =============================================================================

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  fig4_AB <- (binary_plot_05 +
                labs(title = "A. ±0.5 SD threshold (pre-registered primary)")) /
             (binary_plot_10 +
                labs(title = "B. ±1.0 SD threshold")) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 16))

  ggsave("figs/fig4_binary_forest_plot_AB.tiff",
         fig4_AB, width = 10, height = 13, dpi = 300,
         compression = "lzw", bg = "white")
  ggsave("figs/fig4_binary_forest_plot_AB.png",
         fig4_AB, width = 10, height = 13, dpi = 300, bg = "white")
} else {
  warning("Package 'patchwork' not installed: install.packages('patchwork') to build combined Figure 4 (A/B). ",
          "Separate panels were saved as figs/fig4_binary_forest_plot.tiff (A) and figs/fig_s2_binary_forest_1sd.png (B).")
}
