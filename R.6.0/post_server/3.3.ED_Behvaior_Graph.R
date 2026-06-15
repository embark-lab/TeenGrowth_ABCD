load("tabs/6.0/ED_behavior_prediction_results.RData")

library(dplyr)
library(ggplot2)
library(forcats)

pal <- c("#1A4F66", "#EF6C45", "#C2B824", "#6C7A89")

# ── 1. Filter & recode ──────────────────────────────────────────────
df <- logistic_results %>%
  filter(predictor == "bmiz_diff_raw",
         ci %in% c("bmiz +/- 1", "95"),
         forecast_point != "mean",
         forecast_point != "max",
         is.finite(conf.low), is.finite(conf.high),
         outcome != "restriction_current") %>%
  mutate(
    outcome = recode(outcome,
                     "binge_current" = "Binge eating",
                     "other_current" = "Other WCB",
                     "vomit_current" = "Vomiting",
                     "fowg_current"  = "Fear of wt gain"),
    forecast_point = recode(forecast_point,
                            "mean+most_recent" = "Mean + Recent",
                            "pop_median"       = "Population Median"),
    reporter = recode(reporter, parent = "Parent", youth = "Youth"),
    forecast_point = factor(forecast_point,
                            levels = c("Population Median", "Mean + Recent")),
    sex_lab = dplyr::recode(as.character(sex),
                            "1" = "Girls", "2" = "Boys",
                            "Female" = "Girls", "Male" = "Boys",
                            .default = as.character(sex)),
    outcome   = factor(outcome),
    timepoint = factor(timepoint)
  ) %>%
  droplevels()

cat("Rows:", nrow(df), "\n")
cat("Timepoints:", paste(levels(df$timepoint), collapse = ", "), "\n")
cat("Outcomes:", paste(levels(df$outcome), collapse = ", "), "\n")
stopifnot(nrow(df) > 0)

# ── 2. Single faceted plot ──────────────────────────────────────────
n_tp  <- n_distinct(df$timepoint)
n_out <- n_distinct(df$outcome)

final_plot <- ggplot(
  df,
  aes(x = odds_ratio,
      y = forecast_point,
      color = reporter,
      linetype = sex_lab,
      shape = sex_lab,
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
  scale_linetype_manual(values = c(Girls = "solid", Boys = "longdash"),
                        name = "Sex") +
  scale_shape_manual(values = c(Girls = 16, Boys = 17),
                     name = "Sex") +
  labs(x = "Odds ratio",
       y = "Forecast method",
       title = "BMIz deviations predicting ED symptoms across timepoints") +
  theme_minimal(base_size = 13) +
  embarktools::embark_theme_a +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold", size = 12),
    panel.background  = element_rect(fill = NA, colour = NA),
    plot.background   = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key        = element_rect(fill = NA, colour = NA),
    strip.background  = element_blank(),
    plot.title = element_text(family = "Avenir", size = 18,
                              colour = "#1A4F66", hjust = 0.5)
  )

final_plot

# Scale dimensions to content
plot_w <- max(8, n_out * 3)
plot_h <- max(4, n_tp * 2)

ggsave("figs/6.0/ED_behavior_BMIz_diff_graph_allyears.png",
       final_plot, width = plot_w, height = plot_h, dpi = 300, bg = "transparent")