load("tabs/ED_behavior_prediction_results_EDRS.RData")

library(dplyr)
library(ggplot2)
library(forcats)

pal <- c("#1A4F66", "#EF6C45", "#C2B824", "#6C7A89") # navy, coral, chartreuse, gray

dxf <- logistic_results %>%
  filter(timepoint == "Year 2",
         predictor == "bmiz_diff_raw",
         ci %in% c("bmiz +/- 1", "95"),
         forecast_point != "mean",
         forecast_point != "max",
         is.finite(conf.low), is.finite(conf.high), 
         outcome == "binge_current") %>%  # ONLY BINGE EATING
  mutate(
    # human-friendly labels
    outcome = recode(outcome,
                     "binge_current" = "Binge eating"),
    forecast_point = recode(forecast_point,
                            "mean+most_recent" = "eBMIz deviation",
                            "pop_median"       = "BMIz"),
    reporter = recode(reporter, parent = "Parent", youth = "Youth"),
    forecast_point = factor(forecast_point,
                            levels = c("BMIz","eBMIz deviation")),
    sex_lab = dplyr::recode(as.character(sex),
                            "1" = "Girls", "2" = "Boys",
                            "Female" = "Girls", "Male" = "Boys",
                            .default = as.character(sex))
  )

# Single panel plot
binge_plot <- ggplot(
  dxf,
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
                 position = position_dodge2(width = 0.3, padding = 0.05, preserve = "single")) +
  geom_point(position = position_dodge2(width = 0.3, padding = 0.05, preserve = "single"),
             size = 3, stroke = 1.1) +
  scale_color_manual(values = c(Parent = "#1A4F66", Youth = "#EF6C45"),
                     name = "Reporter") +
  scale_linetype_manual(values = c(Girls = "solid", Boys = "longdash"),
                        name = "Sex") +
  scale_shape_manual(values = c(Girls = 16, Boys = 17),
                     name = "Sex") +
  labs(x = "Odds Ratio",
       y = "Logistic Regression Predictor",
       title = "eBMIz Elevation Predicts Binge Eating \n at Ages 11-12 (ABCD)") +
  embarktools::embark_theme_a +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background  = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    plot.title = element_text(family = "Avenir", size = 16, colour = "#1A4F66", hjust = 0.5)
  )

binge_plot

# Save with transparent background
ggsave("figs/binge_eating_eBMIz_elevation.png",
       binge_plot, width = 8, height = 5, dpi = 300, bg = "transparent")