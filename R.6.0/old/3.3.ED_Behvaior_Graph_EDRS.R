load("tabs/ED_behavior_prediction_results.RData")


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
         outcome != "restrict_current") %>%
  mutate(
    # human-friendly labels
    outcome = recode(outcome,
                     "binge_current"      = "Binge eating",
                     "exercise_current"   = "Driven exercise",
                     "fowg_current_half"  = "Fear of wt gain"    ),
    forecast_point = recode(forecast_point,
                            "mean+most_recent" = "eBMIz deviation",
                            "pop_median"       = "BMIz"    ),
    reporter = recode(reporter, parent = "Parent", youth = "Youth"),
    forecast_point = factor(forecast_point,
                            levels = c("BMIz","eBMIz deviation")),
    sex_lab = dplyr::recode(as.character(sex),
                            "1" = "Girls", "2" = "Boys",
                            "Female" = "Girls", "Male" = "Boys",
                            .default = as.character(sex))
  )

# install.packages("patchwork")  # if needed
library(patchwork)


base_geom <- list(
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey65"),
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, 
                 position = position_dodge2(width = 0.3, padding = 0.05, preserve = "single")),
    geom_point(position = position_dodge2(width = 0.3, padding = 0.05, preserve = "single"),
             size = 2.8, stroke = 1.1),

scales_styling <- list(
  scale_color_manual(values = c(Parent = "#1A4F66", Youth = "#EF6C45"),
                     name = "Reporter"),
  scale_linetype_manual(values = c(Girls = "solid", Boys = "longdash"),
                        name = "Sex"),
  scale_shape_manual(values = c(Girls = 16, Boys = 17),  # 1 = open, 16 = filled
                     name = "Sex"))
)

# make the common theme transparent
theme_common <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.background = element_rect(fill = NA, colour = NA),   # transparent panels
    plot.background  = element_rect(fill = NA, colour = NA)     # transparent plot bg
  )

make_panel <- function(outc, show_y_axis = FALSE) {
  ggplot(
    df %>% dplyr::filter(outcome == outc),
    aes(x = odds_ratio,
        y = forecast_point,
        color = reporter,
        linetype = sex_lab,
        shape = sex_lab,
        group = interaction(reporter, forecast_point, sex_lab))
  ) +
    base_geom +
    scales_styling +
    labs(x = "Odds ratio",
         y = if (show_y_axis) "Predictor" else NULL,
         title = outc) +
    theme_common +
    embarktools::embark_theme_a +
    theme(
      legend.position = "bottom",
      # ensure embark theme doesn't add fills
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background  = element_rect(fill = NA, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key        = element_rect(fill = NA, colour = NA),
      strip.background  = element_blank(),
      # hide y-axis labels/ticks for non-left panels
      axis.text.y  = if (show_y_axis) element_text() else element_blank(),
      axis.ticks.y = if (show_y_axis) element_line() else element_blank(),
      axis.title.y = if (show_y_axis) element_text() else element_blank()
    )
}

# Make sure you’re working with the correct dataset
df <- dxf %>%
  mutate(outcome = forcats::fct_inorder(outcome))  

# List of outcomes you want panels for
outcomes <- unique(df$outcome)

plots <- purrr::imap(outcomes, ~ make_panel(.x, show_y_axis = .y == 1))

final_plot <- wrap_plots(plots, nrow = 1, guides = "collect") +
  plot_annotation(title = "Binge Eating at Y2 (11–12)") &
  theme(
    legend.position = "bottom",
    plot.background  = element_rect(fill = NA, colour = NA),   # transparent overall bg
    panel.background = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key        = element_rect(fill = NA, colour = NA),
    plot.title = element_text(family = "Avenir", size = 18, colour = "#1A4F66", hjust = 0.5)
  )

final_plot
# Save with transparent background
ggsave("figs/ED_behavior_BMIz_diff_graph_binge.png",
       final_plot, width = 10, height = 5, dpi = 300, bg = "transparent")
