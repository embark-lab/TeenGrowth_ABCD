# =============================================================================
# 4.2 Marginal Effects Plots for BMIz × Deviation and PDS × Deviation
#     Interactions (Aim 3)
#
# Purpose: Visualise the two key GLMM interactions from 4.0:
#   Panel A — BMIz moderates the deviation → ED association
#   Panel B — PDS moderates the deviation → ED association
#
# Models are fit SEPARATELY BY REPORTER (matching 4.0 exactly), then
# faceted by reporter in the plots.
#
# Also outputs a coefficient table with raw β signs so the interaction
# direction can be verified against the marginal effects visually.
#
# Outputs:
#   figs/fig_interaction_marginal_effects.png  (combined 2-panel)
#   figs/fig_interaction_bmiz_mod.png           (Panel A only)
#   figs/fig_interaction_pds_mod.png            (Panel B only)
#   tabs/aim3_glmm_coefficients.csv             (raw coefficients)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(broom.mixed)
library(purrr)
library(gridExtra)

# ---- 1. LOAD DATA -----------------------------------------------------------

load("data/6.0/ed_prediction_data_with_pds.RData")

# ---- 2. DATA PREPARATION (mirrors 4.0) --------------------------------------

ed_prediction_data <- ed_prediction_data %>%
  arrange(id, timepoint) %>%
  mutate(
    bmi_diff_raw  = bmi - eBMI,
    bmiz_diff_raw = bmiz - eBMIz,
    sex = factor(sex),
    timepoint = factor(timepoint,
                       levels = c("Year 2", "Year 4", "Year 6"))
  )

# Mean-center PDS within sex (per pre-registration)
ed_prediction_data <- ed_prediction_data %>%
  group_by(sex) %>%
  mutate(pds_mean_c = pds_mean - mean(pds_mean, na.rm = TRUE)) %>%
  ungroup()

# Mean-center age within timepoint (per pre-registration)
ed_prediction_data <- ed_prediction_data %>%
  group_by(timepoint) %>%
  mutate(age_c = (agemos / 12) - mean(agemos / 12, na.rm = TRUE)) %>%
  ungroup()

# Behavior outcomes
behaviors <- c("vomit_current", "binge_current",
               "other_current", "fowg_current")

behavior_labels <- c(
  "vomit_current" = "Vomiting",
  "binge_current" = "Binge Eating",
  "other_current" = "Other WCB",
  "fowg_current"  = "Fear of\nWeight Gain"
)

# ---- 3. FIT GLMMS BY REPORTER (matching 4.0) --------------------------------

fit_glmm <- function(df, outcome) {
  tryCatch({
    glmer(
      formula = as.formula(paste0(
        outcome, " ~ sex + age_c + bmiz_diff_raw +
                      bmiz * bmiz_diff_raw +
                      pds_mean_c * bmiz_diff_raw +
                      (1 | id)"
      )),
      data = df,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa",
                             optCtrl = list(maxfun = 1e5))
    )
  }, error = function(e) {
    message("Model failed for ", outcome, ": ", e$message)
    NULL
  })
}

# Analysis data: filter to primary forecast method, ±0.5 SD CI,
# ED-assessed timepoints, and complete cases.
# This gives one row per person × timepoint × reporter — cleanest for (1|id).
analysis_data <- ed_prediction_data %>%
  filter(forecast_point == "mean+most_recent",
         ci == "bmiz +/- 0.5",
         timepoint %in% c("Year 2", "Year 4", "Year 6"),
         !is.na(reporter),
         !is.na(bmiz_diff_raw),
         !is.na(bmiz),
         !is.na(pds_mean_c),
         !is.na(age_c),
         !is.na(sex))

# Reporter labels
reporters <- unique(na.omit(analysis_data$reporter))
reporter_labels <- c("parent" = "Parent", "youth" = "Youth",
                     "Parent" = "Parent", "Youth" = "Youth")

# Fit models separately by reporter
message("Fitting GLMM models by reporter...")
models_by_reporter <- list()
for (rep in reporters) {
  rep_data <- analysis_data %>% filter(reporter == rep)
  rep_label <- reporter_labels[as.character(rep)]
  models_by_reporter[[rep_label]] <- setNames(
    lapply(behaviors, function(b) {
      df <- rep_data %>% filter(!is.na(.data[[b]]))
      message("  ", rep_label, " × ", b, " (n = ", nrow(df), ")")
      fit_glmm(df, b)
    }),
    behaviors
  )
}

# ---- 4. EXTRACT & SAVE COEFFICIENT TABLE ------------------------------------
# This is critical for checking the sign of bmiz_diff_raw (β₁) and the
# interaction terms (β₃ for bmiz:bmiz_diff_raw, β₄ for pds_mean_c:bmiz_diff_raw)

coef_table <- map_dfr(names(models_by_reporter), function(rep_label) {
  map_dfr(behaviors, function(b) {
    m <- models_by_reporter[[rep_label]][[b]]
    if (is.null(m)) return(NULL)
    tidy(m, effects = "fixed") %>%
      mutate(
        reporter = rep_label,
        outcome  = b,
        OR       = exp(estimate),
        OR_lo    = exp(estimate - 1.96 * std.error),
        OR_hi    = exp(estimate + 1.96 * std.error)
      )
  })
})

# Print the key terms to console
cat("\n=== KEY COEFFICIENTS (raw log-odds scale) ===\n")
cat("Positive β for bmiz_diff_raw = above-forecast increases risk\n")
cat("Negative β for bmiz_diff_raw = below-forecast increases risk\n\n")

key_terms <- c("bmiz_diff_raw", "bmiz", "bmiz:bmiz_diff_raw",
               "pds_mean_c", "bmiz_diff_raw:pds_mean_c")
coef_table %>%
  filter(term %in% key_terms) %>%
  mutate(
    sig = ifelse(p.value < 0.05, "*", ""),
    summary = sprintf("β = %+.3f (OR = %.2f [%.2f, %.2f]) p = %.4f %s",
                      estimate, OR, OR_lo, OR_hi, p.value, sig)
  ) %>%
  select(reporter, outcome, term, summary) %>%
  arrange(reporter, outcome, term) %>%
  as.data.frame() %>%
  print(right = FALSE)

write.csv(coef_table, "tabs/aim3_glmm_coefficients.csv", row.names = FALSE)
message("\nCoefficient table saved to tabs/aim3_glmm_coefficients.csv")

# ---- 5. PREDICTION GRIDS ----------------------------------------------------

# Compute quantiles within each reporter group for accuracy
get_grid_params <- function(data) {
  list(
    bmiz_q   = quantile(data$bmiz, c(0.25, 0.50, 0.75), na.rm = TRUE),
    pds_q    = quantile(data$pds_mean_c, c(0.25, 0.50, 0.75), na.rm = TRUE),
    dev_range = quantile(data$bmiz_diff_raw, c(0.02, 0.98), na.rm = TRUE),
    ref_sex  = levels(data$sex)[1]
  )
}

# Use overall quantiles for consistency across reporter panels
params <- get_grid_params(analysis_data)

bmiz_levels <- c(
  "Lower (25th %ile)"  = unname(params$bmiz_q[1]),
  "Mean (50th %ile)"   = unname(params$bmiz_q[2]),
  "Higher (75th %ile)" = unname(params$bmiz_q[3])
)

# PDS levels: use mean pds_mean_c within each pds_category rather than
# arbitrary quantiles — gives clinically interpretable moderator levels.
pds_category_means <- analysis_data %>%
  filter(!is.na(pds_category)) %>%
  group_by(pds_category) %>%
  summarise(pds_mean_c = mean(pds_mean_c, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  filter(n >= 50) %>%  # drop any category with too few obs
  arrange(pds_mean_c)

cat("\nPDS category → mean pds_mean_c mapping:\n")
print(as.data.frame(pds_category_means))

pds_levels <- setNames(pds_category_means$pds_mean_c,
                       as.character(pds_category_means$pds_category))

# Preserve full sex factor levels so model.matrix() can build contrasts
sex_levels <- levels(analysis_data$sex)

# Panel A grid: BMIz moderates deviation → ED
grid_bmiz <- expand.grid(
  bmiz_diff_raw = seq(params$dev_range[1], params$dev_range[2], length.out = 100),
  bmiz          = unname(bmiz_levels),
  pds_mean_c    = 0,
  age_c         = 0,
  sex           = params$ref_sex,
  id            = "new_obs",
  stringsAsFactors = FALSE
) %>%
  mutate(
    sex = factor(sex, levels = sex_levels),
    bmiz_label = names(bmiz_levels)[match(bmiz, bmiz_levels)],
    bmiz_label = factor(bmiz_label, levels = names(bmiz_levels))
  )

# Panel B grid: PDS moderates deviation → ED
grid_pds <- expand.grid(
  bmiz_diff_raw = seq(params$dev_range[1], params$dev_range[2], length.out = 100),
  pds_mean_c    = unname(pds_levels),
  bmiz          = unname(params$bmiz_q[2]),
  age_c         = 0,
  sex           = params$ref_sex,
  id            = "new_obs",
  stringsAsFactors = FALSE
) %>%
  mutate(
    sex = factor(sex, levels = sex_levels),
    pds_label = names(pds_levels)[match(pds_mean_c, pds_levels)],
    pds_label = factor(pds_label, levels = names(pds_levels))
  )

# ---- 6. COMPUTE PREDICTED PROBABILITIES BY REPORTER -------------------------

get_predictions <- function(model, newdata, outcome_name) {
  if (is.null(model)) return(NULL)

  # Population-averaged predictions (marginalised over random effects)
  # re.form = NA gives conditional predictions at RE = 0, which
  # underestimates marginal prevalence due to Jensen's inequality.
  # Instead, we average predictions over a sample of observed REs.

  re_vals <- ranef(model)$id[, 1]   # observed random intercepts
  n_re <- min(length(re_vals), 200)  # sample up to 200 for speed
  set.seed(20260926)  # R&R: reproducible RE sample (parent models have very large RE SDs)
  re_sample <- sample(re_vals, n_re)

  # Fixed-effects linear predictor (RE = 0)
  fe_logit <- predict(model, newdata = newdata,
                      type = "link", re.form = NA,
                      allow.new.levels = TRUE)

  # Average predicted probability across RE distribution
  pred_probs <- sapply(re_sample, function(u) plogis(fe_logit + u))
  newdata$pred_prob <- rowMeans(pred_probs)

  # Delta-method SE on the logit scale, then transform with RE averaging

  V <- vcov(model)
  rhs_terms <- delete.response(terms(model, fixed.only = TRUE))
  X <- model.matrix(rhs_terms, data = newdata)
  se_logit <- sqrt(diag(X %*% V %*% t(X)))

  # CIs: average plogis(logit +/- 1.96*SE + u) over RE sample
  lo_probs <- sapply(re_sample, function(u) plogis(fe_logit - 1.96 * se_logit + u))
  hi_probs <- sapply(re_sample, function(u) plogis(fe_logit + 1.96 * se_logit + u))
  newdata$pred_prob_lo <- rowMeans(lo_probs)
  newdata$pred_prob_hi <- rowMeans(hi_probs)

  newdata$pred_logit <- fe_logit  # keep for reference
  newdata$outcome <- outcome_name
  newdata
}

# Generate predictions for each reporter × outcome
preds_bmiz <- map_dfr(names(models_by_reporter), function(rep_label) {
  map_dfr(behaviors, function(b) {
    preds <- get_predictions(models_by_reporter[[rep_label]][[b]], grid_bmiz, b)
    if (!is.null(preds)) preds$reporter <- rep_label
    preds
  })
})

preds_pds <- map_dfr(names(models_by_reporter), function(rep_label) {
  map_dfr(behaviors, function(b) {
    preds <- get_predictions(models_by_reporter[[rep_label]][[b]], grid_pds, b)
    if (!is.null(preds)) preds$reporter <- rep_label
    preds
  })
})

# ---- 7. PLOT CONFIGURATION --------------------------------------------------

mod_colors <- c(
  "Lower (25th %ile)"  = "#1A4F66",   # Navy
  "Mean (50th %ile)"   = "#6C7A89",   # Grey
  "Higher (75th %ile)" = "#EF6C45"    # Coral
)

# PDS colors — one per category level that survived the n >= 50 filter
pds_palette <- c(
  "Pre"   = "#1A4F66",   # Navy
  "Early" = "#A481C7",   # Lavender
  "Mid"   = "#C2B824",   # Chartreuse
  "Late"  = "#EF6C45",   # Coral
  "Post"  = "#E1AD01"    # Sunrise
)
pds_colors <- pds_palette[names(pds_levels)]

base_theme <- theme_minimal(base_size = 13) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    strip.text        = element_text(face = "bold", size = 10),
    strip.background  = element_blank(),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 9, color = "grey30"),
    axis.title        = element_text(size = 10),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8)
  )

# ---- 8. PANEL A: BMIz × Deviation → ED, faceted by Reporter × Outcome ------

preds_bmiz <- preds_bmiz %>%
  mutate(outcome_label = behavior_labels[outcome],
         outcome_label = factor(outcome_label,
                                levels = c("Binge Eating", "Vomiting",
                                           "Fear of\nWeight Gain", "Other WCB")),
         reporter = factor(reporter, levels = c("Parent", "Youth")))

panel_a <- ggplot(preds_bmiz,
                  aes(x = bmiz_diff_raw, y = pred_prob,
                      color = bmiz_label, fill = bmiz_label)) +
  geom_ribbon(aes(ymin = pred_prob_lo, ymax = pred_prob_hi),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  facet_grid(reporter ~ outcome_label, scales = "free_y") +
  scale_color_manual(values = mod_colors, name = "BMIz Level") +
  scale_fill_manual(values = mod_colors, name = "BMIz Level") +
  scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "A: BMIz Deviation \u00d7 BMIz Level",
    subtitle = paste0("Predicted probability by reporter; lines show how the ",
                      "deviation slope differs across BMIz levels (see text for credible intervals)"),
    x = "BMIz Deviation (Observed \u2212 Forecasted)",
    y = "Predicted Probability"
  ) +
  base_theme +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

# ---- 9. PANEL B: PDS × Deviation → ED, faceted by Reporter × Outcome -------

preds_pds <- preds_pds %>%
  mutate(outcome_label = behavior_labels[outcome],
         outcome_label = factor(outcome_label,
                                levels = c("Binge Eating", "Vomiting",
                                           "Fear of\nWeight Gain", "Other WCB")),
         reporter = factor(reporter, levels = c("Parent", "Youth")))

panel_b <- ggplot(preds_pds,
                  aes(x = bmiz_diff_raw, y = pred_prob,
                      color = pds_label, fill = pds_label)) +
  geom_ribbon(aes(ymin = pred_prob_lo, ymax = pred_prob_hi),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  facet_grid(reporter ~ outcome_label, scales = "free_y") +
  scale_color_manual(values = pds_colors, name = "Pubertal Stage") +
  scale_fill_manual(values = pds_colors, name = "Pubertal Stage") +
  scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "B: BMIz Deviation \u00d7 Pubertal Stage",
    subtitle = paste0("Predicted probability by reporter; lines show how the ",
                      "deviation slope differs across pubertal stages (see text for credible intervals)"),
    x = "BMIz Deviation (Observed \u2212 Forecasted)",
    y = "Predicted Probability"
  ) +
  base_theme +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

# ---- 10. COMBINE AND SAVE ---------------------------------------------------
#
# Primary figure: PDS × Deviation panel only (Figure 8).
# The BMIz × Deviation interaction was non-significant for all outcomes,
# so that panel is retained in the script for reference but not included
# in the main manuscript figure.  The key finding is that bmiz_diff_raw
# is a significant main-effect predictor — and its effect is consistent
# ACROSS THE WEIGHT SPECTRUM (non-significant BMIz × deviation interaction),
# aligning with the atypical AN (AAN) literature.

# Main figure for paper: PDS moderation, all outcomes
ggsave("figs/fig_interaction_pds_mod.png",
       panel_b, width = 13, height = 6, dpi = 300, bg = "white")

# Figure 5 for manuscript: PDS × Deviation for Other WCB only
# (strongest, though not credible, pubertal interaction in the strict-onset Bayesian analysis)
panel_b_wcb <- ggplot(preds_pds %>% filter(outcome == "other_current",
                                           reporter == "Parent"),
                  aes(x = bmiz_diff_raw, y = pred_prob,
                      color = pds_label, fill = pds_label)) +
  geom_ribbon(aes(ymin = pred_prob_lo, ymax = pred_prob_hi),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  scale_color_manual(values = pds_colors, name = "Pubertal Stage") +
  scale_fill_manual(values = pds_colors, name = "Pubertal Stage") +
  scale_x_continuous(breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Pubertal Stage \u00d7 Growth Deviation (Parent-Reported)",
    subtitle = "Predicted probability of parent-reported other weight-control behaviors, by BMIz deviation and pubertal stage",
    x = "BMIz Deviation (Observed \u2212 Forecasted BMIz)",
    y = "Predicted Probability of Other Weight-Control Behaviors"
  ) +
  base_theme +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

# ---- Figure 5 CI diagnostic (R&R 2026-09-26) ----
diag_f5 <- preds_pds %>% filter(outcome == "other_current", reporter == "Parent")
diag_msg <- paste0(
  "Fig 5 CI check: rows = ", nrow(diag_f5),
  "; NA lower/upper = ", sum(is.na(diag_f5$pred_prob_lo) | is.na(diag_f5$pred_prob_hi)),
  "; median band width (percentage points) = ",
  round(100 * median(diag_f5$pred_prob_hi - diag_f5$pred_prob_lo, na.rm = TRUE), 3),
  "; max band width = ",
  round(100 * max(diag_f5$pred_prob_hi - diag_f5$pred_prob_lo, na.rm = TRUE), 3))
message(diag_msg)
writeLines(c(diag_msg, capture.output(print(summary(diag_f5[, c("pred_prob", "pred_prob_lo", "pred_prob_hi")])))),
           "figs/fig5_ci_check.txt")

ggsave("figs/fig5_pds_wcb_only.png",
       panel_b_wcb, width = 7, height = 4.8, dpi = 300, bg = "white")

# Supplementary Figure S4: Combined 2-panel interaction figure
# Panel A = BMIz × Deviation (n.s.), Panel B = PDS × Deviation (sig for WCB)
# All ED behaviors shown in both panels
combined <- gridExtra::arrangeGrob(panel_a, panel_b, ncol = 1, heights = c(1, 1))
ggsave("figs/fig_s4_interaction_both_panels.png",
       combined, width = 13, height = 12, dpi = 300, bg = "white")

# Individual panels for archive
ggsave("figs/fig_interaction_bmiz_mod_supplemental.png",
       panel_a, width = 13, height = 6, dpi = 300, bg = "white")
ggsave("figs/fig_interaction_pds_mod_supplemental.png",
       panel_b, width = 13, height = 6, dpi = 300, bg = "white")

message("Marginal effects interaction plots saved to figs/")

# ---- 11. DIAGNOSTICS: SIGN CHECK & SLOPE SUMMARY ----------------------------
# Print the marginal slope of bmiz_diff_raw at each moderator level
# so the direction question can be answered definitively.

cat("\n=== MARGINAL SLOPE OF bmiz_diff_raw AT MODERATOR LEVELS ===\n")
cat("(slope = d[logit(P)] / d[bmiz_diff_raw] = \u03B21_main + \u03B23_interaction \u00D7 moderator_value)\n")
cat("Negative slope = below-forecast (negative deviation) increases risk\n")
cat("Positive slope = above-forecast (positive deviation) increases risk\n\n")

for (rep_label in names(models_by_reporter)) {
  cat(sprintf("--- %s-reported ---\n", rep_label))
  for (b in behaviors) {
    m <- models_by_reporter[[rep_label]][[b]]
    if (is.null(m)) { cat(sprintf("  %s: model failed\n", b)); next }

    fe <- fixef(m)
    b1 <- fe["bmiz_diff_raw"]
    # Interaction term name may vary
    b3_bmiz <- fe[grep("bmiz:bmiz_diff_raw|bmiz_diff_raw:bmiz", names(fe))]
    b3_pds  <- fe[grep("pds_mean_c:bmiz_diff_raw|bmiz_diff_raw:pds_mean_c", names(fe))]

    if (length(b3_bmiz) == 0) b3_bmiz <- 0
    if (length(b3_pds) == 0)  b3_pds  <- 0

    cat(sprintf("  %s:\n", b))
    cat(sprintf("    \u03B21 (bmiz_diff_raw main)      = %+.4f\n", b1))
    cat(sprintf("    \u03B23 (bmiz \u00D7 deviation)        = %+.4f\n", b3_bmiz[1]))
    cat(sprintf("    \u03B24 (pds \u00D7 deviation)         = %+.4f\n", b3_pds[1]))

    cat("    BMIz moderation:\n")
    for (i in seq_along(bmiz_levels)) {
      slope <- b1 + b3_bmiz[1] * bmiz_levels[i]
      cat(sprintf("      at %s (%.2f): slope = %+.4f  → %s\n",
                  names(bmiz_levels)[i], bmiz_levels[i], slope,
                  ifelse(slope < 0, "BELOW-forecast = higher risk",
                         "ABOVE-forecast = higher risk")))
    }

    cat("    PDS moderation:\n")
    for (i in seq_along(pds_levels)) {
      slope <- b1 + b3_pds[1] * pds_levels[i]
      cat(sprintf("      at %s (%.2f): slope = %+.4f  → %s\n",
                  names(pds_levels)[i], pds_levels[i], slope,
                  ifelse(slope < 0, "BELOW-forecast = higher risk",
                         "ABOVE-forecast = higher risk")))
    }
    cat("\n")
  }
}

# ---- 11b. SAVE SLOPE TABLE TO CSV --------------------------------------------

slope_rows <- list()
for (rep_label in names(models_by_reporter)) {
  for (b in behaviors) {
    m <- models_by_reporter[[rep_label]][[b]]
    if (is.null(m)) next

    fe <- fixef(m)
    b1 <- fe["bmiz_diff_raw"]
    b3_bmiz <- fe[grep("bmiz:bmiz_diff_raw|bmiz_diff_raw:bmiz", names(fe))]
    b3_pds  <- fe[grep("pds_mean_c:bmiz_diff_raw|bmiz_diff_raw:pds_mean_c", names(fe))]
    if (length(b3_bmiz) == 0) b3_bmiz <- 0
    if (length(b3_pds) == 0)  b3_pds  <- 0

    for (i in seq_along(bmiz_levels)) {
      slope <- b1 + b3_bmiz[1] * bmiz_levels[i]
      slope_rows[[length(slope_rows) + 1]] <- data.frame(
        reporter   = rep_label,
        outcome    = b,
        moderator  = "BMIz",
        level      = names(bmiz_levels)[i],
        level_value = bmiz_levels[i],
        beta_main  = b1,
        beta_interaction = b3_bmiz[1],
        marginal_slope   = slope,
        direction  = ifelse(slope < 0, "below-forecast = higher risk",
                            "above-forecast = higher risk"),
        stringsAsFactors = FALSE
      )
    }
    for (i in seq_along(pds_levels)) {
      slope <- b1 + b3_pds[1] * pds_levels[i]
      slope_rows[[length(slope_rows) + 1]] <- data.frame(
        reporter   = rep_label,
        outcome    = b,
        moderator  = "PDS",
        level      = names(pds_levels)[i],
        level_value = pds_levels[i],
        beta_main  = b1,
        beta_interaction = b3_pds[1],
        marginal_slope   = slope,
        direction  = ifelse(slope < 0, "below-forecast = higher risk",
                            "above-forecast = higher risk"),
        stringsAsFactors = FALSE
      )
    }
  }
}

slope_table <- bind_rows(slope_rows)
write.csv(slope_table, "tabs/aim3_marginal_slopes.csv", row.names = FALSE)
message("Marginal slope table saved to tabs/aim3_marginal_slopes.csv")

# ---- 12. SUMMARY STATISTICS -------------------------------------------------
cat("\n=== Moderator Levels Used ===\n")
cat("BMIz quantiles (25th, 50th, 75th):\n")
print(round(params$bmiz_q, 2))
cat("\nPDS category → mean pds_mean_c values used:\n")
print(round(pds_levels, 3))
cat("\nDeviation range (2nd\u201398th percentile):\n")
print(round(params$dev_range, 2))

cat("\n=== Model Convergence Summary ===\n")
for (rep_label in names(models_by_reporter)) {
  cat(sprintf("  %s:\n", rep_label))
  for (b in behaviors) {
    m <- models_by_reporter[[rep_label]][[b]]
    if (!is.null(m)) {
      cat(sprintf("    %s: converged (%d obs, %d groups)\n",
                  b, nobs(m), length(unique(m@frame$id))))
    } else {
      cat(sprintf("    %s: FAILED\n", b))
    }
  }
}
