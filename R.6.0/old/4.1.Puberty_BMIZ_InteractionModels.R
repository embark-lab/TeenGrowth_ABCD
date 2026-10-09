# ============================== #
# Packages & data
# ============================== #
suppressPackageStartupMessages({
  library(dplyr)
  library(forcats)
  library(lme4)
  library(broom.mixed)   # tidy.glmerMod
  library(broom)         # tidy(glm)
  library(sandwich)      # cluster-robust vcov
  library(ggplot2)
  library(glue)
  library(stringr)
})
options(warn = 1)

load("data/ed_prediction_data_EDRS.RData")
load("data/pds_scored.RData")

# Join PDS into main data
ed_prediction_data <- ed_prediction_data %>%
  left_join(pds_scored, by = c("id", "timepoint"))

# ============================== #
# Variables & preprocessing
# ============================== #
behaviors <- c("restrict_current",
               "vomit_current",
               "binge_current",
               "exercise_current",
               "fowg_current_half")

# Categorical coding, differences, centering
ed_prediction_data <- ed_prediction_data %>%
  mutate(
    sex = factor(sex),
    timepoint = factor(timepoint, levels = c("Year 2", "Year 3", "Year 4"))
  ) %>%
  arrange(id, timepoint) %>%
  mutate(
    bmi_diff_raw  = bmi  - eBMI,
    bmiz_diff_raw = bmiz - eBMIz
  ) %>%
  group_by(sex) %>%
  mutate(pds_mean_c = pds_mean - mean(pds_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(timepoint) %>%
  mutate(age_c = (agemos / 12) - mean(agemos / 12, na.rm = TRUE)) %>%
  ungroup()

# Split by reporter
reporter_groups <- ed_prediction_data %>%
  filter(!is.na(reporter)) %>%
  group_by(reporter) %>%
  group_split()

# ============================== #
# Helpers
# ============================== #
has_two_classes <- function(y) length(unique(y[!is.na(y)])) == 2
class_counts    <- function(y) table(factor(y, levels = c(0,1)), useNA = "no")

# z-scale predictors used in the model (stabilizes interactions)
prep_vars <- function(df) {
  df %>%
    mutate(
      bmiz_z  = as.numeric(scale(bmiz,           center = TRUE, scale = TRUE)),
      dbmiz_z = as.numeric(scale(bmiz_diff_raw,  center = TRUE, scale = TRUE)),
      pds_z   = as.numeric(scale(pds_mean_c,     center = TRUE, scale = TRUE)),
      age_z   = as.numeric(scale(age_c,          center = TRUE, scale = TRUE))
    )
}

# Safer GLMM fit with retries
fit_glmer_safe <- function(form, data) {
  m1 <- try(glmer(form, data = data, family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5))),
            silent = TRUE)
  if (!inherits(m1, "try-error") && is.null(m1@optinfo$conv$lme4$messages)) return(m1)
  
  m2 <- try(glmer(form, data = data, family = binomial,
                  control = glmerControl(optimizer = "nloptwrap",
                                         calc.derivs = FALSE,
                                         optCtrl = list(maxfun = 2e5))),
            silent = TRUE)
  if (!inherits(m2, "try-error") && is.null(m2@optinfo$conv$lme4$messages)) return(m2)
  
  if (!inherits(m2, "try-error")) return(m2)
  m1
}

# Tidy (OR scale) + metadata
tidy_glmer_or <- function(m, reporter, outcome, n, events) {
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE,
                    conf.method = "Wald", exponentiate = TRUE) %>%
    mutate(engine = "glmer", reporter = reporter, outcome = outcome,
           N = n, events = events,
           conv_msg = paste(m@optinfo$conv$lme4$messages, collapse = " | "))
}

tidy_glm_cluster_or <- function(form, data, reporter, outcome, n, events, id_var = "id") {
  gm <- glm(form, data = data, family = binomial)
  V  <- sandwich::vcovCL(gm, cluster = data[[id_var]])
  broom::tidy(gm, conf.int = TRUE, exponentiate = TRUE, vcov = V) %>%
    mutate(engine = "glm_robustCL", reporter = reporter, outcome = outcome,
           N = n, events = events, conv_msg = NA_character_)
}

# ============================== #
# Modeling loop
# ============================== #
model_results <- list(); i <- 0

for (grp in reporter_groups) {
  rep_label <- dplyr::first(grp$reporter)
  
  for (outcome in behaviors) {
    message("Fitting model for outcome: ", outcome, " | reporter: ", rep_label)
    
    df_sub <- grp %>%
      filter(!is.na(.data[[outcome]]),
             !is.na(sex),
             !is.na(bmiz),
             !is.na(bmiz_diff_raw),
             !is.na(pds_mean_c),
             !is.na(age_c),
             !is.na(id)) %>%
      prep_vars()
    
    # Guard: rows
    if (nrow(df_sub) < 50) {
      message("  -> Skipping (rows < 50)")
      next
    }
    
    # Guard: both classes
    y <- df_sub[[outcome]]
    if (!has_two_classes(y)) {
      cc <- class_counts(y)
      message("  -> Skipping (Response is constant; counts = ",
              paste(cc, collapse = "/"), ")")
      next
    }
    
    # Use interactions only if each class has >=10 events
    tab <- class_counts(y)
    use_interactions <- all(tab >= 10)
    
    rhs_main <- "sex + age_z + dbmiz_z + bmiz_z + pds_z"
    rhs_int  <- if (use_interactions) " + bmiz_z:dbmiz_z + pds_z:dbmiz_z" else ""
    fml_glmer <- as.formula(paste0(outcome, " ~ ", rhs_main, rhs_int, " + (1 | id)"))
    fml_glm   <- as.formula(paste0(outcome, " ~ ", rhs_main, rhs_int))
    
    # Fit GLMM with fallback to GLM+clustered SEs if RE ~ 0 or failure
    model <- fit_glmer_safe(fml_glmer, df_sub)
    
    n_obs <- nrow(df_sub); ev <- sum(df_sub[[outcome]] == 1, na.rm = TRUE)
    if (inherits(model, "try-error")) {
      message("  -> GLMM failed; using GLM + cluster-robust SEs")
      tid <- tidy_glm_cluster_or(fml_glm, df_sub, rep_label, outcome, n_obs, ev) %>%
        mutate(note = "fallback_glm_cluster")
    } else {
      re_var <- suppressWarnings(try(as.numeric(VarCorr(model)$id[1,1]), silent = TRUE))
      if (!inherits(re_var, "try-error") && !is.na(re_var) && re_var < 1e-6) {
        message("  -> Random-effect variance ~ 0; using GLM + cluster-robust SEs")
        tid <- tidy_glm_cluster_or(fml_glm, df_sub, rep_label, outcome, n_obs, ev) %>%
          mutate(note = "re≈0_glm_cluster")
      } else {
        tid <- tidy_glmer_or(model, rep_label, outcome, n_obs, ev) %>%
          mutate(note = ifelse(!is.null(model@optinfo$conv$lme4$messages),
                               "glmer_convergence_msg", "ok"))
      }
    }
    
    i <- i + 1
    model_results[[i]] <- tid
  }
}

library(stringr)
results_df <- bind_rows(model_results) %>%
  mutate(
    term_norm = if_else(str_detect(term, ":"),
                        vapply(strsplit(term, ":", fixed = TRUE),
                               function(x) paste(sort(x), collapse = ":"),
                               FUN.VALUE = character(1)),
                        term),
    term_clean = case_when(
      term_norm == "(Intercept)"       ~ "Intercept",
      str_starts(term_norm, "sex")     ~ "Sex (ref = ?)",   # handles "sex2", "sexFemale", etc.
      term_norm == "age_z"             ~ "Age (z)",
      term_norm == "dbmiz_z"           ~ "ΔBMIZ",
      term_norm == "bmiz_z"            ~ "BMIZ (z)",
      term_norm == "pds_z"             ~ "PDS (z)",
      term_norm == "bmiz_z:dbmiz_z"    ~ "BMIZ × ΔBMIZ",
      term_norm == "dbmiz_z:pds_z"     ~ "PDS × ΔBMIZ",
      TRUE                             ~ term_norm
    ),
    reporter_clean = recode(reporter, parent = "Parent", youth = "Youth"),
    outcome_clean  = recode(outcome,
                            restrict_current     = "Restriction",
                            binge_current        = "Binge eating",
                            exercise_current     = "Driven exercise",
                            fowg_current_half    = "Fear of weight gain",
                            vomit_current        = "Vomiting")
  )
print(results_df)

# Save results & enriched data
save(results_df, file = "model_results_stratified.RData")
save(ed_prediction_data, file = "data/ed_prediction_data_with_pds.RData")

# ============================== #
# Forest plot: ΔBMIZ + interactions by outcome
# ============================== #
pal <- c(Parent = "#1A4F66", Youth = "#EF6C45")

keep_terms <- c("ΔBMIZ", "BMIZ × ΔBMIZ", "PDS × ΔBMIZ")

forest_dat <- results_df %>%
  filter(term_clean %in% keep_terms) %>%
  mutate(
    term_clean = factor(term_clean, levels = keep_terms),      # consistent order
    reporter_clean = factor(reporter_clean, levels = c("Parent","Youth"))
  ) %>% 
  filter(outcome != "vomit_current")  # too few events for vomit

gg <- ggplot(forest_dat,
             aes(x = estimate, y = term_clean, color = reporter_clean)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey65") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.18,
                 position = position_dodge2(width = 0.55, padding = 0.25)) +
  geom_point(position = position_dodge2(width = 0.55, padding = 0.25),
             size = 2.8) +
  scale_color_manual(values = pal, name = "Reporter") +
  coord_cartesian(xlim = c(0.01, 3)) +
  labs(x = "Odds ratio", y = NULL,
       title = "Effects of BMIz deviations and interactions with \n BMIz and pubertal development (Y2; Age 11-12)") +
  facet_wrap(~ outcome_clean, scales = "free_y") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")) +
  embarktools::embark_theme_a

print(gg)
 ggsave("figs/glmm_forest_bmiz_effects.png", gg, width = 9, height = 6.5, dpi = 300)
