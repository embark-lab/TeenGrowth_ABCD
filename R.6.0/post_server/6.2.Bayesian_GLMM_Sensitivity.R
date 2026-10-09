# =============================================================================
# 6.2 Bayesian GLMM Sensitivity (Aim 3) — PARALLELIZED across the 8 models.
#
# STATISTICS ARE IDENTICAL to the original: same data prep, same weakly
# informative priors, same formula, same chains/iter/warmup/seed/adapt_delta.
# The ONLY change is that the 8 independent models (2 reporters x 4 outcomes)
# are fit concurrently with furrr instead of in a sequential for-loop, and all
# outputs now save to tabs/ (one location).
#
# Verify after running: every model's max Rhat < 1.01 and 0 divergent
# transitions (printed in the diagnostics summary at the end) — same check as
# the original.
#
# RESOURCES: fitting several models at once holds several brms fit objects in
# memory at once (each ~1.3 GB). Run on a node with plenty of RAM (request
# ~64 GB) and set the workers/cores to your allocation (see below).
# =============================================================================

library(dplyr)
library(brms)
library(tidyr)
library(purrr)
library(furrr)
library(future)

# =============================================================================
# 1. DATA PREPARATION (identical to the original)
# =============================================================================
load("data/6.0/ed_prediction_data.RData")
load("data/6.0/pds_scored.RData")

ed_prediction_data <- ed_prediction_data %>%
  left_join(pds_scored, by = c("id", "timepoint"))

behaviors <- c("vomit_current", "binge_current", "other_current", "fowg_current")

ed_prediction_data <- ed_prediction_data %>%
  mutate(
    sex = factor(sex),
    timepoint = factor(timepoint, levels = c("Year 2", "Year 3", "Year 4",
                                             "Year 5", "Year 6"))
  ) %>%
  arrange(id, timepoint) %>%
  mutate(bmi_diff_raw = bmi - eBMI, bmiz_diff_raw = bmiz - eBMIz) %>%
  group_by(sex) %>%
  mutate(pds_mean_c = pds_mean - mean(pds_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(timepoint) %>%
  mutate(age_c = (agemos / 12) - mean(agemos / 12, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(forecast_point == "mean+most_recent",
         ci == "bmiz +/- 0.5",
         timepoint %in% c("Year 2", "Year 4", "Year 6"))

# =============================================================================
# 2. PRIORS (identical)
# =============================================================================
aim3_priors <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(normal(0, 2.5), class = "b"),
  prior(cauchy(0, 2), class = "sd")
)

# =============================================================================
# 3. CORE ALLOCATION
# =============================================================================
# Each model runs `chains_per_model` chains in parallel; we fit `workers` models
# at a time. Keep workers * chains_per_model <= your allocated cores so nothing
# oversubscribes. Defaults size to detectCores(); override with env vars.
chains_per_model <- as.integer(Sys.getenv("BRMS_CHAINS", "4"))
total_cores <- as.integer(Sys.getenv("BRMS_CORES", parallel::detectCores()))
workers <- max(1L, min(8L, floor(total_cores / chains_per_model)))  # <=8 models

plan(multisession, workers = workers)
message(sprintf("Parallel Bayesian fit: %d cores -> %d model(s) at a time x %d chains each.",
                total_cores, workers, chains_per_model))

# =============================================================================
# 4. FIT ONE MODEL (identical model spec to the original)
# =============================================================================
fit_one <- function(rep_label, outcome, data, priors, chains_per_model) {
  suppressMessages({ library(brms); library(dplyr); library(tidyr) })

  df_sub <- data %>%
    filter(reporter == rep_label,
           !is.na(.data[[outcome]]), !is.na(sex), !is.na(bmiz_diff_raw),
           !is.na(bmiz), !is.na(pds_mean_c), !is.na(age_c))
  if (nrow(df_sub) < 50) return(NULL)

  n_events <- sum(df_sub[[outcome]], na.rm = TRUE)
  n_total  <- nrow(df_sub)

  model_formula <- bf(as.formula(paste0(
    outcome, " ~ sex + age_c + bmiz_diff_raw + bmiz * bmiz_diff_raw + ",
    "pds_mean_c * bmiz_diff_raw + (1 | id)"
  )))

  fit <- tryCatch(
    brm(formula = model_formula, data = df_sub, family = bernoulli(),
        prior = priors, chains = chains_per_model, cores = chains_per_model,
        iter = 2000, warmup = 1000, seed = 42,
        control = list(adapt_delta = 0.95, max_treedepth = 12),
        silent = 2, refresh = 0),
    error = function(e) { message("  ERROR ", rep_label, "/", outcome, ": ", e$message); NULL })
  if (is.null(fit)) return(NULL)

  diag_summary <- data.frame(
    reporter = rep_label, outcome = outcome, n_obs = n_total, n_events = n_events,
    n_divergent = sum(nuts_params(fit)$Value[nuts_params(fit)$Parameter == "divergent__"]),
    max_rhat = max(rhat(fit), na.rm = TRUE),
    min_ess_bulk = min(neff_ratio(fit) * 4000, na.rm = TRUE)
  )

  # Save individual model (one location: tabs/)
  save(fit, diag_summary, file = paste0("tabs/bayes_", rep_label, "_", outcome, ".RData"))

  fe_df <- as.data.frame(fixef(fit, summary = TRUE)) %>%
    tibble::rownames_to_column("term") %>%
    rename(estimate = Estimate, std.error = Est.Error,
           conf.low = Q2.5, conf.high = Q97.5) %>%
    mutate(reporter = rep_label, outcome = outcome,
           or = exp(estimate), or_low = exp(conf.low), or_high = exp(conf.high),
           pd = NA_real_)

  post_samples <- as_draws_matrix(fit)
  for (j in seq_len(nrow(fe_df))) {
    pn <- paste0("b_", fe_df$term[j])
    if (pn %in% colnames(post_samples)) {
      sm <- post_samples[, pn]
      fe_df$pd[j] <- max(mean(sm > 0), mean(sm < 0))
    }
  }
  list(fe = fe_df, diag = diag_summary)
}

# =============================================================================
# 5. FIT ALL 8 MODELS IN PARALLEL
# =============================================================================
tasks <- expand.grid(reporter = unique(na.omit(ed_prediction_data$reporter)),
                     outcome  = behaviors, stringsAsFactors = FALSE)

results <- future_map2(
  tasks$reporter, tasks$outcome,
  ~ fit_one(.x, .y, ed_prediction_data, aim3_priors, chains_per_model),
  .options = furrr_options(seed = TRUE)
)
plan(sequential)

results   <- results[!vapply(results, is.null, logical(1))]
bayes_all <- bind_rows(lapply(results, `[[`, "fe"))
diag_all  <- bind_rows(lapply(results, `[[`, "diag"))

cat("\n═══ CONVERGENCE DIAGNOSTICS (verify Rhat<1.01, divergent=0) ═══\n")
print(as.data.frame(diag_all), row.names = FALSE)

# =============================================================================
# 6. COMBINE AND COMPARE WITH FREQUENTIST RESULTS (identical)
# =============================================================================
load("tabs/model_results_stratified.RData")

bayes_all <- bayes_all %>%
  mutate(term_std = case_when(
    term == "Intercept" ~ "(Intercept)",
    term == "sex2" ~ "sex2",
    TRUE ~ term
  ))

comparison <- final_results %>%
  select(term, reporter, outcome,
         freq_or = estimate, freq_se = std.error, freq_p = p.value,
         freq_p_fdr = p.value.fdr) %>%
  left_join(
    bayes_all %>%
      select(term_std, reporter, outcome,
             bayes_or = or, bayes_se = std.error, bayes_pd = pd,
             bayes_or_low = or_low, bayes_or_high = or_high),
    by = c("term" = "term_std", "reporter", "outcome")
  ) %>%
  mutate(
    freq_sig  = freq_p_fdr < 0.05,
    bayes_sig = (bayes_or_low > 1) | (bayes_or_high < 1),
    concordance = case_when(
      freq_sig & bayes_sig ~ "Both significant",
      !freq_sig & !bayes_sig ~ "Neither significant",
      freq_sig & !bayes_sig ~ "Freq only (possible separation artifact)",
      !freq_sig & bayes_sig ~ "Bayes only"
    )
  )

cat("\n--- Overall concordance ---\n")
print(table(comparison$concordance))

# =============================================================================
# 7. SAVE (one location: tabs/)
# =============================================================================
save(bayes_all, comparison, file = "tabs/aim3_bayesian_sensitivity.RData")
cat("\nBayesian sensitivity complete. Saved to tabs/aim3_bayesian_sensitivity.RData\n")
cat("Individual model objects saved to tabs/bayes_*.RData\n")
