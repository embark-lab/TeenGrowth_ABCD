# =============================================================================
# 6.1 Imputation Sensitivity Analysis (Pre-registered) — PARALLELIZED.
#
# STATISTICS ARE IDENTICAL to the original: same mice setup (20 imputations,
# pmm on pds_mean_c, seed 42), same GLMM formula, same Rubin's-rules pooling,
# same FDR, same comparison. The ONLY change is that the 20 per-imputation
# glmer fits (for each reporter x outcome) are fit concurrently with furrr
# instead of in a sequential for-loop, and outputs save to tabs/ (one location).
# =============================================================================

library(dplyr)
library(lme4)
library(broom.mixed)
library(mice)
library(furrr)
library(future)

# =============================================================================
# 0. LOAD DATA (same setup as original)
# =============================================================================
load("data/6.0/ed_prediction_data.RData")
load("data/6.0/pds_scored.RData")

ed_prediction_data <- ed_prediction_data %>%
  left_join(pds_scored, by = c("id", "timepoint"))

ed_prediction_data <- ed_prediction_data %>%
  filter(forecast_point == "mean+most_recent",
         ci == "bmiz +/- 0.5",
         timepoint %in% c("Year 2", "Year 4", "Year 6"))

ed_prediction_data <- ed_prediction_data %>%
  mutate(
    sex = factor(sex),
    timepoint = factor(timepoint, levels = c("Year 2", "Year 3", "Year 4",
                                             "Year 5", "Year 6")),
    bmi_diff_raw  = bmi - eBMI,
    bmiz_diff_raw = bmiz - eBMIz
  ) %>%
  group_by(sex) %>%
  mutate(pds_mean_c = pds_mean - mean(pds_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(timepoint) %>%
  mutate(age_c = (agemos / 12) - mean(agemos / 12, na.rm = TRUE)) %>%
  ungroup()

behaviors <- c("vomit_current", "binge_current", "other_current", "fowg_current")

# =============================================================================
# PART 1: MISSINGNESS DIAGNOSTICS (unchanged)
# =============================================================================
message("\n========== MISSINGNESS DIAGNOSTICS ==========\n")

analysis_vars <- c("sex", "age_c", "bmiz_diff_raw", "bmiz", "pds_mean_c",
                   behaviors, "id", "timepoint", "reporter")
diag_data <- ed_prediction_data %>% select(all_of(analysis_vars))

message("--- Missing values per variable ---")
miss_summary <- sapply(diag_data, function(x) {
  c(n_total = length(x), n_missing = sum(is.na(x)),
    pct_missing = round(100 * mean(is.na(x)), 2))
})
print(t(miss_summary))

message("\n--- Missing PDS by reporter ---")
ed_prediction_data %>%
  group_by(reporter) %>%
  summarise(n = n(),
            pds_missing = sum(is.na(pds_mean_c)), pds_pct = round(100 * mean(is.na(pds_mean_c)), 1),
            age_missing = sum(is.na(age_c)),      age_pct = round(100 * mean(is.na(age_c)), 1),
            .groups = "drop") %>% print()

message("\n--- Missing PDS by timepoint ---")
ed_prediction_data %>%
  group_by(timepoint) %>%
  summarise(n = n(), pds_missing = sum(is.na(pds_mean_c)),
            pds_pct = round(100 * mean(is.na(pds_mean_c)), 1), .groups = "drop") %>% print()

for (rep_val in c("parent", "youth")) {
  for (outcome in behaviors) {
    full_n <- ed_prediction_data %>% filter(reporter == rep_val, !is.na(.data[[outcome]])) %>% nrow()
    cc_n <- ed_prediction_data %>%
      filter(reporter == rep_val, !is.na(.data[[outcome]]), !is.na(pds_mean_c),
             !is.na(age_c), !is.na(bmiz_diff_raw), !is.na(bmiz), !is.na(sex)) %>% nrow()
    message(sprintf("  %s | %s: full=%d, complete-case=%d, lost=%d (%.1f%%)",
                    rep_val, outcome, full_n, cc_n, full_n - cc_n,
                    100 * (full_n - cc_n) / full_n))
  }
}

tryCatch({
  library(naniar)
  message("\n--- Little's MCAR test ---")
  print(mcar_test(diag_data %>% select(-id, -reporter)))
}, error = function(e) {
  message("naniar not installed; skipping MCAR test.")
})

# =============================================================================
# PART 2: MICE IMPUTATION + GLMM SENSITIVITY (glmer fits parallelized)
# =============================================================================
message("\n========== IMPUTATION SENSITIVITY ANALYSIS ==========\n")

# Parallel plan — glmer is single-threaded, so one worker per fit.
n_cores <- as.integer(Sys.getenv("MI_CORES", parallel::detectCores()))
plan(multisession, workers = max(1L, n_cores))
message(sprintf("Parallel imputation-model fitting on %d core(s).", n_cores))

# Fit one GLMM on one completed (imputed) dataset; return tidy fixed effects.
fit_imp_glmer <- function(dat, outcome) {
  suppressMessages({ library(lme4); library(broom.mixed); library(dplyr) })
  df_sub <- dat %>%
    filter(!is.na(.data[[outcome]]), !is.na(bmiz_diff_raw), !is.na(bmiz),
           !is.na(sex), !is.na(age_c), !is.na(pds_mean_c))
  if (nrow(df_sub) < 50) return(NULL)
  tryCatch(
    tidy(
      glmer(as.formula(paste0(outcome, " ~ sex + age_c + bmiz_diff_raw + ",
                              "bmiz * bmiz_diff_raw + pds_mean_c * bmiz_diff_raw + (1 | id)")),
            data = df_sub, family = binomial, control = glmerControl(optimizer = "bobyqa")),
      effects = "fixed", conf.int = FALSE),
    error = function(e) NULL)
}

imputed_results <- list()

for (rep_val in c("parent", "youth")) {
  message(sprintf("\n--- Reporter: %s ---", rep_val))
  rep_data <- ed_prediction_data %>% filter(reporter == rep_val)

  imp_vars <- c("id", "sex", "age_c", "bmiz_diff_raw", "bmiz",
                "pds_mean_c", "timepoint", behaviors)
  imp_data <- rep_data %>% select(all_of(imp_vars))

  if (sum(is.na(imp_data$pds_mean_c)) == 0) {
    message("  No missing PDS data for this reporter; skipping imputation.")
    next
  }

  ini <- mice(imp_data, maxit = 0)
  pred <- ini$predictorMatrix
  meth <- ini$method
  pred[, "id"] <- 0
  pred["id", ] <- 0
  meth[names(meth)] <- ""
  meth["pds_mean_c"] <- "pmm"

  message("  Running mice with 20 imputations...")
  imp <- mice(imp_data, m = 20, method = meth, predictorMatrix = pred,
              seed = 42, printFlag = FALSE)
  imp_all <- complete(imp, "all")   # list of 20 completed datasets

  message("  Fitting GLMMs on each imputed dataset (parallel)...")
  for (outcome in behaviors) {
    message(sprintf("    Outcome: %s", outcome))

    coef_list <- future_map(imp_all, ~ fit_imp_glmer(.x, outcome),
                            .options = furrr_options(seed = TRUE))
    coef_list <- coef_list[!vapply(coef_list, is.null, logical(1))]

    if (length(coef_list) == 0) {
      message(sprintf("    All imputations failed for %s | %s", rep_val, outcome))
      next
    }

    all_coefs <- bind_rows(coef_list, .id = "imp")

    # Pool: mean of estimates, Rubin's variance (identical to original)
    pooled <- all_coefs %>%
      group_by(term) %>%
      summarise(
        m = n(),
        pooled_estimate = mean(estimate),
        within_var = mean(std.error^2),
        between_var = var(estimate),
        total_var = within_var + (1 + 1/m) * between_var,
        pooled_se = sqrt(total_var),
        lambda = (between_var + between_var/m) / total_var,
        df_old = (m - 1) / lambda^2,
        pooled_z = pooled_estimate / pooled_se,
        pooled_p = 2 * pnorm(abs(pooled_z), lower.tail = FALSE),
        pooled_or = exp(pooled_estimate),
        pooled_or_low = exp(pooled_estimate - 1.96 * pooled_se),
        pooled_or_high = exp(pooled_estimate + 1.96 * pooled_se),
        .groups = "drop"
      ) %>%
      mutate(reporter = rep_val, outcome = outcome)

    imputed_results[[paste0(rep_val, "_", outcome)]] <- pooled
  }
}
plan(sequential)

# =============================================================================
# PART 3: COMPARISON TABLE (unchanged; paths -> tabs/)
# =============================================================================
if (length(imputed_results) > 0) {
  all_imputed <- bind_rows(imputed_results)

  all_imputed <- all_imputed %>%
    group_by(reporter, term) %>%
    mutate(p.value.fdr = p.adjust(pooled_p, method = "fdr")) %>%
    ungroup()

  message("\n========== COMPARISON: COMPLETE-CASE vs IMPUTED ==========\n")
  load("tabs/model_results_stratified.RData")

  cc <- final_results %>%
    filter(effect == "fixed") %>%
    select(term, reporter, outcome, estimate, std.error, p.value, p.value.fdr) %>%
    rename(cc_estimate = estimate, cc_se = std.error, cc_p = p.value, cc_p_fdr = p.value.fdr)

  imp_compare <- all_imputed %>%
    select(term, reporter, outcome, pooled_estimate, pooled_se, pooled_p, p.value.fdr) %>%
    rename(mi_estimate = pooled_estimate, mi_se = pooled_se, mi_p = pooled_p, mi_p_fdr = p.value.fdr)

  comparison <- full_join(cc, imp_compare, by = c("term", "reporter", "outcome")) %>%
    mutate(
      cc_sig = cc_p_fdr < 0.05,
      mi_sig = mi_p_fdr < 0.05,
      sig_change = case_when(
        cc_sig & !mi_sig ~ "Lost significance",
        !cc_sig & mi_sig ~ "Gained significance",
        cc_sig & mi_sig ~ "Both significant",
        TRUE ~ "Neither significant"
      )
    )

  message("--- Significance changes ---")
  print(table(comparison$sig_change))

  changed <- comparison %>% filter(sig_change %in% c("Lost significance", "Gained significance"))
  message("\n--- Terms where significance changed ---")
  if (nrow(changed) > 0) {
    print(as.data.frame(changed %>% select(term, reporter, outcome, cc_p_fdr, mi_p_fdr, sig_change)))
  } else {
    message("  No significance changes between complete-case and imputed analyses.")
  }

  message("\n--- Key terms: complete-case vs imputed ORs ---")
  key_terms <- c("bmiz_diff_raw", "bmiz_diff_raw:bmiz", "bmiz_diff_raw:pds_mean_c")
  comparison %>%
    filter(term %in% key_terms) %>%
    mutate(cc_or = round(exp(cc_estimate), 3), mi_or = round(exp(mi_estimate), 3),
           or_diff = round(mi_or - cc_or, 3)) %>%
    select(term, reporter, outcome, cc_or, mi_or, or_diff, cc_p_fdr, mi_p_fdr, sig_change) %>%
    arrange(term, reporter, outcome) %>% as.data.frame() %>% print()

  save(all_imputed, comparison, file = "tabs/aim3_imputation_sensitivity.RData")
  message("\nSaved to tabs/aim3_imputation_sensitivity.RData")
} else {
  message("No imputed results generated (no missing covariate data).")
}

message("\n========== DONE ==========")
