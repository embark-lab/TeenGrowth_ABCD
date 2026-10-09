# =============================================================================
# 0.Run_All.R — full post-server pipeline (Aims 1-3), single-command re-run.
#
# RUN FROM THE PROJECT ROOT (the directory that contains data/6.0/), e.g.:
#   cd ~/ABCD    # or $HOME/ABCD on the cluster
#   Rscript R.6.0/post_server/0.Run_All.R
#
# All results now save to ONE place: tabs/ (tables) and figs/ (figures).
# data/6.0/ still holds pipeline intermediates (bmi_pred_data, ed_prediction_data,
# etc.) — those are not results and stay there.
#
# RESOURCES: includes the Bayesian sensitivity scripts (6.1, 6.2). With the
# parallelized 6.2, give it a multi-core, high-memory allocation (see the
# sbatch note you were sent). Do NOT run on a login node.
#
# On error the pipeline stops; every completed script has already saved its
# outputs, so fix the failing script and resume by trimming `steps` below.
# =============================================================================

# ---- 1. Locate the post-server scripts (robust to layout) -------------------
script_dir <- local({
  cands <- c("R.6.0/post_server", "R.6.0", "paper/R.6.0/post_server", ".")
  hit <- cands[vapply(cands, function(d)
    length(list.files(d, pattern = "^3\\.1\\..*\\.[Rr]$")) > 0, logical(1))]
  if (!length(hit))
    stop("Cannot find post-server scripts. Run from the project root (the dir with data/6.0/).")
  hit[1]
})
message("Using script_dir = ", script_dir)

find_step <- function(prefix) {
  pat <- paste0("^", gsub("\\.", "\\\\.", prefix), "\\..*\\.[Rr]$")
  f <- list.files(script_dir, pattern = pat)
  if (length(f) == 0) stop("No script matching step ", prefix, " in ", script_dir)
  if (length(f) > 1) message("  (multiple matches for ", prefix, "; using ", f[1], ")")
  file.path(script_dir, f[1])
}

# ---- 2. Output dirs — one place each ----------------------------------------
for (d in c("tabs", "figs")) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- 3. Warn about stale full-sample outputs from a pre-fix run -------------
for (s in c("data/6.0/Forecast_Results_FullSample.RData",
            "data/6.0/Forecast_Results_fullsample.RData")) {
  if (file.exists(s)) message("WARNING: stale full-sample file present -> ", s,
                              "  (3.3 would reuse it; delete or regenerate if needed).")
}

# ---- 4. Pipeline, in dependency order ---------------------------------------
# 3.1 first (rebuilds Forecast_Results + inputs from the new bmi_pred_data.RData).
# 8.2 last (aggregates the Bayesian outputs from 6.2).
# 3.2 (full-sample eval) and 7.1 (reverse-direction) are omitted — the paper
# reports no full-sample results.
steps <- c(
  "3.1",   # Evaluate forecasts -> Forecast_Results.RData, uw/ow/ob flags
  "3.5",   # Aim 1 weight-based accuracy metrics
  "4.1",   # ED data join -> ed_prediction_data.RData
  "4.2",   # Aim 2 logistic + binary models
  "4.3",   # Aim 2 forest plots (Figures 3/4)
  "3.3",   # CI width comparison
  "3.4",   # CI stability by n points
  "5.1",   # Aim 3 GLMMs -> model_results_stratified, *_with_pds
  "5.2",   # Aim 3 descriptives
  "5.3",   # Aim 3 marginal-effects plots (Figure 5)
  "6.1",   # Aim 3 imputation sensitivity (SLOW)
  "6.2",   # Aim 3 Bayesian sensitivity (SLOW; parallelized)
  "8.1",   # Figure 2
  "8.2"    # Supplementary Tables (LAST)
)

# ---- 5. Run -----------------------------------------------------------------
t0 <- Sys.time()
for (st in steps) {
  path <- find_step(st)
  message("\n", strrep("=", 62), "\nRunning ", st, ": ", basename(path), "\n", strrep("=", 62))
  tic <- Sys.time()
  tryCatch(
    source(path, local = new.env(parent = globalenv())),
    error = function(e) {
      message("\nERROR in step ", st, " (", basename(path), "): ", conditionMessage(e))
      message("Outputs from earlier steps are saved. Fix this script, then resume by ",
              "removing the completed steps from `steps` at the top.")
      stop(e)
    }
  )
  message(sprintf("Finished %s  (%.1f min)", st,
                  as.numeric(difftime(Sys.time(), tic, units = "mins"))))
}
message(sprintf("\n%s\nALL POST-SERVER SCRIPTS COMPLETED in %.1f min.\n%s",
                strrep("=", 62),
                as.numeric(difftime(Sys.time(), t0, units = "mins")),
                strrep("=", 62)))
