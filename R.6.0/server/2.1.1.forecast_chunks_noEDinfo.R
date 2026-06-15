# --- Minimal, Slurm-friendly chunk forecaster (ABCD 6.0) --------------------

library(dplyr)
library(purrr)
library(TeenGrowth)

# Optional: lock working dir if you want
# setwd("/home/k/kschaumberg/ABCD")  # uncomment for Slurm

# Encourage immediate warning prints (helps live logs)
options(warn = 1)

# Fail fast if required packages aren't available
needed <- c("dplyr","purrr","TeenGrowth")
missing <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(missing)) {
  stop(sprintf("Missing packages: %s", paste(missing, collapse = ", ")))
}

# --- LOGGING helpers ---------------------------------------------------------
fmt_dur <- function(secs) {
  if (is.na(secs) || !is.finite(secs)) return("NA")
  h <- floor(secs/3600); m <- floor((secs %% 3600)/60); s <- round(secs %% 60)
  sprintf("%02dh:%02dm:%02ds", h, m, s)
}
.t0 <- Sys.time()
log_out <- function(msg) {
  ts <- format(Sys.time(), "%F %T")
  cat(sprintf("[%s] %s\n", ts, msg))
  flush.console()
}

# Progress tracker
.progress_done <- 0L
.progress_total <- NA_integer_
tick <- function(msg = "") {
  .progress_done <<- .progress_done + 1L
  elapsed <- as.numeric(difftime(Sys.time(), .t0, units = "secs"))
  pct <- if (!is.na(.progress_total) && .progress_total > 0) .progress_done/.progress_total else NA_real_
  eta <- if (!is.na(pct) && pct > 0) elapsed*(1 - pct)/pct else NA_real_
  cat(sprintf("[%s] step %d/%s (%.0f%%) | elapsed=%s | ETA≈%s %s\n",
              format(Sys.time(), "%F %T"),
              .progress_done,
              if (is.na(.progress_total)) "?" else as.character(.progress_total),
              if (is.na(pct)) 0 else 100*pct,
              fmt_dur(elapsed), fmt_dur(eta),
              if (nzchar(msg)) paste0("| ", msg) else ""))
  flush.console()
}

# Count warnings
.warn_count <- 0L
withCallingHandlers({
  # --- Slurm/env params ------------------------------------------------------
  # --- LOCAL TEST: override Slurm env vars ---
  Sys.setenv(N_CHUNKS = "1", SLURM_ARRAY_TASK_ID = "1")
  
  N_CHUNKS <- as.integer(Sys.getenv("N_CHUNKS", "1"))
  CHUNK_ID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))  # 1-based array
  stopifnot(!is.na(N_CHUNKS), !is.na(CHUNK_ID), CHUNK_ID >= 1, CHUNK_ID <= N_CHUNKS)
  
  dir.create("data/6.0/chunks/logs",  showWarnings = FALSE, recursive = TRUE)
  
  out_rds  <- sprintf("data/6.0/chunks/bmi_pred_data_all_chunk_%d.RData", CHUNK_ID)
  tmp_rds  <- sprintf("%s.tmp", out_rds)
  doneflag <- sprintf("data/6.0/chunks/bmi_pred_data_all_chunk_%d.done",  CHUNK_ID)
  
  # LOCAL TEST: remove old done flag if present
  if (file.exists(doneflag)) file.remove(doneflag)
  
  log_out(sprintf("START | chunk=%d of %d | pid=%d | host=%s",
                  CHUNK_ID, N_CHUNKS, Sys.getpid(), Sys.info()[["nodename"]]))
  
  # --- Ensure partial-save on unexpected exit --------------------------------
  on.exit({
    if (!file.exists(doneflag) && file.exists(tmp_rds)) {
      log_out("on.exit: partial results detected → keeping .tmp for inspection.")
    }
  }, add = TRUE)
  
  # --- Load precomputed inputs ----------------------------------------------
  log_out("Loading precomputed cleaned data & training windows …")
  inp_clean <- "data/6.0/cleaned_ant_threepoints.rds"
  inp_train <- "data/6.0/training_list_ant_threepoints.rds"
  if (!file.exists(inp_clean) || !file.exists(inp_train)) {
    stop(sprintf("Missing input RDS file(s): %s%s",
                 if (!file.exists(inp_clean)) paste0("\n - ", inp_clean) else "",
                 if (!file.exists(inp_train)) paste0("\n - ", inp_train) else ""))
  }
  cleaned_all   <- readRDS(inp_clean)
  training_list <- readRDS(inp_train)   # <- keep as precomputed!
  
  # --- Chunk IDs: balanced split --------------------------------------------
  ids <- sort(unique(cleaned_all$id))
  n_ids <- length(ids)
  if (n_ids == 0L) {
    log_out("No IDs found. Marking done and exiting.")
    file.create(doneflag); stop("Exiting early (would be quit on Slurm)")
  }
  # Balanced assignment: round-robin
  grp <- ((seq_len(n_ids) - 1L) %% N_CHUNKS) + 1L
  ids_chunk <- ids[grp == CHUNK_ID]
  log_out(sprintf("Assigned IDs to this chunk: %s", format(length(ids_chunk), big.mark=",")))
  
  if (length(ids_chunk) == 0L) {
    log_out(sprintf("Chunk %d: no IDs assigned. Marking done.", CHUNK_ID))
    file.create(doneflag)
    stop("Exiting early (would be quit on Slurm)")
  }
  
  # Prefilter cleaned to just this chunk (reduces anti_join size later)
  cleaned <- cleaned_all %>% filter(id %in% ids_chunk)
  
  # --- Train/test windows (use precomputed training_list) --------------------
  timepoints_ordered <- c("Baseline", "Year 1", "Year 2", "Year 3",
                          "Year 4", "Year 5", "Year 6")
  central_values <- c("mean", "max", "mean+most_recent")
  conditions <- list(
    list(ci = "User-Defined", upper_margin = 0.5, lower_margin = 0.5),
    list(ci = 95,           upper_margin = NULL, lower_margin = NULL),
    list(ci = 99,           upper_margin = NULL, lower_margin = NULL)
  )
  
  # LOCAL TEST: only run Y1 training window
  tps_keep <- c("Y1")
  training_list <- training_list[tps_keep]
  testing_list <- purrr::map(training_list, ~ anti_join(cleaned, .x, by = c("id","agemos")))
  names(testing_list) <- names(training_list)
  
  # Progress total = combinations to attempt
  .progress_total <<- length(training_list) * length(central_values) * length(conditions)
  
  # --- Vectorized helpers (define once) --------------------------------------
  vec_bmiz <- Vectorize(bmiz_lookup, vectorize.args = c("bmi","age","sex"))
  vec_bmi  <- Vectorize(bmi_lookup,  vectorize.args = c("data_point","age","sex"))
  
  # --- Forecaster ------------------------------------------------------------
  run_forecasts <- function(training_data, central_value, condition) {
    tryCatch({
      if (!is.null(condition$ci)) {
        forecast_bmi(training_data, central_value = central_value, ci = condition$ci)
      } else {
        forecast_bmi(training_data, central_value = central_value,
                     upper_margin = condition$upper_margin, lower_margin = condition$lower_margin)
      }
    }, error = function(e) {
      log_out(sprintf("Forecast error: %s / %s -> %s",
                      central_value,
                      ifelse(is.null(condition$ci), "margin0.5", as.character(condition$ci)),
                      e$message))
      NULL
    })
  }
  
  # --- Generate forecasts per training window --------------------------------
  forecasts <- list()
  step_i <- 0L
  
  for (tp in names(training_list)) {
    training <- training_list[[tp]]
    log_out(sprintf("Training window: %s | rows=%s | IDs=%s",
                    tp,
                    format(nrow(training), big.mark=","),
                    format(dplyr::n_distinct(training$id), big.mark=",")))
    
    for (cv in central_values) {
      for (cond in conditions) {
        step_i <- step_i + 1L
        key <- if (!is.null(cond$ci)) sprintf("%s_ci_%s", cv, cond$ci) else sprintf("%s_margin_0.5", cv)
        tick(sprintf("tp=%s | cv=%s | cond=%s", tp, cv, key))
        
        t1 <- Sys.time()
        fc <- run_forecasts(training, cv, cond)
        dur <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
        
        if (!is.null(fc)) {
          keep <- fc %>% filter(id %in% ids_chunk, !is.na(eBMIz))
          n_keep <- nrow(keep)
          log_out(sprintf("Produced rows: %s (%.1fs) for %s_%s",
                          format(n_keep, big.mark=","), dur, tp, key))
          if (n_keep > 0) {
            forecasts[[sprintf("%s_%s", tp, key)]] <- keep
          }
        } else {
          log_out(sprintf("No result for %s_%s (error or empty).", tp, key))
        }
        
        # Light memory hygiene every few combos
        if ((step_i %% 6L) == 0L) invisible(gc())
      }
    }
    
    # Add pop-median comparator once per tp (if any models produced rows)
    have_tp <- grep(paste0("^", tp, "_"), names(forecasts), value = TRUE)
    if (length(have_tp)) {
      base <- forecasts[[have_tp[1]]]
      if (!is.null(base) && nrow(base)) {
        log_out(sprintf("Adding pop_median comparator for %s …", tp))
        pop_median <- base %>%
          mutate(
            `.model`    = "pop_median",
            eBMIz       = 0,
            eBMI        = median_bmi,
            lower_eBMIz = -1,
            lower_eBMI  = vec_bmiz(bmi = lower_eBMI, age = agemos, sex = sex, age_unit = "months"),
            upper_eBMIz = 1,
            upper_eBMI  = vec_bmi (data_point = upper_eBMIz, age = agemos, sex = sex, age_unit = "months")
          )
        forecasts[[sprintf("%s_%s", tp, "pop_median")]] <- pop_median
        log_out(sprintf("pop_median rows: %s", format(nrow(pop_median), big.mark=",")))
      }
    }
  }
  
  # --- Nest and save ---------------------------------------------------------
  forecast_names <- names(forecasts)
  tps <- if (length(forecast_names)) unique(sub("_.*", "", forecast_names)) else character()
  nested_forecasts <- setNames(vector("list", length(tps)), tps)
  for (tp in tps) {
    grp <- forecasts[grepl(paste0("^", tp, "_"), forecast_names)]
    names(grp) <- sub(paste0("^", tp, "_"), "", names(grp))
    nested_forecasts[[tp]] <- grp
  }
  
  bmi_pred_data_chunk <- list(
    training_list = training_list,   # duplicated across chunks (OK)
    testing_list  = testing_list,    # chunk-filtered
    forecasts     = nested_forecasts # only this chunk's IDs
  )
  
  log_out("Saving results … (atomic write)")
  save(bmi_pred_data_chunk, file = tmp_rds, compress = FALSE)
  ok <- tryCatch(file.rename(tmp_rds, out_rds), warning = identity, error = identity)
  if (!isTRUE(ok)) warning("file.rename(tmp_rds, out_rds) did not return TRUE; investigate.")
  file.create(doneflag)
  sz <- tryCatch(file.size(out_rds), error = function(e) NA_real_)
  elapsed_total <- as.numeric(difftime(Sys.time(), .t0, units = "secs"))
  cat(sprintf("[%s] ✅ Wrote: %s | size=%s bytes | total elapsed=%s | warnings=%d\n",
              format(Sys.time(), "%F %T"),
              out_rds,
              ifelse(is.na(sz), "NA", format(sz, big.mark=",")),
              fmt_dur(elapsed_total),
              .warn_count))
  flush.console()
  
}, warning = function(w) {
  .warn_count <<- .warn_count + 1L
  log_out(sprintf("WARNING: %s", conditionMessage(w)))
  invokeRestart("muffleWarning")
})