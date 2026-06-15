# Combines data/chunks/bmi_pred_data_chunk_*.RData into data/bmi_pred_data.RData
# Safe to run multiple times. Requires dplyr + purrr.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
})

# Optional: lock working dir (or just cd in your sbatch script)
# setwd("/home/k/kschaumberg/ABCD")

chunk_dir   <- "data/chunks"
out_path    <- "data/bmi_pred_data.RData"
force_flag  <- identical(Sys.getenv("FORCE_COMBINE", "0"), "1")
expected_n  <- suppressWarnings(as.integer(Sys.getenv("N_CHUNKS", NA)))

# ---- Locate chunk outputs ---------------------------------------------------
rdata_files <- sort(Sys.glob(file.path(chunk_dir, "bmi_pred_data_chunk_*.RData")))
done_files  <- sort(Sys.glob(file.path(chunk_dir, "bmi_pred_data_chunk_*.done")))

if (!length(rdata_files)) {
  stop("No chunk files found in ", chunk_dir, call. = FALSE)
}

if (!is.na(expected_n)) {
  if (length(done_files) < expected_n && !force_flag) {
    stop(
      sprintf("Only %d/%d chunks marked done. Set FORCE_COMBINE=1 to combine anyway.",
              length(done_files), expected_n),
      call. = FALSE
    )
  } else if (length(rdata_files) < expected_n) {
    message(sprintf("Warning: found %d RData files but expected %d.", length(rdata_files), expected_n))
  }
}

message(sprintf("Combining %d chunk file(s)...", length(rdata_files)))

# ---- Accumulators -----------------------------------------------------------
training_template <- NULL                # take from the first valid chunk
testing_acc <- list()                    # named list of data frames (Y1, Y2, Y3)
forecasts_acc <- list()                  # nested list: forecasts_acc[[tp]][[variant]] -> df

# ---- Combine loop -----------------------------------------------------------
for (f in rdata_files) {
  message(">> Loading: ", f)
  e <- new.env(parent = emptyenv())
  load(f, envir = e)
  
  if (!exists("bmi_pred_data_chunk", envir = e)) {
    message("   Skipping (missing 'bmi_pred_data_chunk').")
    next
  }
  x <- e$bmi_pred_data_chunk
  
  # Training: identical across chunks -> keep first non-NULL
  if (is.null(training_template) && !is.null(x$training_list)) {
    training_template <- x$training_list
  }
  
  # Testing: union rows per timepoint name
  if (!is.null(x$testing_list) && length(x$testing_list)) {
    for (nm in names(x$testing_list)) {
      df <- x$testing_list[[nm]]
      if (!is.null(df) && nrow(df)) {
        testing_acc[[nm]] <- bind_rows(testing_acc[[nm]], df)
      }
    }
  }
  
  # Forecasts: bind by timepoint -> variant
  if (!is.null(x$forecasts) && length(x$forecasts)) {
    for (tp in names(x$forecasts)) {
      inner <- x$forecasts[[tp]]
      if (is.null(inner) || !length(inner)) next
      if (is.null(forecasts_acc[[tp]])) forecasts_acc[[tp]] <- list()
      
      for (variant in names(inner)) {
        df <- inner[[variant]]
        if (!is.null(df) && nrow(df)) {
          forecasts_acc[[tp]][[variant]] <- bind_rows(forecasts_acc[[tp]][[variant]], df)
        }
      }
    }
  }
}

# ---- De-dup / tidy ----------------------------------------------------------
# Testing: distinct per timepoint list
testing_list <- map(testing_acc, ~distinct(.x))
if (!length(testing_list)) testing_list <- list()

# Forecasts: ensure empty lists are proper lists (not NULLs)
forecasts <- lapply(forecasts_acc, function(inner) {
  if (length(inner)) inner else list()
})

if (is.null(training_template)) {
  stop("No valid training_list found in any chunk.", call. = FALSE)
}

# ---- Final payload ----------------------------------------------------------
bmi_pred_data <- list(
  training_list = training_template,
  testing_list  = testing_list,
  forecasts     = forecasts
)

# ---- Save -------------------------------------------------------------------
dir.create("data", showWarnings = FALSE, recursive = TRUE)
# Compress = FALSE is faster and fine on shared FS; flip to "xz" if you want a smaller file.
save(bmi_pred_data, file = out_path, compress = FALSE)

message("✅ Wrote combined object: ", out_path)
