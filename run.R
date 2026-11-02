#!/usr/bin/env Rscript
# run.R — Master orchestrator for the Triple Win paper replication package
#
# Directory layout:
#   replication/
#   ├── run.R                  (this file)
#   ├── scripts/               (pipeline stages 00–05 + imputeR.R)
#   ├── data/
#   │   ├── raw/               (df_google.rds, CFS_MTL.rds, census/)
#   │   ├── cost_params/       (auto_data.csv, CFS_ville.csv, …)
#   │   └── intermediate/      (censusFSA*.rds, df_final_cost.rds, regdf.rds, …)
#   ├── tables/                (output: gains.tex, reg_modes.tex)
#   └── paper_tables/          (frozen reference tables from the published paper)
#
# Usage:
#   cd replication && Rscript run.R
#   Rscript run.R --from 5              # skip upstream, just produce tables
#   Rscript run.R --only 3              # rebuild only pred_df.rds
#   Rscript run.R --output-dir results  # write tables elsewhere

args <- commandArgs(trailingOnly = TRUE)

from_stage  <- 0
only_stage  <- -1
output_dir  <- NULL   # set after ROOT is determined

i <- 1
while (i <= length(args)) {
  if (args[i] == "--from" && i < length(args)) {
    from_stage <- as.integer(args[i + 1]); i <- i + 2
  } else if (args[i] == "--only" && i < length(args)) {
    only_stage <- as.integer(args[i + 1]); i <- i + 2
  } else if (args[i] == "--output-dir" && i < length(args)) {
    output_dir <- args[i + 1]; i <- i + 2
  } else {
    cat("Unknown argument:", args[i], "\n"); i <- i + 1
  }
}

# ROOT = the replication/ directory (parent of scripts/)
# Works whether invoked via source(), Rscript run.R, or Rscript /abs/path/run.R
ROOT <- tryCatch({
  # When source()'d, sys.frame(1)$ofile is set
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
}, error = function(e) {
  # When run via Rscript, parse the --file argument
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
  } else {
    getwd()
  }
})

if (is.null(output_dir)) output_dir <- file.path(ROOT, "tables")

scripts_dir <- file.path(ROOT, "scripts")
stopifnot(dir.exists(scripts_dir))

should_run <- function(stage) {
  if (only_stage >= 0) return(stage == only_stage)
  return(stage >= from_stage)
}

n_failures <- 0L

cat("==============================================================\n")
cat("  Triple Win Paper — Replication Package\n")
cat("==============================================================\n")
cat(sprintf("  ROOT directory:    %s\n", ROOT))
cat(sprintf("  Scripts directory: %s\n", scripts_dir))
cat(sprintf("  Output directory:  %s\n", output_dir))
if (only_stage >= 0) {
  cat(sprintf("  Running only stage %d\n", only_stage))
} else {
  cat(sprintf("  Running from stage %d\n", from_stage))
}
cat("==============================================================\n\n")

# Stage 0: Census
if (should_run(0)) {
  cat("[Stage 0] Census CSV processing...\n")
  tryCatch(source(file.path(scripts_dir, "00_census.R"), local = TRUE),
           error = function(e) { cat("  FAILED:", e$message, "\n"); n_failures <<- n_failures + 1L })
}

# Stage 1: Cost computation
if (should_run(1)) {
  cat("[Stage 1] Cost computation...\n")
  tryCatch(source(file.path(scripts_dir, "01_costs.R"), local = TRUE),
           error = function(e) { cat("  FAILED:", e$message, "\n"); n_failures <<- n_failures + 1L })
}

# Stage 2: regdf.rds
if (should_run(2)) {
  cat("[Stage 2] Build regdf.rds...\n")
  tryCatch(source(file.path(scripts_dir, "02_regdf.R"), local = TRUE),
           error = function(e) { cat("  FAILED:", e$message, "\n"); n_failures <<- n_failures + 1L })
}

# Stage 3: pred_df.rds
if (should_run(3)) {
  cat("[Stage 3] Build pred_df.rds...\n")
  tryCatch(source(file.path(scripts_dir, "03_pred_df.R"), local = TRUE),
           error = function(e) { cat("  FAILED:", e$message, "\n"); n_failures <<- n_failures + 1L })
}

# Stage 4: Imputation
if (should_run(4)) {
  cat("[Stage 4] Imputation (temp.rds)...\n")
  tryCatch(source(file.path(scripts_dir, "04_impute.R"), local = TRUE),
           error = function(e) { cat("  FAILED:", e$message, "\n"); n_failures <<- n_failures + 1L })
}

# Stage 5: Final analysis -> tables
if (should_run(5)) {
  cat("[Stage 5] Final analysis -> tables...\n")
  tryCatch({
    assign("output_dir", output_dir, envir = globalenv())
    source(file.path(scripts_dir, "05_tables.R"), local = FALSE)
  }, error = function(e) { cat("  FAILED:", e$message, "\n"); n_failures <<- n_failures + 1L })
}

cat("\n==============================================================\n")
cat("  Pipeline complete.\n")

# Verify outputs exist
if (should_run(5) || only_stage == 5) {
  gains_path <- file.path(output_dir, "gains.tex")
  reg_path   <- file.path(output_dir, "reg_modes.tex")
  if (file.exists(gains_path)) {
    cat(sprintf("  gains.tex:     %s\n", normalizePath(gains_path)))
  } else {
    cat("  WARNING: gains.tex was not produced\n")
  }
  if (file.exists(reg_path)) {
    cat(sprintf("  reg_modes.tex: %s\n", normalizePath(reg_path)))
  } else {
    cat("  WARNING: reg_modes.tex was not produced\n")
  }
}
cat("==============================================================\n")

if (n_failures > 0L) quit(status = 1)
