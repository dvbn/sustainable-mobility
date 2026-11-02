#!/usr/bin/env Rscript
# 04_impute.R — K-NN + ranger imputation for FSAs outside ARTM region
#
# Inputs:  data/intermediate/pred_df.rds, scripts/imputeR.R
# Outputs: data/intermediate/temp.rds

if (!exists("ROOT")) ROOT <- dirname(getwd())

lib <- "/opt/tools/r-libs"
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(VIM)
  library(magrittr)
  library(caret)
})

cat("=== Stage 4: Imputation (temp.rds) ===\n")

pred_path   <- file.path(ROOT, "data/intermediate/pred_df.rds")
impute_path <- file.path(ROOT, "scripts/imputeR.R")
stopifnot(file.exists(pred_path), file.exists(impute_path))

source(impute_path)
df <- readRDS(pred_path)
cat(sprintf("  Loaded pred_df.rds: %d FSAs\n", nrow(df)))

df_imputed <- df

# Fix random seed for reproducibility (VIM kNN + ranger are stochastic)
set.seed(42)

# 1. Impute travel distances, times, and switcher shares
dependent_vars <- c("km_driving", "time_driving", "timecong_driving",
                    "km_bicycling", "time_bicycling",
                    "km_transit", "time_transit",
                    "km_walking", "time_walking",
                    "cluster2", "cluster3")
scenario_vars <- c(
  paste0(c("km_driving", "time_driving", "timecong_driving",
           "km_transit", "time_transit",
           "km_bicycling", "time_bicycling",
           "km_walking", "time_walking",
           "part_driving", "part_transit", "part_bicycling", "part_walking",
           "nbpassengers"), "_0"),
  paste0(c("km_driving", "time_driving", "timecong_driving",
           "km_transit", "time_transit",
           "km_bicycling", "time_bicycling",
           "km_walking", "time_walking",
           "part_driving", "part_transit", "part_bicycling", "part_walking",
           "nbpassengers"), "_5"),
  paste0(c("km_driving", "time_driving", "timecong_driving",
           "km_transit", "time_transit",
           "km_bicycling", "time_bicycling",
           "km_walking", "time_walking",
           "part_driving", "part_transit", "part_bicycling", "part_walking",
           "nbpassengers"), "_7"),
  paste0(c("km_driving", "time_driving", "timecong_driving",
           "km_transit", "time_transit",
           "km_bicycling", "time_bicycling",
           "km_walking", "time_walking",
           "part_driving", "part_transit", "part_bicycling", "part_walking",
           "nbpassengers"), "_10")
)
dependent_vars <- c(dependent_vars, scenario_vars)

explanatory_vars <- c(
  "popdensity2016", "share_bachelor",
  "average_aftertax_income_of_households_in_2015_752_total",
  "average_age_of_the_population39_total",
  "X15_to_64_years36_total", "average_household_size58_total",
  "share_15_to_29_minutes", "share_30_to_44_minutes",
  "share_45_to_59_minutes", "share_60_minutes_and_over",
  "share_car_driver", "share_car_passenger",
  "share_public_transit", "share_within_csd"
)

results_df <- data.frame()
cat("  Imputing travel + switcher variables...\n")
for (dep_var in dependent_vars) {
  if (!dep_var %in% colnames(df)) { next }
  results <- tryCatch(
    validate_and_impute(df, dep_var, explanatory_vars),
    error = function(e) { cat(sprintf("    ERROR imputing %s: %s\n", dep_var, e$message)); NULL }
  )
  if (!is.null(results)) {
    df_imputed[[dep_var]] <- results$df[[paste(dep_var, "imp", sep = "_")]]
  }
}

# 2. Impute modal shares
cat("  Imputing modal shares...\n")
variable_config <- list(
  driving   = c("share_car_driver", "share_car_passenger", "X15_to_64_years36_total"),
  bicycling = c("share_bicycle", "share_car_driver", "X15_to_64_years36_total"),
  transit   = c("share_public_transit", "share_car_driver", "X15_to_64_years36_total"),
  walking   = c("share_walked", "share_car_driver", "X15_to_64_years36_total")
)
for (dep_var in names(variable_config)) {
  set.seed(123)
  expl_vars <- variable_config[[dep_var]]
  results <- tryCatch(
    validate_and_impute(df, dep_var, expl_vars),
    error = function(e) { cat(sprintf("    ERROR imputing %s: %s\n", dep_var, e$message)); NULL }
  )
  if (!is.null(results)) {
    df_imputed[[paste0(dep_var, "_imp")]] <- results$df[[paste(dep_var, "imp", sep = "_")]]
  }
}

total_imp <- rowSums(df_imputed[, grep("_imp$", names(df_imputed))], na.rm = TRUE)
for (mobility in c("driving", "bicycling", "transit", "walking")) {
  imp_col <- paste0(mobility, "_imp")
  if (imp_col %in% names(df_imputed) && any(total_imp > 0)) {
    df_imputed[[mobility]] <- df_imputed[[imp_col]] / total_imp
  }
}

# 3. Impute cost parameters
cat("  Imputing cost parameters...\n")
cost_vars <- c("auto_prive", "auto_cache", "auto_interne",
               "tc_prive", "tc_interne", "tc_cache",
               "coutges_transit", "coutcong_transit", "coututilisation_transit", "coutsante_transit",
               "marche_prive", "marche_interne", "marche_cache",
               "velo_prive", "velo_interne", "velo_cache")
cost_explanatory <- c(
  "popdensity2016", "share_bachelor",
  "average_aftertax_income_of_households_in_2015_752_total",
  "median_total_income_of_households_in_2015_742_total",
  "average_age_of_the_population39_total",
  "X15_to_64_years36_total", "average_household_size58_total",
  "share_car_driver", "share_car_passenger",
  "share_public_transit", "share_walked", "share_within_csd"
)
for (dep_var in cost_vars) {
  if (!dep_var %in% colnames(df)) { next }
  results <- tryCatch(
    validate_and_impute(df, dep_var, cost_explanatory),
    error = function(e) { cat(sprintf("    ERROR imputing %s: %s\n", dep_var, e$message)); NULL }
  )
  if (!is.null(results)) {
    df_imputed[[dep_var]] <- results$df[[paste(dep_var, "imp", sep = "_")]]
  }
}

df <- df_imputed
out_path <- file.path(ROOT, "data/intermediate/temp.rds")
saveRDS(df, out_path)
cat(sprintf("  Wrote temp.rds: %d FSAs, %d columns\n", nrow(df), ncol(df)))
cat("Stage 4 complete.\n\n")
