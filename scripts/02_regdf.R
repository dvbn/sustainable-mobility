#!/usr/bin/env Rscript
# 02_regdf.R — Merge routing + cost data, add MTL/ARTM flags, create regdf.rds
#
# Inputs:  data/raw/df_google.rds, data/intermediate/df_final_cost.rds, data/raw/CFS_MTL.rds
# Outputs: data/intermediate/regdf.rds (92,331 obs)

if (!exists("ROOT")) ROOT <- dirname(getwd())

lib <- "/opt/tools/r-libs"
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(geosphere)
})

cat("=== Stage 2: Build regdf.rds ===\n")

raw   <- file.path(ROOT, "data/raw")
inter <- file.path(ROOT, "data/intermediate")
stopifnot(file.exists(file.path(raw, "df_google.rds")),
          file.exists(file.path(inter, "df_final_cost.rds")),
          file.exists(file.path(raw, "CFS_MTL.rds")))

df1 <- readRDS(file.path(raw, "df_google.rds"))
df0 <- readRDS(file.path(inter, "df_final_cost.rds"))
df  <- df1 %>% left_join(df0, by = "ipere")

overlapping_cols <- setdiff(intersect(names(df1), names(df0)), "ipere")
for (col_name in overlapping_cols) {
  x_col <- paste0(col_name, ".x"); y_col <- paste0(col_name, ".y")
  if (y_col %in% names(df)) { df[[col_name]] <- df[[x_col]]; df[[x_col]] <- NULL; df[[y_col]] <- NULL }
}
rm(df0, df1)

df$CFS <- substr(df$postalcode, 1, 3)
df$timecong <- df$timecong_driving - df$time_driving
df$timegain_bicycling    <- df$time_driving - df$time_bicycling
df$timegain_transit      <- df$time_driving - df$time_transit
df$timegain_walking      <- df$time_driving - df$time_walking
df$timeconggain_bicycling <- df$timecong_driving - df$time_bicycling
df$timeconggain_transit   <- df$timecong_driving - df$time_transit
df$timeconggain_walking   <- df$timecong_driving - df$time_walking

CFS_MTL <- readRDS(file.path(raw, "CFS_MTL.rds"))
df <- rename(df, FSA = CFS)
df <- df %>% mutate(MTL = if_else(FSA %in% CFS_MTL, 1, 0), ARTM = if_else(!is.na(km_driving), 1, 0))
unique_pairs_df <- df %>% dplyr::select(d_orism, FSA, MTL, ARTM) %>% distinct(d_orism, FSA, MTL, ARTM)
unique_pairs_df <- rename(unique_pairs_df, sm = d_orism)
df$MTL_des <- 1 - as.integer(df$d_dessm %in% unique(unique_pairs_df$sm[unique_pairs_df$MTL == 0]))
df <- rename(df, CFS = FSA)

# Switcher clusters (target=5)
target <- 5
df$cluster2 <- 0
df$cluster2[(df$d_mode1 != 2 & df$d_mode_agg == "driving") & df$km_driving <= 5 &
            (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1
df$cluster3 <- 0
df$cluster3[(df$d_mode_agg == "driving") & df$km_driving <= 5 &
            (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1
df$cluster2_10km <- 0
df$cluster2_10km[(df$d_mode1 != 2 & df$d_mode_agg == "driving") & df$km_driving <= 10 &
                 (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1
df$cluster3_10km <- 0
df$cluster3_10km[(df$d_mode_agg == "driving") & df$km_driving <= 10 &
                 (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1
df$cluster2_7km <- 0
df$cluster2_7km[(df$d_mode1 != 2 & df$d_mode_agg == "driving") & df$km_driving <= 7 &
                (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1
df$cluster3_7km <- 0
df$cluster3_7km[(df$d_mode_agg == "driving") & df$km_driving <= 7 &
                (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1

df$driving <- 0; df$driving[(df$d_mode1 != 2 & df$d_mode_agg == "driving")] <- 1
df$driving2 <- 0; df$driving2[(df$d_mode_agg == "driving")] <- 1
df$transit <- 0; df$transit[(df$d_mode_agg == "transit")] <- 1
df$walking <- 0; df$walking[(df$d_mode_agg == "walking")] <- 1
df$bicycling <- 0; df$bicycling[(df$d_mode_agg == "bicycling")] <- 1

saveRDS(df, file.path(inter, "regdf.rds"))
cat(sprintf("  Wrote regdf.rds: %d obs, %d columns\n", nrow(df), ncol(df)))
cat("Stage 2 complete.\n\n")
