#!/usr/bin/env Rscript
# 01_costs.R — Compute per-trip cost columns from Google routing data + city cost params
#
# Inputs:  data/raw/df_google.rds, data/raw/CFS_MTL.rds, data/cost_params/*.csv
# Outputs: data/intermediate/df_final_cost.rds

if (!exists("ROOT")) ROOT <- dirname(getwd())

lib <- "/opt/tools/r-libs"
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(geosphere)
})

cat("=== Stage 1: Cost Computation ===\n")

df_path <- file.path(ROOT, "data/raw/df_google.rds")
stopifnot(file.exists(df_path))
df <- readRDS(df_path)
cat(sprintf("  Loaded df_google.rds: %d trips\n", nrow(df)))

# Derive CFS (Forward Sortation Area) and MTL flag if not already present
if (!"CFS" %in% names(df)) df$CFS <- substr(df$postalcode, 1, 3)
if (!"MTL" %in% names(df)) {
  CFS_MTL <- readRDS(file.path(ROOT, "data/raw/CFS_MTL.rds"))
  df$MTL <- as.integer(df$CFS %in% CFS_MTL)
}

# Filter to MTL (ARTM region — has km_driving from Google)
df <- df[df$MTL == 1, ]
cat(sprintf("  After MTL filter: %d trips\n", nrow(df)))

# Distance to downtown Montreal
df$d_distance <- round(distHaversine(
  p1 = cbind(df$d_orilon, df$d_orilat),
  p2 = c(-73.5665923228074, 45.50022929896841)
) / 1000, 1)

# Time gains
df$timegain_bicycling  <- df$time_driving - df$time_bicycling
df$timegain_transit    <- df$time_driving - df$time_transit
df$timegain_walking    <- df$time_driving - df$time_walking
df$timeconggain_bicycling <- df$timecong_driving - df$time_bicycling
df$timeconggain_transit   <- df$timecong_driving - df$time_transit
df$timeconggain_walking   <- df$timecong_driving - df$time_walking
df$time_ebike <- (df$km_bicycling / ((60 * df$km_bicycling / df$time_bicycling + 2))) * 60
df$timeconggain_ebike <- df$timecong_driving - df$time_ebike

# Cost functions
cout_deplacement <- function(temps) {
  taux_horaire <- 28.0; jour_annee <- 250.0; volonte <- 0.5
  return(taux_horaire * jour_annee * volonte * temps / 60 * 2.0)
}
cout_ges <- function(km, time_cong) {
  vitesse <- km / (time_cong / 60); jour_annee <- 250
  return((2.561E-04 * vitesse^2 + 0.04043 * vitesse + 2.427) / (vitesse + 0.4838) * km * 2 * jour_annee)
}
cout_utilisation <- function(km, time) {
  vitesse <- km / (time / 60); jour_annee <- 250; inflation <- 1.1487
  return((0.0003049 * vitesse^2 + 0.2042 * vitesse + 3.138) / (vitesse + 0.4714) * km * 2 * jour_annee * inflation)
}
cout_congestion <- function(km, time, time_cong) {
  temps_sup <- cout_deplacement(time_cong) - cout_deplacement(time)
  utilisation_sup <- cout_utilisation(km, time_cong) - cout_utilisation(km, time)
  return(temps_sup + utilisation_sup)
}
benefice_sante <- function(km, type) {
  if (type == 0) return(-181.395373 * km * 2) else return(-338.1947632 * km * 2)
}

# Load city-level cost parameters
cp <- file.path(ROOT, "data/cost_params")
df_auto   <- as.data.frame(read_csv(file.path(cp, "auto_data.csv"), show_col_types = FALSE))
df_CFS    <- as.data.frame(read_csv(file.path(cp, "CFS_ville.csv"), show_col_types = FALSE))
df_marche <- as.data.frame(read_csv(file.path(cp, "marche_data.csv"), show_col_types = FALSE))
df_TC     <- as.data.frame(read_csv(file.path(cp, "TC_data.csv"), show_col_types = FALSE))
df_velo   <- as.data.frame(read_csv(file.path(cp, "velo_data.csv"), show_col_types = FALSE))

df_auto <- df_auto %>% rename(Name = city)
df_auto$auto_prive   <- df_auto$achat_auto + df_auto$frais_fixe_auto
df_auto$auto_interne <- df_auto$fed_auto + df_auto$qc_auto + df_auto$muni_auto
df_auto$auto_cache   <- df_auto$accident_auto + df_auto$ES_auto
df_marche <- df_marche %>% rename(Name = city)
df_marche$marche_prive   <- df_marche$frais_fixe_marche
df_marche$marche_interne <- df_marche$muni_marche
df_marche$marche_cache   <- df_marche$accident_marche + df_marche$ES_marche
df_TC <- df_TC %>% rename(Name = city)
df_TC$tc_prive   <- df_TC$utilisation_TC + df_TC$frais_fixe_TC
df_TC$tc_interne <- df_TC$cout_public_TC
df_TC$tc_cache   <- df_TC$GES_TC + df_TC$congestion_TC + df_TC$accident_TC + df_TC$ES_TC + df_TC$benefice_TC
df_velo <- df_velo %>% rename(Name = city)
df_velo$velo_prive   <- df_velo$achat_velo + df_velo$frais_fixe_velo
df_velo$velo_interne <- df_velo$fed_velo + df_velo$qc_velo + df_velo$muni_velo
df_velo$velo_cache   <- df_velo$accident_velo + df_velo$ES_velo

df_CFS <- df_CFS %>%
  left_join(df_auto, by = "Name") %>% left_join(df_marche, by = "Name") %>%
  left_join(df_TC, by = "Name") %>% left_join(df_velo, by = "Name")
temp <- colMeans(df_CFS[100:102, 3:49])
df_CFS[100, 3:49] <- temp
df_CFS[, "Name"] <- NULL
df_CFS <- df_CFS[1:100, ]
df <- df %>% left_join(df_CFS, by = c("CFS" = "CFS"))

# Per-mode costs
km <- df$km_driving; temps <- df$time_driving; temps_cong <- df$timecong_driving
fixe_prive <- df$auto_prive; fixe_indirect <- df$auto_interne; fixe_cache <- df$auto_cache
df$coutprive_driving   <- fixe_prive + cout_deplacement(temps) + cout_utilisation(km, temps)
df$coutexterne_driving <- cout_congestion(km, temps, temps_cong) + cout_ges(km, temps_cong) + fixe_cache
df$coutpublic_driving  <- fixe_indirect
df$coutsocial_driving  <- cout_congestion(km, temps, temps_cong) + cout_ges(km, temps_cong) + fixe_cache + fixe_indirect
df$coutges_driving     <- cout_ges(km, temps_cong)
df$coutcong_driving    <- cout_congestion(km, temps, temps_cong)
df$coutdeplacement_driving   <- cout_deplacement(temps)
df$coututilisation_driving   <- cout_utilisation(km, temps)
df$coutsante_driving   <- 0

km_tc <- df$km_transit; temps_tc <- df$time_transit
fixe_prive <- df$tc_prive; fixe_indirect <- df$tc_interne; fixe_cache <- df$tc_cache
df$coutprive_transit   <- fixe_prive + cout_deplacement(temps_tc)
df$coutexterne_transit <- fixe_cache
df$coutpublic_transit  <- fixe_indirect
df$coutsocial_transit  <- fixe_cache + fixe_indirect
df$coutges_transit     <- df$GES_TC
df$coutcong_transit    <- df$congestion_TC
df$coutdeplacement_transit <- cout_deplacement(temps_tc)
df$coututilisation_transit  <- df$utilisation_TC
df$coutsante_transit   <- df$benefice_TC

km_velo <- df$km_bicycling; temps_velo <- df$time_bicycling
fixe_prive <- df$velo_prive; fixe_indirect <- df$velo_interne; fixe_cache <- df$velo_cache
df$coutprive_bicycling   <- fixe_prive + cout_deplacement(temps_velo)
df$coutexterne_bicycling <- fixe_cache + benefice_sante(km_velo, 0)
df$coutpublic_bicycling  <- fixe_indirect
df$coutsocial_bicycling  <- fixe_cache + fixe_indirect + benefice_sante(km_velo, 0)
df$coutges_bicycling <- 0; df$coutcong_bicycling <- 0
df$coutdeplacement_bicycling <- cout_deplacement(temps_velo)
df$coututilisation_bicycling  <- 0
df$coutsante_bicycling <- benefice_sante(km_velo, 0)

km_marche <- df$km_walking; temps_marche <- df$time_walking
fixe_prive <- df$marche_prive; fixe_indirect <- df$marche_interne; fixe_cache <- df$marche_cache
df$coutprive_walking   <- fixe_prive + cout_deplacement(temps_marche)
df$coutexterne_walking <- fixe_cache + benefice_sante(km_marche, 1)
df$coutpublic_walking  <- fixe_indirect
df$coutsocial_walking  <- fixe_cache + fixe_indirect + benefice_sante(km_marche, 1)
df$coutges_walking <- 0; df$coutcong_walking <- 0
df$coutdeplacement_walking <- cout_deplacement(temps_marche)
df$coututilisation_walking  <- 0
df$coutsante_walking <- benefice_sante(km_marche, 1)

out_path <- file.path(ROOT, "data/intermediate/df_final_cost.rds")
saveRDS(df, out_path)
cat(sprintf("  Wrote df_final_cost.rds: %d trips, %d columns\n", nrow(df), ncol(df)))
cat("Stage 1 complete.\n\n")
