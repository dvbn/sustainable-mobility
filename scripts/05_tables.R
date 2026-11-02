#!/usr/bin/env Rscript
# 05_tables.R — Final analysis: gains.tex + reg_modes.tex
#
# Produces:
#   1. gains.tex     — Table 3: Impact of Modal Shifts on Transportation Costs
#   2. reg_modes.tex — Table 1: Regression Results (mode choice determinants)
#
# Key parameter: target=5 (max minutes slower the alternative can be vs driving
#   in congestion). This was the value used to produce the published tables.
#
# Inputs:  data/intermediate/pred_df.rds, data/intermediate/regdf.rds,
#          data/intermediate/temp.rds
# Outputs: tables/gains.tex, tables/reg_modes.tex

if (!exists("ROOT")) ROOT <- dirname(getwd())

if (!exists("output_dir")) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) >= 1) args[1] else file.path(ROOT, "tables")
}

lib <- "/opt/tools/r-libs"
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))

suppressPackageStartupMessages({
  library(broom)
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(ggplot2)
  library(scales)
  library(stargazer)
  library(FNN)
  library(tidyr)
})

pdf(NULL)  # suppress plot device
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
cat("=== Stage 5: Final Analysis ===\n")
cat("Output directory:", normalizePath(output_dir), "\n")

# ---------------------------------------------------------------------------
# 1. KNN Matching — create synthetic QC data for FSAs outside MMC
# ---------------------------------------------------------------------------
pred_df <- readRDS(file.path(ROOT, "data/intermediate/pred_df.rds"))

explanatory_vars <- c(
  "FSA", "popdensity2016", "share_bachelor",
  "average_aftertax_income_of_households_in_2015_752_total",
  "average_age_of_the_population39_total", "average_household_size58_total",
  "share_15_to_29_minutes", "share_30_to_44_minutes",
  "share_45_to_59_minutes", "share_60_minutes_and_over",
  "share_car_driver", "share_car_passenger", "share_public_transit",
  "share_walked", "share_within_csd"
)

group_0 <- na.omit(pred_df[pred_df$ARTM == 0, explanatory_vars])
group_1 <- na.omit(pred_df[pred_df$ARTM == 1 & pred_df$MTL == 0, explanatory_vars])
group_2 <- na.omit(pred_df[pred_df$MTL == 1, explanatory_vars])

group_0[, 2:15] <- as.data.frame(scale(group_0[, 2:15]))
group_1[, 2:15] <- as.data.frame(scale(group_1[, 2:15]))
group_2[, 2:15] <- as.data.frame(scale(group_2[, 2:15]))

knn_result_1_to_2 <- get.knnx(data = group_2[, 2:15], query = group_1[, 2:15], k = 1)
knn_result_0_to_1 <- get.knnx(data = group_2[, 2:15], query = group_0[, 2:15], k = 1)

results_1_to_2 <- data.frame(
  group_1_id = rep(rownames(group_1), each = 1),
  group_2_id = rownames(group_2)[as.vector(knn_result_1_to_2$nn.index)],
  distance   = as.vector(knn_result_1_to_2$nn.dist)
)
results_0_to_1 <- data.frame(
  group_0_id = rep(rownames(group_0), each = 1),
  group_1_id = rownames(group_1)[as.vector(knn_result_0_to_1$nn.index)],
  distance   = as.vector(knn_result_0_to_1$nn.dist)
)

results_1_to_2$group_0_id <- group_1[results_1_to_2$group_1_id, 1]
results_1_to_2$group_1_id <- group_2[results_1_to_2$group_2_id, 1]
results_0_to_1$group_0_id <- group_0[results_0_to_1$group_0_id, 1]
results_0_to_1$group_1_id <- group_1[results_0_to_1$group_1_id, 1]

# ---------------------------------------------------------------------------
# 2. Load survey data + join FSA infrastructure + build QC synthetic rows
# ---------------------------------------------------------------------------
df_FSA <- readRDS(file.path(ROOT, "data/intermediate/temp.rds"))
df     <- readRDS(file.path(ROOT, "data/intermediate/regdf.rds"))
df     <- rename(df, FSA = CFS)

# Select census/share + city cost params from temp.rds (drop travel/cost/scenario cols)
fsa_keep_patterns <- c("^FSA$", "population_2016", "population_2021", "^X0_to_14", "^X15_to_64",
  "^X65_years", "^landarea", "^share_", "^median_", "^number_of_employment",
  "^average_", "^worked_at_home", "^worked_outside", "^no_fixed_workplace",
  "^worked_at_usual", "^usual_place_of_work",
  "^auto_prive$", "^auto_interne$", "^auto_cache$",
  "^tc_prive$", "^tc_interne$", "^tc_cache$",
  "^velo_prive$", "^velo_interne$", "^velo_cache$",
  "^marche_prive$", "^marche_interne$", "^marche_cache$",
  "popdensity", "share_worker", "share_homeworker", "share_30_minutes")
fsa_keep <- grep(paste(fsa_keep_patterns, collapse = "|"), names(df_FSA), value = TRUE)
df_FSA <- df_FSA[, fsa_keep]

# Drop from regdf: derived time vars, intermediate mode vars, cost cols, cluster/modal flags
# (these will come from temp.rds or be recomputed)
drop_cols <- c(
  "d_distance", "timegain_bicycling", "timegain_transit", "timegain_walking",
  "timeconggain_bicycling", "timeconggain_transit", "timeconggain_walking",
  "time_ebike", "timeconggain_ebike", "km_auto", "time_auto", "time_cong_auto",
  "auto_prive", "auto_interne", "auto_cache", "km_marche", "time_marche",
  "marche_prive", "marche_interne", "marche_cache", "km_TC", "time_TC",
  "tc_prive", "tc_interne", "tc_cache", "km_velo", "time_velo",
  "velo_prive", "velo_interne", "velo_cache",
  "coutprive_driving", "coutexterne_driving", "coutpublic_driving",
  "coutsocial_driving", "coutges_driving", "coutcong_driving",
  "coutdeplacement_driving", "coututilisation_driving", "coutsante_driving",
  "coutprive_transit", "coutexterne_transit", "coutpublic_transit",
  "coutsocial_transit", "coutges_transit", "coutcong_transit",
  "coutdeplacement_transit", "coututilisation_transit", "coutsante_transit",
  "coutprive_bicycling", "coutexterne_bicycling", "coutpublic_bicycling",
  "coutsocial_bicycling", "coutges_bicycling", "coutcong_bicycling",
  "coutdeplacement_bicycling", "coututilisation_bicycling", "coutsante_bicycling",
  "coutprive_walking", "coutexterne_walking", "coutpublic_walking",
  "coutsocial_walking", "coutges_walking", "coutcong_walking",
  "coutdeplacement_walking", "coututilisation_walking", "coutsante_walking",
  "cluster2", "cluster3", "cluster2_10km", "cluster3_10km",
  "cluster2_7km", "cluster3_7km",
  "driving", "driving2", "transit", "walking", "bicycling"
)
df[, intersect(drop_cols, names(df))] <- NULL
df <- df %>% left_join(df_FSA, by = c("FSA" = "FSA"), copy = TRUE, keep = FALSE)

# Build synthetic QC rows from KNN matches
df_QC   <- data.frame()
FSAlist <- unique(group_0$FSA[!is.na(group_0$FSA)])
FSAlist <- FSAlist[grepl("^[JHG]", FSAlist)]

for (i in seq_along(FSAlist)) {
  FSAmatch <- results_0_to_1$group_1_id[which(results_0_to_1$group_0_id == FSAlist[i])]
  FSAmatch <- df[df$FSA %in% FSAmatch, ]
  FSAmatch$FSA <- FSAlist[i]
  df_QC <- rbind(df_QC, FSAmatch)
  df_QC$p_fexp[df_QC$FSA == FSAlist[i]] <-
    df_QC$p_fexp[df_QC$FSA == FSAlist[i]] *
    pred_df$population_20161_total[pred_df$FSA == FSAlist[i]] /
    unique(df_QC$population_20161_total[df_QC$FSA == FSAlist[i]])
}

df$ARTM      <- 1
df_QC$ARTM   <- 0
df_QC$MTL    <- 0
df_QC$MTL_des <- 0
df <- rbind(df, df_QC)

# ---------------------------------------------------------------------------
# 3. Population formula (census 2021)
#    Workers 15+ (employment recipients) + children 0-14
# ---------------------------------------------------------------------------
pred_df$pop <- pred_df$number_of_employment_income_recipients_aged_15_years_and_over_in_private_households_in_2020_100_data118_total_2021 +
  pred_df$population_20211_total_2021 * pred_df$X0_to_14_years35_total_2021 / 100

# IMPORTANT: compute populations BEFORE MTL recoding (MTL is still 0/1 here)
popmtl  <- sum(pred_df[pred_df$FSA %in% unique(df$FSA[df$MTL == 1]),          "pop"], na.rm = TRUE)
popartm <- sum(pred_df[pred_df$FSA %in% unique(df$FSA[df$ARTM == 1 & df$MTL == 0]), "pop"], na.rm = TRUE)
popqc   <- sum(pred_df[pred_df$FSA %in% unique(df$FSA[df$ARTM != 1]),         "pop"], na.rm = TRUE)

cat(sprintf("Population — MTL: %.0f  ARTM-suburbs: %.0f  QC-rest: %.0f  Total: %.0f\n",
            popmtl, popartm, popqc, popmtl + popartm + popqc))

# ---------------------------------------------------------------------------
# 4. Cost functions
# ---------------------------------------------------------------------------
cout_deplacement <- function(temps) {
  taux_horaire <- 28.0; jour_annee <- 250.0; volonte <- 0.5
  cout_temps <- taux_horaire * jour_annee * volonte
  return(cout_temps * temps / 60 * 2.0)
}

cout_ges2 <- function(km, time) {
  vitesse <- km / (time / 60); jour_annee <- 250
  return((2.561E-04 * vitesse^2 + 0.04043 * vitesse + 2.427) / (vitesse + 0.4838) * km * 2 * jour_annee)
}

cout_ges <- function(km, temps) {
  vitesse <- km / (temps / 60); jour_annee <- 250
  return((2.386E-04 * vitesse^2 + 0.03993 * vitesse + 2.374) / (vitesse + 0.4474) * km * 2 * jour_annee)
}

cout_taxe_carbone <- function(km, temps) {
  vitesse <- km / (temps / 60); jour_annee <- 250; taxe <- 0.14
  return((0.0002243 * vitesse^2 + 0.03645 * vitesse + 2.188) / (vitesse + 0.4488) * km * 2 * jour_annee * taxe)
}

cout_utilisation <- function(km, time) {
  vitesse <- km / (time / 60); jour_annee <- 250; inflation <- 1.1487
  return((0.0003049 * vitesse^2 + 0.2042 * vitesse + 3.138) / (vitesse + 0.4714) * km * 2 * jour_annee * inflation)
}

cout_congestion <- function(km, time, time_cong) {
  temps_sup       <- cout_deplacement(time_cong) - cout_deplacement(time)
  ges_supp        <- cout_ges2(km, time_cong) - cout_ges2(km, time)
  utilisation_sup <- cout_utilisation(km, time_cong) - cout_utilisation(km, time)
  return(temps_sup + ges_supp + utilisation_sup)
}

benefice_sante <- function(km, mode) {
  if (mode == "velo") {
    return(-181.395373 * km * 2)
  } else if (mode == "marche") {
    return(-338.1947632 * km * 2)
  } else {
    return(0)
  }
}

# ---------------------------------------------------------------------------
# 5. Per-mode cost calculations
# ---------------------------------------------------------------------------

# Transport mode indicators
df$driving <- 0
df$driving[(df$d_mode1 != 2 & df$d_mode_agg == "driving")] <- 1
df$driving2 <- 0
df$driving2[(df$d_mode_agg == "driving")] <- 1
df$driving_passenger <- 0
df$driving_passenger[(df$d_mode1 == 2 & df$d_mode_agg == "driving")] <- 1
df$driving_passenger[df$p_permis != 1 & df$driving == 1] <- 1
df$driving[df$p_permis != 1] <- 0
df$m_auto[df$m_auto == 0 & df$driving == 1] <- 1
df$transit <- 0;    df$transit[(df$d_mode_agg == "transit")] <- 1
df$walking <- 0;    df$walking[(df$d_mode_agg == "walking")] <- 1
df$bicycling <- 0;  df$bicycling[(df$d_mode_agg == "bicycling")] <- 1

# DRIVER costs
km <- df$km_driving; temps <- df$time_driving; temps_cong <- df$timecong_driving
fixe_prive <- df$auto_prive; fixe_indirect <- df$auto_interne; fixe_cache <- df$auto_cache
df$coutprive_driving   <- fixe_prive + cout_deplacement(temps_cong) + cout_utilisation(km, temps_cong)
df$coutexterne_driving <- cout_ges2(km, temps_cong) + fixe_cache
df$coutpublic_driving  <- fixe_indirect
df$coutsocial_driving  <- cout_ges2(km, temps_cong) + fixe_cache + fixe_indirect
df$coutges_driving     <- cout_ges(km, temps_cong)
df$ges_driving         <- cout_ges(km, temps_cong) / 472.67
df$coutcong_driving    <- cout_congestion(km, temps, temps_cong)
df$coutdeplacement_driving <- cout_deplacement(temps)
df$coututilisation_driving <- cout_utilisation(km, temps_cong)
df$coutsante_driving   <- 0

# PASSENGER costs
df$coutprive_passenger   <- cout_deplacement(temps)
df$coutexterne_passenger <- cout_deplacement(temps_cong) - cout_deplacement(temps)
df$coutpublic_passenger  <- fixe_indirect
df$coutsocial_passenger  <- cout_congestion(km, temps, temps_cong)
df$coutges_passenger     <- 0; df$ges_passenger <- 0
df$coutcong_passenger    <- cout_deplacement(temps_cong) - cout_deplacement(temps)
df$coutdeplacement_passenger <- cout_deplacement(temps)
df$coututilisation_passenger <- 0; df$coutsante_passenger <- 0

# TRANSIT costs
df$congestion_TC[is.na(df$congestion_TC)] <- median(df$congestion_TC, na.rm = TRUE)
df$GES_TC[is.na(df$GES_TC)]               <- median(df$GES_TC, na.rm = TRUE)
df$utilisation_TC[is.na(df$utilisation_TC)] <- median(df$utilisation_TC, na.rm = TRUE)
df$benefice_TC[is.na(df$benefice_TC)]       <- median(df$benefice_TC, na.rm = TRUE)
km_tc <- df$km_transit; temps_tc <- df$time_transit
fixe_prive <- df$tc_prive; fixe_indirect <- df$tc_interne; fixe_cache <- df$tc_cache
df$coutprive_transit   <- fixe_prive + cout_deplacement(temps_tc)
df$coutexterne_transit <- fixe_cache
df$coutpublic_transit  <- fixe_indirect
df$coutsocial_transit  <- fixe_cache + fixe_indirect
df$coutges_transit     <- df$GES_TC
df$ges_transit         <- df$GES_TC / 472.67
df$coutcong_transit    <- df$congestion_TC
df$coutdeplacement_transit <- cout_deplacement(temps_tc)
df$coututilisation_transit  <- df$utilisation_TC
df$coutsante_transit   <- df$benefice_TC

# BICYCLING costs
km_velo <- df$km_bicycling; temps_velo <- df$time_bicycling
fixe_prive <- df$velo_prive; fixe_indirect <- df$velo_interne; fixe_cache <- df$velo_cache
df$coutprive_bicycling   <- fixe_prive + cout_deplacement(temps_velo)
df$coutexterne_bicycling <- fixe_cache + benefice_sante(km_velo, "velo")
df$coutpublic_bicycling  <- fixe_indirect
df$coutsocial_bicycling  <- fixe_cache + fixe_indirect + benefice_sante(km_velo, "velo")
df$coutges_bicycling     <- 0; df$ges_bicycling <- 0; df$coutcong_bicycling <- 0
df$coutdeplacement_bicycling <- cout_deplacement(temps_velo)
df$coututilisation_bicycling  <- 0
df$coutsante_bicycling   <- benefice_sante(km_velo, "velo")

# WALKING costs
km_marche <- df$km_walking; temps_marche <- df$time_walking
fixe_prive <- df$marche_prive; fixe_indirect <- df$marche_interne; fixe_cache <- df$marche_cache
df$coutprive_walking   <- fixe_prive + cout_deplacement(temps_marche)
df$coutexterne_walking <- fixe_cache + benefice_sante(km_marche, "marche")
df$coutpublic_walking  <- fixe_indirect
df$coutsocial_walking  <- fixe_cache + fixe_indirect + benefice_sante(km_marche, "marche")
df$coutges_walking     <- 0; df$ges_walking <- 0; df$coutcong_walking <- 0
df$coutdeplacement_walking <- cout_deplacement(temps_marche)
df$coututilisation_walking  <- 0
df$coutsante_walking   <- benefice_sante(km_marche, "marche")

# ---------------------------------------------------------------------------
# 6. Time gains and best-alternative mode
# ---------------------------------------------------------------------------
df$timegain_bicycling    <- df$time_driving - df$time_bicycling
df$timegain_transit      <- df$time_driving - df$time_transit
df$timegain_walking      <- df$time_driving - df$time_walking
df$timeconggain_bicycling <- df$timecong_driving - df$time_bicycling
df$timeconggain_transit   <- df$timecong_driving - df$time_transit
df$timeconggain_walking   <- df$timecong_driving - df$time_walking
df$time_ebike <- (df$km_bicycling / ((60 * df$km_bicycling / df$time_bicycling + 2))) * 60
df$timeconggain_ebike     <- df$timecong_driving - df$time_ebike

df$time_min      <- suppressWarnings(
  apply(df[, c("time_bicycling", "time_walking", "time_transit")], 1, min, na.rm = TRUE)
)
df$min_time_index <- as.factor(apply(df[, c("time_bicycling", "time_walking", "time_transit")], 1,
                                      function(x) which.min(replace(x, is.na(x), Inf))))

# ---------------------------------------------------------------------------
# 7. Modal shift simulation — target = 5 minutes
# ---------------------------------------------------------------------------
target <- 5  # THE key parameter: alternative can be up to 5 min slower than driving-in-congestion

for (km_thresh in c(5, 7, 10)) {
  sfx <- paste0("_", km_thresh, "km")

  # cluster2: drivers who can switch (drive <= threshold km, alt within target min)
  c2 <- rep(0L, nrow(df))
  c2[df$driving == 1 & df$km_driving <= km_thresh &
     (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1L
  df[[paste0("cluster2", sfx)]] <- c2

  # cluster3: all car users (driver + passenger) who can switch
  c3 <- rep(0L, nrow(df))
  c3[(df$d_mode_agg == "driving") & df$km_driving <= km_thresh &
     (df$timeconggain_bicycling > -target | df$timeconggain_walking > -target | df$timeconggain_transit > -target)] <- 1L
  df[[paste0("cluster3", sfx)]] <- c3

  # Counterfactual mode assignments
  df[[paste0("driving", sfx)]]           <- as.numeric(df$driving == 1 & (c3 == 0))
  df[[paste0("driving_passenger", sfx)]] <- as.numeric(df$driving_passenger & (c3 == 0))
  df[[paste0("bicycling", sfx)]]         <- as.numeric(df$d_mode_agg == "bicycling" | (df$min_time_index == 1) * (c3 == 1))
  df[[paste0("walking", sfx)]]           <- as.numeric(df$d_mode_agg == "walking"   | (df$min_time_index == 2) * (c3 == 1))
  df[[paste0("transit", sfx)]]           <- as.numeric(df$d_mode_agg == "transit"   | (df$min_time_index == 3) * (c3 == 1))

  # Counterfactual costs
  for (cost in c("coutprive", "coutpublic", "coutexterne")) {
    df[[paste0(cost, sfx)]] <-
      df[[paste0(cost, "_driving")]]   * df[[paste0("driving", sfx)]] +
      df[[paste0(cost, "_passenger")]] * df[[paste0("driving_passenger", sfx)]] +
      df[[paste0(cost, "_bicycling")]] * df[[paste0("bicycling", sfx)]] +
      df[[paste0(cost, "_walking")]]   * df[[paste0("walking", sfx)]] +
      df[[paste0(cost, "_transit")]]   * df[[paste0("transit", sfx)]]
  }
  df[[paste0("totalcost", sfx)]] <- df[[paste0("coutprive", sfx)]] + df[[paste0("coutpublic", sfx)]] + df[[paste0("coutexterne", sfx)]]
  df[[paste0("coutges", sfx)]]   <- df$coutges_driving * df[[paste0("driving", sfx)]] + df$coutges_transit * df[[paste0("transit", sfx)]]
  df[[paste0("quantiteges", sfx)]] <- df$ges_driving * df[[paste0("driving", sfx)]] + df$ges_transit * df[[paste0("transit", sfx)]]
  df[[paste0("coutsante", sfx)]] <- df$coutsante_bicycling * df[[paste0("bicycling", sfx)]] +
    df$coutsante_walking * df[[paste0("walking", sfx)]] + df$coutsante_transit * df[[paste0("transit", sfx)]]
}

# Baseline (business as usual) costs
df$coutprive   <- df$coutprive_driving * df$driving + df$coutprive_passenger * df$driving_passenger + df$coutprive_bicycling * df$bicycling + df$coutprive_walking * df$walking + df$coutprive_transit * df$transit
df$coutpublic  <- df$coutpublic_driving * df$driving + df$coutpublic_passenger * df$driving_passenger + df$coutpublic_bicycling * df$bicycling + df$coutpublic_walking * df$walking + df$coutpublic_transit * df$transit
df$coutexterne <- df$coutexterne_driving * df$driving + df$coutexterne_passenger * df$driving_passenger + df$coutexterne_bicycling * df$bicycling + df$coutexterne_walking * df$walking + df$coutexterne_transit * df$transit
df$totalcost   <- df$coutprive + df$coutpublic + df$coutexterne
df$coutges     <- df$coutges_driving * df$driving + df$coutges_transit * df$transit
df$quantiteges <- df$ges_driving * df$driving + df$ges_transit * df$transit
df$coutsante   <- df$coutsante_bicycling * df$bicycling + df$coutsante_walking * df$walking + df$coutsante_transit * df$transit

# Purchase costs (for "achat" row)
df$achat     <- df$auto_prive * df$driving + df$marche_prive * df$walking + (df$tc_prive - df$utilisation_TC) * df$transit + df$velo_prive * df$bicycling
df$achat_5km <- df$auto_prive * df$driving_5km + df$marche_prive * df$walking_5km + (df$tc_prive - df$utilisation_TC) * df$transit_5km + df$velo_prive * df$bicycling_5km
df$achat_7km <- df$auto_prive * df$driving_7km + df$marche_prive * df$walking_7km + (df$tc_prive - df$utilisation_TC) * df$transit_7km + df$velo_prive * df$bicycling_7km
df$achat_10km <- df$auto_prive * df$driving_10km + df$marche_prive * df$walking_10km + (df$tc_prive - df$utilisation_TC) * df$transit_10km + df$velo_prive * df$bicycling_10km

# ---------------------------------------------------------------------------
# 8. Control variable recoding
# ---------------------------------------------------------------------------
df <- df %>% mutate(
  m_pers = case_when(m_pers == 1 ~ "1", m_pers == 2 ~ "2", m_pers >= 3 ~ "3+", TRUE ~ as.character(m_pers)),
  m_auto = case_when(m_auto == 0 ~ "0", m_auto == 1 ~ "1", m_auto >= 2 ~ "2+", TRUE ~ as.character(m_pers)),
  p_grage = case_when(
    p_age < 18              ~ "0-17",
    p_age >= 18 & p_age <= 34 ~ "18-34",
    p_age >= 35 & p_age <= 54 ~ "35-54",
    p_age > 54              ~ ">54",
    TRUE ~ as.character(p_age)
  ),
  p_statut = case_when(
    p_statut != 3 & p_statut != 6 ~ "workers",
    p_statut == 3 | p_statut == 6 ~ "students",
    TRUE ~ as.character(p_statut)
  ),
  MTL = case_when(
    MTL == 1 & MTL_des == 1 ~ "MTL -> MTL",
    MTL == 1 & MTL_des == 0 ~ "MTL -> Out",
    MTL == 0 & MTL_des == 1 ~ "Out -> MTL",
    MTL == 0 & MTL_des == 0 ~ "Out -> Out",
    TRUE ~ as.character(p_statut)
  )
)

# ---------------------------------------------------------------------------
# 9. Regression table (reg_modes.tex)
# ---------------------------------------------------------------------------
dependent_vars_0 <- c("driving", "driving_passenger", "km_driving", "transit", "bicycling", "walking")

models <- lapply(dependent_vars_0, function(dep_var) {
  formula <- as.formula(paste(dep_var, "~ -1 + p_grage + p_statut + p_sexe + m_pers + m_auto + MTL + factor(FSA)"))
  lm(formula, data = df[df$ARTM == 1, ], weights = p_fexp)
})

robust_se <- lapply(models, function(model) sqrt(diag(vcovHC(model, type = "HC1"))))

reg_modes_path <- file.path(output_dir, "reg_modes.tex")
tex_output <- tryCatch(
  capture.output(stargazer(
    models,
    title = "Regression Results",
    type = "latex",
    se = robust_se,
    omit = "factor\\(FSA\\)",
    dep.var.labels = dependent_vars_0,
    notes = "Robust standard errors in parentheses.",
    notes.align = "l"
  )),
  error = function(e) character(0)
)
if (length(tex_output) > 0) {
  tex_output <- tex_output[!grepl("^% Date and time:", tex_output)]
  writeLines(tex_output, reg_modes_path)
  cat("Wrote:", reg_modes_path, "\n")
} else {
  warning("Could not produce reg_modes.tex")
}

# ---------------------------------------------------------------------------
# 10. Population-scaled aggregate costs (MCC and Quebec)
# ---------------------------------------------------------------------------
compute_all_totals <- function(df, base_varnames, suffixes, denom = 1e6) {
  results <- list()
  for (base_var in base_varnames) {
    for (suffix in suffixes) {
      var <- paste0(base_var, suffix)
      if (!var %in% colnames(df)) { warning(paste("Variable", var, "not found. Skipping.")); next }
      model1 <- lm(as.formula(paste(var, "~1")), data = df[df$MTL %in% c("MTL -> MTL", "MTL -> Out"), ], weights = p_fexp)
      model2 <- lm(as.formula(paste(var, "~1")), data = df[df$MTL %in% c("Out -> MTL") | df$MTL != "Out -> Out", ], weights = p_fexp)
      model3 <- lm(as.formula(paste(var, "~1")), data = df[df$ARTM == 0, ], weights = p_fexp)
      total  <- (popmtl * coef(model1) + popartm * coef(model2) + popqc * coef(model3)) / denom
      results[[var]] <- list(total = total)
    }
  }
  do.call(rbind, lapply(names(results), function(v) data.frame(Variable = v, Total = results[[v]]$total)))
}

compute_mcc_totals <- function(df, base_varnames, suffixes, denom = 1e6) {
  results <- list()
  for (base_var in base_varnames) {
    for (suffix in suffixes) {
      var <- paste0(base_var, suffix)
      if (!var %in% colnames(df)) { warning(paste("Variable", var, "not found. Skipping.")); next }
      model1 <- lm(as.formula(paste(var, "~1")), data = df[df$MTL %in% c("MTL -> MTL", "MTL -> Out"), ], weights = p_fexp)
      model2 <- lm(as.formula(paste(var, "~1")), data = df[df$MTL %in% c("Out -> MTL") | df$MTL != "Out -> Out", ], weights = p_fexp)
      total  <- (popmtl * coef(model1) + popartm * coef(model2)) / denom
      results[[var]] <- list(total = total)
    }
  }
  do.call(rbind, lapply(names(results), function(v) data.frame(Variable = v, Total = results[[v]]$total)))
}

base_varnames <- c("quantiteges", "totalcost", "coutprive", "coutpublic", "coutexterne", "coutsante", "achat")
suffixes      <- c("", "_5km", "_7km", "_10km")

results_df    <- compute_all_totals(df, base_varnames, suffixes, denom = 1e6)
results_dfmcc <- compute_mcc_totals(df, base_varnames, suffixes, denom = 1e6)
results_df$Total2 <- results_dfmcc$Total

# ---------------------------------------------------------------------------
# 11. Reshape into gains table (scenario - baseline)
# ---------------------------------------------------------------------------
results_df <- results_df %>%
  mutate(
    BaseVariable = gsub("(_5km|_7km|_10km)$", "", Variable),
    Suffix       = sub("^[^_]*", "", Variable)
  )
results_df$Suffix[seq(1, 28, by = 4)] <- "_0km"

results_df_wide <- results_df %>%
  pivot_wider(id_cols = BaseVariable, names_from = Suffix, values_from = c(Total2, Total))

results_df_wide0 <- results_df_wide
results_df_wide0[, 2:5] <- results_df_wide0[, 2]
results_df_wide0[, 6:9] <- results_df_wide0[, 6]
results_df_wide_diff <- results_df_wide0
results_df_wide_diff[, 2:9] <- round(results_df_wide[, 2:9] - results_df_wide0[, 2:9], 2)
results_df_wide_perc <- results_df_wide0
results_df_wide_perc[, 2:9] <- round((results_df_wide[, 2:9] - results_df_wide0[, 2:9]) / results_df_wide0[, 2:9], 3)
results_df_wide_diff[, c(2, 6)] <- NULL
results_df_wide_perc[, c(2, 6)] <- NULL

cat("\n========== GAINS (results_df_wide_diff) ==========\n")
print(results_df_wide_diff)
cat("\n========== % CHANGE (results_df_wide_perc) ==========\n")
print(results_df_wide_perc)

# ---------------------------------------------------------------------------
# 12. Export gains.tex
# ---------------------------------------------------------------------------
# Helper: format number with comma separators, rounded to nearest 10
fmt <- function(x) formatC(round(x, -1), format = "f", digits = 0, big.mark = ",")
# Helper: format percentage
pct <- function(x) paste0("(", formatC(round(x * 100, 1), format = "f", digits = 1), "\\%)")

# Extract values by row name
get_row <- function(base_var) {
  idx <- which(results_df_wide_diff$BaseVariable == base_var)
  d   <- as.numeric(results_df_wide_diff[idx, 2:7])
  p   <- as.numeric(results_df_wide_perc[idx, 2:7])
  list(d = d, p = p)
}

tc  <- get_row("totalcost")
cp  <- get_row("coutprive")
cpu <- get_row("coutpublic")
ce  <- get_row("coutexterne")
cs  <- get_row("coutsante")
co2 <- get_row("quantiteges")
ac  <- get_row("achat")

# Marginal private cost = coutprive - achat (operating cost, not purchase)
# Compute from exact diff values; percentage uses exact baseline marginal
mp_d <- cp$d - ac$d
idx_cp0 <- which(results_df_wide0$BaseVariable == "coutprive")
idx_ac0 <- which(results_df_wide0$BaseVariable == "achat")
base_marg_all <- as.numeric(results_df_wide0[idx_cp0, 2:9]) -
                 as.numeric(results_df_wide0[idx_ac0, 2:9])
# After column deletion, 6 remaining cells are positions 2,3,4,6,7,8 of the original 8
mp_p <- mp_d / base_marg_all[c(2, 3, 4, 6, 7, 8)]

gains_tex <- paste0(
  "% Table created by 05_tables.R (replication package)\n",
  "\\begin{table}[H] \\centering \n",
  "\\footnotesize \\singlespacing\n",
  "  \\caption{Impact of Modal Shifts on Transportation Costs and Emissions}\n",
  " \\label{table:gains} \n",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccc} \n",
  "\\\\[-1.8ex]\\hline \n",
  "\\hline \\\\[-1.8ex] \n",
  "& \\multicolumn{3}{c}{MMC} & \\multicolumn{3}{c}{Quebec} \\\\ \n",
  "\\cline{2-4} \\cline{5-7} \n",
  " & 5km & 7km & 10km & 5km & 7km & 10km \\\\ \n",
  "\\hline \\\\[-1.8ex] \n",
  sprintf("Total cost (M\\$) & %s & %s & %s & %s & %s & %s \\\\ \n",
          fmt(tc$d[1]), fmt(tc$d[2]), fmt(tc$d[3]), fmt(tc$d[4]), fmt(tc$d[5]), fmt(tc$d[6])),
  sprintf(" & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(tc$p[1]), pct(tc$p[2]), pct(tc$p[3]), pct(tc$p[4]), pct(tc$p[5]), pct(tc$p[6])),
  "\\hline\n",
  sprintf("Private cost (M\\$) & %s & %s & %s & %s & %s & %s \\\\ \n",
          fmt(cp$d[1]), fmt(cp$d[2]), fmt(cp$d[3]), fmt(cp$d[4]), fmt(cp$d[5]), fmt(cp$d[6])),
  sprintf(" & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(cp$p[1]), pct(cp$p[2]), pct(cp$p[3]), pct(cp$p[4]), pct(cp$p[5]), pct(cp$p[6])),
  sprintf("$\\quad$ \\textit{Marg. Priv. Cost (M\\$)} & %s & %s & %s & %s & %s & %s \\\\ \n",
          fmt(mp_d[1]), fmt(mp_d[2]), fmt(mp_d[3]), fmt(mp_d[4]), fmt(mp_d[5]), fmt(mp_d[6])),
  sprintf(" & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(mp_p[1]), pct(mp_p[2]), pct(mp_p[3]), pct(mp_p[4]), pct(mp_p[5]), pct(mp_p[6])),
  sprintf("Public cost (M\\$) & %s & %s & %s & %s & %s & %s \\\\ \n",
          fmt(cpu$d[1]), fmt(cpu$d[2]), fmt(cpu$d[3]), fmt(cpu$d[4]), fmt(cpu$d[5]), fmt(cpu$d[6])),
  sprintf(" & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(cpu$p[1]), pct(cpu$p[2]), pct(cpu$p[3]), pct(cpu$p[4]), pct(cpu$p[5]), pct(cpu$p[6])),
  sprintf("External cost (M\\$) & %s & %s & %s & %s & %s & %s \\\\ \n",
          fmt(ce$d[1]), fmt(ce$d[2]), fmt(ce$d[3]), fmt(ce$d[4]), fmt(ce$d[5]), fmt(ce$d[6])),
  sprintf(" & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(ce$p[1]), pct(ce$p[2]), pct(ce$p[3]), pct(ce$p[4]), pct(ce$p[5]), pct(ce$p[6])),
  sprintf("$\\quad$ \\textit{Health cost (M\\$)} & %s & %s & %s & %s & %s & %s \\\\ \n",
          fmt(cs$d[1]), fmt(cs$d[2]), fmt(cs$d[3]), fmt(cs$d[4]), fmt(cs$d[5]), fmt(cs$d[6])),
  sprintf(" & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(cs$p[1]), pct(cs$p[2]), pct(cs$p[3]), pct(cs$p[4]), pct(cs$p[5]), pct(cs$p[6])),
  # CO2 row: divide by 472.67 already done in quantiteges, values are in Mt
  sprintf("$\\quad$  \\textit{CO$_2$ (Mt)} & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f \\\\ \n",
          co2$d[1], co2$d[2], co2$d[3], co2$d[4], co2$d[5], co2$d[6]),
  sprintf(" & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(co2$p[1]), pct(co2$p[2]), pct(co2$p[3]), pct(co2$p[4]), pct(co2$p[5]), pct(co2$p[6])),
  "% \\hline\n",
  sprintf("%% Achat (M\\$) & %s & %s & %s & %s & %s & %s \\\\ \n",
          fmt(ac$d[1]), fmt(ac$d[2]), fmt(ac$d[3]), fmt(ac$d[4]), fmt(ac$d[5]), fmt(ac$d[6])),
  sprintf("%%  & %s & %s & %s & %s & %s & %s \\\\ \n",
          pct(ac$p[1]), pct(ac$p[2]), pct(ac$p[3]), pct(ac$p[4]), pct(ac$p[5]), pct(ac$p[6])),
  "\\hline \\\\[-1.8ex] \n",
  "\\end{tabular} \n",
  "\\end{table} "
)

gains_path <- file.path(output_dir, "gains.tex")
writeLines(gains_tex, gains_path)
cat("Wrote:", gains_path, "\n")

invisible(dev.off())
cat("Stage 5 complete.\n\n")
