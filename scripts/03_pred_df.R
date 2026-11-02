#!/usr/bin/env Rscript
# 03_pred_df.R — FSA-level aggregation + census merge → pred_df.rds
#
# Translated directly from MainProgram.Rmd chunks:
#   aggregate_weighted_cfs (lines 956-1044) + census merge (lines 1046-1356)
#
# Inputs:  data/intermediate/regdf.rds, data/intermediate/censusFSA2016.rds,
#          data/intermediate/censusFSA2021.rds, data/raw/CFS_MTL.rds,
#          data/raw/landarea_fsa.rds
# Outputs: data/intermediate/pred_df.rds

if (!exists("ROOT")) ROOT <- dirname(getwd())

lib <- "/opt/tools/r-libs"
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
})

cat("=== Stage 3: Build pred_df.rds ===\n")

inter <- file.path(ROOT, "data/intermediate")
raw   <- file.path(ROOT, "data/raw")
stopifnot(file.exists(file.path(inter, "regdf.rds")))

df <- readRDS(file.path(inter, "regdf.rds"))
cat(sprintf("  Loaded regdf.rds: %d obs\n", nrow(df)))

# ---------------------------------------------------------------------------
# Part A: Compute FSA-level weighted averages from regdf
# ---------------------------------------------------------------------------
# The Rmd reads regdf.rds (which has CFS column), then does weighted FSA-level
# aggregation for baseline + 3 scenario variants (5km, 7km, 10km).

# Baseline time/cost variables
df$time_min <- suppressWarnings(
  apply(df[, c("time_bicycling", "time_walking", "time_transit")], 1, min, na.rm = TRUE)
)
df$min_time_index <- apply(df[, c("time_bicycling", "time_walking", "time_transit")], 1,
                           function(x) which.min(replace(x, is.na(x), Inf)))

# Helper: compute FSA-level weighted df for a given scenario
compute_weighted <- function(df, target_km, target_min = 5) {
  if (!is.null(target_km)) {
    df$cluster3 <- 0
    df$cluster3[(df$d_mode_agg == "driving") & df$km_driving <= target_km &
                (df$timeconggain_bicycling > -target_min | df$timeconggain_walking > -target_min |
                 df$timeconggain_transit > -target_min)] <- 1
    df$part_bicycling <- (df$d_mode_agg == "bicycling" | (df$min_time_index == 1) * (df$cluster3 == 1))
    df$part_walking   <- (df$d_mode_agg == "walking"   | (df$min_time_index == 2) * (df$cluster3 == 1))
    df$part_driving   <- (df$d_mode_agg == "driving" & (df$cluster3 != 1))
    df$part_transit   <- (df$d_mode_agg == "transit"   | (df$min_time_index == 3) * (df$cluster3 == 1))
  }
  df$ratio <- df$part_driving
  wdf <- df %>%
    group_by(CFS) %>%
    summarise(
      CFS = first(CFS), comm_name = first(comm_name),
      across(c(km_driving, time_driving, timecong_driving),
             ~sum(.x * d_fexp * part_driving, na.rm = TRUE) / sum(d_fexp * part_driving, na.rm = TRUE)),
      across(c(km_transit, time_transit),
             ~sum(.x * d_fexp * part_transit, na.rm = TRUE) / sum(d_fexp * part_transit, na.rm = TRUE)),
      across(c(km_bicycling, time_bicycling),
             ~sum(.x * d_fexp * part_bicycling, na.rm = TRUE) / sum(d_fexp * part_bicycling, na.rm = TRUE)),
      across(c(km_walking, time_walking),
             ~sum(.x * d_fexp * part_walking, na.rm = TRUE) / sum(d_fexp * part_walking, na.rm = TRUE)),
      across(c(part_driving, part_transit, part_bicycling, part_walking),
             ~sum(.x * d_fexp, na.rm = TRUE) / sum(d_fexp, na.rm = TRUE)),
      across(c(ratio),
             ~sum(.x * d_fexp * (d_mode1 == 2) * (d_mode_agg == "driving"), na.rm = TRUE) /
              sum(d_fexp * (d_mode_agg == "driving"), na.rm = TRUE)),
      .groups = "drop"
    )
  wdf$nbpassengers <- 1 + wdf$ratio / (1 - wdf$ratio)
  wdf$ratio <- NULL
  wdf
}

# Baseline: current modal shares (no shift)
df$part_driving   <- (df$d_mode_agg == "driving")
df$part_bicycling <- (df$d_mode_agg == "bicycling")
df$part_walking   <- (df$d_mode_agg == "walking")
df$part_transit   <- (df$d_mode_agg == "transit")
weighted_df0 <- compute_weighted(df, target_km = NULL)

# Scenario variants
weighted_df5  <- compute_weighted(df, target_km = 5)
weighted_df7  <- compute_weighted(df, target_km = 7)
weighted_df10 <- compute_weighted(df, target_km = 10)

# Full aggregation (with cost columns) — this is the main weighted_df
df$ratio <- df$driving
weighted_df <- df %>%
  group_by(CFS) %>%
  summarise(
    CFS = first(CFS), comm_name = first(comm_name),
    across(c(km_driving, time_driving, timecong_driving,
             coutprive_driving, coutpublic_driving, coutexterne_driving,
             coutsocial_driving, coutges_driving, coutcong_driving,
             coutdeplacement_driving, coututilisation_driving, coutsante_driving),
           ~sum(.x * d_fexp * driving2, na.rm = TRUE) / sum(d_fexp * driving2, na.rm = TRUE)),
    across(c(km_transit, time_transit,
             coutprive_transit, coutpublic_transit, coutexterne_transit,
             coutsocial_transit, coutges_transit, coutcong_transit,
             coutdeplacement_transit, coututilisation_transit, coutsante_transit),
           ~sum(.x * d_fexp * transit, na.rm = TRUE) / sum(d_fexp * transit, na.rm = TRUE)),
    across(c(km_bicycling, time_bicycling,
             coutprive_bicycling, coutpublic_bicycling, coutexterne_bicycling,
             coutsocial_bicycling, coutges_bicycling, coutcong_bicycling,
             coutdeplacement_bicycling, coututilisation_bicycling, coutsante_bicycling),
           ~sum(.x * d_fexp * bicycling, na.rm = TRUE) / sum(d_fexp * bicycling, na.rm = TRUE)),
    across(c(km_walking, time_walking,
             coutprive_walking, coutpublic_walking, coutexterne_walking,
             coutsocial_walking, coutges_walking, coutcong_walking,
             coutdeplacement_walking, coututilisation_walking, coutsante_walking),
           ~sum(.x * d_fexp * walking, na.rm = TRUE) / sum(d_fexp * walking, na.rm = TRUE)),
    across(c(driving, driving2, transit, bicycling, walking, cluster2, cluster3),
           ~sum(.x * d_fexp, na.rm = TRUE) / sum(d_fexp, na.rm = TRUE)),
    across(c(auto_prive, auto_interne, auto_cache,
             tc_prive, tc_interne, tc_cache,
             velo_prive, velo_interne, velo_cache,
             marche_prive, marche_interne, marche_cache),
           ~sum(.x * d_fexp, na.rm = TRUE) / sum(d_fexp * !is.na(.x), na.rm = TRUE)),
    across(c(ratio),
           ~sum(.x * d_fexp * (d_mode1 == 2) * (driving), na.rm = TRUE) /
            sum(d_fexp * (driving), na.rm = TRUE)),
    .groups = "drop"
  )
weighted_df$nbpassengers <- 1 + weighted_df$ratio / (1 - weighted_df$ratio)
weighted_df$ratio <- NULL

# ---------------------------------------------------------------------------
# Part B: Merge scenario shares into one wide dataframe
# ---------------------------------------------------------------------------
# Rename scenario columns with suffixes, keeping CFS as join key
wdf0_renamed  <- weighted_df0 %>%
  rename_with(~paste0(.x, "_0"), .cols = -c(CFS, comm_name))
wdf5_renamed  <- weighted_df5 %>%
  rename_with(~paste0(.x, "_5"), .cols = -c(CFS, comm_name))
wdf7_renamed  <- weighted_df7 %>%
  rename_with(~paste0(.x, "_7"), .cols = -c(CFS, comm_name))
wdf10_renamed <- weighted_df10 %>%
  rename_with(~paste0(.x, "_10"), .cols = -c(CFS, comm_name))

weighted_df <- rename(weighted_df, FSA = CFS)
merged_df <- weighted_df %>%
  left_join(wdf0_renamed %>% dplyr::select(-comm_name), by = c("FSA" = "CFS")) %>%
  left_join(wdf5_renamed %>% dplyr::select(-comm_name), by = c("FSA" = "CFS")) %>%
  left_join(wdf7_renamed %>% dplyr::select(-comm_name), by = c("FSA" = "CFS")) %>%
  left_join(wdf10_renamed %>% dplyr::select(-comm_name), by = c("FSA" = "CFS"))

# Set non-travel columns to NA where comm_name is missing (FSAs with no survey data)
travel_cols <- c("FSA", "comm_name",
                 "km_driving", "time_driving", "timecong_driving",
                 "km_transit", "time_transit",
                 "km_bicycling", "time_bicycling",
                 "km_walking", "time_walking")
merged_df[is.na(merged_df$comm_name), setdiff(colnames(merged_df), travel_cols)] <- NA

weighted_df <- merged_df %>%
  mutate(across(everything(), ~ifelse(is.nan(.), 0, .)))
weighted_df <- rename(weighted_df, CFS = FSA)

# ---------------------------------------------------------------------------
# Part C: Merge with Census 2016
# ---------------------------------------------------------------------------
census_df <- readRDS(file.path(inter, "censusFSA2016.rds"))

# LANDAREA: from pre-extracted lookup (originally from shapefile)
landarea_path <- file.path(raw, "landarea_fsa.rds")
if (file.exists(landarea_path)) {
  la <- readRDS(landarea_path)
  census_df <- left_join(census_df, la, by = c("geo" = "FSA"))
  census_df <- rename(census_df, landarea = LANDAREA)
  cat("  Loaded LANDAREA from landarea_fsa.rds\n")
} else {
  census_df$landarea <- NA
  cat("  WARNING: landarea_fsa.rds not found, LANDAREA will be NA\n")
}

census_df <- suppressWarnings(census_df %>%
  mutate(across(.cols = -geo, .fns = ~as.numeric(as.character(.)))))

census_df <- census_df %>%
  dplyr::select(where(~sum(is.na(.)) != nrow(census_df))) %>%
  dplyr::select(-matches("(_male$|_female$)"))

# Commuting duration shares (use [[...]] for column access — works with any name)
total_col <- "total_commuting_duration_for_the_employed_labour_force_aged_15_years_and_over_in_private_households_with_a_usual_place_of_work_or_no_fixed_workplace_address_25_sample_data1937_total"
cat_cols <- c("less_than_15_minutes1938_total",
              "X15_to_29_minutes1939_total",
              "X30_to_44_minutes1940_total",
              "X45_to_59_minutes1941_total",
              "X60_minutes_and_over1942_total")

census_df$share_less_than_15_minutes <- census_df[[cat_cols[1]]] / census_df[[total_col]]
census_df$share_15_to_29_minutes     <- census_df[[cat_cols[2]]] / census_df[[total_col]]
census_df$share_30_to_44_minutes     <- census_df[[cat_cols[3]]] / census_df[[total_col]]
census_df$share_45_to_59_minutes     <- census_df[[cat_cols[4]]] / census_df[[total_col]]
census_df$share_60_minutes_and_over  <- census_df[[cat_cols[5]]] / census_df[[total_col]]

# Commuting mode shares
total_mode <- "total_main_mode_of_commuting_for_the_employed_labour_force_aged_15_years_and_over_in_private_households_with_a_usual_place_of_work_or_no_fixed_workplace_address_25_sample_data1930_total"
mode_cols  <- c("car_truck_van_as_a_driver1931_total", "car_truck_van_as_a_passenger1932_total",
                "public_transit1933_total", "walked1934_total", "bicycle1935_total", "other_method1936_total")

census_df$share_car_driver     <- pmin(pmax(census_df[[mode_cols[1]]] / census_df[[total_mode]], 0), 1)
census_df$share_car_passenger  <- pmin(pmax(census_df[[mode_cols[2]]] / census_df[[total_mode]], 0), 1)
census_df$share_public_transit <- pmin(pmax(census_df[[mode_cols[3]]] / census_df[[total_mode]], 0), 1)
census_df$share_walked         <- pmin(pmax(census_df[[mode_cols[4]]] / census_df[[total_mode]], 0), 1)
census_df$share_bicycle        <- pmin(pmax(census_df[[mode_cols[5]]] / census_df[[total_mode]], 0), 1)
census_df$share_other_method   <- pmin(pmax(census_df[[mode_cols[6]]] / census_df[[total_mode]], 0), 1)

# Commuting destination shares
total_dest <- "total_commuting_destination_for_the_employed_labour_force_aged_15_years_and_over_in_private_households_with_a_usual_place_of_work_25_sample_data1925_total"
dest_cols  <- c("commute_within_census_subdivision_csd_of_residence1926_total",
                "commute_to_a_different_census_subdivision_csd_within_census_division_cd_of_residence1927_total",
                "commute_to_a_different_census_subdivision_csd_and_census_division_cd_within_province_or_territory_of_residence1928_total",
                "commute_to_a_different_province_or_territory1929_total")
census_df$share_within_csd <- census_df[[dest_cols[1]]] / census_df[[total_dest]]
census_df$share_to_different_csd_within_cd <- census_df[[dest_cols[2]]] / census_df[[total_dest]]
census_df$share_to_different_csd_and_cd_within_province_or_territory <- census_df[[dest_cols[3]]] / census_df[[total_dest]]
census_df$share_to_different_province_or_territory <- census_df[[dest_cols[4]]] / census_df[[total_dest]]

census_df$share_bachelor <- census_df[["bachelors_degree1708_total"]] / census_df[["population_20161_total"]]

# Select final 2016 columns (use all_of to handle X-prefixed names)
census_2016_cols <- c("geo", "population_20161_total",
                      "X0_to_14_years35_total", "X15_to_64_years36_total", "X65_years_and_over37_total",
                      "landarea", "share_bachelor",
                      "median_total_income_of_households_in_2015_742_total",
                      "number_of_employment_income_recipients_aged_15_years_and_over_in_private_households_100_data670_total",
                      "average_aftertax_income_of_households_in_2015_752_total",
                      "average_age_of_the_population39_total", "average_household_size58_total",
                      "share_less_than_15_minutes", "share_15_to_29_minutes", "share_30_to_44_minutes",
                      "share_45_to_59_minutes", "share_60_minutes_and_over",
                      "share_car_driver", "share_car_passenger", "share_public_transit",
                      "share_walked", "share_bicycle", "share_other_method",
                      "share_within_csd", "share_to_different_csd_within_cd",
                      "share_to_different_csd_and_cd_within_province_or_territory",
                      "share_to_different_province_or_territory",
                      "worked_at_home1921_total", "worked_outside_canada1922_total",
                      "no_fixed_workplace_address1923_total", "worked_at_usual_place1924_total")
census_df <- census_df %>% dplyr::select(all_of(census_2016_cols))
census2016_df <- census_df

# ---------------------------------------------------------------------------
# Part D: Merge with Census 2021
# ---------------------------------------------------------------------------
census_df <- readRDS(file.path(inter, "censusFSA2021.rds"))

# LANDAREA for 2021
if (file.exists(landarea_path)) {
  la <- readRDS(landarea_path)
  census_df <- left_join(census_df, la, by = c("geo" = "FSA"))
  census_df <- rename(census_df, landarea = LANDAREA)
}

census_df <- suppressWarnings(census_df %>%
  mutate(across(.cols = -geo, .fns = ~as.numeric(as.character(.)))))

census_df <- census_df %>%
  dplyr::select(where(~sum(is.na(.)) != nrow(census_df)))

# 2021 commuting duration shares
total_col_21 <- "total_commuting_duration_for_the_employed_labour_force_aged_15_years_and_over_with_a_usual_place_of_work_or_no_fixed_workplace_address_25_sample_data2611_total"
cat_cols_21  <- c("less_than_15_minutes2612_total",
                  "X15_to_29_minutes2613_total",
                  "X30_to_44_minutes2614_total",
                  "X45_to_59_minutes2615_total",
                  "X60_minutes_and_over2616_total")

census_df$share_less_than_15_minutes <- census_df[[cat_cols_21[1]]] / census_df[[total_col_21]]
census_df$share_15_to_29_minutes     <- census_df[[cat_cols_21[2]]] / census_df[[total_col_21]]
census_df$share_30_to_44_minutes     <- census_df[[cat_cols_21[3]]] / census_df[[total_col_21]]
census_df$share_45_to_59_minutes     <- census_df[[cat_cols_21[4]]] / census_df[[total_col_21]]
census_df$share_60_minutes_and_over  <- census_df[[cat_cols_21[5]]] / census_df[[total_col_21]]

# 2021 commuting mode shares
total_mode_21 <- "total_main_mode_of_commuting_for_the_employed_labour_force_aged_15_years_and_over_with_a_usual_place_of_work_or_no_fixed_workplace_address_25_sample_data2603_total"
mode_cols_21  <- c("car_truck_or_van_as_a_driver2605_total", "car_truck_or_van_as_a_passenger2606_total",
                   "public_transit2607_total", "walked2608_total", "bicycle2609_total", "other_method2610_total")

census_df$share_car_driver     <- census_df[[mode_cols_21[1]]] / census_df[[total_mode_21]]
census_df$share_car_passenger  <- census_df[[mode_cols_21[2]]] / census_df[[total_mode_21]]
census_df$share_public_transit <- census_df[[mode_cols_21[3]]] / census_df[[total_mode_21]]
census_df$share_walked         <- census_df[[mode_cols_21[4]]] / census_df[[total_mode_21]]
census_df$share_bicycle        <- census_df[[mode_cols_21[5]]] / census_df[[total_mode_21]]
census_df$share_other_method   <- census_df[[mode_cols_21[6]]] / census_df[[total_mode_21]]

# 2021 commuting destination shares
total_dest_21 <- "total_commuting_destination_for_the_employed_labour_force_aged_15_years_and_over_with_a_usual_place_of_work_25_sample_data2598_total"
dest_cols_21  <- c("commute_within_census_subdivision_csd_of_residence2599_total",
                   "commute_to_a_different_census_subdivision_csd_within_census_division_cd_of_residence2600_total",
                   "commute_to_a_different_census_subdivision_csd_and_census_division_cd_within_province_or_territory_of_residence2601_total",
                   "commute_to_a_different_province_or_territory2602_total")
census_df$share_within_csd <- census_df[[dest_cols_21[1]]] / census_df[[total_dest_21]]
census_df$share_to_different_csd_within_cd <- census_df[[dest_cols_21[2]]] / census_df[[total_dest_21]]
census_df$share_to_different_csd_and_cd_within_province_or_territory <- census_df[[dest_cols_21[3]]] / census_df[[total_dest_21]]
census_df$share_to_different_province_or_territory <- census_df[[dest_cols_21[4]]] / census_df[[total_dest_21]]

census_df$share_bachelor <- census_df[["bachelors_degree_or_higher2024_total"]] / census_df[["population_20211_total"]]

# Select final 2021 columns
census_2021_cols <- c("geo", "population_20211_total",
                      "X0_to_14_years35_total", "X15_to_64_years36_total", "X65_years_and_over37_total",
                      "landarea", "share_bachelor",
                      "median_aftertax_income_of_household_in_2020244_total",
                      "number_of_employment_income_recipients_aged_15_years_and_over_in_private_households_in_2020_100_data118_total",
                      "average_aftertax_income_of_household_in_2020253_total",
                      "average_age_of_the_population39_total", "average_household_size57_total",
                      "share_less_than_15_minutes", "share_15_to_29_minutes", "share_30_to_44_minutes",
                      "share_45_to_59_minutes", "share_60_minutes_and_over",
                      "share_car_driver", "share_car_passenger", "share_public_transit",
                      "share_walked", "share_bicycle", "share_other_method",
                      "share_within_csd", "share_to_different_csd_within_cd",
                      "share_to_different_csd_and_cd_within_province_or_territory",
                      "share_to_different_province_or_territory",
                      "worked_at_home2594_total", "worked_outside_canada2595_total",
                      "no_fixed_workplace_address2596_total", "usual_place_of_work2597_total")
census_df <- census_df %>% dplyr::select(all_of(census_2021_cols))

# Suffix 2021 columns (except CFS and landarea)
census_df <- rename(census_df, CFS = geo)
census_df <- census_df %>%
  rename_with(.fn = ~if_else(.x == "CFS" | .x == "landarea", .x, paste0(.x, "_2021")),
              .cols = everything())

# ---------------------------------------------------------------------------
# Part E: Final merge — census2016 + census2021 + weighted_df + MTL/ARTM flags
# ---------------------------------------------------------------------------
census2016_df <- rename(census2016_df, CFS = geo)

merged_df <- left_join(census2016_df, weighted_df, by = "CFS")
merged_df <- left_join(census_df, merged_df, by = "CFS")

CFS_MTL <- readRDS(file.path(raw, "CFS_MTL.rds"))
merged_df <- rename(merged_df, FSA = CFS)
merged_df <- merged_df %>%
  mutate(MTL  = if_else(FSA %in% CFS_MTL, 1, 0),
         ARTM = if_else(!is.na(km_driving), 1, 0))

# Derived columns
merged_df$popdensity2016 <- merged_df$population_20161_total / merged_df$landarea.x
merged_df$popdensity2021 <- merged_df$population_20211_total_2021 / merged_df$landarea.x
merged_df$share_30_minutes_and_over <- merged_df$share_30_to_44_minutes + merged_df$share_45_to_59_minutes + merged_df$share_60_minutes_and_over
merged_df$MTL[is.na(merged_df$MTL)]   <- 0
merged_df$ARTM[is.na(merged_df$ARTM)] <- 0

# Share of mobile workers
merged_df$share_worker <- (merged_df$worked_at_usual_place1924_total + merged_df$no_fixed_workplace_address1923_total) /
  (merged_df$worked_outside_canada1922_total + merged_df$worked_at_home1921_total +
   merged_df$worked_at_usual_place1924_total + merged_df$no_fixed_workplace_address1923_total)
merged_df$share_worker_2021 <- (merged_df$usual_place_of_work2597_total_2021 + merged_df$no_fixed_workplace_address2596_total_2021) /
  (merged_df$worked_outside_canada2595_total_2021 + merged_df$worked_at_home2594_total_2021 +
   merged_df$usual_place_of_work2597_total_2021 + merged_df$no_fixed_workplace_address2596_total_2021)
merged_df$share_homeworker <- merged_df$worked_at_home1921_total /
  (merged_df$worked_outside_canada1922_total + merged_df$worked_at_home1921_total +
   merged_df$worked_at_usual_place1924_total + merged_df$no_fixed_workplace_address1923_total)
merged_df$share_homeworker_2021 <- merged_df$worked_at_home2594_total_2021 /
  (merged_df$worked_outside_canada2595_total_2021 + merged_df$worked_at_home2594_total_2021 +
   merged_df$usual_place_of_work2597_total_2021 + merged_df$no_fixed_workplace_address2596_total_2021)

out_path <- file.path(inter, "pred_df.rds")
saveRDS(merged_df, out_path)
cat(sprintf("  Wrote pred_df.rds: %d FSAs, %d columns\n", nrow(merged_df), ncol(merged_df)))
cat("Stage 3 complete.\n\n")
