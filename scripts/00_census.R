#!/usr/bin/env Rscript
# 00_census.R — Extract census variables from Statistics Canada CSV files
#
# Inputs:  data/raw/census/.../2016 and 2021 CSV files
# Outputs: data/intermediate/censusFSA2016.rds, censusFSA2021.rds

if (!exists("ROOT")) ROOT <- dirname(getwd())

lib <- "/opt/tools/r-libs"
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

cat("=== Stage 0: Census Processing ===\n")

# --- 2016 Census ---
csv_2016 <- file.path(ROOT, "data/raw/census/CENSUSBYFSA_98-401-X2016046_eng_CSV/98-401-X2016046_English_CSV_data.csv")
out_2016 <- file.path(ROOT, "data/intermediate/censusFSA2016.rds")

if (!file.exists(csv_2016)) {
  cat("SKIP: 2016 Census CSV not found. Using existing censusFSA2016.rds\n")
} else {
  cat("Processing 2016 Census...\n")
  data <- read.csv(csv_2016)

  create_simplified_df_2016 <- function(data, variable_name, variable_code) {
    simplified_name <- tolower(gsub("[^[:alnum:] ]", "", variable_name))
    simplified_name <- gsub(" +", "_", simplified_name)
    data_filtered <- data %>%
      filter(DIM..Profile.of.Forward.Sortation.Areas..2247. == variable_name &
             Member.ID..Profile.of.Forward.Sortation.Areas..2247. == variable_code) %>%
      dplyr::select(-CENSUS_YEAR, -GEO_LEVEL, -GEO_NAME, -GNR, -GNR_LF, -DATA_QUALITY_FLAG,
                    -ALT_GEO_CODE, -DIM..Profile.of.Forward.Sortation.Areas..2247.,
                    -Member.ID..Profile.of.Forward.Sortation.Areas..2247.,
                    -Notes..Profile.of.Forward.Sortation.Areas..2247.) %>%
      rename(geo = GEO_CODE..POR.) %>%
      rename_with(~paste0(simplified_name, variable_code, "_", c("total", "male", "female")),
                  .cols = c("Dim..Sex..3...Member.ID...1...Total...Sex",
                            "Dim..Sex..3...Member.ID...2...Male",
                            "Dim..Sex..3...Member.ID...3...Female"))
    final_col_names <- c("geo",
                         paste0(simplified_name, variable_code, "_total"),
                         paste0(simplified_name, variable_code, "_male"),
                         paste0(simplified_name, variable_code, "_female"))
    data_filtered %>% dplyr::select(all_of(final_col_names))
  }

  input_vector <- c(
    "Median after-tax income of households in 2015 ($)", 743,
    "Population, 2016", 1,
    "0 to 14 years", 35,
    "15 to 64 years", 36,
    "65 years and over", 37,
    "Average age of the population", 39,
    "Average household size", 58,
    "Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data", 1930,
    "Car, truck, van - as a driver", 1931,
    "Car, truck, van - as a passenger", 1932,
    "Public transit", 1933,
    "Walked", 1934,
    "Bicycle", 1935,
    "Other method", 1936,
    "Total - Commuting duration for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data", 1937,
    "Less than 15 minutes", 1938,
    "15 to 29 minutes", 1939,
    "30 to 44 minutes", 1940,
    "45 to 59 minutes", 1941,
    "60 minutes and over", 1942,
    "Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data (183)", 1698,
    "Bachelor's degree", 1708,
    "Number of employment income recipients aged 15 years and over in private households - 100% data", 670,
    "Median employment income in 2015 among recipients ($)", 671,
    "Total - Income statistics in 2015 for private households by household size - 100% data", 741,
    "Median total income of households in 2015 ($)", 742,
    "Total - Income statistics in 2015 for private households by household size - 25% sample data (44)", 750,
    "Average total income of households in 2015 ($)", 751,
    "Average after-tax income of households in 2015 ($)", 752,
    "Total - Place of work status for the employed labour force aged 15 years and over in private households - 25% sample data (229)", 1920,
    "Worked at home", 1921,
    "Worked outside Canada", 1922,
    "No fixed workplace address", 1923,
    "Worked at usual place", 1924,
    "Total - Commuting destination for the employed labour force aged 15 years and over in private households with a usual place of work - 25% sample data", 1925,
    "Commute within census subdivision (CSD) of residence", 1926,
    "Commute to a different census subdivision (CSD) within census division (CD) of residence", 1927,
    "Commute to a different census subdivision (CSD) and census division (CD) within province or territory of residence", 1928,
    "Commute to a different province or territory", 1929
  )

  variable_matrix <- as.data.frame(matrix(input_vector, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(variable_matrix) <- c("Name", "Code")
  simplified_dfs <- list()
  for (i in seq_len(nrow(variable_matrix))) {
    simplified_dfs[[i]] <- create_simplified_df_2016(data, variable_matrix$Name[i],
                                                     as.numeric(variable_matrix$Code[i]))
  }
  final_df <- simplified_dfs[[1]]
  for (i in 2:length(simplified_dfs)) {
    final_df <- full_join(final_df, simplified_dfs[[i]], by = "geo")
  }
  names(final_df) <- make.names(names(final_df))
  saveRDS(final_df, out_2016)
  cat(sprintf("  Wrote censusFSA2016.rds: %d FSAs\n", nrow(final_df)))
  rm(data)
}

# --- 2021 Census ---
csv_2021 <- file.path(ROOT, "data/raw/census/CENSUSBYFSA_98-401-X2021013_eng_CSV/98-401-X2021013_English_CSV_data.csv")
out_2021 <- file.path(ROOT, "data/intermediate/censusFSA2021.rds")

if (!file.exists(csv_2021)) {
  cat("SKIP: 2021 Census CSV not found. Using existing censusFSA2021.rds\n")
} else {
  cat("Processing 2021 Census...\n")
  data <- read.csv(csv_2021)

  create_simplified_df_2021 <- function(data, variable_code) {
    variable_name <- unique(data$CHARACTERISTIC_NAME[data$CHARACTERISTIC_ID == variable_code])
    simplified_name <- tolower(gsub("[^[:alnum:] ]", "", variable_name))
    simplified_name <- gsub(" +", "_", simplified_name)
    simplified_name <- gsub("^_+|_+$", "", simplified_name)
    data_filtered <- data %>%
      filter(CHARACTERISTIC_NAME == variable_name & CHARACTERISTIC_ID == variable_code) %>%
      dplyr::select(-CENSUS_YEAR, -GEO_LEVEL, -DATA_QUALITY_FLAG, -CHARACTERISTIC_NAME,
                    -CHARACTERISTIC_ID, -ALT_GEO_CODE, -CHARACTERISTIC_NOTE,
                    -TNR_SF, -TNR_LF, -SYMBOL, -SYMBOL.1, -SYMBOL.2, -SYMBOL.3,
                    -SYMBOL.4, -SYMBOL.5) %>%
      rename(geo = GEO_NAME) %>%
      rename_with(~paste0(simplified_name, variable_code, "_total"), .cols = c("C1_COUNT_TOTAL"))
    final_col_names <- c("geo", paste0(simplified_name, variable_code, "_total"))
    data_filtered %>% dplyr::select(all_of(final_col_names))
  }

  input_codes <- c(244, 1, 35, 36, 37, 39, 57, 2603, 2605, 2606, 2607, 2608, 2609, 2610,
                   2611, 2612, 2613, 2614, 2615, 2616, 2014, 2024, 118, 119, 243, 252, 253,
                   2593, 2594, 2595, 2596, 2597, 2598, 2599, 2600, 2601, 2602)
  simplified_dfs <- list()
  for (i in seq_along(input_codes)) {
    simplified_dfs[[i]] <- create_simplified_df_2021(data, input_codes[i])
  }
  final_df <- simplified_dfs[[1]]
  for (i in 2:length(simplified_dfs)) {
    final_df <- full_join(final_df, simplified_dfs[[i]], by = "geo")
  }
  names(final_df) <- make.names(names(final_df))
  saveRDS(final_df, out_2021)
  cat(sprintf("  Wrote censusFSA2021.rds: %d FSAs\n", nrow(final_df)))
  rm(data)
}

cat("Stage 0 complete.\n\n")
