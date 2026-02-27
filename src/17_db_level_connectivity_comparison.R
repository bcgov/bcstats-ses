# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# =============================================================================
# SCRIPT: 17_db_level_connectivity_comparison.R
#
# PURPOSE:
#   Compare NBD (federal) and CITZ (provincial) broadband data at the
#   PHH_ID level for direct 1:1 comparison, then aggregate to DB/DA/CSD.
#
# DATA STRUCTURE:
#   - Both NBD and CITZ have PHH_ID column
#   - PHH_ID is a unique identifier representing the same location in both datasets
#   - Direct join by PHH_ID = 1:1 (no aggregation needed!)
#
# WHY THIS APPROACH:
#   - PHH_ID join gives precise point-to-point comparison
#   - No aggregation = no information loss
#   - Easier to trace discrepancies back to source
#   - Can aggregate to any higher geography as needed
#   - PowerBI gets clean data, no complex calculations needed
#
# WHAT THIS SCRIPT DOES:
#   1. Loads NBD data (keeps at PHH_ID level)
#   2. Loads CITZ data (keeps at PHH_ID level)
#   3. Joins NBD and CITZ by PHH_ID (direct 1:1 join)
#   4. Computes deltas and flags outliers at PHH_ID level
#   5. Aggregates to DB, DA, and CSD levels
#   6. Exports clean datasets for PowerBI
#
# OUTPUTS:
#   - nbd_clean.csv       : NBD data at PHH_ID level
#   - citz_clean.csv      : CITZ data at PHH_ID level
#   - nbd_comparison.csv  : Direct PHH_ID to PHH_ID comparison
#   - db_comparison.csv  : DB-level summary (Dissemination Block)
#   - da_comparison.csv  : DA-level summary (Dissemination Area)
#   - csd_comparison.csv : CSD-level summary (Census Subdivision)
#   - *_dict.csv         : Data dictionaries
#   - logs/17_*.log      : Run logs
#
# GEOGRAPHY HIERARCHY:
#   PHH_ID → DB (Dissemination Block) → DA (Dissemination Area) → CSD (Census Subdivision)
#
# REFERENCES:
#   - Script 14: bc_NBD_connectivity_data.R (original NBD processing)
#   - Script 16: bc_citz_connectivity_data.R (original CITZ processing)
# =============================================================================

# =============================================================================
# SECTION 1: SETUP AND CONFIGURATION
# =============================================================================

# ---- Packages ----
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(lubridate)
library(glue)
library(logger)
library(datadictionary)
library(ggplot2)

# Source utility functions
source("./src/utils.R")

# ---- Configuration ----
config <- config::get()

lan_path <- config$lan_path
connectivity_data_path <- config$conectivity_data_path

output_path <- file.path(lan_path, connectivity_data_path, "outputs")
dir.create(output_path, showWarnings = FALSE)

# ---- Logger setup ----
log_threshold(INFO)
log_dir <- file.path(output_path, "logs")
dir.create(log_dir, showWarnings = TRUE, recursive = TRUE)
log_file <- file.path(
  log_dir,
  paste0("17_db_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)
log_appender(appender_tee(log_file))
log_info("Starting 17_db_level_connectivity_comparison.R")
log_info("Log file: {log_file}")

# =============================================================================
# SECTION 2: CONSTANTS
# =============================================================================

# Outlier threshold: flag if |delta| >= 20%
OUTLIER_THRESHOLD <- 0.20

# Speed tiers (same as script 16)
SPEED_TIERS <- list(
  list(label = "50_10", down = 50, up = 10),
  list(label = "25_5", down = 25, up = 5),
  list(label = "10_2", down = 10, up = 2),
  list(label = "5_1", down = 5, up = 1)
)

# Connection types (including combined = wired OR wireless)
CONNECTION_TYPES <- c("wired", "wireless", "combined")

log_info("Outlier threshold: {OUTLIER_THRESHOLD}")
log_info(
  "Speed tiers: {paste(sapply(SPEED_TIERS, `[[`, 'label'), collapse = ', ')}"
)

# =============================================================================
# SECTION 3: HELPER FUNCTIONS
# =============================================================================

# ---- Helper: Parse speed strings (from script 16) ----
#' Parse CITZ speed threshold strings into numeric values
#' @param speed_str Character vector of speed labels (e.g., "50_10", "<5_1")
#' @return Data frame with down, up, is_lt5_1 columns
parse_speed <- function(speed_str) {
  is_lt <- !is.na(speed_str) & str_detect(speed_str, "^<")
  ok <- !is.na(speed_str) & str_detect(speed_str, "^[0-9]+_[0-9]+$")

  data.frame(
    down = suppressWarnings(as.numeric(ifelse(
      ok,
      str_extract(speed_str, "^[0-9]+(?=_)"),
      ifelse(is_lt, 0, NA)
    ))),
    up = suppressWarnings(as.numeric(ifelse(
      ok,
      str_extract(speed_str, "(?<=_)[0-9]+$"),
      ifelse(is_lt, 0, NA)
    ))),
    is_lt5_1 = is_lt
  )
}

# ---- Helper: Flag all tiers for a speed column ----
#' Create boolean flags for each speed tier
#' @param parsed Data frame from parse_speed
#' @param prefix Connection type prefix ("wired" or "wireless")
#' @return Data frame with boolean flag columns
flag_all_tiers <- function(parsed, prefix) {
  result <- data.frame(row_id = seq_len(nrow(parsed)))

  for (tier in SPEED_TIERS) {
    col_name <- paste0("is_", prefix, "_", tier$label)
    result[[col_name]] <- !is.na(parsed$down) &
      !is.na(parsed$up) &
      parsed$down >= tier$down &
      parsed$up >= tier$up
  }

  col_lt <- paste0("is_", prefix, "_lt5_1")
  result[[col_lt]] <- parsed$is_lt5_1
  result$row_id <- NULL
  result
}

# ---- Helper: Compute combined max threshold from wired and wireless ----
#' Compute combined max threshold = the higher of wired or wireless
#' This mimics NBD's "combined" approach (wired OR wireless)
#' @param wired_max Character: wired max threshold (e.g., "50_10", "25_5", "<5_1")
#' @param wireless_max Character: wireless max threshold
#' @return Character: the higher of the two (NA if both are NA)
compute_combined_max <- function(wired_max, wireless_max) {
  # Define tier hierarchy (higher index = higher speed)
  tier_order <- c("", "<5_1", "5_1", "10_2", "25_5", "50_10")

  # Helper to get tier rank
  get_rank <- function(x) {
    if (is.na(x) || x == "") {
      return(0)
    }
    which(tier_order == x)
  }

  wired_rank <- get_rank(wired_max)
  wireless_rank <- get_rank(wireless_max)

  # If both NA, return NA
  if (wired_rank == 0 && wireless_rank == 0) {
    return(NA_character_)
  }

  # Return the higher tier
  if (wired_rank >= wireless_rank) {
    return(wired_max)
  } else {
    return(wireless_max)
  }
}

# ---- Helper: Convert threshold to numeric value ----
#' Convert speed threshold string to numeric for regression/analysis
#' @param threshold Character: speed threshold (e.g., "50_10", "25_5", "<5_1", NA)
#' @return Numeric: 5=50_10, 4=25_5, 3=10_2, 2=5_1, 1=<5_1, 0=NA/empty
threshold_to_numeric <- function(threshold) {
  if (is.na(threshold) || threshold == "" || is.null(threshold)) {
    return(0)
  }

  threshold <- as.character(threshold)

  if (threshold == "50_10") {
    return(5)
  } else if (threshold == "25_5") {
    return(4)
  } else if (threshold == "10_2") {
    return(3)
  } else if (threshold == "5_1") {
    return(2)
  } else if (threshold == "<5_1") {
    return(1)
  } else {
    return(0)
  }
}

# ---- Helper: Compute PHH-weighted share at DB level ----
#' For a single PHH DB record, convert boolean flags to proportions
#' Since NBD is already at DB level, each DB has 1 PHH = 100%
compute_phh_props_at_db <- function(phh_row) {
  # PHH data already has 1 row per DB with boolean/numeric availability
  # Just rename columns to match CITZ naming convention
  result <- list()

  for (type in CONNECTION_TYPES) {
    for (tier in SPEED_TIERS) {
      # PHH column names: prop_{type}_{tier}
      phh_col <- paste0("prop_", type, "_", tier$label)

      if (phh_col %in% names(phh_row)) {
        result[[paste0("phh_", type, "_", tier$label)]] <- phh_row[[phh_col]]
      } else {
        result[[paste0("phh_", type, "_", tier$label)]] <- NA_real_
      }
    }
    # LT5_1 flag
    lt_col <- paste0("prop_", type, "_lt5_1")
    if (lt_col %in% names(phh_row)) {
      result[[paste0("phh_", type, "_lt5_1")]] <- phh_row[[lt_col]]
    } else {
      # Can derive from: 1 - prop_{type}_5_1
      prop_5_1 <- phh_row[[paste0("prop_", type, "_5_1")]]
      result[[paste0("phh_", type, "_lt5_1")]] <- ifelse(
        is.na(prop_5_1),
        NA,
        1 - prop_5_1
      )
    }
  }

  as.data.frame(result)
}

# ---- Helper: Aggregate DB comparison to CSD/DA ----
#' Aggregate DB-level comparison to higher geography
#' @param df Data frame with db_code and comparison columns
#' @param group_col Column to group by (csd_code or da_code)
#' @param weight_col Weight column for aggregation
#' @return Aggregated data frame
aggregate_comparison <- function(df, group_col, weight_col = "dwell_total") {
  group_name <- deparse(substitute(group_col))

  # Get all delta and outlier columns
  delta_cols <- names(df)[str_detect(names(df), "^delta_")]
  outlier_cols <- names(df)[str_detect(names(df), "^outlier_")]

  df %>%
    filter(!is.na({{ group_col }})) %>%
    group_by({{ group_col }}) %>%
    summarise(
      # Count of DBs
      n_db = n(),

      # Sum of dwellings (for weighting)
      dwell_total = sum(.data[[weight_col]], na.rm = TRUE),

      # Mean delta (weighted by dwellings if available)
      across(
        all_of(delta_cols),
        ~ mean(.x, na.rm = TRUE),
        .names = "mean_{.col}"
      ),

      # Count of outliers by type
      across(
        all_of(outlier_cols),
        ~ sum(.x == "OK", na.rm = TRUE),
        .names = "n_{.col}_ok"
      ),
      across(
        all_of(outlier_cols),
        ~ sum(.x == "CITZ >> PHH", na.rm = TRUE),
        .names = "n_{.col}_citz_higher"
      ),
      across(
        all_of(outlier_cols),
        ~ sum(.x == "PHH >> CITZ", na.rm = TRUE),
        .names = "n_{.col}_phh_higher"
      ),

      .groups = "drop"
    )
}

# =============================================================================
# SECTION 4: LOAD AND PROCESS NBD DATA AT PHH_ID LEVEL
# =============================================================================
log_info("==== PART 1: Load NBD data at PHH_ID level ====")

# Load NBD Speeds (Current) - raw PHH-level data
path_phh_current_csv <- file.path(
  lan_path,
  connectivity_data_path,
  "NBD_PHH_Speeds",
  "PHH_Speeds_Current-PHH_Vitesses_Actuelles_BC.csv"
)

# Load NBD21 points for DBUID mapping
path_nbd21_points_csv <- file.path(
  lan_path,
  connectivity_data_path,
  "PHH_2021_CSV\\PHH_2021_CSV\\PHH-BC.csv"
)

# Load TMF for DB -> DA -> CSD mapping
tmf_csv <- file.path(
  lan_path,
  config$file_path$tmf_file_path,
  config$file_name$tmf_file_name
)

# ---- 4.1) Load NBD Speeds ----
log_info("Loading NBD speeds: {path_phh_current_csv}")
nbd_spd <- read_csv(path_phh_current_csv, show_col_types = FALSE)

# Define columns we need
bool_cols <- c(
  "Combined_lt5_1_Combine",
  "Wired_lt5_1_Filaire",
  "Wireless_lt5_Sans_fil",
  "Combined_5_1_Combine",
  "Wired_5_1_Filaire",
  "Wireless_5_1_Sans_fil",
  "Combined_10_2_Combine",
  "Wired_10_2_Filaire",
  "Wireless_10_2_Sans_fil",
  "Combined_25_5_Combine",
  "Wired_25_5_Filaire",
  "Wireless_25_5_Sans_fil",
  "Combined_50_10_Combine",
  "Wired_50_10_Filaire",
  "Wireless_50_10_Sans_fil",
  "Avail_LTE_Mobile_Dispo"
)
enum_cols <- c(
  "Combined_Max_Threshold-Combine_Seuil_Max",
  "Wired_Max_Threshold-Filaire_Seuil_Max",
  "Wireless_Max_Threshold-Sans_fil_Seuil_Max"
)

# Validate columns
req_spd <- c("PHH_ID", bool_cols, enum_cols)
missing_spd <- setdiff(req_spd, names(nbd_spd))
if (length(missing_spd) > 0) {
  stop("Missing PHH columns: ", paste(missing_spd, collapse = ", "))
}

# Convert boolean text to numeric
nbd_spd[bool_cols] <- lapply(
  nbd_spd[bool_cols],
  function(x) as.numeric(as.character(x))
)

log_info("NBD speeds loaded: {nrow(nbd_spd)} rows")

# ---- 4.2) Load NBD21 for DBUID mapping ----
log_info("Loading NBD21 points for DBUID mapping")
nbd21 <- read_csv(
  path_nbd21_points_csv,
  show_col_types = FALSE,
  col_types = cols(
    PHH_ID = col_double(),
    DBUID_Ididu = col_double(),
    .default = col_skip()
  )
)

# Create DB code (zero-padded)
nbd_keys <- nbd21 %>%
  transmute(
    PHH_ID,
    db_code = str_pad(as.character(DBUID_Ididu), width = 11, pad = "0")
  )

# ---- 4.3) Load TMF ----
log_info("Loading TMF for geographic mapping")
tmf <- read_csv(tmf_csv, col_types = cols(.default = "c"))

tmf_min <- tmf %>%
  select(
    db_code = DSSMNTNBLC,
    da_code = DSSMNTNRD,
    csd_code = CNSSCNSLDT
  ) %>%
  mutate(db_code = str_sub(db_code, start = nchar(db_code) - 10L))

# ---- 4.4) Join NBD to geographic codes ----
log_info("Joining NBD to geographic codes")
nbd_with_geo <- nbd_spd %>%
  inner_join(nbd_keys, by = "PHH_ID") %>%
  left_join(tmf_min, by = "db_code")

log_info("NBD with geography: {nrow(nbd_with_geo)} rows")
log_info("  - With CSD: {sum(!is.na(nbd_with_geo$csd_code))}")
log_info("  - With DA: {sum(!is.na(nbd_with_geo$da_code))}")

# ---- 4.5) Keep NBD at PHH_ID level (no aggregation!) ----
# KEY: We join directly by PHH_ID - no aggregation needed
# Each PHH_ID represents the same location in both datasets
log_info("Keeping NBD at PHH_ID level (1:1 join with CITZ)")

nbd_clean <- nbd_with_geo %>%
  transmute(
    PHH_ID,
    db_code,
    da_code,
    csd_code,

    # Wired availability flags (already 0/1 per PHH)
    phh_wired_50_10 = Wired_50_10_Filaire,
    phh_wired_25_5 = Wired_25_5_Filaire,
    phh_wired_10_2 = Wired_10_2_Filaire,
    phh_wired_5_1 = Wired_5_1_Filaire,
    phh_wired_lt5_1 = Wired_lt5_1_Filaire,

    # Wireless availability flags
    phh_wireless_50_10 = Wireless_50_10_Sans_fil,
    phh_wireless_25_5 = Wireless_25_5_Sans_fil,
    phh_wireless_10_2 = Wireless_10_2_Sans_fil,
    phh_wireless_5_1 = Wireless_5_1_Sans_fil,
    phh_wireless_lt5_1 = Wireless_lt5_Sans_fil,

    # Combined availability flags
    phh_combined_50_10 = Combined_50_10_Combine,
    phh_combined_25_5 = Combined_25_5_Combine,
    phh_combined_10_2 = Combined_10_2_Combine,
    phh_combined_5_1 = Combined_5_1_Combine,
    phh_combined_lt5_1 = Combined_lt5_1_Combine,

    # LTE mobile
    phh_lte_mobile = Avail_LTE_Mobile_Dispo,

    # Max threshold (for reference)
    phh_combined_max = as.character(`Combined_Max_Threshold-Combine_Seuil_Max`),

    # Numeric version of combined max threshold for regression/analysis
    # 5=50_10, 4=25_5, 3=10_2, 2=5_1, 1=<5_1, 0=NA
    phh_combined_max_numeric = vapply(
      as.character(`Combined_Max_Threshold-Combine_Seuil_Max`),
      threshold_to_numeric,
      FUN.VALUE = 0
    )
  )

# Save NBD clean
nbd_clean_path <- file.path(output_path, "nbd_clean.csv")
write_csv(nbd_clean, nbd_clean_path)
log_info("NBD clean saved: {nbd_clean_path} ({nrow(nbd_clean)} rows)")

# =============================================================================
# SECTION 5: LOAD AND PROCESS CITZ DATA AT PHH_ID LEVEL
# =============================================================================
log_info("==== PART 2: Load CITZ data at PHH_ID level ====")

# ---- 5.1) Load CITZ micro-data ----
citz_path <- file.path(
  lan_path,
  "2024 SES Index/data/raw_data/internet_connectivity/CITZ",
  "CITZ_SHR_Connectivity_Status_January2025.csv"
)
log_info("Loading CITZ data: {citz_path}")
citz <- read_csv(citz_path, show_col_types = FALSE)
log_info("CITZ loaded: {nrow(citz)} rows, {ncol(citz)} cols")

# Check if PHH_ID column exists
if (!"PHH_ID" %in% names(citz)) {
  stop("CITZ data does not have PHH_ID column!")
}
log_info("PHH_ID column found in CITZ data")

# ---- 5.2) Parse speeds and create flags ----
log_info("Parsing CITZ speed thresholds")

# Parse wired and wireless max thresholds
wired_parsed <- parse_speed(citz$Wired_Max_Threshold_Current)
wireless_parsed <- parse_speed(citz$Wireless_Max_Threshold_Current)

# Create boolean flags
wired_flags <- flag_all_tiers(wired_parsed, "wired")
wireless_flags <- flag_all_tiers(wireless_parsed, "wireless")

# Compute combined flags: wired OR wireless (if either has the tier, combined = 1)
combined_flags <- data.frame(
  is_combined_50_10 = (wired_flags$is_wired_50_10 |
    wireless_flags$is_wireless_50_10),
  is_combined_25_5 = (wired_flags$is_wired_25_5 |
    wireless_flags$is_wireless_25_5),
  is_combined_10_2 = (wired_flags$is_wired_10_2 |
    wireless_flags$is_wireless_10_2),
  is_combined_5_1 = (wired_flags$is_wired_5_1 | wireless_flags$is_wireless_5_1),
  is_combined_lt5_1 = (wired_flags$is_wired_lt5_1 &
    wireless_flags$is_wireless_lt5_1) # both below 5_1
)

# ---- 5.3) Add geographic codes via TMF ----
log_info("Adding geographic codes to CITZ")

# Create DB code from DBUID
citz <- citz %>%
  mutate(
    db_code = str_pad(as.character(DBUID_Ididu), width = 11, pad = "0"),
    TDwell2021 = as.numeric(TDwell2021)
  )

# Join to TMF for DA/CSD codes
citz_geo <- citz %>%
  left_join(tmf_min, by = "db_code")

# Use DAUID from CITZ if available, otherwise use TMF da_code
if ("DAUID" %in% names(citz)) {
  citz_geo <- citz_geo %>%
    mutate(da_code = if_else(is.na(da_code), as.character(DAUID), da_code))
}

log_info("CITZ with geography: {nrow(citz_geo)} rows")
log_info("  - With CSD: {sum(!is.na(citz_geo$csd_code))}")
log_info("  - With DA: {sum(!is.na(citz_geo$da_code))}")

# ---- 5.4) Clean CITZ at PHH_ID level ----
log_info("Standardizing CITZ columns at PHH_ID level")

# Compute combined_max once to reuse for both character and numeric versions
citz_combined_max_vals <- mapply(
  compute_combined_max,
  citz_geo$Wired_Max_Threshold_Current,
  citz_geo$Wireless_Max_Threshold_Current
)

citz_clean <- citz_geo %>%
  transmute(
    PHH_ID, # Keep PHH_ID for direct join!
    db_code,
    da_code,
    csd_code = as.integer(CENSUS_SUBDIVISION_ID),
    csd_name = CENSUS_SUBDIVISION_NAME,
    TDwell2021,

    # Wired availability flags (from parsed speeds) - convert TRUE/FALSE to 1/0
    citz_wired_50_10 = as.numeric(wired_flags$is_wired_50_10),
    citz_wired_25_5 = as.numeric(wired_flags$is_wired_25_5),
    citz_wired_10_2 = as.numeric(wired_flags$is_wired_10_2),
    citz_wired_5_1 = as.numeric(wired_flags$is_wired_5_1),
    citz_wired_lt5_1 = as.numeric(wired_flags$is_wired_lt5_1),

    # Wireless availability flags - convert TRUE/FALSE to 1/0
    citz_wireless_50_10 = as.numeric(wireless_flags$is_wireless_50_10),
    citz_wireless_25_5 = as.numeric(wireless_flags$is_wireless_25_5),
    citz_wireless_10_2 = as.numeric(wireless_flags$is_wireless_10_2),
    citz_wireless_5_1 = as.numeric(wireless_flags$is_wireless_5_1),
    citz_wireless_lt5_1 = as.numeric(wireless_flags$is_wireless_lt5_1),

    # Combined availability flags (wired OR wireless) - convert TRUE/FALSE to 1/0
    citz_combined_50_10 = as.numeric(combined_flags$is_combined_50_10),
    citz_combined_25_5 = as.numeric(combined_flags$is_combined_25_5),
    citz_combined_10_2 = as.numeric(combined_flags$is_combined_10_2),
    citz_combined_5_1 = as.numeric(combined_flags$is_combined_5_1),
    citz_combined_lt5_1 = as.numeric(combined_flags$is_combined_lt5_1),

    # Original max thresholds (for reference)
    citz_wired_max = Wired_Max_Threshold_Current,
    citz_wireless_max = Wireless_Max_Threshold_Current,

    # Combined max threshold (higher of wired or wireless)
    citz_combined_max = citz_combined_max_vals,

    # Numeric version of combined max threshold for regression/analysis
    # 5=50_10, 4=25_5, 3=10_2, 2=5_1, 1=<5_1, 0=NA
    citz_combined_max_numeric = vapply(
      citz_combined_max_vals,
      threshold_to_numeric,
      FUN.VALUE = 0
    ),

    # Connectivity status
    connectivity_status = CONNECTIVITY_STATUS
  )

# Save CITZ clean
citz_clean_path <- file.path(output_path, "citz_clean.csv")
write_csv(citz_clean, citz_clean_path)
log_info("CITZ clean saved: {citz_clean_path} ({nrow(citz_clean)} rows)")

# =============================================================================
# SECTION 6: DIRECT PHH_ID TO PHH_ID COMPARISON
# =============================================================================
log_info("==== PART 3: PHH_ID to PHH_ID comparison ====")

# ---- 6.1) Join PHH and CITZ by PHH_ID ----
# KEY: Direct 1:1 join - no aggregation needed!
# Each PHH_ID exists in both datasets and represents the same location
log_info("Joining PHH and CITZ by PHH_ID (direct 1:1 join)")

# Inner join - only keep PHH_IDs that exist in both datasets
comparison <- nbd_clean %>%
  inner_join(
    citz_clean %>%
      select(-db_code, -da_code, -csd_code), # Keep geography from PHH side
    by = "PHH_ID",
    suffix = c("_phh", "_citz")
  ) %>%
  # Re-join geography (from PHH side which has TMF codes)
  left_join(
    nbd_clean %>% select(PHH_ID, db_code, da_code, csd_code)
    # by = "PHH_ID"
  )

log_info("Comparison: {nrow(comparison)} matched PHH_IDs")

# Save comparison
comparison_path <- file.path(output_path, "nbd_comparison.csv")
write_csv(comparison, comparison_path)
log_info("PHH comparison saved: {comparison_path} ({nrow(comparison)} rows)")

# ---- 6.2) Compute deltas and flag outliers ----
log_info("Computing deltas and outlier flags")

for (type in CONNECTION_TYPES) {
  for (tier in SPEED_TIERS) {
    phh_col <- paste0("phh_", type, "_", tier$label)
    citz_col <- paste0("citz_", type, "_", tier$label)
    delta_col <- paste0("delta_", type, "_", tier$label)
    outlier_col <- paste0("outlier_", type, "_", tier$label)

    if (phh_col %in% names(comparison) && citz_col %in% names(comparison)) {
      comparison <- comparison %>%
        mutate(
          # Delta: CITZ - PHH (proportion difference)
          !!delta_col := .data[[citz_col]] - .data[[phh_col]],

          # Outlier flag
          !!outlier_col := case_when(
            is.na(.data[[delta_col]]) ~ "NA",
            .data[[delta_col]] >= OUTLIER_THRESHOLD ~ "CITZ >> PHH",
            .data[[delta_col]] <= -OUTLIER_THRESHOLD ~ "PHH >> CITZ",
            TRUE ~ "OK"
          )
        )
    }
  }
}

# Summary statistics
log_info("Outlier summary at PHH_ID level:")
for (type in CONNECTION_TYPES) {
  for (tier in SPEED_TIERS) {
    outlier_col <- paste0("outlier_", type, "_", tier$label)
    if (outlier_col %in% names(comparison)) {
      n_total <- sum(!is.na(comparison[[outlier_col]]))
      n_ok <- sum(comparison[[outlier_col]] == "OK", na.rm = TRUE)
      n_citz <- sum(comparison[[outlier_col]] == "CITZ >> PHH", na.rm = TRUE)
      n_phh <- sum(comparison[[outlier_col]] == "PHH >> CITZ", na.rm = TRUE)

      log_info(
        "  {type} {tier$label}: {n_ok}/{n_total} OK, {n_citz} CITZ higher, {n_phh} PHH higher"
      )
    }
  }
}

# ---- 6.3) Save PHH_ID comparison ----
comparison_path <- file.path(output_path, "nbd_comparison.csv")
write_csv(comparison, comparison_path)
log_info(
  "PHH_ID comparison saved: {comparison_path} ({nrow(comparison)} rows)"
)

# =============================================================================
# SECTION 7: AGGREGATE TO CSD AND DA LEVELS
# =============================================================================
log_info("==== PART 4: Aggregate to CSD/DA levels ====")

# ---- 7.1) CSD-level aggregation ----
log_info("Aggregating to CSD level")

csd_comparison <- comparison %>%
  filter(!is.na(csd_code)) %>%
  group_by(csd_code) %>%
  summarise(
    # Basic counts
    n_phh_id = n(),

    # Mean deltas
    across(
      starts_with("delta_"),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),

    # Outlier counts by type
    across(
      starts_with("outlier_"),
      ~ sum(.x == "OK", na.rm = TRUE),
      .names = "n_{.col}_ok"
    ),
    across(
      starts_with("outlier_"),
      ~ sum(.x == "CITZ >> PHH", na.rm = TRUE),
      .names = "n_{.col}_citz_higher"
    ),
    across(
      starts_with("outlier_"),
      ~ sum(.x == "PHH >> CITZ", na.rm = TRUE),
      .names = "n_{.col}_phh_higher"
    ),

    .groups = "drop"
  ) %>%
  # Add CSD name from CITZ data
  left_join(
    citz_clean %>%
      distinct(csd_code, csd_name) |>
      mutate(csd_code = as.character(csd_code)),
    by = "csd_code"
  ) %>%
  relocate(csd_code, csd_name, n_phh_id)

# Save CSD comparison
csd_comparison_path <- file.path(output_path, "csd_comparison.csv")
write_csv(csd_comparison, csd_comparison_path)
log_info(
  "CSD comparison saved: {csd_comparison_path} ({nrow(csd_comparison)} rows)"
)

# ---- 7.2) DB-level aggregation ----
# DB = Dissemination Block level (one level below DA)
log_info("Aggregating to DB level")

db_comparison <- comparison %>%
  filter(!is.na(db_code)) %>%
  group_by(db_code) %>%
  summarise(
    # Basic counts
    n_phh_id = n(),

    # Keep geography
    da_code = first(na.omit(da_code)),
    csd_code = first(na.omit(csd_code)),

    # Mean deltas
    across(
      starts_with("delta_"),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),

    # Outlier counts by type
    across(
      starts_with("outlier_"),
      ~ sum(.x == "OK", na.rm = TRUE),
      .names = "n_{.col}_ok"
    ),
    across(
      starts_with("outlier_"),
      ~ sum(.x == "CITZ >> PHH", na.rm = TRUE),
      .names = "n_{.col}_citz_higher"
    ),
    across(
      starts_with("outlier_"),
      ~ sum(.x == "PHH >> CITZ", na.rm = TRUE),
      .names = "n_{.col}_phh_higher"
    ),

    .groups = "drop"
  ) %>%
  relocate(db_code, da_code, csd_code, n_phh_id)

# Save DB comparison
db_comparison_path <- file.path(output_path, "db_comparison.csv")
write_csv(db_comparison, db_comparison_path)
log_info(
  "DB comparison saved: {db_comparison_path} ({nrow(db_comparison)} rows)"
)

# ---- 7.3) DA-level aggregation ----
log_info("Aggregating to DA level")

da_comparison <- comparison %>%
  filter(!is.na(da_code)) %>%
  group_by(da_code) %>%
  summarise(
    # Basic counts
    n_phh_id = n(),

    # Mean deltas
    across(
      starts_with("delta_"),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),

    # Outlier counts by type
    across(
      starts_with("outlier_"),
      ~ sum(.x == "OK", na.rm = TRUE),
      .names = "n_{.col}_ok"
    ),
    across(
      starts_with("outlier_"),
      ~ sum(.x == "CITZ >> PHH", na.rm = TRUE),
      .names = "n_{.col}_citz_higher"
    ),
    across(
      starts_with("outlier_"),
      ~ sum(.x == "PHH >> CITZ", na.rm = TRUE),
      .names = "n_{.col}_phh_higher"
    ),

    .groups = "drop"
  )

# Save DA comparison
da_comparison_path <- file.path(output_path, "da_comparison.csv")
write_csv(da_comparison, da_comparison_path)
log_info(
  "DA comparison saved: {da_comparison_path} ({nrow(da_comparison)} rows)"
)

# =============================================================================
# SECTION 8: DATA DICTIONARIES
# =============================================================================
log_info("==== PART 5: Creating data dictionaries ====")

# ---- PHH clean dictionary ----
nbd_labels <- list(
  "PHH_ID" = "Pseudo-household ID (unique identifier in both PHH and CITZ)",
  "db_code" = "Dissemination Block code (11-digit, zero-padded)",
  "da_code" = "Dissemination Area code from TMF",
  "csd_code" = "Census Subdivision code from TMF",
  "phh_wired_50_10" = "PHH: 1 if wired >= 50/10 Mbps available, else 0",
  "phh_wired_25_5" = "PHH: 1 if wired >= 25/5 Mbps available, else 0",
  "phh_wired_10_2" = "PHH: 1 if wired >= 10/2 Mbps available, else 0",
  "phh_wired_5_1" = "PHH: 1 if wired >= 5/1 Mbps available, else 0",
  "phh_wired_lt5_1" = "PHH: 1 if wired < 5/1 Mbps (below threshold)",
  "phh_wireless_50_10" = "PHH: 1 if wireless >= 50/10 Mbps available, else 0",
  "phh_wireless_25_5" = "PHH: 1 if wireless >= 25/5 Mbps available, else 0",
  "phh_wireless_10_2" = "PHH: 1 if wireless >= 10/2 Mbps available, else 0",
  "phh_wireless_5_1" = "PHH: 1 if wireless >= 5/1 Mbps available, else 0",
  "phh_wireless_lt5_1" = "PHH: 1 if wireless < 5/1 Mbps (below threshold)",
  "phh_combined_50_10" = "PHH: 1 if either wired or wireless >= 50/10 Mbps available, else 0",
  "phh_combined_25_5" = "PHH: 1 if either wired or wireless >= 25/5 Mbps available, else 0",
  "phh_combined_10_2" = "PHH: 1 if either wired or wireless >= 10/2 Mbps available, else 0",
  "phh_combined_5_1" = "PHH: 1 if either wired or wireless >= 5/1 Mbps available, else 0",
  "phh_combined_lt5_1" = "PHH: 1 if both wired and wireless < 5/1 Mbps (below threshold)",
  "phh_lte_mobile" = "PHH: 1 if LTE mobile available, else 0",
  "phh_combined_max" = "PHH: Maximum combined threshold category",
  "phh_combined_max_numeric" = "PHH: Numeric version of combined max threshold"
)
nbd_clean |> glimpse()
nbd_dict <- create_dictionary(nbd_clean, var_labels = nbd_labels)
write.csv(
  nbd_dict,
  file.path(output_path, "nbd_clean_dict.csv"),
  row.names = FALSE
)

citz_clean |> glimpse()
# ---- CITZ clean dictionary ----
citz_labels <- list(
  "PHH_ID" = "Pseudo-household ID (unique identifier in both PHH and CITZ)",
  "db_code" = "Dissemination Block code (11-digit, zero-padded)",
  "da_code" = "Dissemination Area code",
  "csd_code" = "Census Subdivision code",
  "csd_name" = "Census Subdivision name",
  "TDwell2021" = "2021 dwelling count (weight for aggregation)",
  "citz_wired_50_10" = "CITZ: 1 if wired >= 50/10 Mbps available, else 0",
  "citz_wired_25_5" = "CITZ: 1 if wired >= 25/5 Mbps available, else 0",
  "citz_wired_10_2" = "CITZ: 1 if wired >= 10/2 Mbps available, else 0",
  "citz_wired_5_1" = "CITZ: 1 if wired >= 5/1 Mbps available, else 0",
  "citz_wired_lt5_1" = "CITZ: 1 if wired < 5/1 Mbps (below threshold)",
  "citz_wireless_50_10" = "CITZ: 1 if wireless >= 50/10 Mbps available, else 0",
  "citz_wireless_25_5" = "CITZ: 1 if wireless >= 25/5 Mbps available, else 0",
  "citz_wireless_10_2" = "CITZ: 1 if wireless >= 10/2 Mbps available, else 0",
  "citz_wireless_5_1" = "CITZ: 1 if wireless >= 5/1 Mbps available, else 0",
  "citz_wireless_lt5_1" = "CITZ: 1 if wireless < 5/1 Mbps (below threshold)",
  "citz_combined_50_10" = "CITZ: 1 if either wired or wireless >= 50/10 Mbps available, else 0",
  "citz_combined_25_5" = "CITZ: 1 if either wired or wireless >= 25/5 Mbps available, else 0",
  "citz_combined_10_2" = "CITZ: 1 if either wired or wireless >= 10/2 Mbps available, else 0",
  "citz_combined_5_1" = "CITZ: 1 if either wired or wireless >= 5/1 Mbps available, else 0",
  "citz_combined_lt5_1" = "CITZ: 1 if both wired and wireless < 5/1 Mbps (below threshold)",
  "citz_combined_max" = "CITZ: Maximum combined threshold (higher of wired or wireless)",
  "citz_combined_max_numeric" = "CITZ: Numeric version of combined max threshold (5=50_10, ..., 1=<5_1, 0=NA)",
  "citz_wired_max" = "CITZ: Wired maximum threshold (string)",
  "citz_wireless_max" = "CITZ: Wireless maximum threshold (string)",
  "connectivity_status" = "CITZ: Overall connectivity status"
)

citz_dict <- create_dictionary(citz_clean, var_labels = citz_labels)
write.csv(
  citz_dict,
  file.path(output_path, "citz_clean_dict.csv"),
  row.names = FALSE
)

comparison |> glimpse()
comp_labels <- list(
  "PHH_ID" = "Pseudo-household ID (join key)",
  "db_code" = "Dissemination Block code",
  "csd_code" = "Census Subdivision code",
  "csd_name" = "Census Subdivision name",
  "da_code" = "Dissemination Area code",
  "TDwell2021" = "2021 dwelling count (weight for aggregation)",
  "phh_wired_50_10" = "PHH: 1 if wired >= 50/10 Mbps available, else 0",
  "phh_wired_25_5" = "PHH: 1 if wired >= 25/5 Mbps available, else 0",
  "phh_wired_10_2" = "PHH: 1 if wired >= 10/2 Mbps available, else 0",
  "phh_wired_5_1" = "PHH: 1 if wired >= 5/1 Mbps available, else 0",
  "phh_wired_lt5_1" = "PHH: 1 if wired < 5/1 Mbps (below threshold)",
  "phh_wireless_50_10" = "PHH: 1 if wireless >= 50/10 Mbps available, else 0",
  "phh_wireless_25_5" = "PHH: 1 if wireless >= 25/5 Mbps available, else 0",
  "phh_wireless_10_2" = "PHH: 1 if wireless >= 10/2 Mbps available, else 0",
  "phh_wireless_5_1" = "PHH: 1 if wireless >= 5/1 Mbps available, else 0",
  "phh_wireless_lt5_1" = "PHH: 1 if wireless < 5/1 Mbps (below threshold)",
  "phh_combined_50_10" = "PHH: 1 if either wired or wireless >= 50/10 Mbps available, else 0",
  "phh_combined_25_5" = "PHH: 1 if either wired or wireless >= 25/5 Mbps available, else 0",
  "phh_combined_10_2" = "PHH: 1 if either wired or wireless >= 10/2 Mbps available, else 0",
  "phh_combined_5_1" = "PHH: 1 if either wired or wireless >= 5/1 Mbps available, else 0",
  "phh_combined_lt5_1" = "PHH: 1 if both wired and wireless < 5/1 Mbps (below threshold)",
  "phh_lte_mobile" = "PHH: 1 if LTE mobile available, else 0",
  "phh_combined_max" = "PHH: Maximum combined threshold category",
  "phh_combined_max_numeric" = "PHH: Numeric version of combined max threshold",
  "citz_wired_50_10" = "CITZ: 1 if wired >= 50/10 Mbps available, else 0",
  "citz_wired_25_5" = "CITZ: 1 if wired >= 25/5 Mbps available, else 0",
  "citz_wired_10_2" = "CITZ: 1 if wired >= 10/2 Mbps available, else 0",
  "citz_wired_5_1" = "CITZ: 1 if wired >= 5/1 Mbps available, else 0",
  "citz_wired_lt5_1" = "CITZ: 1 if wired < 5/1 Mbps (below threshold)",
  "citz_wireless_50_10" = "CITZ: 1 if wireless >= 50/10 Mbps available, else 0",
  "citz_wireless_25_5" = "CITZ: 1 if wireless >= 25/5 Mbps available, else 0",
  "citz_wireless_10_2" = "CITZ: 1 if wireless >= 10/2 Mbps available, else 0",
  "citz_wireless_5_1" = "CITZ: 1 if wireless >= 5/1 Mbps available, else 0",
  "citz_wireless_lt5_1" = "CITZ: 1 if wireless < 5/1 Mbps (below threshold)",
  "citz_combined_50_10" = "CITZ: 1 if either wired or wireless >= 50/10 Mbps available, else 0",
  "citz_combined_25_5" = "CITZ: 1 if either wired or wireless >= 25/5 Mbps available, else 0",
  "citz_combined_10_2" = "CITZ: 1 if either wired or wireless >= 10/2 Mbps available, else 0",
  "citz_combined_5_1" = "CITZ: 1 if either wired or wireless >= 5/1 Mbps available, else 0",
  "citz_combined_lt5_1" = "CITZ: 1 if both wired and wireless < 5/1 Mbps (below threshold)",
  "citz_combined_max" = "CITZ: Maximum combined threshold (higher of wired or wireless)",
  "citz_combined_max_numeric" = "CITZ: Numeric version of combined max threshold (5=50_10, ..., 1=<5_1, 0=NA)",
  "citz_wired_max" = "CITZ: Wired maximum threshold (string)",
  "citz_wireless_max" = "CITZ: Wireless maximum threshold (string)",
  "connectivity_status" = "CITZ: Overall connectivity status",
  "delta_wired_50_10" = "Difference: CITZ - PHH (wired 50/10 Mbps)",
  "delta_wired_25_5" = "Difference: CITZ - PHH (wired 25/5 Mbps)",
  "delta_wired_10_2" = "Difference: CITZ - PHH (wired 10/2 Mbps)",
  "delta_wired_5_1" = "Difference: CITZ - PHH (wired 5/1 Mbps)",

  "delta_wireless_50_10" = "Difference: CITZ - PHH (wireless 50/10 Mbps)",
  "delta_wireless_25_5" = "Difference: CITZ - PHH (wireless 25/5 Mbps)",
  "delta_wireless_10_2" = "Difference: CITZ - PHH (wireless 10/2 Mbps)",
  "delta_wireless_5_1" = "Difference: CITZ - PHH (wireless 5/1 Mbps)",

  "delta_combined_50_10" = "Difference: CITZ - PHH (combined 50/10 Mbps)",
  "delta_combined_25_5" = "Difference: CITZ - PHH (combined 25/5 Mbps)",
  "delta_combined_10_2" = "Difference: CITZ - PHH (combined 10/2 Mbps)",
  "delta_combined_5_1" = "Difference: CITZ - PHH (combined 5/1 Mbps)",

  "outlier_wired_50_10" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wired 50/10 Mbps)",
  "outlier_wired_25_5" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wired 25/5 Mbps)",
  "outlier_wired_10_2" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wired 10/2 Mbps)",
  "outlier_wired_5_1" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wired 5/1 Mbps)",
  "outlier_wireless_50_10" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wireless 50/10 Mbps)",
  "outlier_wireless_25_5" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wireless 25/5 Mbps)",
  "outlier_wireless_10_2" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wireless 10/2 Mbps)",
  "outlier_wireless_5_1" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (wireless 5/1 Mbps)",
  "outlier_combined_50_10" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (combined 50/10 Mbps)",
  "outlier_combined_25_5" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (combined 25/5 Mbps)",
  "outlier_combined_10_2" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (combined 10/2 Mbps)",
  "outlier_combined_5_1" = "Outlier flag: OK, CITZ >> PHH, PHH >> CITZ, or NA (combined 5/1 Mbps)"
)


comp_dict <- create_dictionary(comparison, var_labels = comp_labels)
write.csv(
  comp_dict,
  file.path(output_path, "nbd_comparison_dict.csv"),
  row.names = FALSE
)

log_info("Data dictionaries saved")

# =============================================================================
# SECTION 9: SUMMARY
# =============================================================================
log_info("==== SUMMARY ====")
log_info("Files created:")
log_info("  1. nbd_clean.csv - PHH data at PHH_ID level")
log_info("  2. citz_clean.csv - CITZ data at PHH_ID level")
log_info("  3. nbd_comparison.csv - Direct PHH_ID to PHH_ID comparison")
log_info("  4. db_comparison.csv - DB-level summary")
log_info("  5. da_comparison.csv - DA-level summary")
log_info("  6. csd_comparison.csv - CSD-level summary")
log_info("")
log_info("Comparison statistics:")
log_info("  - Total matched PHH_IDs: {nrow(comparison)}")
log_info("  - Unique DBs: {nrow(db_comparison)}")
log_info("  - Unique DAs: {nrow(da_comparison)}")
log_info("  - Unique CSDs: {nrow(csd_comparison)}")
log_info("")
log_info("Done: 17_db_level_connectivity_comparison.R")

# =============================================================================
# ADDITIONAL NOTES FOR FUTURE ANALYSTS
# =============================================================================
#
# WHAT THIS ENABLES:
#   1. PowerBI dashboards can load clean CSVs directly
#   2. No complex calculations in PowerBI - just visualization
#   3. Can drill down: CSD → DA → DB → PHH_ID for investigation
#   4. Outlier flags highlight areas needing data quality review
#
# POWERBI DASHBOARD SUGGESTIONS:
#   1. CSD summary: Map of mean delta by CSD
#   2. DB/DA summary: Table of DB codes with outlier counts
#   3. Outlier summary: Bar chart of outlier counts by type
#   4. Drill-through: Click CSD → see DA breakdown → click DA → see DB details → click DB → see PHH_ID
#   5. Filters: By delta range, outlier type, CSD/DA/DB code
#
# GEOGRAPHY HIERARCHY:
#   CSD (municipality) → DA (neighborhood, 400-700 people) → DB (few households) → PHH_ID (point)
#
# DATA QUALITY NOTES:
#   - PHH_ID must exist in both datasets for comparison
#   - Inner join keeps matched PHH_IDs only
#   - To see unmatched: use left_join or full_join variants
#   - delta = 1 means 100% disagreement (CITZ says available, PHH says not)
#
# EXTENSIONS:
#   - Add time-series: compare snapshots from different dates
#   - Add confidence intervals if sample sizes available
#   - Add statistical tests for significance
# =============================================================================
