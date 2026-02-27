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

# ==============================================================================
# SCRIPT: 14_bc_phh_connectivity_data.R
# PURPOSE: Load NBD-PHH-Speeds for British Columbia and aggregate to census
#          geography levels (DA and CSD) with PHH-weighted connectivity metrics.
#
# WHY THIS MATTERS:
# - NBD (National Broadband Data) from ISED provides pseudo-household (PHH)
#   speed availability at 250m precision points across Canada
# - Aggregating to census geographies allows BC Stats to analyze broadband
#   connectivity at community (CSD) and neighborhood (DA) levels
# - These metrics support policy decisions, infrastructure planning, and
#   universal broadband fund targeting
#
# WHAT THIS SCRIPT DOES:
# 1. Loads PHH speed availability data (Current snapshot)
# 2. Loads PHH demographic reference points (for DBUID mapping)
# 3. Loads TMF (Translation/Matching File) to link DB → DA → CSD
# 4. Joins speed data to geographic codes
# 5. Aggregates PHH-level speeds to DA and CSD using proportions
# 6. Computes max-threshold distributions (enum categories)
# 7. Creates served flag (≥75% at 50/10 Mbps = served)
# 8. Exports CSVs and data dictionaries
#
# KEY CONCEPTS:
# - PHH (Pseudo-Household): Representative point for dwelling clusters, used by
#   ISED to estimate coverage without individual address data
# - DGUID: Statistics Canada's geographic unique identifier (structure: VINTAGE + TYPE + SCHEMA + UID)
# - TMF: Translation/Matching File that links different census geography levels
# - Speed tiers: Download/Upload Mbps (e.g., 50_10 = 50 Mbps down, 10 Mbps up)
# - Combined: Either wired OR wireless available at that tier
# - Served: 75%+ of PHHs meet the 50/10 target (CRTC standard)
# ==============================================================================

#
# ==============================================================================
# SECTION 1: CONFIGURATION - File Paths and Parameters
# ==============================================================================
#
# HOW DATA FLOWS THROUGH THIS SCRIPT:
#
#   [NBD PHH Speeds CSV] ─┬─> [Join by PHH_ID] ─┐
#                        │                       ├─> [Join by DB code] ─> [Aggregate to DA] ─> DA CSV
#   [PHH21 Points CSV] ──┘                       │
#                                                ├─> [Join by DB code] ─> [Aggregate to CSD] ─> CSD CSV
#   [TMF CSV] ──────────────────────────────────┘
#
# The TMF (Translation Matching File) is the glue that links:
#   DB (Dissemination Block) → DA (Dissemination Area) → CSD (Census Subdivision)
#
# Without TMF, we'd need spatial joins (slower, requires boundary shapefiles)
# With TMF, we can do direct table joins (faster, just DBUID matching)

# ==============================================================================
# SECTION 2: LOAD REQUIRED PACKAGES
# ==============================================================================
# Core data manipulation
library(forcats) # Factor handling (for enum threshold levels)
library(sf) # Spatial data (if using boundary files)
library(dplyr) # Data transformation
library(readr) # Fast CSV reading
library(stringr) # String manipulation
library(tidyr) # Pivoting/widening data

# Date and formatting
library(lubridate) # Date handling
library(glue) # String interpolation
library(ggplot2) # Visualization (optional, for maps)

# BC-specific data access
library(bcdata) # BC Data Catalogue API (for searching census codes)
library(datadictionary) # Create data dictionaries with variable labels

# Source utility functions (plotting helpers, etc.)
source("./src/utils.R")

# ==============================================================================
# SECTION 3: LOAD CONFIGURATION
# ==============================================================================
# Config file (config.yml) contains:
#   - lan_path: Network drive path
#   - connectivity_data_path: Path to broadband data folder
#   - file_path/tmf_file_path: TMF folder location
#   - file_name/tmf_file_name: TMF filename
config <- config::get()

# Extract key paths from config
lan_path <- config$lan_path
connectivity_data_path <- config$conectivity_data_path


# ==============================================================================
# SECTION 4: DEFINE DATA SOURCE PATHS
# ==============================================================================
#
# DATA SOURCE A: NBD PHH Speeds (Current)
# - File: PHH_Speeds_Current-PHH_Vitesses_Actuelles_BC.csv
# - Contains: Speed availability at each pseudo-household point for BC
# - Fields: PHH_ID, boolean flags for each speed tier (5/1, 10/2, 25/5, 50/10)
# - This is ISED's latest "current" snapshot (typically 2019 or 2023 data)
#
# DATA SOURCE B: PHH21 Demographics (BC)
# - File: PHH-BC.csv
# - Contains: PHH_ID, DBUID_Ididu (Dissemination Block UID), population/dwelling weights
# - Purpose: Links PHH points to geographic areas via DBUID
# - Note: Weight columns often contain zeros, limiting their usefulness
#
# DATA SOURCE C: TMF (Translation Matching File)
# - File: config$file_name$tmf_file_name (e.g., "tmf2021.csv")
# - Contains: DGUID-based mappings between census geographies
# - Key columns: DSSMNTNBLC (DB), DSSMNTNRD (DA), CNSSCNSLDT (CSD)
# - Filter: PRUID = "2021A000259" (BC = 59)
#
# OPTIONAL: Census Boundary Shapefiles
# - Use if you need spatial joins instead of TMF
# - DA boundary: da_000b21a_e.shp
# - CSD boundary: csd_000b21a_e.shp
# - Currently commented out (not needed with TMF approach)

# PHH Speeds (Current) - main connectivity data
path_phh_current_csv <- file.path(
  lan_path,
  connectivity_data_path,
  "NBD_PHH_Speeds",
  "PHH_Speeds_Current-PHH_Vitesses_Actuelles_BC.csv"
)

# PHH21 Demographics - for DBUID mapping and optional weighting
# DBUID = Dissemination Block Unique Identifier (smallest census unit)
path_phh21_points_csv <- file.path(
  lan_path,
  connectivity_data_path,
  "PHH_2021_CSV\\PHH_2021_CSV\\PHH-BC.csv"
)


# file linking DB → DA → CSD
# the metadata says 2016, but the data itself says its from census 2021, so use as crosswalk basis
bcdata::bcdc_search("Dissemination Block")
bcdata::bcdc_tidy_resources("76909e49-8ba8-44b1-b69e-dba1fe9ecfba")
db_shapefiles <- bcdata::bcdc_query_geodata(
  '76909e49-8ba8-44b1-b69e-dba1fe9ecfba'
) |>
  collect()

db_da_csd_ids <- db_shapefiles |>
  janitor::clean_names() |>
  select(
    db_code = dissemination_block_id,
    da_code = dissemination_area_id,
    csd_code = census_subdivision_id,
    land_area = feature_area_sqm
  ) |>
  st_drop_geometry() |>
  distinct(db_code, da_code, csd_code, land_area)
#  file linking DB → DA → CSD

# Province filter: BC = 59 (PRUID = "2021A000259")
pruid_filter <- "2021A000259"

# Weighting option (currently disabled - weight columns have zeros)
use_weighted <- FALSE
weight_col <- NA_character_ # Could use "TDwell2021_TLog2021" or "Pop2021" if available

# Output configuration
output_path <- file.path(
  lan_path,
  connectivity_data_path,
  "outputs"
)
dir.create(output_path, showWarnings = FALSE)

out_da_csv <- file.path(output_path, "da_phh_current_coverage_bc.csv")
out_csd_csv <- file.path(output_path, "csd_phh_current_coverage_bc.csv")


# ==============================================================================
# SECTION 5: LOAD AND PREPARE PHH SPEEDS DATA
# ==============================================================================
#
# WHAT WE'RE DOING HERE:
# 1. Load the PHH speeds CSV
# 2. Verify required columns exist
# 3. Convert boolean text ("Yes"/"No") to numeric (0/1)
# 4. Convert threshold categories to ordered factors
#
# UNDERSTANDING THE COLUMNS:
# - bool_cols: Binary availability flags (1 = available, 0 = not available)
#   Format: [Technology]_[Download]_[Upload]_[Language]
#   Examples: Combined_50_10_Combine, Wired_25_5_Filaire, Wireless_10_2_Sans_fil
#
# - enum_cols: Categorical maximum threshold reached
#   Values: "", "<5_1", "5_1", "10_2", "25_5", "50_10"
#   Represents the HIGHEST speed tier available at that PHH point
#
# SPEED TIER NOTATION:
# "50_10" means 50 Mbps download / 10 Mbps upload
# "Combined" = wired OR wireless available (either technology works)
# "Wired" = fiber/cable/DSL only
# "Wireless" = fixed wireless/satellite only

# Define column groups - these are the boolean availability flags
bool_cols <- c(
  "Combined_lt5_1_Combine", # Below 5/1 Mbps (combined)
  "Wired_lt5_1_Filaire", # Below 5/1 Mbps (wired only)
  "Wireless_lt5_Sans_fil", # Below 5/1 Mbps (wireless only)
  "Combined_5_1_Combine", # At least 5/1 Mbps (combined)
  "Wired_5_1_Filaire", # At least 5/1 Mbps (wired only)
  "Wireless_5_1_Sans_fil", # At least 5/1 Mbps (wireless only)
  "Combined_10_2_Combine", # At least 10/2 Mbps (combined)
  "Wired_10_2_Filaire", # At least 10/2 Mbps (wired only)
  "Wireless_10_2_Sans_fil", # At least 10/2 Mbps (wireless only)
  "Combined_25_5_Combine", # At least 25/5 Mbps (combined)
  "Wired_25_5_Filaire", # At least 25/5 Mbps (wired only)
  "Wireless_25_5_Sans_fil", # At least 25/5 Mbps (wireless only)
  "Combined_50_10_Combine", # At least 50/10 Mbps (combined)
  "Wired_50_10_Filaire", # At least 50/10 Mbps (wired only)
  "Wireless_50_10_Sans_fil", # At least 50/10 Mbps (wireless only)
  "Avail_LTE_Mobile_Dispo" # LTE mobile service available
)

# Maximum threshold categories - the highest tier available at each PHH
enum_cols <- c(
  "Combined_Max_Threshold-Combine_Seuil_Max", # Combined max tier
  "Wired_Max_Threshold-Filaire_Seuil_Max", # Wired max tier
  "Wireless_Max_Threshold-Sans_fil_Seuil_Max" # Wireless max tier
)

# Load PHH speeds CSV
phh_spd <- read_csv(path_phh_current_csv, show_col_types = FALSE)

# Validate: Check all required columns exist
req_spd <- c("PHH_ID", bool_cols, enum_cols)
missing_spd <- setdiff(req_spd, names(phh_spd))
if (length(missing_spd) > 0) {
  stop(
    "Missing fields in PHH Current CSV: ",
    paste(missing_spd, collapse = ", ")
  )
}

# Convert Yes/No text to numeric 0/1 for calculations
# Why numeric? Easier to compute proportions (mean of 0s and 1s = proportion)
phh_spd[bool_cols] <- lapply(
  phh_spd[bool_cols],
  function(x) as.numeric(as.character(x))
)

# Define enum levels in ORDER (important for factor ordering)
# Empty string = no service or unknown
# <5_1 = below 5/1 (underserved)
# 5_1, 10_2, 25_5, 50_10 = progressively better service
enum_levels <- c("", "<5_1", "5_1", "10_2", "25_5", "50_10")
phh_spd[enum_cols] <- lapply(
  phh_spd[enum_cols],
  function(x) forcats::fct(x, levels = enum_levels)
)

# ==============================================================================
# SECTION 6: LOAD PHH21 DEMOGRAPHIC POINTS
# ==============================================================================
#
# WHY WE NEED THIS DATA:
# - PHH Speeds file has PHH_ID but not geographic codes
# - PHH21 Points file links PHH_ID → DBUID_Ididu (Dissemination Block UID)
# - DBUID is the key to join with TMF (which maps DB → DA → CSD)
#
# OPTIONAL WEIGHTING:
# - The file includes population and dwelling counts per PHH
# - Could weight aggregation by population or dwellings
# - HOWEVER: These fields often contain zeros, limiting practical use
# - Currently disabled (use_weighted = FALSE)
#
# COLUMNS WE EXTRACT:
# - PHH_ID: Links to speed data
# - DBUID_Ididu: Dissemination Block UID (geographic key)
# - Pop2021, TDwell2021_TLog2021, URDwell2021_RH2021: Weight candidates

# Load PHH21 points - only needed columns to save memory
phh21 <- read_csv(
  path_phh21_points_csv,
  show_col_types = FALSE,
  col_types = cols(
    PHH_ID = col_double(), # Links to speed data
    DBUID_Ididu = col_double(), # Geographic key (DB)
    Pop2021 = col_double(), # Population (often zero)
    TDwell2021_TLog2021 = col_double(), # Total dwellings (often zero)
    URDwell2021_RH2021 = col_double(), # Urban/rural dwellings (often zero)
    Pruid_Pridu = col_double(), # Province UID (filter to BC=59)
    .default = col_skip() # Skip everything else
  )
)
# Note: Weight columns contain zeros, so weighting is not practical
phh21 |> glimpse()
# ==============================================================================
# SECTION 7: LOAD TMF AND EXTRACT GEOGRAPHIC CODES
# ==============================================================================
# https://catalogue.data.gov.bc.ca/dataset/current-census-dissemination-blocks
#
# - Current Census Dissemination Blocks
# - Maps between different census geography levels
# - Allows us to go from DB (Dissemination Block) → DA → CSD
#
# DGUID STRUCTURE (Statistics Canada Geographic Unique Identifier):
# Format: VINTAGE(4) + TYPE(1) + SCHEMA(4) + GEOGRAPHIC_UNIQUE_ID(1-12)
#
# Example breakdown:
#   "2021A000259"        → PRUID (Province): 2021, Type A, Schema 0002, UID 59 (BC)
#   "2021A00051001519"  → CSDUID:          2021, Type A, Schema 0005, UID 1001519
#   "2021S051210010165" → DAUID:           2021, Type S, Schema 0512, UID 10010165
#   "2021S051310010165001" → DBUID:       2021, Type S, Schema 0513, UID 10010165001
#
# TYPE CODES:
#   A = Administrative (standard census geographies)
#   S = Statistical (derived/aggregated units)
#   C = Combined
#   B = Blended
#   Z = Other
#

db_da_csd_ids |> glimpse()


# ==============================================================================
# SECTION 8: LINK PHH TO GEOGRAPHIC CODES
# ==============================================================================
#
# THE JOIN CHAIN:
# PHH Speeds (has PHH_ID)
#   → PHH21 Points (has PHH_ID + DBUID_Ididu)
#   → TMF (has DB code → DA code → CSD code)
#
# WHY TWO STEPS?
# - PHH Speeds doesn't have geographic codes directly
# - PHH21 Points provides the DBUID key
# - TMF translates DBUID to higher geographies
#
# PROCESS:
# 1. Convert PHH21 DBUID from numeric to zero-padded character (11 digits)
# 2. Join to TMF by matching DB codes

# Convert PHH21 DBUID to zero-padded character (DB codes are 11 digits)
# Example: 10010165001 (numeric) → "10010165001" (character with leading zeros preserved)
phh_keys <- phh21 %>%
  transmute(
    PHH_ID,
    db_code = str_pad(as.character(DBUID_Ididu), width = 11, pad = "0")
  )

# Join PHH to TMF - this brings in DA and CSD codes
phh_joined <- phh_keys %>%
  left_join(db_da_csd_ids, by = "db_code")

# Sanity check: How many PHHs didn't match to a geographic area?
sum(is.na(phh_joined$da_code)) # PHHs with no DA mapping
sum(is.na(phh_joined$csd_code)) # PHHs with no CSD mapping

phh_joined |> glimpse()

# ==============================================================================
# SECTION 9: JOIN SPEEDS TO GEOGRAPHIC AREAS
# ==============================================================================
#
# NOW WE COMBINE:
# - PHH Speeds (availability data) with
# - PHH Joined (geographic codes from TMF)
#
# This gives us: for each PHH point → speeds + DA code + CSD code
# Then we can aggregate to DA/CSD level

# Join speeds to PHH with geographic codes
# Inner join ensures we only keep PHHs that have geographic mapping
phh_speeds_with_geo <- phh_spd %>% # PHH speeds table (Current)
  inner_join(phh_joined, by = "PHH_ID") # brings in da_code, csd_code

phh_speeds_with_geo |> glimpse()

# Calculate missing mapping rate - how many PHH don't have geographic codes?
na_map_rate <- mean(
  is.na(phh_speeds_with_geo$da_code) | is.na(phh_speeds_with_geo$csd_code)
)
message("PHH rows without DA/CSD mapping: ", round(100 * na_map_rate, 2), "%")


# ==============================================================================
# SECTION 10: AGGREGATION HELPER FUNCTION
# ==============================================================================
#
# WHAT THIS DOES:
# - Computes either simple mean (unweighted) or weighted mean
# - Handles edge cases: empty vectors, all NA, zero weights
#
# WHY A CUSTOM FUNCTION?
# - We need to switch between weighted/unweighted based on use_weighted flag
# - Must handle NA values properly (ignore them in calculation)
# - Must handle zero weight sums (avoid division by zero)
#
# OUTPUT:
# - Proportion (0 to 1) representing share of PHHs with that speed tier available

prop_fun <- function(x, w = NULL) {
  # Handle empty input
  if (length(x) == 0) {
    return(NA_real_)
  }

  # Unweighted: simple mean (ignoring NAs)
  if (is.null(w)) {
    return(mean(x, na.rm = TRUE))
  }

  # Weighted: handle NAs in weights
  w[is.na(w)] <- 0 # Treat missing weights as zero

  # Check for valid weight sum
  if (sum(w) <= 0) {
    return(NA_real_)
  }

  # Weighted mean
  weighted.mean(x, wt = w, na.rm = TRUE)
}

# ==============================================================================
# SECTION 11: AGGREGATE TO DISSEMINATION AREA (DA) LEVEL
# ==============================================================================
#
# WHAT WE'RE COMPUTING FOR EACH DA:
# 1. n_phh: Count of PHH points in this DA
# 2. Proportions at each speed tier (combined, wired, wireless)
# 3. LTE mobile availability
# 4. Max threshold distribution (what's the highest tier reached?)
# 5. Served flag (≥75% at 50/10 = served)
#
# WHY DA LEVEL?
# - DA is the smallest census unit with full population data
# - Good for neighborhood-level analysis
# - More granular than CSD but not as fine as individual PHH points
#
# SPEED TIER LOGIC:
# "At least 5/1" means prop_combined_5_1 = proportion of PHHs where 5/1 IS available
# This is a CUMULATIVE measure (if you have 50/10, you also have 25/5, 10/2, 5/1)
# So prop_combined_50_10 <= prop_combined_25_5 <= prop_combined_10_2 <= prop_combined_5_1

wvec = NULL # No weighting (unweighted mean = simple proportion)

da_cov <- phh_speeds_with_geo %>%
  filter(!is.na(da_code)) %>%
  group_by(da_code) %>%
  summarize(
    n_phh = n(),

    # Combined tiers
    prop_combined_5_1 = prop_fun(Combined_5_1_Combine, wvec),
    prop_combined_10_2 = prop_fun(Combined_10_2_Combine, wvec),
    prop_combined_25_5 = prop_fun(Combined_25_5_Combine, wvec),
    prop_combined_50_10 = prop_fun(Combined_50_10_Combine, wvec),

    # Wired tiers
    prop_wired_5_1 = prop_fun(Wired_5_1_Filaire, wvec),
    prop_wired_10_2 = prop_fun(Wired_10_2_Filaire, wvec),
    prop_wired_25_5 = prop_fun(Wired_25_5_Filaire, wvec),
    prop_wired_50_10 = prop_fun(Wired_50_10_Filaire, wvec),

    # Wireless tiers
    prop_wireless_5_1 = prop_fun(Wireless_5_1_Sans_fil, wvec),
    prop_wireless_10_2 = prop_fun(Wireless_10_2_Sans_fil, wvec),
    prop_wireless_25_5 = prop_fun(Wireless_25_5_Sans_fil, wvec),
    prop_wireless_50_10 = prop_fun(Wireless_50_10_Sans_fil, wvec),

    # LTE mobile
    prop_lte_mobile = prop_fun(Avail_LTE_Mobile_Dispo, wvec),

    .groups = "drop"
  )

# ==============================================================================
# SECTION 12: COMPUTE MAX THRESHOLD DISTRIBUTION (ENUM)
# ==============================================================================
#
# WHAT IS MAX THRESHOLD?
# - Each PHH has a "maximum threshold" category - the highest speed tier available
# - This is the enum field: Combined_Max_Threshold-Combine_Seuil_Max
# - Values: "", "<5_1", "5_1", "10_2", "25_5", "50_10"
#
# WHY THIS MATTERS:
# - Proportions (above) tell us "what % have access to X"
# - Enums tell us "what's the distribution of BEST available speed"
# - Combined, they give a complete picture of connectivity
#
# EXAMPLE:
# - If prop_combined_50_10 = 0.60 and prop_combined_enum_50_10 = 0.40
# - Interpretation: 60% have 50/10 available, but only 40% have it as their BEST option
# - The other 20% have 50/10 but a higher tier is available to them too

# Max-threshold distribution (Combined technology)
da_enum <- phh_speeds_with_geo %>%
  filter(!is.na(da_code)) %>%
  mutate(
    Combined_Max_Threshold = as.character(
      `Combined_Max_Threshold-Combine_Seuil_Max`
    )
  ) %>%
  group_by(da_code, Combined_Max_Threshold) %>%
  summarize(
    n_enum = if (is.null(wvec)) n() else sum(wvec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(da_code) %>%
  mutate(prop_enum = n_enum / sum(n_enum)) %>%
  pivot_wider(
    names_from = Combined_Max_Threshold,
    values_from = prop_enum,
    names_prefix = "prop_combined_enum_",
    values_fill = 0
  )

# Served flag: ≥75% of PHHs at 50/10 combined = served
served_threshold <- 0.75
da_cov <- da_cov %>%
  left_join(da_enum, by = "da_code") %>%
  mutate(
    served_50_10_phh = if_else(prop_combined_50_10 >= served_threshold, 1L, 0L)
  )

# ==============================================================================
# SECTION 13: AGGREGATE TO CENSUS SUBDIVISION (CSD) LEVEL
# ==============================================================================
#
# WHAT IS CSD?
# - Census Subdivision = municipality or equivalent (city, town, village, etc.)
# - This is the main unit for community-level analysis
# - BC has ~800+ CSDs ranging from large cities to small unincorporated areas
#
# WHY AGGREGATE TO BOTH DA AND CSD?
# - DA: Fine-grained neighborhood analysis
# - CSD: Community/municipal level for policy and planning
# - Both are useful for different purposes
#
# THE SAME LOGIC applies as DA aggregation:
# - Count PHHs in each CSD
# - Calculate proportions at each speed tier
# - Compute enum distribution
# - Flag served areas (≥75% at 50/10)

csd_cov <- phh_speeds_with_geo %>%
  filter(!is.na(csd_code)) %>%
  group_by(csd_code) %>%
  summarize(
    n_phh = n(),

    # Combined tiers (wired OR wireless available)
    prop_combined_5_1 = prop_fun(Combined_5_1_Combine, wvec),
    prop_combined_10_2 = prop_fun(Combined_10_2_Combine, wvec),
    prop_combined_25_5 = prop_fun(Combined_25_5_Combine, wvec),
    prop_combined_50_10 = prop_fun(Combined_50_10_Combine, wvec),

    # Wired-only tiers
    prop_wired_5_1 = prop_fun(Wired_5_1_Filaire, wvec),
    prop_wired_10_2 = prop_fun(Wired_10_2_Filaire, wvec),
    prop_wired_25_5 = prop_fun(Wired_25_5_Filaire, wvec),
    prop_wired_50_10 = prop_fun(Wired_50_10_Filaire, wvec),

    # Wireless-only tiers
    prop_wireless_5_1 = prop_fun(Wireless_5_1_Sans_fil, wvec),
    prop_wireless_10_2 = prop_fun(Wireless_10_2_Sans_fil, wvec),
    prop_wireless_25_5 = prop_fun(Wireless_25_5_Sans_fil, wvec),
    prop_wireless_50_10 = prop_fun(Wireless_50_10_Sans_fil, wvec),

    # LTE mobile availability
    prop_lte_mobile = prop_fun(Avail_LTE_Mobile_Dispo, wvec),

    .groups = "drop"
  )
# phh_speeds_with_geo |> glimpse()

# CSD max threshold distribution
csd_enum <- phh_speeds_with_geo %>%
  filter(!is.na(csd_code)) %>%
  mutate(
    Combined_Max_Threshold = as.character(
      `Combined_Max_Threshold-Combine_Seuil_Max`
    )
  ) %>%
  group_by(csd_code, Combined_Max_Threshold) %>%
  summarize(
    n_enum = if (is.null(wvec)) n() else sum(wvec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(csd_code) %>%
  mutate(prop_enum = n_enum / sum(n_enum)) %>%
  pivot_wider(
    names_from = Combined_Max_Threshold,
    values_from = prop_enum,
    names_prefix = "prop_combined_enum_",
    values_fill = 0
  )

# Join enum data and create served flag
csd_cov <- csd_cov %>%
  left_join(csd_enum, by = "csd_code") %>%
  mutate(
    served_50_10_phh = if_else(prop_combined_50_10 >= served_threshold, 1L, 0L)
  )
csd_cov |> glimpse()
# ==============================================================================
# SECTION 14: EXPORT RESULTS TO CSV
# ==============================================================================
#
# OUTPUT FILES:
# 1. da_phh_current_coverage_bc.csv - DA-level connectivity metrics
# 2. csd_phh_current_coverage_bc.csv - CSD-level connectivity metrics
# 3. da_phh_spd_dict_dict.csv - Data dictionary for DA metrics
# 4. csd_phh_spd_dict_dict.csv - Data dictionary for CSD metrics
#
# WHY DATA DICTIONARIES?
# - Column names like "prop_combined_50_10" aren't self-explanatory
# - Data dictionaries document what each field means
# - Useful for future analysts and when sharing data

# Export the aggregated data
write_csv(da_cov, out_da_csv)
write_csv(csd_cov, out_csd_csv)

message("DA rows: ", nrow(da_cov), " | CSD rows: ", nrow(csd_cov))
message("DA CSV: ", out_da_csv)
message("CSD CSV: ", out_csd_csv)


# ==============================================================================
# SECTION 15: CREATE DATA DICTIONARIES
# ==============================================================================
#
# DATA DICTIONARY DEFINITIONS:
# These document what each output column means. Essential for:
# - Future reference (your future self will thank you!)
# - Sharing with other analysts
# - Publishing data to open data portal
#
# DA-LEVEL DICTIONARY:
# - da_code: DA identifier (from DGUID, excluding vintage/type/schema prefix)
# - n_phh: Count of PHH points matched to this DA
# - prop_combined_X_Y: Proportion with combined (wired OR wireless) at X/Y Mbps
# - prop_wired_X_Y: Proportion with wired only at X/Y Mbps
# - prop_wireless_X_Y: Proportion with wireless only at X/Y Mbps
# - prop_lte_mobile: Proportion with LTE mobile service
# - prop_combined_enum_X_Y: Distribution of max threshold category
# - served_50_10_phh: Binary flag (1 if ≥75% at 50/10)

da_phh_spd_dict_labels <- c(
  # Keys & counts
  "da_code" = "Dissemination Area (DA) code (short UID extracted from DA DGUID: PR(2)+CD(2)+DA(4))",
  "n_phh" = "Number of pseudo-households (PHHs) matched to this DA",

  # Combined (wired + wireless) availability proportions
  "prop_combined_5_1" = "Share of PHHs with combined (wired or wireless) availability at ≥ 5/1 Mbps",
  "prop_combined_10_2" = "Share of PHHs with combined availability at ≥ 10/2 Mbps",
  "prop_combined_25_5" = "Share of PHHs with combined availability at ≥ 25/5 Mbps",
  "prop_combined_50_10" = "Share of PHHs with combined availability at ≥ 50/10 Mbps",

  # Wired-only availability proportions
  "prop_wired_5_1" = "Share of PHHs with wired availability at ≥ 5/1 Mbps",
  "prop_wired_10_2" = "Share of PHHs with wired availability at ≥ 10/2 Mbps",
  "prop_wired_25_5" = "Share of PHHs with wired availability at ≥ 25/5 Mbps",
  "prop_wired_50_10" = "Share of PHHs with wired availability at ≥ 50/10 Mbps",

  # Wireless-only availability proportions
  "prop_wireless_5_1" = "Share of PHHs with wireless availability at ≥ 5/1 Mbps",
  "prop_wireless_10_2" = "Share of PHHs with wireless availability at ≥ 10/2 Mbps",
  "prop_wireless_25_5" = "Share of PHHs with wireless availability at ≥ 25/5 Mbps",
  "prop_wireless_50_10" = "Share of PHHs with wireless availability at ≥ 50/10 Mbps",

  # Mobile LTE
  "prop_lte_mobile" = "Share of PHHs where LTE mobile service is available",

  # Enum distribution (Combined_Max_Threshold-Combine_Seuil_Max)
  "n_enum" = "Denominator used for enum distribution in this DA (PHH count or weighted total if weights were used)",
  "prop_combined_enum_25_5" = "Proportion of PHHs whose maximum combined threshold category is 25/5",
  "prop_combined_enum_50_10" = "Proportion of PHHs whose maximum combined threshold category is 50/10",
  "prop_combined_enum_5_1" = "Proportion of PHHs whose maximum combined threshold category is 5/1",
  "prop_combined_enum_NA" = "Proportion of PHHs with missing/unknown maximum combined threshold (no enum value)",
  "prop_combined_enum_<5_1" = "Proportion of PHHs whose maximum combined threshold category is <5/1",
  "prop_combined_enum_10_2" = "Proportion of PHHs whose maximum combined threshold category is 10/2",

  # Served flag
  "served_50_10_phh" = "Binary flag: 1 if prop_combined_50_10 ≥ 0.75 (≥ 75% of PHHs at 50/10), else 0"
)

# Create and export DA data dictionary
da_phh_spd_dict_dict = create_dictionary(
  da_cov,
  var_labels = da_phh_spd_dict_labels
)

# Note: Using write.csv (comma delimiter) instead of write.csv2 (semicolon)
# because dictionary labels contain commas
write.csv(
  da_phh_spd_dict_dict,
  here::here(output_path, "da_phh_spd_dict_dict.csv")
)

# ==============================================================================
# SECTION 16: CSD-LEVEL DATA DICTIONARY
# ==============================================================================
# Same structure as DA dictionary but for Census Subdivision level
# CSD = municipality / community level (more useful for policy)

csd_phh_spd_dict_labels <- c(
  # Keys & counts
  "csd_code" = "Census Subdivision (CSD) code (short UID extracted from CSD DGUID: PR(2)+CD(2)+CSD(3))",
  "n_phh" = "Number of pseudo-households (PHHs) matched to this CSD",

  # Combined (wired + wireless) availability proportions
  "prop_combined_5_1" = "Share of PHHs with combined (wired or wireless) availability at ≥ 5/1 Mbps",
  "prop_combined_10_2" = "Share of PHHs with combined availability at ≥ 10/2 Mbps",
  "prop_combined_25_5" = "Share of PHHs with combined availability at ≥ 25/5 Mbps",
  "prop_combined_50_10" = "Share of PHHs with combined availability at ≥ 50/10 Mbps",

  # Wired-only availability proportions
  "prop_wired_5_1" = "Share of PHHs with wired availability at ≥ 5/1 Mbps",
  "prop_wired_10_2" = "Share of PHHs with wired availability at ≥ 10/2 Mbps",
  "prop_wired_25_5" = "Share of PHHs with wired availability at ≥ 25/5 Mbps",
  "prop_wired_50_10" = "Share of PHHs with wired availability at ≥ 50/10 Mbps",

  # Wireless-only availability proportions
  "prop_wireless_5_1" = "Share of PHHs with wireless availability at ≥ 5/1 Mbps",
  "prop_wireless_10_2" = "Share of PHHs with wireless availability at ≥ 10/2 Mbps",
  "prop_wireless_25_5" = "Share of PHHs with wireless availability at ≥ 25/5 Mbps",
  "prop_wireless_50_10" = "Share of PHHs with wireless availability at ≥ 50/10 Mbps",

  # Mobile LTE
  "prop_lte_mobile" = "Share of PHHs where LTE mobile service is available",

  # Enum distribution (Combined_Max_Threshold-Combine_Seuil_Max)
  "n_enum" = "Denominator used for enum distribution in this CSD (PHH count or weighted total if weights were used)",
  "prop_combined_enum_25_5" = "Proportion of PHHs whose maximum combined threshold category is 25/5",
  "prop_combined_enum_50_10" = "Proportion of PHHs whose maximum combined threshold category is 50/10",
  "prop_combined_enum_5_1" = "Proportion of PHHs whose maximum combined threshold category is 5/1",
  "prop_combined_enum_NA" = "Proportion of PHHs with missing/unknown maximum combined threshold (no enum value)",
  "prop_combined_enum_<5_1" = "Proportion of PHHs whose maximum combined threshold category is <5/1",
  "prop_combined_enum_10_2" = "Proportion of PHHs whose maximum combined threshold category is 10/2",

  # Served flag
  "served_50_10_phh" = "Binary flag: 1 if prop_combined_50_10 ≥ 0.75 (≥ 75% of PHHs at 50/10), else 0"
)

# Create and export CSD data dictionary
csd_phh_spd_dict_dict = create_dictionary(
  csd_cov,
  var_labels = csd_phh_spd_dict_labels
)

# Note: Using write.csv (comma delimiter) instead of write.csv2 (semicolon)
# because dictionary labels contain commas
write.csv(
  csd_phh_spd_dict_dict,
  here::here(output_path, "csd_phh_spd_dict_dict.csv")
)

# ----------------------------------------------------
# Data dictionary from Open data portal:
# ------------------------------------------+---------------------------------------------------------------------------------
# Field                                     | Description
# ------------------------------------------+---------------------------------------------------------------------------------
# PHH_ID                                    | Unique identifier for pseudo-household (PHH) representative point
# Combined_lt5_1_Combine                    | Boolean value indicating if speeds smaller than 5/1 Mbps Download/Upload (DL/UL)
#                                               are available considering all wired and wireless technologies
# Wired_lt5_1_Filaire                       | Boolean value indicating if speeds smaller than 5/1 Mbps DL/UL
#                                               are available looking at wired technologies only
# Wireless_lt5_Sans_fil                     | Boolean value indicating if speeds smaller than 5/1 Mbps DL/UL
#                                               are available looking at wireless technologies only
# Combined_5_1_Combine                      | Boolean value indicating if 5/1 Mbps DL/UL speeds
#                                               are available considering all wired and wireless technologies
# Wired_5_1_Filaire                         | Boolean value indicating if 5/1 Mbps DL/UL speeds
#                                               are available looking at wired technologies only
# Wireless_5_1_Sans_fil                     | Boolean value indicating if 5/1 Mbps DL/UL speeds
#                                               are available looking at wireless technologies only
# Combined_10_2_Combine                     | Boolean value indicating if 10/2 Mbps DL/UL speeds
#                                               are available considering all wired and wireless technologies
# Wired_10_2_Filaire                        | Boolean value indicating if 10/2 Mbps DL/UL speeds
#                                               are available looking at wired technologies only
# Wireless_10_2_Sans_fil                    | Boolean value indicating if 10/2 Mbps DL/UL speeds
#                                               are available looking at wireless technologies only
# Combined_25_5_Combine                     | Boolean value indicating if 25/5 Mbps DL/UL speeds
#                                               are available considering all wired and wireless technologies
# Wired_25_5_Filaire                        | Boolean value indicating if 25/5 Mbps DL/UL speeds
#                                               are available looking at wired technologies only
# Wireless_25_5_Sans_fil                    | Boolean value indicating if 25/5 Mbps DL/UL speeds
#                                               are available looking at wireless technologies only
# Combined_50_10_Combine                    | Boolean value indicating if 50/10 Mbps DL/UL speeds
#                                               are available considering all wired and wireless technologies
# Wired_50_10_Filaire                       | Boolean value indicating if 50/10 Mbps DL/UL speeds
#                                               are available looking at wired technologies only
# Wireless_50_10_Sans_fil                   | Boolean value indicating if 50/10 Mbps DL/UL speeds
#                                               are available looking at wireless technologies only
# Combined_Max_Threshold-Combine_Seuil_Max  | Enum value indicating the top speed threshold that is reached
#                                               considering all wired and wireless technologies
# Wired_Max_Threshold-Filaire_Seuil_Max     | Enum value indicating the top speed threshold that is reached
#                                               looking at wired technologies only
# Wireless_Max_Threshold-Sans_fil_Seuil_Max | Enum value indicating the top speeds threshold that is reached
#                                               looking at wireless technologies only
# Avail_LTE_Mobile_Dispo                    | Boolean value indicating if LTE Mobile service is available
# ------------------------------------------+---------------------------------------------------------------------------------

# ------------------------------------------+---------------------------------------------------------------------------------
# Data Type                                 | Values
# ------------------------------------------+---------------------------------------------------------------------------------
# Boolean                                   | 0 = False/No, 1 = True/Yes
# Enum                                      | String values within {"","<5_1","5_1","10_2","25_5","50_10"}
#                                           |   "50_10" being the highest threshold, it does not preclude the possibility
#                                           |   of higher speeds from being available
#                                           |   E.g.1: A speed of 400/100 Mbps meats the 50_10 threshold (falls in that category)
#                                           |   E.g.2: A speed of 50/5 Mbps falls under the 25_5 category because this is where
#                                           |            both download and upload criteria are met
#                                           |   E.g.3: A speed of 4/1 Mbps falls under the <5_1 category
# ------------------------------------------+---------------------------------------------------------------------------------
#-----------------------------------------------------
