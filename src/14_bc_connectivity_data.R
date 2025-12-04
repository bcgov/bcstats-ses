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

#
# # This script demonstrates loading NBD‑PHH‑Speeds (Current) for BC,
# and computing PHH‑weighted proportions (i.e., the share of PHHs that have a given speed tier).
# This follows ISED/CRTC’s recommended “pseudo‑household” approach for precision.

# Since NBD‑PHH‑Speeds is evaluated at pseudo‑household (PHH) representative points,
# we can aggregate to Census Subdivision (CSD) by spatially joining those PHH points to CSD polygons
# or if we have DB ids, we can directlly join to CSD via DGRF.

# Output dataset: csd_out
#
# n_phh: number of PHH points matched to the CSD (denominator for proportions).
# prop_combined_5_1, prop_combined_10_2, prop_combined_25_5, prop_combined_50_10: share of PHHs with at-least the tier available (Combined).
# Parallel metrics for Wired and Wireless only.
# prop_lte_mobile: share of PHHs with LTE mobile available.
# prop_combined_enum_<level>: distribution across max threshold categories ("", <5_1, 5_1, 10_2, 25_5, 50_10) indicating the top tier reached at each PHH (Combined).
# served_50_10_phh: binary flag if ≥ 75% PHHs in the CSD meet 50/10 (edit threshold as desired).
# 5-1 (10_2, etc.): 5 is download speed and 1 is upload speed.
#
# References
#
# National Broadband Data (NBD) overview and methodology, including pseudo‑households and precision at 250m: Innovation, Science and Economic Development Canada. [ised-isde.canada.ca]
# 2021 Census Boundary Files (CSD polygons, NAD83 LCC) and relationship file: Statistics Canada, Open Government. [open.canada.ca]

#
# ------------------
# --- Aggregate NBD-PHH-Speeds to Census Subdivision (CSD) level ---

# Packages
library(forcats)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(glue)
library(ggplot2)
library(datadictionary)
source("./src/utils.R") # get the functions for plotting maps

# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()

# Extract settings from config
lan_path <- config$lan_path
connectivity_data_path <- config$conectivity_data_path


########################################################################
# CSD info 2021 Dissemination Geographies Relationship File (DGRF) from Statistics Canada.
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21
# or https://www150.statcan.gc.ca/n1/pub/92-150-g/92-150-g2021001-eng.htm

# -------------------------------------------------------------
# Aggregate CURRENT NBD PHH speeds (BC) to DA and CSD via DGRF
# Date: Sys.Date()
# -------------------------------------------------------------

# -----------------------------
# 1) CONFIG — file paths
# -----------------------------

# A) PHH speeds (Current) for BC
path_phh_current_csv <- file.path(
  lan_path,
  connectivity_data_path,
  "NBD_PHH_Speeds",
  "PHH_Speeds_Current-PHH_Vitesses_Actuelles_BC.csv"
)

# B) PHH21 demographic points (BC) — needed for DBUID_Ididu and optional weights
#    Expected columns from dictionary: PHH_ID, DBUID_Ididu, Pop2021, TDwell2021_TLog2021, URDwell2021_RH2021
path_phh21_points_csv <- file.path(
  lan_path,
  connectivity_data_path,
  "PHH_2021_CSV\\PHH_2021_CSV\\PHH-BC.csv"
)

# C) 2021 DGRF (Dissemination Geographies Relationship File) — nationwide CSV
#    This links DBUID -> DAUID -> CSDUID (and others). We'll filter PRUID=59 for BC.

DGRF_url <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/files-fichiers/2021_98260004.zip"

zip_path <- tempfile(fileext = ".zip")

httr::GET(
  DGRF_url,
  httr::write_disk(zip_path, overwrite = TRUE),
  httr::progress()
)

csv_name <- unzip(zip_path, list = TRUE)$Name[grep(
  "\\.csv$",
  unzip(zip_path, list = TRUE)$Name,
  ignore.case = TRUE
)][1]

path_dgrf_csv <- unz(zip_path, csv_name)

# D) Optional: 2021 Boundary files for DA and CSD (digital/cartographic)
#    Use the 2021 boundary shapefiles and filter to PRUID=59 for BC, then join the tables for mapping
# path_da_boundary <- "data/2021_boundaries/DA/da_000b21a_e.shp" # <-- set to your DA shapefile
# path_csd_boundary <- "data/2021_boundaries/CSD/csd_000b21a_e.shp" # <-- set to your CSD shapefile

# Province filter (BC = 59)
pruid_filter <- "2021A000259"

# Weighting: set TRUE and choose one of the PHH21 weight fields if desired
use_weighted <- FALSE
weight_col <- NA_character_ # e.g., "TDwell2021_TLog2021" or "Pop2021" or "URDwell2021_RH2021"

# Outputs
output_path <- file.path(
  lan_path,
  connectivity_data_path,
  "outputs"
)

dir.create(output_path, showWarnings = FALSE)
out_da_csv <- file.path(output_path, "da_phh_current_coverage_bc.csv")
out_csd_csv <- file.path(output_path, "csd_phh_current_coverage_bc.csv")
# out_gpkg <- "outputs/phh_current_bc.gpkg" # DA & CSD layers if boundaries are provided

# -------------------------------------------------------------
# 2) Load PHH Speeds (Current) — keep needed fields, coerce types
# -------------------------------------------------------------
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

phh_spd <- read_csv(path_phh_current_csv, show_col_types = FALSE)
req_spd <- c("PHH_ID", bool_cols, enum_cols)
missing_spd <- setdiff(req_spd, names(phh_spd))
if (length(missing_spd) > 0) {
  stop(
    "Missing fields in PHH Current CSV: ",
    paste(missing_spd, collapse = ", ")
  )
}

# Coerce booleans to numeric 0/1
phh_spd[bool_cols] <- lapply(
  phh_spd[bool_cols],
  function(x) as.numeric(as.character(x))
)

# Enum levels per dictionary
enum_levels <- c("", "<5_1", "5_1", "10_2", "25_5", "50_10")
phh_spd[enum_cols] <- lapply(
  phh_spd[enum_cols],
  function(x) forcats::fct(x, levels = enum_levels)
)

# -------------------------------------------------------------
# 3) Load PHH21 points (BC) — we only need IDs, DBUID & weights
# -------------------------------------------------------------
phh21 <- read_csv(
  path_phh21_points_csv,
  show_col_types = FALSE,
  col_types = cols(
    PHH_ID = col_double(),
    DBUID_Ididu = col_double(),
    Pop2021 = col_double(),
    TDwell2021_TLog2021 = col_double(),
    URDwell2021_RH2021 = col_double(),
    Pruid_Pridu = col_double(),
    .default = col_skip()
  )
)
# all zeros in "TDwell2021_TLog2021" or "Pop2021" or "URDwell2021_RH2021" so could not be used as weight
phh21 |> glimpse()
# -------------------------------------------------------------
# 4) Load DGRF (DB -> DA -> CSD) and filter to BC
# -------------------------------------------------------------
# DGRF fields vary slightly by release; we just need DBUID, DAUID, CSDUID (and PRUID if present)
dgrf <- read_csv(
  path_dgrf_csv,
  col_types = cols(.default = "c") # all columns as character
)
col_db <- "DBDGUID_IDIDUGD"
col_da <- "DADGUID_ADIDUGD"
col_csd <- "CSDDGUID_SDRIDUGD"
col_pr <- "PRDGUID_PRIDUGD"

# Keep minimal columns & filter PRUID=59
dgrf_min <- dgrf %>%
  select(
    DBUID = rlang::sym(col_db),
    DAUID = rlang::sym(col_da),
    CSDUID = rlang::sym(col_csd),
    PRUID = rlang::sym(col_pr)
  )

dgrf_min |> glimpse()

# DGUIDs (Dissemination Geography Unique Identifiers). A DGUID is a structured string:
#   DGUID = Vintage (4) │ Type (1) │ Schema (4) │ Geographic Unique Identifier (1–12)
#
#
# Vintage: the reference year (e.g., 2021)
# Type: the geography family (e.g., A=Administrative, S=Statistical, C=Combined, B=Blended, Z=Other)
# Schema: a 4‑digit code that indicates the geography class within the Type
# Geographic Unique Identifier: the actual code you will use (e.g., DBUID, DAUID, CSDUID), typically numeric and decodable into PR/CD/DA/DB parts for statistical geographies. [www12.statcan.gc.ca], [www150.statcan.gc.ca]
#
# sample:
#
# PRUID = "2021A000210" → Vintage 2021, Type A (Administrative), Schema 0002 (province/territory class), Unique ID 10 (province code 10). [www12.statcan.gc.ca]
# CSDUID = "2021A00051001519" → Vintage 2021, Type A, Schema 0005 (CSD class), Unique ID 1001519 (CSD code). [www12.statcan.gc.ca]
# DAUID = "2021S051210010165" → Vintage 2021, Type S (Statistical), Schema 0512 (DA class), Unique ID 10010165 → PR 10, CD 01, DA 0165. [www12.statcan.gc.ca]
# DBUID = "2021S051310010165001" → Vintage 2021, Type S, Schema 0513 (DB class), Unique ID 10010165001 → PR 10, CD 01, DA 0165, DB 001. [www12.statcan.gc.ca]

if (!is.na(col_pr) && col_pr %in% names(dgrf_min)) {
  names(dgrf_min)[names(dgrf_min) == col_pr] <- "PRUID"
  dgrf_min <- dgrf_min %>% filter(PRUID == pruid_filter)
}


# 1) Extract short codes from DGRF DGUIDs (chars 10+: geographic unique identifier)
dgrf_keys <- dgrf_min %>%
  mutate(
    db_code = substr(DBUID, 10, nchar(DBUID)), # 11 digits: PR(2)+CD(2)+DA(4)+DB(3)
    da_code = substr(DAUID, 10, nchar(DAUID)), #  8 digits: PR(2)+CD(2)+DA(4)
    csd_code = substr(CSDUID, 10, nchar(CSDUID)), #  7 digits: PR(2)+CD(2)+CSD(3) (admin)
    pr_code = substr(PRUID, 10, nchar(PRUID)) #  2 digits: province code
  ) %>%
  select(db_code, da_code, csd_code, pr_code)

# 2) Normalize PHH DBUID_Ididu (numeric -> zero-padded character of width 11)
phh_keys <- phh21 %>%
  transmute(
    PHH_ID,
    db_code = str_pad(as.character(DBUID_Ididu), width = 11, pad = "0")
  )

# 3) Join PHH to DGRF by short DB code
phh_joined <- phh_keys %>%
  left_join(dgrf_keys, by = "db_code")

# 4) Optional sanity checks
sum(is.na(phh_joined$da_code)) # PHHs with no DA mapping
sum(is.na(phh_joined$csd_code)) # PHHs with no CSD mapping

phh_joined |> glimpse()

# -------------------------------------------------------------
# 5) Join Speeds -> PHH21 (by PHH_ID), then -> DGRF (by DBUID)
# -------------------------------------------------------------
phh_speeds_with_geo <- phh_spd %>% # your PHH speeds table (Current)
  inner_join(phh_joined, by = "PHH_ID") # brings in da_code, csd_code
phh_speeds_with_geo |> glimpse()

# How many PHH lack a DA/CSD mapping?
na_map_rate <- mean(
  is.na(phh_speeds_with_geo$da_code) | is.na(phh_speeds_with_geo$csd_code)
)
message("PHH rows without DA/CSD mapping: ", round(100 * na_map_rate, 2), "%")


# -------------------------------------------------------------
# 6) Aggregation helpers (weighted/unweighted proportions)
# -------------------------------------------------------------

# Suppose you already joined speeds to PHH_ID; here we add DA/CSD codes and aggregate:
#
prop_fun <- function(x, w = NULL) {
  if (length(x) == 0) {
    return(NA_real_)
  }
  if (is.null(w)) {
    return(mean(x, na.rm = TRUE))
  }
  w[is.na(w)] <- 0
  if (sum(w) <= 0) {
    return(NA_real_)
  }
  weighted.mean(x, wt = w, na.rm = TRUE)
}

# -------------------------------------------------------------
# 7) Aggregate to DA
# -------------------------------------------------------------
wvec = NULL # no weight

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

# Max-threshold distribution (Combined)
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

# Served flag (≥ 75% PHHs at 50/10 combined)
served_threshold <- 0.75
da_cov <- da_cov %>%
  left_join(da_enum, by = "da_code") %>%
  mutate(
    served_50_10_phh = if_else(prop_combined_50_10 >= served_threshold, 1L, 0L)
  )

# -------------------------------------------------------------
# 8) Aggregate to CSD
# -------------------------------------------------------------
csd_cov <- phh_speeds_with_geo %>%
  filter(!is.na(csd_code)) %>%
  group_by(csd_code) %>%
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
phh_speeds_with_geo |> glimpse()

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

csd_cov <- csd_cov %>%
  left_join(csd_enum, by = "csd_code") %>%
  mutate(
    served_50_10_phh = if_else(prop_combined_50_10 >= served_threshold, 1L, 0L)
  )
csd_cov |> glimpse()
# -------------------------------------------------------------
# 9) Export clean tables (CSV)
# -------------------------------------------------------------
write_csv(da_cov, out_da_csv)
write_csv(csd_cov, out_csd_csv)

message("DA rows: ", nrow(da_cov), " | CSD rows: ", nrow(csd_cov))
message("DA CSV: ", out_da_csv)
message("CSD CSV: ", out_csd_csv)


#################################################################################################
# Data dictionary
#################################################################################################

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

da_phh_spd_dict_dict = create_dictionary(
  da_cov,
  var_labels = da_phh_spd_dict_labels
)

# since there are comma "," in the labels so sometimes we use write.csv2 with semicolon ";" as delimiter.
write.csv(
  da_phh_spd_dict_dict,
  here::here(output_path, "da_phh_spd_dict_dict.csv")
)

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

csd_phh_spd_dict_dict = create_dictionary(
  csd_cov,
  var_labels = csd_phh_spd_dict_labels
)

# since there are comma "," in the labels so sometimes we use write.csv2 with semicolon ";" as delimiter.
write.csv(
  csd_phh_spd_dict_dict,
  here::here(output_path, "csd_phh_spd_dict_dict.csv")
)
