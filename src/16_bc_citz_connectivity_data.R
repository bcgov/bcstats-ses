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
# SCRIPT: 16_bc_citz_connectivity_data.R
# PURPOSE: Reconcile provincial CITZ broadband data with federal NBD baseline
# =============================================================================
#
# WHAT THIS SCRIPT DOES:
#   1. Loads CITZ micro-level broadband data (one row per dissemination block)
#   2. Parses speed threshold strings into numeric values
#   3. Creates boolean flags for each speed tier (50/10, 25/5, 10/2, 5/1, <5/1)
#   4. Aggregates dwelling-weighted shares at CSD and DA levels
#   5. Loads NBD baseline from script 14
#   6. Reconciles CITZ vs NBD shares, computing deltas and flagging outliers
#   7. Generates data dictionaries for all outputs
#
# WHY THIS MATTERS:
#   - Federal (NBD) and provincial (CITZ) data measure coverage independently
#   - Comparing them reveals discrepancies that may indicate:
#     * Data quality issues in one or both sources
#     * Methodological differences (timing, definitions, coverage)
#     * Genuine changes in infrastructure between data collection dates
#   - Outlier flags highlight areas needing investigation
#
# DATA FLOW:
#
#   [CITZ Micro Data] ──────┐
#   (per DB, speeds as      │
#    strings like "50_10")  │
#                            ├─> [Reconcile] ─> [Output CSVs with deltas]
#   [NBD Baseline] ─────────┤
#   (from script 14,        │
#    aggregated by CSD/DA) │
#
# KEY CONCEPTS:
#   - CITZ: BC Ministry of Citizens' Services (provincial broadband program)
#           Manages BC's connectivity initiatives and collects its own coverage data
#           Data may be more current but focused on program-eligible areas
#   - NBD: National Broadband Data Pseudo-Household (federal ISED/NBD methodology)
#           ISED's standard methodology, more comprehensive geographic coverage
#           May be less current than provincial data
#   - Speed tiers: Download/Upload in Mbps (50_10 = 50 down, 10 up)
#   - Wired: Fiber, cable, DSL
#   - Wireless: Fixed wireless, satellite
#   - Delta: CITZ share - NBD share (positive = CITZ reports higher coverage)
#   - Outlier: |delta| >= 20% → flagged for investigation
#
# GEOGRAPHIC HIERARCHY:
#   DB (Dissemination Block) → DA (Dissemination Area) → CSD (Census Subdivision)
#   - DB: ~few households, smallest unit
#   - DA: 400-700 people, neighborhood level
#   - CSD: Municipality, community level
#
# REFERENCES:
#   - ISED National Broadband Data: https://ised-isde.canada.ca/site/high-speed-internet-canada
#   - BC CITZ Broadband Program: https://www2.gov.bc.ca/gov/content/governments/organizational-structure/ministries-organizations/ministries/citizens-services
#   - Statistics Canada Census Geography: https://www12.statcan.gc.ca/census-recensement/2021/geo/index-eng.cfm
# =============================================================================

# ---- Packages ----
library(readr)
library(dplyr)
library(stringr)
library(tidyr) # for bind_cols used when attaching tier flags
library(datadictionary) # for create_dictionary to build data dictionaries
library(logger) # structured logging to console + file

# =============================================================================
# SECTION 1: CONFIGURATION & SETUP
# =============================================================================

# ---- Configuration ----
# config.yml holds all environment-specific paths (LAN, file locations, etc.)
config <- config::get()

lan_path <- config$lan_path
connectivity_data_path <- config$conectivity_data_path

output_path <- file.path(lan_path, connectivity_data_path, "outputs")
dir.create(output_path, showWarnings = FALSE)

# ---- Logger setup ----
# Log to both console and a timestamped file so runs are auditable.
log_threshold(INFO)
log_dir <- file.path(output_path, "logs")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(
  log_dir,
  paste0(
    "16_bc_citz_connectivity_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".log"
  )
)
log_appender(appender_tee(log_file))
log_info("Starting 16_bc_citz_connectivity_data.R")
log_info("Log file: {log_file}")
log_info("Output path: {output_path}")

# =============================================================================
# SECTION 2: CONSTANTS - CONFIGURABLE PARAMETERS
# =============================================================================
#
# NOTE FOR FUTURE DEVELOPERS:
#   To add a new speed tier (e.g., 100_20 for gigabit service):
#   1. Add to SPEED_TIERS list below: list(label = "100_20", down = 100, up = 20)
#   2. All helper functions will automatically handle the new tier
#   3. Data dictionaries will automatically include it
#
# This design pattern makes the script easily extensible without modifying
# the core logic functions.

# If the absolute delta between CITZ and NBD shares exceeds this threshold,
# the geographic unit is flagged as an outlier for investigation.
OUTLIER_THRESHOLD <- 0.20

# Speed tiers follow ISED/CRTC broadband definitions: download_upload in Mbps.
# Each tier represents a "at least this speed" cumulative test.
# The "<5_1" category is handled separately (not a threshold test but a label).
SPEED_TIERS <- list(
  list(label = "50_10", down = 50, up = 10), # Universal service objective
  list(label = "25_5", down = 25, up = 5),
  list(label = "10_2", down = 10, up = 2),
  list(label = "5_1", down = 5, up = 1)
)

# The CITZ data reports separate max thresholds for wired and wireless.
# Script 14 also tracks "combined" but CITZ does not have a combined field,
# so we only reconcile wired and wireless here.
CONNECTION_TYPES <- c("wired", "wireless")

log_info("Outlier threshold: {OUTLIER_THRESHOLD}")
log_info(
  "Speed tiers: {paste(sapply(SPEED_TIERS, `[[`, 'label'), collapse = ', ')}"
)
log_info("Connection types: {paste(CONNECTION_TYPES, collapse = ', ')}")

# =============================================================================
# SECTION 3: HELPER FUNCTIONS
# =============================================================================
# See inline documentation for each function.
# All functions are generic over SPEED_TIERS and CONNECTION_TYPES - see
# Section 2 for how to add new tiers/types.
# =============================================================================

# ---- Helper: Parse speed threshold strings ----
#' Parse speed threshold strings into numeric download/upload values.
#'
#' The CITZ data stores max thresholds as strings like "50_10", "25_5", "<5_1".
#' This function extracts the numeric download and upload speeds.
#' For "<5_1" entries (which don't match the numeric pattern), we set down=0,
#' up=0 so they correctly fail all ">= tier" tests, and flag them via is_lt5_1.
#' NA entries (no service) remain NA so they are excluded from share calculations.
#'
#' @param speed_str Character vector of speed labels.
#' @return Data frame with columns: down (numeric), up (numeric), is_lt5_1 (logical).
parse_speed <- function(speed_str) {
  # Detect the "<5_1" special case (starts with "<")
  is_lt <- !is.na(speed_str) & str_detect(speed_str, "^<")
  # Standard tiers match the pattern "digits_digits"
  ok <- !is.na(speed_str) & str_detect(speed_str, "^[0-9]+_[0-9]+$")

  n_na <- sum(is.na(speed_str))
  n_lt <- sum(is_lt)
  n_ok <- sum(ok)
  log_debug(
    "parse_speed: {length(speed_str)} values, {n_ok} valid, {n_lt} <5_1, {n_na} NA"
  )

  data.frame(
    # Extract download speed (everything before "_") or 0 for "<5_1"
    down = suppressWarnings(as.numeric(ifelse(
      ok,
      str_extract(speed_str, "^[0-9]+(?=_)"),
      ifelse(is_lt, 0, NA)
    ))),
    # Extract upload speed (everything after "_") or 0 for "<5_1"
    up = suppressWarnings(as.numeric(ifelse(
      ok,
      str_extract(speed_str, "(?<=_)[0-9]+$"),
      ifelse(is_lt, 0, NA)
    ))),
    is_lt5_1 = is_lt
  )
}

#' Create boolean flags for each speed tier from parsed speed values.
#'
#' For each tier in SPEED_TIERS, tests whether download >= tier$down AND
#' upload >= tier$up. This produces cumulative "at least" flags: a location
#' meeting 50/10 will also meet 25/5, 10/2, and 5/1.
#'
#' @param parsed Data frame from parse_speed (columns: down, up, is_lt5_1).
#' @param prefix Connection type prefix for column names ("wired" or "wireless").
#' @return Data frame with boolean columns: is_{prefix}_{tier_label} per tier,
#'   plus is_{prefix}_lt5_1.
flag_all_tiers <- function(parsed, prefix) {
  result <- data.frame(row_id = seq_len(nrow(parsed)))

  # Test each tier threshold (cumulative: >= down AND >= up)
  for (tier in SPEED_TIERS) {
    col_name <- paste0("is_", prefix, "_", tier$label)
    result[[col_name]] <- !is.na(parsed$down) &
      !is.na(parsed$up) &
      parsed$down >= tier$down &
      parsed$up >= tier$up
    log_debug(
      "flag_all_tiers [{prefix}]: {sum(result[[col_name]])} rows meet {tier$label}"
    )
  }

  # The "<5_1" flag comes directly from parse_speed (not a threshold test)
  col_lt <- paste0("is_", prefix, "_lt5_1")
  result[[col_lt]] <- parsed$is_lt5_1
  log_debug("flag_all_tiers [{prefix}]: {sum(result[[col_lt]])} rows are <5_1")

  result$row_id <- NULL
  result
}

#' Aggregate dwelling-weighted speed tier shares by geographic unit.
#'
#' For each group (CSD or DA), computes:
#'   share = sum(TDwell2021 where flag is TRUE) / sum(TDwell2021)
#' This gives the proportion of dwellings with at least a given speed tier.
#' Uses dplyr::across() to compute all tier shares in a single summarise call.
#'
#' @param df Data frame with is_{type}_{tier} boolean columns and TDwell2021.
#' @param group_col Unquoted column name to group by (e.g. CENSUS_SUBDIVISION_ID).
#' @return Tibble with dwell_total and share_{type}_{tier} columns per group.
aggregate_all_shares <- function(df, group_col) {
  # Build the list of all tier flag column names to aggregate
  tier_cols <- c()
  for (type in CONNECTION_TYPES) {
    for (tier in SPEED_TIERS) {
      tier_cols <- c(tier_cols, paste0("is_", type, "_", tier$label))
    }
    tier_cols <- c(tier_cols, paste0("is_", type, "_lt5_1"))
  }

  result <- df %>%
    filter(!is.na({{ group_col }}), !is.na(TDwell2021)) %>%
    group_by({{ group_col }}) %>%
    summarise(
      dwell_total = sum(TDwell2021, na.rm = TRUE),
      # For each tier flag, compute dwelling-weighted share
      across(
        all_of(tier_cols),
        ~ ifelse(
          sum(TDwell2021, na.rm = TRUE) > 0,
          sum(ifelse(.x, TDwell2021, 0), na.rm = TRUE) /
            sum(TDwell2021, na.rm = TRUE),
          NA_real_
        ),
        .names = "share_{.col}"
      ),
      .groups = "drop"
    ) %>%
    # Clean up column names: share_is_wired_50_10 -> share_wired_50_10
    rename_with(
      ~ str_replace(.x, "^share_is_", "share_"),
      starts_with("share_is_")
    )

  log_info("aggregate_all_shares: {nrow(result)} groups aggregated")
  result
}

#' Compute PHH-weighted share for a single proportion column.
#'
#' The NBD baseline from script 14 has prop_{type}_{tier} columns (e.g.
#' prop_wired_50_10) and an n_phh count. If n_phh exists, we compute a
#' PHH-weighted mean across slices within each group; otherwise, simple mean.
#'
#' @param df NBD baseline data frame.
#' @param group_col Unquoted column name to group by.
#' @param prop_col Character name of the proportion column (e.g. "prop_wired_50_10").
#' @return Tibble with group column and {prop_col}_phh.
compute_phh_share <- function(df, group_col, prop_col) {
  has_n_phh <- "n_phh" %in% names(df)
  has_prop <- prop_col %in% names(df)
  out_name <- paste0(prop_col, "_phh")

  # If the PHH baseline doesn't have this tier column, fill with NA
  if (!has_prop) {
    log_warn("PHH baseline missing column '{prop_col}'; filling with NA")
    return(
      df %>%
        group_by({{ group_col }}) %>%
        summarise(!!out_name := NA_real_, .groups = "drop")
    )
  }

  df %>%
    group_by({{ group_col }}) %>%
    summarise(
      !!out_name := if (has_n_phh) {
        # Weighted mean: more PHH points = more influence
        sum(.data[[prop_col]] * n_phh, na.rm = TRUE) /
          sum(n_phh, na.rm = TRUE)
      } else {
        mean(.data[[prop_col]], na.rm = TRUE)
      },
      .groups = "drop"
    )
}

#' Compute PHH-weighted shares for ALL wired and wireless tiers.
#'
#' Iterates over every combination of CONNECTION_TYPES x SPEED_TIERS and
#' calls compute_phh_share for each, then joins them into a single table.
#'
#' @param df NBD baseline data frame.
#' @param group_col Unquoted column name to group by.
#' @return Tibble with group column and prop_{type}_{tier}_phh columns.
compute_all_phh_shares <- function(df, group_col) {
  # Start with just the distinct group keys
  result <- df %>%
    group_by({{ group_col }}) %>%
    summarise(.groups = "drop") %>%
    select({{ group_col }})

  group_col_str <- deparse(substitute(group_col))

  # Iteratively join each tier's PHH share
  for (type in CONNECTION_TYPES) {
    for (tier in SPEED_TIERS) {
      prop_col <- paste0("prop_", type, "_", tier$label)
      share_df <- compute_phh_share(df, {{ group_col }}, prop_col)
      result <- result %>%
        left_join(share_df, by = group_col_str)
    }
  }
  log_info(
    "compute_all_phh_shares: computed {length(CONNECTION_TYPES) * length(SPEED_TIERS)} tier shares for {nrow(result)} groups"
  )
  result
}

#' Reconcile CITZ vs NBD shares for all tiers and flag outliers.
#'
#' For each tier, computes:
#'   delta = CITZ share - NBD share
#' Then flags outliers where |delta| >= OUTLIER_THRESHOLD. This helps identify
#' geographic units where the provincial and federal data disagree significantly.
#'
#' @param citz_df Tibble from aggregate_all_shares (share_{type}_{tier} cols).
#' @param phh_df Tibble from compute_all_phh_shares (prop_{type}_{tier}_phh cols).
#' @param join_col Character name of the join key column.
#' @return Reconciled tibble with delta_{type}_{tier} and outlier_{type}_{tier} columns.
reconcile_all_shares <- function(citz_df, phh_df, join_col) {
  # Inner join keeps only geographic units present in both sources
  joined <- citz_df %>%
    inner_join(phh_df, by = join_col)

  log_info(
    "reconcile_all_shares: {nrow(joined)} rows after inner join on '{join_col}' (CITZ: {nrow(citz_df)}, PHH: {nrow(phh_df)})"
  )

  for (type in CONNECTION_TYPES) {
    for (tier in SPEED_TIERS) {
      citz_col <- paste0("share_", type, "_", tier$label)
      phh_col <- paste0("prop_", type, "_", tier$label, "_phh")
      delta_col <- paste0("delta_", type, "_", tier$label)
      flag_col <- paste0("outlier_", type, "_", tier$label)

      if (citz_col %in% names(joined) && phh_col %in% names(joined)) {
        joined <- joined %>%
          mutate(
            # Positive delta = CITZ reports higher coverage than PHH
            !!delta_col := .data[[citz_col]] - .data[[phh_col]],
            # Classify the discrepancy
            !!flag_col := case_when(
              is.na(.data[[delta_col]]) ~ "NA",
              .data[[delta_col]] >= OUTLIER_THRESHOLD ~ "CITZ >> PHH",
              .data[[delta_col]] <= -OUTLIER_THRESHOLD ~ "PHH >> CITZ",
              TRUE ~ "OK"
            )
          )
        n_outliers <- sum(
          joined[[flag_col]] != "OK" & joined[[flag_col]] != "NA",
          na.rm = TRUE
        )
        if (n_outliers > 0) {
          log_warn(
            "reconcile [{type} {tier$label}]: {n_outliers} outliers detected"
          )
        }
      } else {
        log_warn("reconcile: skipping {type} {tier$label} -- missing column(s)")
      }
    }
  }
  joined
}

#' Log and print summary statistics for the primary reconciliation tier.
#'
#' Provides a quick diagnostic view of the delta distribution (mean, median,
#' 10th/90th percentiles) to assess overall agreement between CITZ and PHH.
#'
#' @param df Reconciled data frame.
#' @param label Human-readable label for the geographic level (e.g. "CSD", "DA").
#' @param delta_col Which delta column to summarize (default: wired 50/10).
print_reconciliation_summary <- function(
  df,
  label,
  delta_col = "delta_wired_50_10"
) {
  if (!delta_col %in% names(df)) {
    log_warn("Column '{delta_col}' not found; skipping summary for {label}")
    return(invisible(NULL))
  }
  stats <- df %>%
    summarise(
      n = n(),
      mean_delta = mean(.data[[delta_col]], na.rm = TRUE),
      median_delta = median(.data[[delta_col]], na.rm = TRUE),
      p10 = quantile(.data[[delta_col]], 0.10, na.rm = TRUE),
      p90 = quantile(.data[[delta_col]], 0.90, na.rm = TRUE)
    )
  log_info(
    "Reconciliation summary ({label}, {delta_col}): n={stats$n}, mean={round(stats$mean_delta, 4)}, median={round(stats$median_delta, 4)}, p10={round(stats$p10, 4)}, p90={round(stats$p90, 4)}"
  )
  print(stats)
}


# ============================================================================
# CITZ micro-data: one row per phh within dissemination block with max speed thresholds
citz_path <- file.path(
  lan_path,
  "2024 SES Index/data/raw_data/internet_connectivity/CITZ",
  "CITZ_SHR_Connectivity_Status_January2025.csv"
)
log_info("Loading CITZ micro data from: {citz_path}")
citz <- read_csv(citz_path, show_col_types = FALSE)
log_info("CITZ micro data loaded: {nrow(citz)} rows, {ncol(citz)} cols")

# ---- 1.2) Parse speeds and flag all tiers ----
# Remove rows without a valid CSD identifier or dwelling weight,
# since we cannot aggregate them meaningfully.
citz_clean <- citz %>%
  filter(!is.na(CENSUS_SUBDIVISION_ID), !is.na(TDwell2021))
log_info(
  "CITZ after filtering NA CSD/TDwell: {nrow(citz_clean)} rows (dropped {nrow(citz) - nrow(citz_clean)})"
)

# Parse the string speed labels into numeric values and flag each tier
log_info("Parsing wired speed thresholds")
wired_parsed <- parse_speed(citz_clean$Wired_Max_Threshold_Current)
log_info("Parsing wireless speed thresholds")
wireless_parsed <- parse_speed(citz_clean$Wireless_Max_Threshold_Current)

# Convert parsed speeds into boolean columns (is_wired_50_10, etc.)
wired_flags <- flag_all_tiers(wired_parsed, "wired")
wireless_flags <- flag_all_tiers(wireless_parsed, "wireless")

# Attach the boolean flags back to the cleaned CITZ data
citz_clean <- bind_cols(citz_clean, wired_flags, wireless_flags)


# ============================================================================
# PART 1: CSD-level reconciliation
#
# =============================================================================
# SECTION 4: CSD-LEVEL RECONCILIATION
# =============================================================================
# CSD (Census Subdivision) = municipality level
# Compare CITZ vs PHH broadband shares at the community/municipal level
# This is the primary output for policy analysis
# ============================================================================
log_info("==== PART 1: CSD-level reconciliation ====")

# ---- 1.1) Load inputs ----
# PHH baseline: aggregated by script 14 from federal NBD pseudo-household data
log_info(
  "Loading NBD CSD baseline from: {file.path(output_path, 'csd_nbd_current_coverage_bc.csv')}"
)
nbd_csd <- read_csv(
  file.path(output_path, "csd_nbd_current_coverage_bc.csv"),
  show_col_types = FALSE
)
log_info("NBD CSD baseline loaded: {nrow(nbd_csd)} rows, {ncol(nbd_csd)} cols")


# ---- 1.3) Aggregate CITZ to CSD (all tiers) ----
# Dwelling-weighted shares: what proportion of dwellings in each CSD
# have at least a given speed tier?
log_info("Aggregating CITZ to CSD level")
citz_csd <- aggregate_all_shares(citz_clean, CENSUS_SUBDIVISION_ID) %>%
  mutate(csd_code = as.integer(CENSUS_SUBDIVISION_ID)) %>%
  select(-CENSUS_SUBDIVISION_ID)

# ---- 1.4) Prepare PHH/CSD baseline (all tiers) ----
# Compute PHH-weighted means for each tier from the federal baseline
log_info("Computing PHH shares at CSD level")
nbd_csd1 <- nbd_csd %>%
  mutate(csd_code = as.integer(csd_code)) %>%
  compute_all_phh_shares(csd_code)

# ---- 1.5) Reconcile and flag outliers ----
# Join CITZ and PHH shares, compute deltas, flag large discrepancies
log_info("Reconciling CSD-level shares")
recon_csd <- reconcile_all_shares(citz_csd, nbd_csd1, "csd_code")

# Add human-readable CSD names from the CITZ file for easier review
csd_lookup <- citz %>%
  distinct(
    csd_code = as.integer(CENSUS_SUBDIVISION_ID),
    CENSUS_SUBDIVISION_NAME
  )

recon_csd_named <- recon_csd %>%
  left_join(csd_lookup, by = "csd_code") %>%
  relocate(csd_code, CENSUS_SUBDIVISION_NAME) %>%
  arrange(desc(abs(delta_wired_50_10)))

# ---- 1.6) Save CSD outputs ----
csd_out_path <- file.path(output_path, "csd_connectivity_reconciled.csv")
write_csv(recon_csd_named, csd_out_path)
log_info(
  "CSD reconciled output saved: {csd_out_path} ({nrow(recon_csd_named)} rows)"
)
print_reconciliation_summary(recon_csd_named, "CSD")

# =============================================================================
# SECTION 5: DA-LEVEL RECONCILIATION
# =============================================================================
# DA (Dissemination Area) = neighborhood level (400-700 people)
# Finer geographic granularity than CSD for identifying specific areas
# of discrepancy between provincial and federal data
# Requires TMF join to map DBUID -> DAUID
# ============================================================================
# PART 2: DA-level reconciliation
#
# DA (Dissemination Area) is a finer geographic unit than CSD (typically
# 400-700 people). This gives more granular reconciliation but requires
# joining CITZ dissemination block IDs (DBUID) to DA codes via the TMF
# (Translation Master File).
# ============================================================================
log_info("==== PART 2: DA-level reconciliation ====")

# ---- 2.1) Prepare CITZ micro data at DB level ----
# Select and rename the columns needed for DA-level analysis.
# DAUID is initialized as NA; it will be filled from the CITZ data directly
# (if the column exists) or derived via TMF join.
citz_da <- citz %>%
  transmute(
    DBUID_Ididu = DBUID_Ididu,
    DAUID = as.character(NA),
    CSDUID = CENSUS_SUBDIVISION_ID,
    TDwell2021 = as.numeric(TDwell2021),
    Wired_Max = Wired_Max_Threshold_Current,
    Wireless_Max = Wireless_Max_Threshold_Current,
    CONNECTIVITY_STATUS = CONNECTIVITY_STATUS
  )

# Some versions of the CITZ file include DAUID directly
has_da <- "DAUID" %in% names(citz)
if (has_da) {
  citz_da$DAUID <- as.character(citz$DAUID)
  log_info("DAUID column found in CITZ data")
} else {
  log_info("DAUID column not in CITZ data; will derive from TMF")
}

# ---- 2.2) Load TMF (DB -> DA -> CSD) ----
# The Translation Master File maps dissemination blocks (DB) to dissemination
# areas (DA) and census subdivisions (CSD). Column names are abbreviated
# French/English codes from Statistics Canada (e.g. DSSMNTNBLC = dissemination block).
tmf_csv <- file.path(
  lan_path,
  config$file_path$tmf_file_path,
  config$file_name$tmf_file_name
)
log_info("Loading TMF from: {tmf_csv}")

tmf <- read_csv(tmf_csv, col_types = cols(.default = "c"))
log_info("TMF loaded: {nrow(tmf)} rows")

# Extract the short geographic codes from the full DGUID strings.
# The DB code is the last 11 characters of the DSSMNTNBLC DGUID.
tmf_min <- tmf %>%
  select(db_code = DSSMNTNBLC, da_code = DSSMNTNRD, csd_code = CNSSCNSLDT) %>%
  mutate(db_code = str_sub(db_code, start = nchar(db_code) - 10L))

# ---- 2.3) Join DAUID via TMF ----
# Match CITZ records to TMF by zero-padded DBUID, then fill in DAUID
# for records where it was missing.
log_info("Joining CITZ to TMF by db_code")
citz_da <- citz_da %>%
  mutate(
    db_code = str_pad(as.character(DBUID_Ididu), width = 11, pad = "0")
  ) %>%
  left_join(tmf_min, by = "db_code") %>%
  mutate(DAUID = if_else(is.na(DAUID), da_code, DAUID))

n_da_missing <- sum(is.na(citz_da$DAUID))
log_info(
  "After TMF join: {n_da_missing} rows still missing DAUID out of {nrow(citz_da)}"
)

# If DAUID was not in the original CITZ file AND the TMF join failed
# completely, we cannot proceed with DA-level analysis.
if (!has_da && all(is.na(citz_da$DAUID))) {
  log_error("DAUID is not in the CITZ file and TMF join failed")
  stop(
    "DAUID is not in the CITZ file and TMF join failed. ",
    "Please join DAUID via your TMF before proceeding."
  )
}

# ---- 2.4) Parse speeds and flag all tiers ----
# Same parsing logic as CSD level, applied to the DA-level data
log_info("Parsing wired speed thresholds (DA level)")
wired_da <- parse_speed(citz_da$Wired_Max)
log_info("Parsing wireless speed thresholds (DA level)")
wireless_da <- parse_speed(citz_da$Wireless_Max)

wired_da_flags <- flag_all_tiers(wired_da, "wired")
wireless_da_flags <- flag_all_tiers(wireless_da, "wireless")

citz_da <- bind_cols(citz_da, wired_da_flags, wireless_da_flags)

# ---- 2.5) Aggregate CITZ to DA (all tiers) ----
log_info("Aggregating CITZ to DA level")
da_citz <- aggregate_all_shares(citz_da, DAUID)

# ---- 2.6) Save DA-level CITZ table ----
# This intermediate output is useful even without reconciliation,
# as it provides the provincial view of DA-level connectivity.
da_citz_csv <- file.path(output_path, "da_connectivity_current_citz.csv")
write_csv(da_citz, da_citz_csv)
log_info("DA CITZ output saved: {da_citz_csv} ({nrow(da_citz)} rows)")

# ---- 2.7) Reconcile with PHH DA baseline (if available) ----
# The PHH DA baseline may not exist if script 14 was not run at DA level.
nbd_da_path <- file.path(output_path, "da_nbd_current_coverage_bc.csv")

if (file.exists(nbd_da_path)) {
  log_info("Loading NBD DA baseline from: {nbd_da_path}")
  nbd_da <- read_csv(nbd_da_path, show_col_types = FALSE)
  log_info("NBD DA baseline loaded: {nrow(nbd_da)} rows")

  # The NBD DA file may use "da_code" or "DAUID" as the key column name;
  # harmonize to DAUID for a clean join.
  nbd_da1 <- nbd_da %>%
    mutate(
      DAUID = if ("da_code" %in% names(.)) {
        as.character(da_code)
      } else {
        as.character(DAUID)
      }
    ) %>%
    compute_all_phh_shares(DAUID)

  log_info("Reconciling DA-level shares")
  da_recon <- reconcile_all_shares(da_citz, nbd_da1, "DAUID")

  da_recon_path <- file.path(output_path, "da_connectivity_reconciled.csv")
  write_csv(da_recon, da_recon_path)
  log_info(
    "DA reconciled output saved: {da_recon_path} ({nrow(da_recon)} rows)"
  )
  print_reconciliation_summary(da_recon, "DA")
} else {
  log_warn(
    "NBD DA baseline not found at {nbd_da_path}; skipping DA reconciliation"
  )
}

# =============================================================================
# SECTION 6: DATA DICTIONARIES
# =============================================================================
# Each output CSV gets a companion data dictionary explaining every column.
# Labels are generated programmatically from SPEED_TIERS and CONNECTION_TYPES
# constants - ensuring they stay in sync when you add new tiers.
# ============================================================================
log_info("==== PART 3: Data dictionaries ====")

#' Build data dictionary labels for CITZ aggregation output columns.
#' Generates labels for dwell_total and all share_{type}_{tier} columns.
build_citz_agg_labels <- function(geo_id_label, geo_id_desc) {
  labels <- c()
  labels[[geo_id_label]] <- geo_id_desc
  labels[["dwell_total"]] <- "Total 2021 dwelling-weight (sum of TDwell2021)."

  for (type in CONNECTION_TYPES) {
    type_label <- ifelse(type == "wired", "wired", "wireless")
    for (tier in SPEED_TIERS) {
      col <- paste0("share_", type, "_", tier$label)
      labels[[col]] <- paste0(
        "Dwelling-weighted share with ",
        type_label,
        " max threshold >= ",
        tier$down,
        "/",
        tier$up,
        " Mbps."
      )
    }
    lt_col <- paste0("share_", type, "_lt5_1")
    labels[[lt_col]] <- paste0(
      "Dwelling-weighted share with ",
      type_label,
      " max threshold < 5/1 Mbps."
    )
  }
  labels
}

#' Build data dictionary labels for reconciled output columns.
#' Extends the CITZ aggregation labels with PHH share, delta, and outlier flag
#' columns for each tier.
build_recon_labels <- function(
  geo_id_label,
  geo_id_desc,
  include_csd_name = FALSE
) {
  labels <- build_citz_agg_labels(geo_id_label, geo_id_desc)

  if (include_csd_name) {
    labels[[
      "CENSUS_SUBDIVISION_NAME"
    ]] <- "Census Subdivision name from the CITZ micro file."
  }

  for (type in CONNECTION_TYPES) {
    type_label <- ifelse(type == "wired", "wired", "wireless")
    for (tier in SPEED_TIERS) {
      phh_col <- paste0("prop_", type, "_", tier$label, "_phh")
      delta_col <- paste0("delta_", type, "_", tier$label)
      flag_col <- paste0("outlier_", type, "_", tier$label)

      labels[[phh_col]] <- paste0(
        "PHH (federal NBD) ",
        type_label,
        " >= ",
        tier$down,
        "/",
        tier$up,
        " share, PHH-weighted."
      )
      labels[[delta_col]] <- paste0(
        "Difference: CITZ minus PHH share for ",
        type_label,
        " >= ",
        tier$down,
        "/",
        tier$up,
        "."
      )
      labels[[flag_col]] <- paste0(
        "Outlier flag for ",
        type_label,
        " ",
        tier$label,
        " (threshold +/-",
        OUTLIER_THRESHOLD,
        ")."
      )
    }
  }
  labels
}

# ---- 3.1) CSD-level reconciliation data dictionary ----
csd_recon_csv <- file.path(output_path, "csd_connectivity_reconciled.csv")

if (file.exists(csd_recon_csv)) {
  log_info("Building CSD reconciliation data dictionary")
  csd_recon_tbl <- read_csv(csd_recon_csv, show_col_types = FALSE)
  csd_labels <- build_recon_labels(
    "csd_code",
    "Census Subdivision (CSD) code (integer).",
    include_csd_name = TRUE
  )
  # Only keep labels for columns that actually exist in the output
  csd_labels <- csd_labels[names(csd_labels) %in% names(csd_recon_tbl)]

  csd_recon_dict <- create_dictionary(csd_recon_tbl, var_labels = csd_labels)
  csd_dict_path <- file.path(
    output_path,
    "csd_connectivity_reconciled_dict.csv"
  )
  write.csv(csd_recon_dict, csd_dict_path, row.names = FALSE)
  log_info("CSD data dictionary saved: {csd_dict_path}")
} else {
  log_warn("CSD reconciled CSV not found; skipping CSD data dictionary")
}

# ---- 3.2) DA-level CITZ data dictionary ----
if (file.exists(da_citz_csv)) {
  log_info("Building DA CITZ data dictionary")
  da_citz_tbl <- read_csv(da_citz_csv, show_col_types = FALSE)
  da_citz_labels <- build_citz_agg_labels(
    "DAUID",
    "Dissemination Area (DA) unique identifier (2021 DAUID, character)."
  )
  da_citz_labels <- da_citz_labels[
    names(da_citz_labels) %in% names(da_citz_tbl)
  ]

  da_citz_dict <- create_dictionary(da_citz_tbl, var_labels = da_citz_labels)
  da_citz_dict_path <- file.path(
    output_path,
    "da_connectivity_current_citz_dict.csv"
  )
  write.csv(da_citz_dict, da_citz_dict_path, row.names = FALSE)
  log_info("DA CITZ data dictionary saved: {da_citz_dict_path}")
} else {
  log_warn("DA CITZ CSV not found; skipping DA CITZ data dictionary")
}

# ---- 3.3) DA-level reconciliation data dictionary ----
da_recon_csv <- file.path(output_path, "da_connectivity_reconciled.csv")

if (file.exists(da_recon_csv)) {
  log_info("Building DA reconciliation data dictionary")
  da_recon_tbl <- read_csv(da_recon_csv, show_col_types = FALSE)
  da_recon_labels <- build_recon_labels(
    "DAUID",
    "Dissemination Area (DA) unique identifier (2021 DAUID, character)."
  )
  da_recon_labels <- da_recon_labels[
    names(da_recon_labels) %in% names(da_recon_tbl)
  ]

  da_recon_dict <- create_dictionary(da_recon_tbl, var_labels = da_recon_labels)
  da_recon_dict_path <- file.path(
    output_path,
    "da_connectivity_reconciled_dict.csv"
  )
  write.csv(da_recon_dict, da_recon_dict_path, row.names = FALSE)
  log_info("DA reconciliation data dictionary saved: {da_recon_dict_path}")
} else {
  log_warn(
    "DA reconciled CSV not found; skipping DA reconciliation data dictionary"
  )
}

log_info("Done: 16_bc_citz_connectivity_data.R")

# =============================================================================
# ADDITIONAL NOTES FOR FUTURE ANALYSTS
# =============================================================================
#
# UNDERSTANDING THE RECONCILIATION OUTPUT:
#   - delta = CITZ share - PHH share
#   - Positive delta (> 0): CITZ reports higher coverage than PHH
#   - Negative delta (< 0): PHH reports higher coverage than CITZ
#   - outlier = "CITZ >> PHH": CITZ is 20%+ higher (possible over-reporting by CITZ)
#   - outlier = "PHH >> CITZ": PHH is 20%+ higher (possible under-reporting by CITZ)
#   - outlier = "OK": Within 20% tolerance
#   - outlier = "NA": Missing data in one or both sources
#
# COMMON CAUSES OF DISCREPANCIES:
#   1. Timing differences: CITZ may be more recent than PHH snapshot
#   2. Methodology: Different definitions of "available"
#   3. Data quality: Missing or incorrect DBUID mappings
#   4. Coverage: Different geographic scope (e.g., CITZ focuses on underserved areas)
#
# POTENTIAL IMPROVEMENTS:
#   1. Add combined (wired + wireless) reconciliation - CITZ doesn't have this
#      but it would align with script 14's output
#   2. Add time-series tracking by saving deltas before updating
#   3. Implement weighted outlier threshold (weight by n_phh or dwell_total)
#   4. Add visualization of delta distributions
#   5. Investigate systematically "PHH >> CITZ" areas - may indicate CITZ
#      needs to update their database
#   6. Consider adding confidence intervals if underlying data supports it
#
# RUNNING THIS SCRIPT:
#   - Requires: script 14 output files (csd_phh_current_coverage_bc.csv, etc.)
#   - Requires: CITZ micro-data file in the specified path
#   - Requires: TMF file for DA-level join
#   - Output: Check logs/ folder for detailed run information
# =============================================================================
