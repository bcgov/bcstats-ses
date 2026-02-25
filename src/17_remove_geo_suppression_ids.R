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

#-------------------------------------------------------------------------------------------
# Purpose: Remove indigenous geographies from SEI Data Catalogue files
#           - CHSA: Nisga's regions (via CHSA_TYPE)
#           - CSD: Indian reserves, Nisga'a lands, Indian government districts (via CSD_TYPE)
# Input: Database queries using official StatsCan CSD type codes and CHSA type info
# Output: 4 filtered CSV files for BC Data Catalogue (2023)
#
# CSD Types to suppress:
#   IRI  = Indian reserve
#   NL   = Nisga'a land
#   IGD  = Indian government district
#
# CHSA Types to suppress:
#   Nisga's regions (identified by name pattern)
#-------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(readxl)
library(DBI)
library(odbc)
library(ggplot2)
library(bcdata)
source("./src/utils.R")

# Load configuration
config <- config::get()

#-------------------------------------------------------------------------------------------
# 1. SET PATHS
#-------------------------------------------------------------------------------------------

# Output path for 2023 data catalogue products
output_path <- file.path(
  config$lan_path,
  "2024 SES Index",
  "bc data catalogue",
  "Data Catalogue final products",
  "2023"
)

# Create output directory if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
  cat("Created output directory:", output_path, "\n")
}

#-------------------------------------------------------------------------------------------
# 2. IDENTIFY INDIGENOUS CSDs USING STATSCAN GEOGRAPHIC ATTRIBUTE FILE
#-------------------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Identifying Indigenous CSDs using StatsCan Geographic Attribute File\n")
cat("========================================\n\n")


# Download 2021 Geographic Attribute File for BC CSDs directly from StatsCan
# Reference: https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo044
cat("Downloading StatsCan Geographic Attribute File for BC CSDs...\n")

# the geographic attribute file
geo_attr_url <- 'https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/files-fichiers/2021_92-151_X.zip'
zip_path <- tempfile(fileext = ".zip")

# Download with timeout
tryCatch(
  {
    download.file(geo_attr_url, zip_path, mode = "wb", quiet = TRUE)

    # Create temp directory for unzip (this is the fix)
    unzip_dir <- file.path(tempdir(), "geo_attr")
    if (!dir.exists(unzip_dir)) {
      dir.create(unzip_dir, recursive = TRUE)
    }

    # Unzip to temp directory
    unzip(zip_path, exdir = unzip_dir)

    # Find the CSV file (usually only one)
    csv_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE)
    geo_attr_file <- csv_files[1]
    # The Byte: \xc9 is the hexadecimal value for É in the Latin-1 character set.
    # The Mismatch: R (especially on modern systems) often expects UTF-8, where É is represented by two bytes (\xc3\xa9).
    geo_attr <- read_csv(
      geo_attr_file,
      show_col_types = FALSE,
      locale = locale(encoding = "Latin1")
    )
    cat("Successfully downloaded and extracted geographic attribute file\n")
  },
  error = function(e) {
    cat("Warning: Download/extraction failed:", conditionMessage(e), "\n")
    geo_attr <<- NULL
  }
)
geo_attr |> glimpse()
colnames(geo_attr)
# this table has name, type, id for db, da, population center, ct, cma, csd, er,ccs,sac,dpl,fed, cd, province,
# The names are long, and we will simplify them.
# Filter for indigenous CSD types
# The column name may vary - try different possibilities
# Use first column that contains type info
csd_type_col <- names(geo_attr)[grepl(
  "CSDTYPE",
  names(geo_attr),
  ignore.case = TRUE
)][1]
# CSDTYPE_SDRGENRE
csd_id_col <- names(geo_attr)[grepl(
  "CSDDGUID",
  names(geo_attr),
  ignore.case = TRUE
)][1]
# "CSDDGUID_SDRIDUGD"
csd_name_col <- names(geo_attr)[grepl(
  "CSDNAME",
  names(geo_attr),
  ignore.case = TRUE
)][1]
# "CSDNAME_SDRNOM"

cat("Using column: ", csd_type_col, "\n")

# Filter the geographic attribute data frame to identify Indigenous CSDs
bc_csds <- geo_attr %>%
  # Why: Only keep records for British Columbia (PRUID_PRIDU == 59)
  # What: Restricts to BC rows in the StatsCan file
  filter(PRUID_PRIDU == 59) %>%
  # Why: Select only relevant columns for downstream processing
  # What: Renames columns to standard names for consistency
  # How: Uses dynamic column names for CSD UID, name, and type
  select(
    CSD_UID = csd_id_col,
    CSD_NAME = csd_name_col,
    CSDTYPE = csd_type_col
  ) |>
  distinct()
# 751 csds in 2021 census

# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo012
# Census subdivisions (CSDs) are classified into 57 types according to official designations adopted by provincial, territorial or federal authorities.
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_5
# Key 2021 Census CSD Types and Abbreviations:

bc_csds |> count(CSDTYPE)

#     C: City
#     T: Town
#     VL: Village
#     CV: City / Cité
#     CY – City
#     TV: Town / Ville
#     DM – District municipality
#     IGD: Indian government district
#     IRI: Indian reserve / Réserve indienne
#     IM: Island municipality
#     LGD: Local government district
#     RCR: Rural community / Communauté rurale
#     RDA – Regional district electoral area
#     RGM – Regional municipality
#     NL :	Nisga'a land
#     S-É: 	Indian settlement / Établissement indien
#     TAL – Tla'amin Lands
#     TWL – Tsawwassen Lands

# Indigenous CSD types to suppress (as specified by user)
# IRI = Indian reserve
# IGD = Indian government district
# NL  = Nisga'a land
# S-É: 	Indian settlement / Établissement indien
# TAL = Tla'amin Lands
# TWL – Tsawwassen Lands
indig_csd_types <- c('IRI', 'IGD', 'NL', 'S-É', 'TAL', 'TWL')

cat("Indigenous CSD types being suppressed:\n")
cat("  ", paste(indig_csd_types, collapse = ", "), "\n\n")

bc_indig_csds <- bc_csds %>%
  # Why: Only keep CSDs with types matching those to be suppressed (e.g., IRI, NL, IGD, TAL)
  # What: Identifies Indigenous CSDs by type code
  # How: Uses the dynamically detected column for CSD type
  filter(CSDTYPE %in% indig_csd_types) %>%
  arrange(CSD_UID)
# 427 indigenous csds in 2021 census
# our Geo Suppression IDs.xlsx only has 322 indigenous CSDs.

cat("Found", nrow(bc_indig_csds), "CSDs with indigenous types\n")
cat("Sample:\n")
print(head(bc_indig_csds, 10))

# Convert CSD_UID to short format: remove non-digit characters and leading '2021A0005'
csd_to_remove <- bc_indig_csds$CSD_UID %>%
  gsub("^2021A0005", "", .) %>%
  as.character()

# If no CSDs found from download, try database query as fallback
if (length(csd_to_remove) == 0) {
  cat("\nNo CSDs from download. Attempting database query as fallback...\n")

  con <- dbConnect(
    odbc::odbc(),
    Driver = config$data_server$driver,
    Server = config$data_server$server,
    Database = config$data_server$database,
    Trusted_Connection = "Yes"
  )

  # Programmatically generate the LIKE conditions from the vector
  like_conditions <- paste0(
    "GEO_NAME like '%(",
    indig_csd_types,
    ")%'",
    collapse = " or "
  )

  indig_csds_query <- sprintf(
    paste0(
      "SELECT DISTINCT ALT_GEO_CODE, GEO_NAME, ",
      "CASE WHEN %s THEN 'Y' ELSE 'N' END as Indigenous_community\n",
      "FROM [Population_Labour_Social].[Prod].[FCT_CENSUS_2021_BC_CSD_UD]\n",
      "WHERE %s"
    ),
    like_conditions,
    like_conditions
  )

  cat("Generated SQL query:\n")
  cat(indig_csds_query, "\n\n")

  indig_csds <- dbGetQuery(con, indig_csds_query) %>%
    rename(
      CSD_UID = ALT_GEO_CODE,
      CSD_NAME = GEO_NAME,
      CSDTYPE = Indigenous_community
    )

  cat("Found", nrow(indig_csds), "CSDs from database fallback\n")
  cat("Sample:\n")
  print(head(indig_csds, 10))

  csd_to_remove <- as.character(indig_csds$CSD_UID)

  dbDisconnect(con)
}

#-------------------------------------------------------------------------------------------
# 3. IDENTIFY NISGA'S CHSAs USING CHSA NAME PATTERN
#-------------------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Identifying Nisga's CHSAs\n")
cat("========================================\n\n")

bcdata::bcdc_search("Community Health Service Area")
bcdc_get_record("68f2f577-28a7-46b4-bca9-7e9770f2f357")
bc_chsa_resources <- bcdata::bcdc_tidy_resources(
  '68f2f577-28a7-46b4-bca9-7e9770f2f357'
)

bc_chsa <- bcdc_query_geodata('68f2f577-28a7-46b4-bca9-7e9770f2f357') |>
  collect()

chsa_to_remove <- bc_chsa |>
  dplyr::select(
    HLTH_CHSA_SYSID,
    CMNTY_HLTH_SERV_AREA_NAME,
    CMNTY_HLTH_SERV_AREA_CODE,
    CHSA_POPULATION_CENSUS
  ) |>
  filter(stringr::str_detect(CMNTY_HLTH_SERV_AREA_NAME, "Nisga")) |>
  pull(CMNTY_HLTH_SERV_AREA_CODE)


#-------------------------------------------------------------------------------------------
# 4. LOAD SEI DATA FILES
#-------------------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Loading SEI Data Files\n")
cat("========================================\n\n")

# Input files (2023 data catalogue products)
sei_files <- list(
  SEI_DET_CHSA = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_DET_CHSA
  ),
  SEI_LONG_CHSA = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_LONG_CHSA
  ),
  SEI_DET_CSD = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_DET_CSD
  ),
  SEI_LONG_CSD = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_LONG_CSD
  )
)

#-------------------------------------------------------------------------------------------
# 5. FUNCTION: REMOVE GEOGRAPHIES
#-------------------------------------------------------------------------------------------

remove_geographies <- function(
  sei_data,
  geo_col,
  geo_codes_to_remove,
  geo_type
) {
  original_count <- nrow(sei_data)

  # Convert to character for consistent matching
  sei_data <- sei_data %>%
    mutate(across(all_of(geo_col), as.character))

  # Filter out specified geographies
  sei_filtered <- sei_data %>%
    filter(!.data[[geo_col]] %in% geo_codes_to_remove)

  removed_count <- original_count - nrow(sei_filtered)

  cat(sprintf(
    "  %s: Removed %d records (%.2f%%)\n",
    geo_type,
    removed_count,
    (removed_count / original_count) * 100
  ))

  return(sei_filtered)
}

#-------------------------------------------------------------------------------------------
# 6. PROCESS CHSA FILES
#-------------------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Processing CHSA Files\n")
cat("========================================\n")

if (length(chsa_to_remove) > 0) {
  # SEI_DET_CHSA
  cat("\n1. SEI_DET_CHSA\n")
  sei_det_chsa <- read_csv(
    sei_files$SEI_DET_CHSA,
    col_types = cols(CHSA_UID = col_character()),
    show_col_types = FALSE
  )
  cat("  Original records:", nrow(sei_det_chsa), "\n")

  sei_det_chsa_filtered <- remove_geographies(
    sei_data = sei_det_chsa,
    geo_col = "CHSA_UID",
    geo_codes_to_remove = chsa_to_remove,
    geo_type = "CHSA"
  )

  # Write output
  output_file <- file.path(
    output_path,
    "SEI_DET_CHSA_2023.csv"
  )
  write_csv(sei_det_chsa_filtered, output_file)
  cat("  Output:", output_file, "\n")

  # SEI_LONG_CHSA
  cat("\n2. SEI_LONG_CHSA\n")
  sei_long_chsa <- read_csv(
    sei_files$SEI_LONG_CHSA,
    col_types = cols(CHSA_UID = col_character()),
    show_col_types = FALSE
  )
  cat("  Original records:", nrow(sei_long_chsa), "\n")

  # Keep consistent with DET - filter out CHSAs removed in DET
  sei_long_chsa_filtered <- remove_geographies(
    sei_data = sei_long_chsa,
    geo_col = "CHSA_UID",
    geo_codes_to_remove = chsa_to_remove,
    geo_type = "CHSA"
  )

  cat(
    "  Removed:",
    nrow(sei_long_chsa) - nrow(sei_long_chsa_filtered),
    "records\n"
  )

  output_file <- file.path(
    output_path,
    "SEI_LONG_CHSA_2023.csv"
  )
  write_csv(sei_long_chsa_filtered, output_file)
  cat("  Output:", output_file, "\n")
} else {
  cat("\nNo CHSAs to remove. Skipping CHSA files.\n")
}

#-------------------------------------------------------------------------------------------
# 7. PROCESS CSD FILES
#-------------------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Processing CSD Files\n")
cat("========================================\n")

if (length(csd_to_remove) > 0) {
  # SEI_DET_CSD
  cat("\n3. SEI_DET_CSD\n")
  sei_det_csd <- read_csv(
    sei_files$SEI_DET_CSD,
    col_types = cols(CSD_UID = col_character()),
    show_col_types = FALSE
  )
  cat("  Original records:", nrow(sei_det_csd), "\n")

  sei_det_csd_filtered <- remove_geographies(
    sei_data = sei_det_csd,
    geo_col = "CSD_UID",
    geo_codes_to_remove = csd_to_remove,
    geo_type = "CSD"
  )

  output_file <- file.path(
    output_path,
    "SEI_DET_CSD_2023.csv"
  )
  write_csv(sei_det_csd_filtered, output_file)
  cat("  Output:", output_file, "\n")

  # SEI_LONG_CSD
  cat("\n4. SEI_LONG_CSD\n")
  sei_long_csd <- read_csv(
    sei_files$SEI_LONG_CSD,
    col_types = cols(CSD_UID = col_character()),
    show_col_types = FALSE
  )
  cat("  Original records:", nrow(sei_long_csd), "\n")

  # Keep consistent with DET - filter out CSDs removed in DET
  sei_long_csd_filtered <- remove_geographies(
    sei_data = sei_long_csd,
    geo_col = "CSD_UID",
    geo_codes_to_remove = csd_to_remove,
    geo_type = "CSD"
  )

  cat(
    "  Removed:",
    nrow(sei_long_csd) - nrow(sei_long_csd_filtered),
    "records\n"
  )

  output_file <- file.path(
    output_path,
    "SEI_LONG_CSD_2023.csv"
  )
  write_csv(sei_long_csd_filtered, output_file)
  cat("  Output:", output_file, "\n")
} else {
  cat("\nNo CSDs to remove. Skipping CSD files.\n")
}

#-------------------------------------------------------------------------------------------
# 8. SUMMARY
#-------------------------------------------------------------------------------------------

cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")
cat("Indigenous CSDs suppressed (by CSDTYPE):\n")
cat("  CSD types:", paste(indig_csd_types, collapse = ", "), "\n")
cat("  Total CSDs removed:", length(csd_to_remove), "\n")
cat("\Indigenous CHSAs suppressed:\n")
cat("  Total CHSAs removed:", length(chsa_to_remove), "\n")
cat("\nOutput files written to:\n", output_path, "\n")

# List output files
cat("\nOutput files:\n")
output_files <- list.files(
  output_path,
  pattern = ".csv",
  full.names = TRUE
)
for (f in output_files) {
  cat("  ", basename(f), "\n")
}
