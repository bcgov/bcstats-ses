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
# Purpose: Remove specific geography IDs from SEI Data Catalogue files
# Input: Geo Suppression IDs Excel file (GEO_TYPE, GEO_CODE, GEO_NAME)
# Output: 4 filtered CSV files for BC Data Catalogue (2023)
#-------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
source("./src/utils.R")

# Load configuration
config <- config::get()

#-------------------------------------------------------------------------------------------
# 1. SET PATHS
#-------------------------------------------------------------------------------------------

# Input: Geo Suppression IDs file
geo_suppression_file <- file.path(
  config$lan_path,
  "2024 SES Index",
  "scripts",
  "Percent Indigenous population",
  "output pcnt indigenous population",
  "Geo Suppression IDs.xlsx"
)

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
# 2. LOAD GEO SUPPRESSION LIST
#-------------------------------------------------------------------------------------------

cat("Loading geo suppression list from:\n", geo_suppression_file, "\n")

geo_suppression <- read_excel(geo_suppression_file)

cat("\nGeo Suppression List:\n")
print(geo_suppression)

# Separate by GEO_TYPE
chsa_to_remove <- geo_suppression %>%
  filter(GEO_TYPE == "CHSA") %>%
  pull(GEO_CODE) %>%
  as.character()

csd_to_remove <- geo_suppression %>%
  filter(GEO_TYPE == "CSD") %>%
  pull(GEO_CODE) %>%
  as.character()

cat("\nSummary:\n")
cat("  CHSAs to remove:", length(chsa_to_remove), "\n")
cat("  CSDs to remove:", length(csd_to_remove), "\n")

#-------------------------------------------------------------------------------------------
# 3. LOAD SEI DATA FILES
#-------------------------------------------------------------------------------------------

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
# 4. FUNCTION: REMOVE GEOGRAPHIES
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
# 5. PROCESS CHSA FILES
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
  sei_long_chsa_filtered <- sei_long_chsa %>%
    filter(!CHSA_UID %in% chsa_to_remove)

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
# 6. PROCESS CSD FILES
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
  sei_long_csd_filtered <- sei_long_csd %>%
    filter(!CSD_UID %in% csd_to_remove)

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
# 7. SUMMARY
#-------------------------------------------------------------------------------------------

cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")
cat("Geographies removed:\n")
cat("  CHSAs:", paste(chsa_to_remove, collapse = ", "), "\n")
cat("  CSDs:", paste(csd_to_remove, collapse = ", "), "\n")
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
