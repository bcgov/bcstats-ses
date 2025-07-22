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

library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(glue)
library(dbplyr)
library(datadictionary)
source("./src/utils.R") # get the functions for plotting maps and retrieve tables from sqlserver.
# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()
# use this config <- config::get(config = "development" ) to switch to development environment.

# Extract settings from config

lan_path <- config$lan_path

province = "British Columbia"


###################################################
# The CISR and CISV were designed to assess an area’s social resilience and social vulnerability to natural hazards and disasters.
# The Canadian Index of Social Resilience (CISR) and the Canadian Index of Social Vulnerability (CISV).

# https://www150.statcan.gc.ca/n1/pub/45-20-0001/452000012025001-eng.htm

###################################################

# check if csv file already exists
CISR_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CISR_CISV/"
)
CISR_csv_file = file.path(CISR_csv_folder, "cisr_scores_quintiles-eng.csv")


# Download CISR data
cisr_data <- download_and_process_dataset(
  url = "https://www150.statcan.gc.ca/pub/45-20-0001/2025001/csv/cisr-eng.zip",
  csv_folder = CISR_csv_folder,
  file_name = "cisr_scores_quintiles-eng.csv"
) %>%
  filter(PROVINCE_OR_TERRITORY == province)

cisr_data %>% glimpse()
readr::write_csv(cisr_data, here::here("out", "cisr_data.csv"))


#################################################################################################
# Data dictionary
#################################################################################################
# the note.csv from the same zip file provides the meta data/dictionary, but not organized in the way we want.

cisr_note <- read_note(
  CISR_csv_folder,
  file_name = "cisr_notes-eng.csv",
  filter_pattern = "notes"
)

cisr_note <- cisr_note %>% pull(THE_CANADIAN_INDEX_OF_SOCIAL_RESILIENCE)

cisr_data_dict_labels = c(
  "DISSEMINATION_AREA_DA" = cisr_note[2],
  "PROVINCE_OR_TERRITORY" = province,
  "DIMENSION_1_SCORES" = paste(cisr_note[3], cisr_note[6]),
  "DIMENSION_2_SCORES" = paste(cisr_note[4], cisr_note[6]),
  "DIMENSION_3_SCORES" = paste(cisr_note[5], cisr_note[6]),
  "CISR_SCORES" = cisr_note[7],
  "CISR_QUINTILES" = cisr_note[8],
  "CISR_MOST_RESILIENT_DIMENSION" = cisr_note[9]
)

cisr_data_dict = create_dictionary(
  cisr_data,
  var_labels = cisr_data_dict_labels
)

write_csv(cisr_data_dict, here::here("out/cisr_data_dict.csv"))
write_csv(cisr_data_dict, file.path(CISR_csv_folder, "cisr_data_dict.csv"))


#################################################################################################

# check if csv file already exists
CISV_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CISR_CISV/"
)
CISV_csv_file = file.path(CISR_csv_folder, "cisv_scores_quintiles-eng.csv")


# Download CISR data
cisv_data <- download_and_process_dataset(
  url = "https://www150.statcan.gc.ca/pub/45-20-0001/2025001/csv/cisv-eng.zip",
  csv_folder = CISV_csv_folder,
  file_name = "cisv_scores_quintiles-eng.csv",
  filter_pattern = "cisv_scores_quintiles"
) %>%
  filter(PROVINCE_OR_TERRITORY == province)

cisv_data %>% glimpse()
readr::write_csv(cisv_data, here::here("out", "cisv_data.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

cisv_note <- read_note(
  CISV_csv_folder,
  file_name = "cisv_notes-eng.csv",
  filter_pattern = "notes"
)

cisv_note <- cisv_note %>% pull(THE_CANADIAN_INDEX_OF_SOCIAL_VULNERABILITY)


cisv_data_dict_labels = c(
  "DISSEMINATION_AREA_DA" = cisv_note[2],
  "PROVINCE_OR_TERRITORY" = province,
  "DIMENSION_1_SCORES" = paste(cisv_note[3], cisv_note[7]),
  "DIMENSION_2_SCORES" = paste(cisv_note[4], cisv_note[7]),
  "DIMENSION_3_SCORES" = paste(cisv_note[5], cisv_note[7]),
  "DIMENSION_4_SCORES" = paste(cisv_note[6], cisv_note[7]),
  "CISV_SCORES" = cisv_note[8],
  "CISV_QUINTILES" = cisv_note[9],
  "CISV_MOST_RESILIENT_DIMENSION" = cisv_note[10]
)


cisv_data_dict = create_dictionary(
  cisv_data,
  var_labels = cisv_data_dict_labels
)

write_csv(cisv_data_dict, here::here("out/cisv_data_dict.csv"))
write_csv(cisv_data_dict, file.path(CISV_csv_folder, "cisv_data_dict.csv"))


###################################################
# The Canadian Index of Multiple Deprivation https://www150.statcan.gc.ca/n1/pub/45-20-0001/452000012023002-eng.htm
# The Canadian Index of Multiple Deprivation (CIMD) is a composite index that measures multiple dimensions of deprivation at the neighbourhood level in Canada.
#         users should consult The Canadian Index of Multiple Deprivation: User Guide, 2021 related to this product.
#         Data tables: Reference period: 2021
# https://www150.statcan.gc.ca/n1/pub/45-20-0001/452000012023001-eng.htm
# Canada CSV | XLSX
#####################################################
# Function to download, unzip, and read dataset
# check if csv file already exists
CIMD_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CIMD/"
)
CIMD_csv_file = file.path(CIMD_csv_folder, "bc_scores_quintiles_EN.csv")


# Download CISR data
cidm_data <- download_and_process_dataset(
  url = "https://www150.statcan.gc.ca/pub/45-20-0001/2023001/csv/bc_scores_quintiles_csv-eng.zip.zip",
  csv_folder = CIMD_csv_folder,
  file_name = "bc_scores_quintiles_EN.csv",
  filter_pattern = "sbc_scores_quintiles_EN"
) %>%
  filter(PROVINCE == province)

cidm_data %>% glimpse()
readr::write_csv(cidm_data, here::here("out", "cidm_data.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

cidm_note <- read_note(
  CIMD_csv_folder,
  file_name = "bc_notes_EN.csv",
  filter_pattern = "notes"
)

cidm_note <- cidm_note %>%
  pull(THE_CANADIAN_INDEX_OF_MULTIPLE_DEPRIVATION_BRITISH_COLUMBIA)


cidm_dict_labels = c(
  "DISSEMINATION_AREA_DA" = cidm_note[2],
  "PROVINCE" = province,
  "DA_POPULATION" = "Population in DA",
  "RESIDENTIAL_INSTABILITY_QUINTILES" = paste(cidm_note[3], cidm_note[8]),
  "RESIDENTIAL_INSTABILITY_SCORES" = paste(cidm_note[3], cidm_note[7]),
  "ETHNO_CULTURAL_COMPOSITION_QUINTILES" = paste(cidm_note[4], cidm_note[8]),
  "ETHNO_CULTURAL_COMPOSITION_SCORES" = paste(cidm_note[4], cidm_note[7]),
  "ECONOMIC_DEPENDENCY_QUINTILES" = paste(cidm_note[5], cidm_note[8]),
  "ECONOMIC_DEPENDENCY_SCORES" = paste(cidm_note[5], cidm_note[7]),
  "SITUATIONAL_VULNERABILITY_QUINTILES" = paste(cidm_note[6], cidm_note[8]),
  "SITUATIONAL_VULNERABILITY_SCORES" = paste(cidm_note[6], cidm_note[7])
)

cidm_data_dict = create_dictionary(cidm_data, var_labels = cidm_dict_labels)

write_csv(cidm_data_dict, here::here("out/cidm_data_dict.csv"))
write_csv(cidm_data_dict, file.path(CIMD_csv_folder, "cidm_data_dict.csv"))
