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

# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()
# use this config <- config::get(config = "development" ) to switch to development environment.

# Extract settings from config

gcs_table <- config$tables$gcs
chsa_table <- config$tables$chsa

# Establish database connection using config values
tryCatch(
  {
    con <- dbConnect(
      odbc(),
      Driver = config$database$driver,
      Server = config$database$server,
      Database = config$database$database,
      trusted_connection = config$database$trusted_connection
    )
    cat("Successfully connected to the database\n")
  },
  error = function(e) {
    stop(glue("Failed to connect to database: {e$message}"))
  }
)


# Create GCS data reference

cat("Retrieving GCS data...\n")
gcs_data <- tbl_long_cols_mssql(con, "Prod", gcs_table) %>%
  # tbl(con, in_schema("Prod", gcs_table)) %>%
  filter(ACTIVE == 'Y') %>%
  mutate(CD = substr(CDCSD_2021, 1, 2)) %>%
  mutate(CDCSD_DA = paste0(CDCSD_2021, DA_2021)) %>%
  mutate(DA = paste0("59", substr(CDCSD_2021, 1, 2), DA_2021)) %>%
  select(CD, CDCSD_2021, DA_2021, CDCSD_DA, DA, CHSA, MUN_NAME_2021)

gcs_data %>% glimpse()

# get CHSA data reference
cat("Retrieving CHSA data...\n")
chsa_data <- tbl(con, in_schema("Prod", chsa_table)) %>%
  select(CHSA, CHSA_NAME)

chsa_data %>% glimpse()


# Join GCS data with CHSA data
cat("Joining GCS data with CHSA data...\n")
gcs_chsa_da_data <- gcs_data %>%
  left_join(chsa_data, by = "CHSA")

gcs_chsa_da_cnt_data = gcs_chsa_da_data %>%
  group_by(CDCSD_2021, CHSA, MUN_NAME_2021, CHSA_NAME) %>%
  summarise(DA_CNT = n_distinct(DA)) %>%
  select(CDCSD_2021, CHSA, DA_CNT, MUN_NAME_2021, CHSA_NAME) %>% # have to move the character column to the end of the select due to MSSQL issue.
  collect()

# Check if the join was successful
if (nrow(gcs_chsa_da_data) == 0) {
  stop("Join failed: GCS data and CHSA data do not match.")
} else {
  cat(glue("Join successful: {nrow(gcs_data)} rows returned.\n"))
}

# count how many distinct DAs are in the GCS data
distinct_da_count <- gcs_data %>%
  distinct(DA) %>%
  pull(DA) %>%
  length()

cat(glue("Distinct DAs in GCS data: {distinct_da_count}\n"))

# count how many distinct DAs are in the GCS data by CHSA
distinct_da_chsa_count <- gcs_data %>%
  group_by(CHSA) %>%
  summarise(DA_CNT = n_distinct(DA)) %>%
  ungroup()


# Check if one DA is covering two CHSAs
da_multiple_cdcsd <- gcs_data %>%
  group_by(DA) %>%
  summarise(CDCSD_CNT = n_distinct(CDCSD_2021)) %>%
  filter(CDCSD_CNT > 1) %>%
  collect()
# No, it is safe that da and csd is one to one.

# Check if one DA is covering two CHSAs
da_multiple_chsa <- gcs_data %>%
  group_by(DA) %>%
  summarise(CHSA_CNT = n_distinct(CHSA)) %>%
  filter(CHSA_CNT > 1) %>%
  collect()

if (nrow(da_multiple_chsa) > 0) {
  cat(glue(
    "WARNING: {nrow(da_multiple_chsa)} DAs are covering multiple CHSAs!\n"
  ))
  print(da_multiple_chsa)

  # If you want to see the details of these DAs with their multiple CHSAs
  problem_das <- da_multiple_chsa %>% pull(DA)
  da_chsa_details <- gcs_data %>%
    filter(DA %in% problem_das) %>%
    count(DA, CHSA, CDCSD_2021, MUN_NAME_2021) %>%
    collect()

  print(da_chsa_details)
} else {
  cat("No DAs covering multiple CHSAs found.\n")
}


# count how many distinct CHSAs are in the GCS data
distinct_chsa_count <- gcs_data %>%
  distinct(CHSA) %>%
  pull(CHSA) %>%
  length()

cat(glue("Distinct CHSAs in GCS data: {distinct_chsa_count}\n"))


# Check if one CHSA is covering two CDCSD
chsa_multiple_cdcsd <- gcs_data %>%
  group_by(CHSA) %>%
  summarise(CDCSD_CNT = n_distinct(CDCSD_2021)) %>%
  filter(CDCSD_CNT > 1) %>%
  collect()

if (nrow(chsa_multiple_cdcsd) > 0) {
  cat(glue(
    "WARNING: {nrow(chsa_multiple_cdcsd)} CHSAs are covering multiple CDSCSs!\n"
  ))
  print(chsa_multiple_cdcsd)

  # If you want to see the details of these DAs with their multiple CHSAs
  problem_chsas <- chsa_multiple_cdcsd %>% pull(CHSA)
  chsa_cdcsd_details <- gcs_data %>%
    filter(CHSA %in% problem_chsas) %>%
    count(CHSA, CDCSD_2021, MUN_NAME_2021) %>%
    collect()

  print(chsa_cdcsd_details)
} else {
  cat("No CHSAs covering multiple CDCSD found.\n")
}


cat(glue("Distinct DAs in GCS data by CHSA: {nrow(distinct_da_chsa_count)}\n"))

# get all combination of da and chsa and the count of the postal code
da_chsa_details <- gcs_data %>%
  count(DA, CHSA, CDCSD_2021, MUN_NAME_2021) %>%
  left_join(chsa_data, by = "CHSA") %>%
  collect()

da_chsa_details <- da_chsa_details %>%
  rename(
    POSTAL_CODE_CNT = n
  ) %>%
  group_by(DA) %>%
  mutate(CHSA_CNT_BY_DA = n_distinct(CHSA)) %>%
  ungroup() %>%
  group_by(CHSA) %>%
  mutate(DA_CNT_BY_CHSA = n_distinct(DA)) %>%
  ungroup()


da_chsa_details %>%
  glimpse()

readr::write_csv(da_chsa_details, here::here("out", "da_chsa_details.csv"))

#################################################################################################
# Data dictionary
#################################################################################################

da_chsa_data_dict_labels = c(
  "DA" = "DAs are small, relatively stable geographic units composed of one or more adjacent dissemination blocks where populations generally range from 400 to 700 people. DAs cover all the territory of Canada and are the smallest standard geographic area for which all census data are disseminated.",
  "CHSA" = "Community Health Service Area (CHSA)",
  "CDCSD_2021" = "Census Subdivision",
  "MUN_NAME_2021" = "Census Subdivision Name",
  "POSTAL_CODE_CNT" = "Number of postal code in a combinationof DA and CHSA",
  "CHSA_NAME" = "Community Health Service Area (CHSA) Name",
  "CHSA_CNT_BY_DA" = "Number of CHSA within DA.",
  "DA_CNT_BY_CHSA" = "Number of DA within CHSA."
)

da_chsa_data_dict = create_dictionary(
  da_chsa_details,
  var_labels = da_chsa_data_dict_labels
)

write_csv(da_chsa_data_dict, here::here("out/da_chsa_data_dict.csv"))
