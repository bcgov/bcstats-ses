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
source("./src/utils.R") # get the functions for plotting maps

province = "British Columbia"
# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()
# use this config <- config::get(config = "development" ) to switch to development environment.

# Extract settings from config

#Setting paths
bc_stats_project_lan <- config$file_path$bc_stats_project

Path_conversion <- file.path(
  bc_stats_project_lan,
  "ConversionTables",
  "TranslationMasterFiles",
  "2025"
)


#Loading the conversion table

#Conversion table

conversion_table <- readxl::read_xls(file.path(
  Path_conversion,
  paste0(
    "Translation_Master_File_",
    "2021_Census_Completed_",
    "Provincial_Runs_2025_01_10",
    " (identifiers).xls"
  )
))

# get population estiamte in DB level.
conversion_table_db_chsa_pop <- conversion_table %>%
  select(DBUID = "DISSEMINATION_BLOCK_ID", CHSA, Pop_2021_Adjusted) # we don't use the DA id from this table. Instead, we use the DA id from geo attr table from StatsCAN


######################################################################################################
# Geographic attribute file: "2021_92-151_X statcan" refers to the 2021 Geographic Attribute File from Statistics Canada. This file contains geographic data at the dissemination block level, including information like geographic codes, names, population and dwelling counts, land area, unique identifiers, and DGUIDs. It's a key resource for understanding the geographic framework used in the 2021 Census.
# https://www150.statcan.gc.ca/n1/pub/92-151-g/92-151-g2021001-eng.htm
# https://www150.statcan.gc.ca/n1/en/catalogue/92-151-X
######################################################################################################

# check if csv file already exists
gaf_csv_folder = file.path(
  bc_stats_project_lan,
  "Household Projections",
  "10_Annual Household Projections",
  "hsh2022",
  "data",
  "Geographical Attributes",
  "2021"
)

gaf_csv_file_name <- "2021_92-151_X.csv"

# gaf_csv_file = file.path(CISR_csv_folder, gaf_csv_file_name)

# Download CISR data
gaf_data <- download_and_process_dataset(
  url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/files-fichiers/2021_92-151_X.zip",
  csv_folder = gaf_csv_folder,
  file_name = gaf_csv_file_name,
  filter_pattern = "2021"
)

geo_attrs <- gaf_data %>%
  filter(PRENAME_PRANOM == province) %>%
  select(
    # Rename columns to make them similar to 2016/2011 data
    DBUID = DBUID_IDIDU,
    DAUID = DAUID_ADIDU
    # PRUID = PRUID_PRIDU,
    # PRNAME = PRENAME_PRANOM,
    # CDUID = CDUID_DRIDU,
    # CDNAME = CDNAME_DRNOM,
    # CDTYPE = CDTYPE_DRGENRE,
    # CCSUID = CCSUID_SRUIDU,
    # CCSNAME = CCSNAME_SRUNOM,
    # CSDUID = CSDUID_SDRIDU,
    # CSDNAME = CSDNAME_SDRNOM,
    # CSDTYPE = CSDTYPE_SDRGENRE,
    # ERUID = ERUID_REIDU,
    # ERNAME = ERNAME_RENOM,
    # SACCODE = SACCODE_CSSCODE,
    # SACTYPE = SACTYPE_CSSGENRE,
    # CMAUID = CMAUID_RMRIDU,
    # CMAPUID = CMAPUID_RMRPIDU,
    # CMANAME = CMANAME_RMRNOM,
    # CMATYPE = CMATYPE_RMRGENRE,
    # CTUID = CTUID_SRIDU,
    # CTNAME = CTNAME_SRNOM,
    # ADAUID = ADAUID_ADAIDU
  ) %>%
  unique()


#Joining with the conversion table
conversion_table <- conversion_table_db_chsa_pop %>%
  left_join(
    geo_attrs %>%
      mutate(across(c(DBUID, DAUID), .fns = as.character)),
    by = "DBUID"
  )

#Selecting CHSA table
conv_table_CHSA <- conversion_table %>%
  select(DBUID, DAUID, CHSA, Pop_2021_Adjusted)

#Changing names to work under the loop.
names(conv_table_CHSA) <- c("DAs", "CHSA", "Pop_2021_Adjusted")

#Creating population adjusted weights to distribute CHSAs
conv_table_CHSA <- conv_table_CHSA %>%
  group_by(DAs) %>%
  mutate(All_pop = sum(Pop_2021_Adjusted, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(DAs, CHSA) %>%
  summarise(
    prorate = sum(Pop_2021_Adjusted, na.rm = TRUE),
    All_pop = mean(All_pop),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(DAs) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(DAs, CHSA) %>%
  mutate(
    prorate = prorate / All_pop,
    prorate = ifelse(is.finite(prorate), prorate, 1 / count)
  ) %>%
  select(-c(All_pop, count)) %>%
  rename(`Dissemination Area` = DAs) %>%
  arrange(`Dissemination Area`, CHSA)

write_csv(
  conv_table_CHSA,
  file.path(
    LAN,
    "Requests",
    "Ad hoc data requests",
    "DA to CHSA conversion",
    "output",
    "Dissemination Area to CHSAs.csv"
  )
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

nrow_gcs_chsa <- gcs_chsa_da_data %>% count() %>% collect() %>% pull()
# Check if the join was successful
if (nrow_gcs_chsa == 0) {
  stop("Join failed: GCS data and CHSA data do not match.")
} else {
  cat(glue("Join successful: {nrow_gcs_chsa} rows returned.\n"))
}


gcs_chsa_da_cnt_data = gcs_chsa_da_data %>%
  group_by(CDCSD_2021, CHSA, MUN_NAME_2021, CHSA_NAME) %>%
  summarise(DA_CNT = n_distinct(DA)) %>%
  select(CDCSD_2021, CHSA, DA_CNT, MUN_NAME_2021, CHSA_NAME) %>% # have to move the character column to the end of the select due to MSSQL issue.
  collect()


# count how many distinct DAs are in the GCS data
distinct_da_count <- gcs_data %>%
  distinct(DA) %>%
  pull(DA) %>%
  length()

cat(glue("Distinct DAs in GCS data: {distinct_da_count}\n"))

# count how many distinct CHSAs are in the GCS data
distinct_chsa_count <- gcs_data %>%
  distinct(CHSA) %>%
  pull(CHSA) %>%
  length()

cat(glue("Distinct CHSAs in GCS data: {distinct_chsa_count}\n"))

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
