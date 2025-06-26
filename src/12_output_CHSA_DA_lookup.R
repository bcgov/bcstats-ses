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

# get CHSA data reference
cat("Retrieving CHSA data...\n")
chsa_data <- tbl(con, in_schema("Prod", chsa_table)) %>%
  select(CHSA, CHSA_NAME) %>%
  collect()

chsa_data %>% glimpse()


#Setting paths
bc_stats_project_lan <- config$file_path$bc_stats_project

Path_conversion <- file.path(
  bc_stats_project_lan,
  "ConversionTables",
  "TranslationMasterFiles",
  "2025"
)


#Loading the conversion table

#Conversion table is a table where population team saves estimated population

conversion_table <- readxl::read_xls(file.path(
  Path_conversion,
  paste0(
    "Translation_Master_File_",
    "2021_Census_Completed_",
    "Provincial_Runs_2025_01_10",
    " (identifiers).xls"
  )
))

# get population estimate in DB level. There are many geography in this table, but we don't use them.
conversion_table_db_chsa_pop <- conversion_table %>%
  select(DBUID = "DISSEMINATION_BLOCK_ID", CHSA, Pop_2021_Adjusted) # we don't use the DA id from this table. Instead, we use the DA id from geo attr table from StatsCAN according to Jonathan


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

# Download and process CISR data
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
    # PRUID = PRUID_PRIDU,# for the future use.
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
conversion_table_db_da_chsa_pop <- conversion_table_db_chsa_pop %>%
  left_join(
    geo_attrs %>%
      mutate(across(c(DBUID, DAUID), .fns = as.character)),
    by = "DBUID"
  )


# count how many distinct DBUIDs are in the GCS data
distinct_db_count <- conversion_table_db_da_chsa_pop %>%
  distinct(DBUID) %>%
  pull(DBUID) %>%
  length()

cat(glue("Distinct DBUIDs in GCS data: {distinct_db_count}\n"))

# count how many distinct DAUIDs are in the GCS data
distinct_da_count <- conversion_table_db_da_chsa_pop %>%
  distinct(DAUID) %>%
  pull(DAUID) %>%
  length()

cat(glue("Distinct DBUIDs in GCS data: {distinct_da_count}\n"))

# count how many distinct CHSAs are in the GCS data
distinct_chsa_count <- conversion_table_db_da_chsa_pop %>%
  distinct(CHSA) %>%
  pull(CHSA) %>%
  length()

cat(glue("Distinct CHSAs in GCS data: {distinct_chsa_count}\n"))

# Check if one DB is covering two CHSAs
db_multiple_chsa <- conversion_table_db_da_chsa_pop %>%
  group_by(DBUID) %>%
  summarise(CHSA_CNT = n_distinct(CHSA)) %>%
  filter(CHSA_CNT > 1) %>%
  collect()

if (nrow(db_multiple_chsa) > 0) {
  cat(glue(
    "WARNING: {nrow(db_multiple_chsa)} DBs are covering multiple CHSAs!\n"
  ))
  print(db_multiple_chsa)
} else {
  cat("No DBs covering multiple CHSAs found.\n")
}

# good, no DB has covered two CHSAs.

# Check if one DB is covering two DAs
db_multiple_da <- conversion_table_db_da_chsa_pop %>%
  group_by(DBUID) %>%
  summarise(DA_CNT = n_distinct(DAUID)) %>%
  filter(DA_CNT > 1) %>%
  collect()

if (nrow(db_multiple_da) > 0) {
  cat(glue(
    "WARNING: {nrow(db_multiple_da)} DBs are covering multiple DAs!\n"
  ))
  print(db_multiple_da)
} else {
  cat("No DBs covering multiple DAs found.\n")
}

# good, no DB has covered two DAs.

# Check if one DA is covering two CHSAs
da_multiple_chsa <- conversion_table_db_da_chsa_pop %>%
  group_by(DAUID) %>%
  summarise(CHSA_CNT = n_distinct(CHSA)) %>%
  filter(CHSA_CNT > 1) %>%
  collect()

if (nrow(da_multiple_chsa) > 0) {
  cat(glue(
    "WARNING: {nrow(da_multiple_chsa)} DAs are covering multiple CHSAs!\n"
  ))
  print(da_multiple_chsa)

  # If you want to see the details of these DAs with their multiple CHSAs
  # problem_das <- da_multiple_chsa %>% pull(DAUID)
  # da_chsa_details <- conversion_table_db_da_chsa_pop %>%
  #   filter(DAUID %in% problem_das) %>%
  #   count(DAUID, CHSA) %>%
  #   collect()
  #
  # print(da_chsa_details)
} else {
  cat("No DAs covering multiple CHSAs found.\n")
}


# no duplication or overlapping if group by DB
# so if our base table is DA level, and we want to aggregate DA level table to CHSA level table,
# we need to split DAs into different CHSAs with population as weights.
# for example, 20% DA01's population contribute to CHSA01, and 80% DA01's population contribute to CHSA02.
# and 40% DA01's population contribute to CHSA01 as well.
# To get CHSA01's average x1 value, we have x1_CHSA01 = (0.2*POP_DA01 * x1_DA01 + 0.4*POP_DA_02 *x1_DA02)/(0.2*POP_DA01 + 0.4*POP_DA_02)
# where (0.2*POP_DA01 + 0.4*POP_DA_02) is the population in CHSA01, 0.2*POP_DA01 is the population in DA01_CHSA01.
# so we need to get the DA01_CHSA01 population and DA population and CHSA population to get the  population ratio within DA like 0.2 and  population weights within CHSA.
# we call the new intermediate region as CHSADA which are smaller than CHSA and DA but larger than DB.

# Now we have a DB level table
#Creating population adjusted weights to distribute CHSAs
db_da_chsa_pop_cnt <- conversion_table_db_da_chsa_pop %>%
  group_by(CHSA, DAUID) %>%
  mutate(
    chsada_pop = sum(Pop_2021_Adjusted, na.rm = TRUE),
    cnt_db_in_chsada = n()
  ) %>%
  ungroup() %>%
  group_by(DAUID) %>%
  mutate(
    da_pop = sum(Pop_2021_Adjusted, na.rm = TRUE),
    chsada_to_da_pop_ratio = chsada_pop / da_pop,
    cnt_db_in_da = n() # the same within da
  ) %>%
  ungroup() %>%
  group_by(CHSA) %>%
  mutate(
    chsa_pop = sum(Pop_2021_Adjusted, na.rm = TRUE),
    chsada_to_chsa_pop_ratio = chsada_pop / chsa_pop,
    cnt_db_in_chsa = n() # the same within chsa
  ) %>%
  ungroup() %>%
  # start to get the weights. now the table is still in DB level since we only implement mutate operation not summarise operation.
  # the weights for our purpose (aggregate da value to chsa value) will be chsada_pop/chsa_pop for da value.
  # the weights for disaggregate chsa value to da value) will be chsada_pop/da_pop for da value, which is the weight/prorate in Jonathan's original code.
  count(
    chsada_pop,
    cnt_db_in_chsada,
    DAUID,
    da_pop,
    chsada_to_da_pop_ratio,
    cnt_db_in_da, # all the same within da
    CHSA,
    chsa_pop,
    chsada_to_chsa_pop_ratio,
    cnt_db_in_chsa, # all the same within chsa
    sort = T,
    name = "cnt_db" # should be the same as cnt_db_in_chsada, remove it later
  ) %>%
  mutate(
    chsada_id = row_number(),
    CHSA = as.character(CHSA)
  ) %>%
  left_join(chsa_data)
db_da_chsa_pop_cnt %>% glimpse()
# the number of chsada is similar to the number of DA. 8084
cat(glue::glue("There are {nrow(db_da_chsa_pop_cnt)} CHSADAs found.\n"))
# original from Jonathan
# conv_table_CHSA <- conversion_table_db_da_chsa_pop %>%
#   group_by(DAs) %>%
#   mutate(All_pop = sum(Pop_2021_Adjusted, na.rm = TRUE)) %>% # All_pop is the DA population
#   ungroup() %>%
#   group_by(DAs, CHSA) %>%
#   summarise(
#     prorate = sum(Pop_2021_Adjusted, na.rm = TRUE), # since this is within DA/CHSA group, so this is population within overlapped region between DA and CHSA. for example, DA01_CHSA01,and DA02_CHSA01.
#     All_pop = mean(All_pop), # since this within DA/CHSA group, All_pop again is the DA population. It seems to be the same as the original All_pop
#     .groups = "drop"
#   ) %>%
#   ungroup() %>%
#   group_by(DAs) %>%
#   mutate(count = n()) %>%
#   ungroup() %>%
#   group_by(DAs, CHSA) %>% # if there is no summary in the mutate statement, it seems not necessary.
#   mutate(
#     prorate = prorate / All_pop,
#     prorate = ifelse(is.finite(prorate), prorate, 1 / count)
#   ) %>%
#   select(-c(All_pop, count)) %>%
#   rename(`Dissemination Area` = DAs) %>%
#   arrange(`Dissemination Area`, CHSA)

db_da_chsa_pop_cnt %>%
  glimpse()

write_csv(
  db_da_chsa_pop_cnt %>%
    select(-cnt_db),
  here::here("out", "db_da_chsa_pop_cnt.csv")
)

db_da_chsa_pop_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/chsa_da_crosswalk/"
)

write_csv(
  db_da_chsa_pop_cnt %>%
    select(-cnt_db),
  file.path(db_da_chsa_pop_csv_folder, "db_da_chsa_pop_cnt")
)


#################################################################################################
# Data dictionary
#################################################################################################

da_chsa_data_dict_labels = c(
  "chsada_id" = "The intersection region of CHSA and DA, and the id is created using row number.",
  "chsada_pop" = "The estimated population within a CHSADA region, and source is BCStats population team",
  "cnt_db_in_chsada" = "The number of DBs within a CHSADA",
  "DAUID" = "DAs are small, relatively stable geographic units composed of one or more adjacent dissemination blocks where populations generally range from 400 to 700 people. DAs cover all the territory of Canada and are the smallest standard geographic area for which all census data are disseminated.",
  "da_pop" = "The estimated population within a DA, and source is BCStats population team",
  "chsada_to_da_pop_ratio" = "The proportion of estimated population within a chsada over a DA, and source is BCStats population team",
  "cnt_db_in_da" = "The number of DBs within a DA",
  "CHSA" = "Community Health Service Area (CHSA)",
  "CHSA_NAME" = "Community Health Service Area (CHSA) Name",
  "chsa_pop" = "The estimated population within a CHSA, and source is BCStats population team",
  "chsada_to_chsa_pop_ratio" = "The proportion of estimated population within a chsada over a CHSA, and source is BCStats population team",
  "cnt_db_in_chsa " = "The number of DBs within a CHSA"
)

length(da_chsa_data_dict_labels)
ncol(
  db_da_chsa_pop_cnt %>%
    select(-cnt_db)
)
da_chsa_data_dict = create_dictionary(
  db_da_chsa_pop_cnt %>%
    select(-cnt_db),
  var_labels = da_chsa_data_dict_labels
)

write_csv(da_chsa_data_dict, here::here("out/da_chsa_data_dict.csv"))
write_csv(
  da_chsa_data_dict,
  file.path(db_da_chsa_pop_csv_folder, "da_chsa_data_dict.csv")
)
