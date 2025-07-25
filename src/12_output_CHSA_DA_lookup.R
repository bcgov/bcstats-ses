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
library(bcdata)
source("./src/utils.R") # get the functions for plotting maps

province = "British Columbia"
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

#####################################################################
# CHSA
# https://catalogue.data.gov.bc.ca/dataset/community-health-service-areas-boundaries
#####################################################################

# get CHSA data reference
cat("Retrieving CHSA data...\n")
chsa_data <- tbl(con, in_schema("Prod", chsa_table)) %>%
  select(CHSA, CHSA_NAME) %>%
  collect()

chsa_data %>% glimpse()

bcdata::bcdc_search("CHSA")

# 12: Community Health Service Areas Boundaries (multiple, wms, kml, json, xlsx)
# ID: 68f2f577-28a7-46b4-bca9-7e9770f2f357
# Name: community-health-service-areas-boundaries

bcdc_get_record("68f2f577-28a7-46b4-bca9-7e9770f2f357")

bcdc_tidy_resources('68f2f577-28a7-46b4-bca9-7e9770f2f357')

chsa_bd <- bcdc_query_geodata('68f2f577-28a7-46b4-bca9-7e9770f2f357') |>
  collect()


chsa_tbl <- bcdc_get_data(
  '68f2f577-28a7-46b4-bca9-7e9770f2f357',
  resource = bcdc_tidy_resources('68f2f577-28a7-46b4-bca9-7e9770f2f357') |>
    filter(str_detect(name, "Master table")) |>
    pull(id)
)

#####################################################################
# Create GCS data reference, use this table to create a crosswalk of CHSA and DB from 2016 to 2023
# and then later join the DB population to create weights for crosswolk of CHSA and DA
#
#####################################################################
cat("Retrieving GCS data...\n")

# postal code region level
tbl_long_cols_mssql(con, "Prod", gcs_table) %>%
  # tbl(con, in_schema("Prod", gcs_table)) %>%
  filter(ACTIVE == 'Y') %>%
  colnames() %>%
  paste0(collapse = ",") |>
  glimpse()

gcs_data <- tbl_long_cols_mssql(con, "Prod", gcs_table) %>% collect()

gcs_data |>
  count(DB_2021)
# only 29,459 DBs??
gcs_data |>
  count(DA_2021)
# only 3,714 DAs?? Not good, will consider to add Jonathan's TMF a

# gcs_data %>%
#   filter(
#     DB_2021 == "59090896001"
#   )
#
# gcs_data %>%
#   filter(
#     DB_2016 == "59090896001"
#   )
#
# # DB 59090896001 was not in 2016, and was initiated in 2021
# # and all those 7 postal code regions switched from one DB to another DB
#
# gcs_data %>%
#   filter(
#     DB_2021 == "59090124002"
#   )
#
# gcs_data %>%
#   filter(
#     DB_2016 == "59090124002"
#   )
#
# # DB 59090124002 was in 2016 and breaks into different DB in 2021
# # DB only 2021 and 2016, but DA CSD back to 2011
# it may cause problem later

# start working on DB2021 and CHSA
# each row/year is expanded to 2019:2023
gcs_chsa_db_lookup_2021 <- gcs_data %>%
  count(
    CHSA,
    DAUID = DA_2021,
    DBUID = DB_2021,
    name = "CNT_POSTALCODE"
  ) %>%
  tidyr::expand_grid(YEAR = 2019:2023) %>%
  left_join(chsa_data)


# start working on DB2021 and CHSA
gcs_chsa_db_lookup_2016 <- gcs_data %>%
  count(
    CHSA,
    DAUID = DA_2016,
    DBUID = DB_2016,
    name = "CNT_POSTALCODE"
  ) %>%
  tidyr::expand_grid(YEAR = 2016:2018) %>%
  left_join(chsa_data)


gcs_chsa_db_lookup_2016_2023 <- gcs_chsa_db_lookup_2021 %>%
  bind_rows(gcs_chsa_db_lookup_2016) |>
  select(-CNT_POSTALCODE) |>
  select(YEAR, CHSA, CHSA_NAME, DBUID)

gcs_chsa_db_lookup_2016_2023 |> glimpse()
# 234,073
#########################################################################
# Use Jonathan's TMF file, limitation is that it only has DB 2021 and DA 2021.
# So may take both and make a set of CHSA, DB, and YEAR
#########################################################################

#Setting pathsAdd commentMore actions
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

conversion_table |>
  # glimpse()
  count(DISSEMINATION_AREA_ID)
# 7,848

db_da_chsa_2021_jonathan <- conversion_table |>
  select(
    CHSA,
    DBUID = "DISSEMINATION_BLOCK_ID",
    DAUID = "DISSEMINATION_AREA_ID"
  ) |>
  distinct() |>
  mutate(CHSA = as.character(CHSA)) |>
  left_join(chsa_data)

db_da_chsa_2021_jonathan |> glimpse()
# 52423 DBs

db_da_chsa_2016_2023_jonathan <- db_da_chsa_2021_jonathan |>
  expand_grid(YEAR = 2016:2023) |>
  select(YEAR, CHSA, CHSA_NAME, DBUID)

db_da_chsa_2016_2023_jonathan |> glimpse()
# 419,384 rows

db_da_chsa_lookup_2016_2023 <- gcs_chsa_db_lookup_2016_2023 |>
  bind_rows(db_da_chsa_2016_2023_jonathan) |>
  distinct()

db_da_chsa_lookup_2016_2023 |>
  glimpse()
# 429,323 rows
# So we recover some (10000) DB2016 from GCS table

#################################################################
# DB population from Jonathan
#################################################################
#Setting paths
bc_stats_project_lan <- config$file_path$bc_stats_project

# replace this data source with Jonathan's DB population estimate from 2016 to 2023

# file path of DB population estimates
db_population_file_path = file.path(
  bc_stats_project_lan,
  config$file_path$population_folder
)

# list all the rds files
db_population_files = list.files(db_population_file_path, full.names = T)
# read all the rds files
db_population_df = db_population_files %>%
  # Filter to include only RDS files and exclude directories
  purrr::keep(function(path) {
    file.exists(path) &&
      !dir.exists(path) &&
      tolower(tools::file_ext(path)) == "rds"
  }) %>%
  # Read the remaining RDS files
  purrr::map(readRDS) %>%
  bind_rows()


# get population estimate in DB level. There are many geography in this table, but we don't use them.
db_population_after2016_df <- db_population_df %>%
  filter(Sex == "Total") %>%
  filter(Age == "TOTAL") %>%
  select(-Sex, -Age) %>%
  filter(Type == "Dissemination Block") %>%
  select(-Type) %>%
  select(
    YEAR = Year,
    DBUID = ID,
    DAUID = parentID,
    POPULATION = Population
  ) %>%
  filter(
    YEAR >= 2016
  )

db_population_after2016_df |> glimpse()

db_population_after2016_df |>
  count(DAUID)
# 7,848 DAs.which seems right.

# Check if one DB is covering two DAs
db_multiple_da <- db_population_after2016_df %>%
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

#########################################################################
# Join two tables together and gcs/TMF as base
#########################################################################

bc_db_da_chsa_pop <- db_da_chsa_lookup_2016_2023 |>
  left_join(
    db_population_after2016_df |>
      mutate(DBUID = as.character(DBUID)),
    by = join_by(YEAR, DBUID)
  )

bc_db_da_chsa_pop |> glimpse()
# Rows: 429,323

# fix NA DAUID, if DAUIA from population estimates table is missing, we extract them from DBUID
bc_db_da_chsa_pop |>
  filter(is.na(DAUID))

bc_db_da_chsa_pop <- bc_db_da_chsa_pop |>
  mutate(
    DAUID = as.character(DAUID),
    DAUID = if_else(
      is.na(DAUID),
      str_sub(DBUID, 1, 8),
      DAUID
    )
  )

##########################################################
# Validation
#########################################################

# count how many distinct DBUIDs are in the GCS data
distinct_db_count <- bc_db_da_chsa_pop %>%
  distinct(DBUID) %>%
  pull(DBUID) %>%
  length()

cat(glue("Distinct DBUIDs in GCS/TMF data: {distinct_db_count}\n"))

# count how many distinct DAUIDs are in the GCS data
distinct_da_count <- bc_db_da_chsa_pop %>%
  distinct(DAUID) %>%
  pull(DAUID) %>%
  length()

cat(glue("Distinct DBUIDs in GCS data: {distinct_da_count}\n"))
# 8182 which seems more than we ecpected from StatsCAN
# count how many distinct CHSAs are in the GCS data
distinct_chsa_count <- bc_db_da_chsa_pop %>%
  distinct(CHSA) %>%
  pull(CHSA) %>%
  length()

cat(glue("Distinct CHSAs in GCS data: {distinct_chsa_count}\n"))
# 231

# Check if one DB is covering two CHSAs
db_multiple_chsa <- bc_db_da_chsa_pop %>%
  group_by(DBUID, YEAR) %>%
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

# 4 DBs covered two CHSAs.
# for those four DB, their population should be shared by tow CHSAs
# so assume 50% share for each CHSA
# only for year 2016 to 2018

bc_db_da_chsa_pop <- bc_db_da_chsa_pop |>
  mutate(
    POPULATION = if_else(
      DBUID %in%
        (db_multiple_chsa |> distinct(DBUID) |> pull(DBUID)) &
        (YEAR %in% 2016:2018),
      POPULATION / 2,
      POPULATION
    )
  )

# Check if one DB is covering two DAs
db_multiple_da <- bc_db_da_chsa_pop %>%
  group_by(DBUID, YEAR) %>%
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

# No DBs have covered two DAs.

# Check if one DA is covering two CHSAs
da_multiple_chsa <- bc_db_da_chsa_pop %>%
  group_by(DAUID, YEAR) %>%
  summarise(CHSA_CNT = n_distinct(CHSA)) %>%
  filter(CHSA_CNT > 1) %>%
  collect()

if (nrow(da_multiple_chsa) > 0) {
  cat(glue(
    "WARNING: {nrow(da_multiple_chsa)} DAs and YEAR combination are covering multiple CHSAs!\n"
  ))
  print(da_multiple_chsa)

  # If you want to see the details of these DAs with their multiple CHSAs
  # problem_das <- da_multiple_chsa %>% pull(DAUID)
  # da_chsa_details <- bc_db_da_chsa_pop %>%
  #   filter(DAUID %in% problem_das) %>%
  #   count(DAUID, CHSA) %>%
  #   collect()
  #
  # print(da_chsa_details)
} else {
  cat("No DAs covering multiple CHSAs found.\n")
}
# 1770 DAs and YEAR combination are covering multiple CHSAs!

# so if our base table is DA level, and we want to aggregate DA level table to CHSA level table,
# we need to split DAs into different CHSAs with population as weights.
# for example, 20% DA01's population contribute to CHSA01, and 80% DA01's population contribute to CHSA02.
# and 40% DA01's population contribute to CHSA01 as well.
# To get CHSA01's average x1 value, we have x1_CHSA01 = (0.2*POP_DA01 * x1_DA01 + 0.4*POP_DA_02 *x1_DA02)/(0.2*POP_DA01 + 0.4*POP_DA_02)
# where (0.2*POP_DA01 + 0.4*POP_DA_02) is the population in CHSA01, 0.2*POP_DA01 is the population in DA01_CHSA01.
# so we need to get the DA01_CHSA01 population and DA population and CHSA population to get the  population ratio within DA like 0.2 and  population weights within CHSA.
# we call the new intermediate region as CHSADA which are smaller than CHSA and DA but larger than DB.

# Now we have a DB level table
# Creating population adjusted weights to distribute CHSAs
db_to_da_chsa_pop_cnt <- bc_db_da_chsa_pop %>%
  group_by(YEAR, CHSA, DAUID) %>%
  mutate(
    chsada_pop = sum(POPULATION, na.rm = TRUE),
    cnt_db_in_chsada = n()
  ) %>%
  ungroup() %>%
  group_by(YEAR, DAUID) %>%
  mutate(
    da_pop = sum(POPULATION, na.rm = TRUE),
    chsada_to_da_pop_ratio = chsada_pop / da_pop,
    cnt_db_in_da = n() # the same within da
  ) %>%
  ungroup() %>%
  group_by(YEAR, CHSA) %>%
  mutate(
    chsa_pop = sum(POPULATION, na.rm = TRUE),
    chsada_to_chsa_pop_ratio = chsada_pop / chsa_pop,
    cnt_db_in_chsa = n() # the same within chsa
  ) %>%
  ungroup() %>%
  # start to get the weights. now the table is still in DB level since we only implement mutate operation not summarise operation.
  # the weights for our purpose (aggregate da value to chsa value) will be chsada_pop/chsa_pop for da value.
  # the weights for disaggregate chsa value to da value) will be chsada_pop/da_pop for da value, which is the weight/prorate in Jonathan's original code.
  count(
    YEAR,
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
    chsada_id = str_c(CHSA, DAUID), # Not work if we have year
    CHSA = as.character(CHSA)
  )


db_to_da_chsa_pop_cnt %>% glimpse()
# the number of chsada is similar to the number of DA. 8084
cat(glue::glue("There are {nrow(db_to_da_chsa_pop_cnt)} CHSADAs found.\n"))
# original from Jonathan
# conv_table_CHSA <- bc_db_da_chsa_pop %>%
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

db_to_da_chsa_pop_cnt <- db_to_da_chsa_pop_cnt %>%
  left_join(chsa_data)


write_csv(
  db_to_da_chsa_pop_cnt %>%
    select(-cnt_db),
  here::here("out", "db_to_da_chsa_pop_cnt.csv")
)

db_da_chsa_pop_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/chsa_da_crosswalk/"
)

write_csv(
  db_to_da_chsa_pop_cnt %>%
    select(-cnt_db),
  file.path(db_da_chsa_pop_csv_folder, "db_to_da_chsa_pop_cnt.csv")
)


#################################################################################################
# Data dictionary
#################################################################################################

da_chsa_data_dict_labels = c(
  "YEAR" = "The calendar year",
  "chsada_pop" = "The estimated population within a CHSADA region, and source is BCStats population team",
  "cnt_db_in_chsada" = "The number of DBs within a CHSADA",
  "DAUID" = "DAs are small, relatively stable geographic units composed of one or more adjacent dissemination blocks where populations generally range from 400 to 700 people. DAs cover all the territory of Canada and are the smallest standard geographic area for which all census data are disseminated.",
  "da_pop" = "The estimated population within a DA, and source is BCStats population team",
  "chsada_to_da_pop_ratio" = "The proportion of estimated population within a chsada over a DA, and source is BCStats population team",
  "cnt_db_in_da" = "The number of DBs within a DA",
  "CHSA" = "Community Health Service Area (CHSA)",
  "chsa_pop" = "The estimated population within a CHSA, and source is BCStats population team",
  "chsada_to_chsa_pop_ratio" = "The proportion of estimated population within a chsada over a CHSA, and source is BCStats population team",
  "cnt_db_in_chsa " = "The number of DBs within a CHSA",
  "chsada_id" = "The intersection region of CHSA and DA, and the id is created using CHSA id and DA id.",
  "CHSA_NAME" = "Community Health Service Area (CHSA) Name"
)

length(da_chsa_data_dict_labels)
ncol(
  db_to_da_chsa_pop_cnt %>%
    select(-cnt_db)
)
da_chsa_data_dict = create_dictionary(
  db_to_da_chsa_pop_cnt %>%
    select(-cnt_db),
  var_labels = da_chsa_data_dict_labels
)

write_csv(da_chsa_data_dict, here::here("out/da_chsa_data_dict.csv"))
write_csv(
  da_chsa_data_dict,
  file.path(db_da_chsa_pop_csv_folder, "da_chsa_data_dict.csv")
)
