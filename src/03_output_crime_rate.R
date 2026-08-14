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

# this file is used for cleaning the crime rate Excel file and exporting to DIP.
# To access the LAN, we need to install the safepaths package
# library("remotes")
# install_github("bcgov/safepaths")

pacman::p_load(
  odbc,
  DBI,
  futile.logger,
  tidyverse,
  config,
  bcmaps,
  bcdata,
  janitor,
  cansim,
  # safepaths,
  arrow,
  duckdb,
  datadictionary
)

lan_path <- config::get("lan_path")

# Year-sensitive, non-secret refresh parameters (git-tracked).
# Update values in config_year.yml at each annual refresh.
# load_year_config() also runs validate_refresh() (ADR-0005).
source("R/config.R")
year_config <- load_year_config()

# ---------------------------------------------------------------------------
# ANNUAL REFRESH PARAMETER
# ---------------------------------------------------------------------------
# Start year of the crime data window pulled from cansim. Adjust each refresh
# to widen/narrow the historical window (mirrors the output_year convention in
# script 15). The comparison against REF_DATE is kept as a string to preserve
# the existing filtering semantics.
crime_start_year <- 2000

## -------------------------- Logging Setup -------------------------------------------------------
## -----------------------------------------------------------------------------------------------
log_file <- "./R/execution_log.txt"
flog.appender(appender.file(log_file), name = "file_logger")
flog.threshold(INFO, name = "file_logger")

log_info <- function(msg) {
  flog.info(msg, name = "file_logger")
  print(paste(Sys.time(), "|", msg))
}


######################################################################################
# Crime rate data
######################################################################################

########################################################################################################
# B.C. crime trends and STATISTICS
# https://www2.gov.bc.ca/gov/content/justice/criminal-justice/policing-in-bc/publications-STATISTICS-legislation/crime-police-resource-STATISTICS
# Incident-based crime STATISTICS, by detailed VIOLATIONS, police services in British Columbia 1, 2, 3, 4, 5
# Frequency: Annual
# STATISTICS Canada. Table 35-10-0184-01 Incident-based crime STATISTICS, by detailed VIOLATIONS, police services in British Columbia, annual (number unless otherwise noted)
# Release date: 2023-07-27,
# This is outdated
# GEOgraphy: Province or territory, Policing district/zone

# https://www2.gov.bc.ca/assets/gov/law-crime-and-justice/criminal-justice/police/publications/STATISTICS/bc-crime-STATISTICS-2022.xlsx

# Policing district/zone is different from
# https://catalogue.data.gov.bc.ca/dataset/policing-jurisdictions-and-regions-in-bc
# annual and policing district data. Luckily, BC stats team already aggregate the one data variable (total rate excluding traffic) to region level which is close to CD.

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510018401
# For our project, we use the most recently updated data Table 35-10-0184-01 from StatsCAN
########################################################################################################
# use get_cansim_sqlite when working with large tables such as table 35-10-0184-01 which is several gigabytes in size,
# making it impractical to load entirely into memory.
# get_cansim_sqlite saves a sqlite file to cansim.cache_path folder.
cansim_id <- "35-10-0184-01"
# options(cansim.cache_path = use_network_path("data/cansim_cache"))
# it is too slow to index in sqlite on a network drive,
# So switch to a local folder, and create a copy from LAN use_network_path("data/cansim_cache") to C drive repo_folder/data.
# this only needs to run once:
# fs::file_copy(
#   use_network_path("data/cansim_cache/35100184-eng.sqlite"),
#   "./data/35100184-eng.sqlite"
# )
cansim_cache_path <- year_config$cansim_cache_path
Sys.unsetenv("CANSIM_CACHE_PATH")
if (!dir.exists(cansim_cache_path)) {
  dir.create(cansim_cache_path, recursive = TRUE)
}
Sys.setenv(CANSIM_CACHE_PATH = cansim_cache_path)
# options(cansim.cache_path = "./data")
# getOption("cansim.cache_path")
log_info(glue::glue("Opening cansim connection for table {cansim_id}..."))
connection <- cansim::get_cansim_connection(
  cansim_id,
  cache_path = Sys.getenv("CANSIM_CACHE_PATH"),
  format = 'sqlite',
  refresh = TRUE # only occasionally refresh since this table was updated in Frequency: Annual Table: 35-10-0184-01 (formerly CANSIM 252-0081) Release date: 2025-07-22
)
log_info("cansim connection established")

# --- Exploration / sanity-check prints (unused downstream; kept commented) ---
# connection %>% glimpse()
# connection %>% count(GEO)
# available_years <- connection %>% count(REF_DATE) |> collect()

violations_list <- connection %>%
  count(Violations) %>%
  collect() %>%
  mutate(Violation_id = gsub(".*\\[(\\d+)\\].*", "\\1", Violations)) %>%
  janitor::clean_names(case = "screaming_snake") # clean the names. We prefer all uppercase


# # 314 types of crime
# we should choose the most important ones.

# str(VIOLATIONS_selected_list)
# Econ team's idea: only keep three types of crimes
# 1. Total, all Criminal Code VIOLATIONS (excluding traffic) [50]
# 2. Total violent Criminal Code VIOLATIONS [100]
# 3. Homicide [110]

violations_selected_list <- violations_list %>%
  filter(VIOLATION_ID %in% c(50, 100, 110))

log_info(glue::glue(
  "Selected {nrow(violations_selected_list)} violation types: ",
  "{paste(violations_selected_list$VIOLATION_ID, collapse = ', ')}"
))


# Distinct geographies in the cansim table (exploration; unused downstream).
# crime_GEO_list <- connection %>% count(GEO) %>% collect()
# crime_GEOUID_list <- connection %>% count(GeoUID) %>% collect()

# Pull crime stats: restrict to the data window, the selected violation types,
# and the two statistics we report (rate per 100k, and pct change in rate).
bc_crime_stats <- connection %>%
  filter(
    # GEO=="British Columbia",
    # str_starts( GEOUID, "59"),
    REF_DATE >= as.character(crime_start_year), # focus on most recent years
    Violations %in% violations_selected_list$VIOLATIONS, #c("Assault, level 1 [1430]"   ,"Assault, level 2, weapon or bodily harm [1420]"   ) ,  #  ,
    Statistics %in%
      c("Rate per 100,000 population", "Percentage change in rate")
  ) %>%
  # filter(REF_DATE  > lubridate::today() - lubridate::years(11))%>%
  cansim::collect_and_normalize() # it will create many other supporting variables

bc_crime_stats <- bc_crime_stats %>%
  janitor::clean_names(case = "screaming_snake") # clean the names. We prefer all uppercase

# Distinct RESP (police-service respondent) geography lookup from the pulled data.
#  RESP stands for Respondent Codes (specifically referred to in the comments as "Police Services Respondent Codes").
# These codes represent the different policing jurisdictions or "respondents" (such as a municipal police force or a specific RCMP detachment)
# that provide the crime statistics.
# The script uses these codes to link crime data from these specific respondents to broader geographic areas like Dissemination Areas (DAs).
bc_resp_lookup <- bc_crime_stats %>%
  count(GEO, GEO_UID)

log_info(glue::glue(
  "Loaded BC crime stats: {nrow(bc_crime_stats)} rows across ",
  "{nrow(bc_resp_lookup)} RESPs"
))

# policy zone is like: Colwood, British Columbia, Royal Canadian Mounted Police, municipal [59819]

##########################################################################
# For CSD or DAs, many CSDs or DAs have to share RESP together, so it is better to calculate the ratios within each RESP and CSDs or DAs could share the ratios.
# If one CD or CSD has two or more RESPs, we could average them weighting by the number of the postal code regions within the RESPs.
# Econ team provides a lookup table for us to link DA to RESP, which is from population projection project.
###########################################################################

DA_RESP_lookup <- readxl::read_excel(
  path = glue::glue(
    "{lan_path}/2024 SES Index/data/raw_data/crime_rate/Pop by DA and RESP.xlsx"
  ),
  sheet = "DA RESP"
)

DA_RESP_lookup <- DA_RESP_lookup %>%
  filter(!is.na(RESP) & !RESP == 'NULL') %>%
  janitor::clean_names(case = "screaming_snake") # clean the names. We prefer all uppercase

log_info(glue::glue(
  "Loaded DA-RESP lookup: {nrow(DA_RESP_lookup)} rows"
))

# DA_2021 here is only 4 digits; it is not the full census DA UID. It is
# resolved to the long DA_NUM below via the TMF (GCS) table.
# DA_RESP_lookup %>% count(DA_2021)  # exploration
# TMF_file <- use_network_path("2024 SES Index/data/raw_data/TMF/GCS_202406.csv")
# use the GCS file in the decimal/unary database

db_config <- config::get("data_server")
my_schema <- db_config$myschema

con <- DBI::dbConnect(
  odbc(),
  Driver = db_config$driver,
  Server = db_config$server,
  Database = db_config$database,
  Trusted_Connection = "Yes"
)

log_info("Connected to SQL Server database")


# TMF <- read_csv(TMF_file)
TMF <- tbl(
  con,
  Id(schema = year_config$gcs$schema, name = year_config$gcs$table)
)
# standardize the DA number, append the prefix BC code 59, so it is easy to join to other tables.
# Kept as character since it is an identifier, not a quantity. Coerce inputs to
# character first so leading zeros in CD_2021/DA_2021 are preserved.
TMF <- TMF %>%
  mutate(
    DA_NUM = str_c("59", as.character(CD_2021), as.character(DA_2021), sep = "")
  )

TMF_CR <-
  TMF %>%
  janitor::clean_names(case = "screaming_snake") %>%
  count(CD_2021, DA_2021, DA_NUM, RESP)

DA_RESP_lookup_long <- DA_RESP_lookup %>%
  mutate(
    DA_2021 = str_pad(DA_2021, width = 4, pad = "0", side = "left")
  ) %>%
  left_join(
    TMF_CR %>% mutate(RESP = as.character(RESP)) |> collect(),
    by = c("DA_2021" = "DA_2021", "RESP" = "RESP") # the combination of short DA_2021 and RESP is unique, which gives us the unique long DA_NUM
  )

# DA_RESP_lookup_long %>% count(RESP)  # exploration

# create a table with all possible combinations of REF_DATE, VIOLATIONS, STATISTICS for each RESP and DA.
DA_RESP_lookup_with_year <- bc_crime_stats %>%
  distinct(REF_DATE, VIOLATIONS, STATISTICS) %>%
  cross_join(DA_RESP_lookup_long %>% select(RESP, DA_NUM, POP_CNT, PC_CNT))

# Build the complete grid of (year x violation x statistic) for every RESP/DA,
# then attach the actual crime values from the cansim pull.
bc_da_crime_stats_year <- DA_RESP_lookup_with_year %>%
  left_join(
    bc_crime_stats %>%
      select(
        REF_DATE,
        GEO_UID,
        GEO,
        VIOLATIONS,
        CLASSIFICATION_CODE_FOR_VIOLATIONS,
        STATISTICS,
        VALUE
      ),
    by = join_by("REF_DATE", "VIOLATIONS", "STATISTICS", "RESP" == "GEO_UID")
  )

# Collapse multiple RESPs within a DA to a single DA-level rate, weighting each
# RESP's value by its population so larger police jurisdictions count more.
# bc_da_crime_stats_year %>% names %>% paste(collapse = ",")  # exploration

bc_da_crime_stats_year_weighted_by_pop <- bc_da_crime_stats_year %>%
  group_by(
    REF_DATE,
    VIOLATIONS,
    CLASSIFICATION_CODE_FOR_VIOLATIONS,
    STATISTICS,
    DA_NUM
  ) %>% # now only group by DA and year without RESP
  summarise(VALUE = weighted.mean(VALUE, w = POP_CNT))


log_info(glue::glue(
  "Computed population-weighted DA crime rates: ",
  "{nrow(bc_da_crime_stats_year_weighted_by_pop)} rows"
))

# Derive the end year from the data itself so the description never goes stale.
crime_data_end_year <- max(
  as.numeric(bc_da_crime_stats_year_weighted_by_pop$REF_DATE),
  na.rm = TRUE
)

# since the data has ',' in the cells, we use write.csv2
if (!dir.exists("out")) {
  dir.create("out")
}

crime_rate_output_file <- here::here(glue::glue(
  "out/BC_DA_Crime_Rate_DIP_{crime_data_end_year}.csv"
))

write_csv2(bc_da_crime_stats_year_weighted_by_pop, crime_rate_output_file)

log_info(glue::glue(
  "Wrote DA crime rate output to {crime_rate_output_file}"
))
# write.csv2(use_network_path(
#   "2024 SES Index/data/output/BC_DA_Crime_Rate_DIP.csv"
# ))

# later, we may calculate the moving average of the crime rate instead of using the observed rate.

##############################################################
# Data Dictionary
#############################################################

crime_rate_dict_labels <- c(
  "REF_DATE" = glue::glue(
    "The year of the observation (in '%Y' format): from {crime_start_year} to {crime_data_end_year}"
  ),
  "VIOLATIONS" = "Violation type and classification, such as violent criminal code violations|homicide|attempted murder|assault|breaking|entering|",
  "CLASSIFICATION_CODE_FOR_VIOLATIONS" = "The classification code for the violation",
  "STATISTICS" = "The statistic being measured, including Rate per 100,000 population, Percentage change in rate",
  "DA_NUM" = "Dessemination area id in 2021 Canadian Census",
  "VALUE" = "VALUE: Rate per 100,000 population or Percentage change in rate"
)

crime_rate_dict <- create_dictionary(
  bc_da_crime_stats_year_weighted_by_pop,
  var_labels = crime_rate_dict_labels
)
# write the dictionary to DIP, since the data has ',' in the cells, we use write.csv2
write_csv2(crime_rate_dict, here::here("out/Crime_Rate_Dict_DIP.csv"))
write.csv2(
  crime_rate_dict,
  use_network_path("2024 SES Index/data/output/Crime_Rate_Dict_DIP.csv")
)

log_info("Wrote crime rate data dictionary to DIP and LAN")
log_info("03_output_crime_rate.R completed successfully")
