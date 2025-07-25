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
  tidyverse,
  config,
  bcmaps,
  bcdata,
  janitor,
  cansim,
  safepaths,
  arrow,
  duckdb,
  datadictionary
)

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
fs::file_copy(
  use_network_path("data/cansim_cache/35100184-eng.sqlite"),
  "./data/35100184-eng.sqlite"
)
options(cansim.cache_path = "./data")

getOption("cansim.cache_path")
connection <- cansim::get_cansim_sqlite(
  cansim_id,
  # auto_refresh=TRUE,
  cache_path = getOption("cansim.cache_path")
  # refresh=TRUE # only occasionally refresh
)
# ignore the warning, the cache does not have the date right. It is retrieved in July 30th 2024, so it is updated.
#
connection %>%
  glimpse()


violations_list = connection %>%
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

violations_selected_list = violations_list %>%
  filter(VIOLATION_ID %in% c(50, 100, 110))


crime_GEO_list = connection %>%
  count(GEO) %>%
  collect()
# # 237 regions: Policing district/zone. id Police Services Respondent Codes: RESP like 59774
#  [59774] need to parse out and join to TMF RESP

crime_GEOUID_list = connection %>%
  count(GeoUID) %>%
  collect()
# 237 GEOUIDs: Policing district/zone. id Police Services Respondent Codes: RESP like 59774 or 59926
# Only look at data after 2000
bc_crime_stats <- connection %>%
  filter(
    # GEO=="British Columbia",
    # str_starts( GEOUID, "59"),
    REF_DATE >= "2000", # focus on most recent years
    Violations %in% violations_selected_list$VIOLATIONS, #c("Assault, level 1 [1430]"   ,"Assault, level 2, weapon or bodily harm [1420]"   ) ,  #  ,
    Statistics %in%
      c("Rate per 100,000 population", "Percentage change in rate")
  ) %>%
  # filter(REF_DATE  > lubridate::today() - lubridate::years(11))%>%
  cansim::collect_and_normalize() # it will create many other supporting variables

bc_crime_stats <- bc_crime_stats %>%
  janitor::clean_names(case = "screaming_snake") # clean the names. We prefer all uppercase

bc_resp_lookup = bc_crime_stats %>%
  count(GEO, GEO_UID)
# 237 resps

# policy zone is like: Colwood, British Columbia, Royal Canadian Mounted Police, municipal [59819]

##########################################################################
# For CSD or DAs, many CSDs or DAs have to share RESP together, so it is better to calculate the ratios within each RESP and CSDs or DAs could share the ratios.
# If one CD or CSD has two or more RESPs, we could average them weighting by the number of the postal code regions within the RESPs.
# Econ team provides a lookup table for us to link DA to RESP, which is from population projection project.
###########################################################################

DA_RESP_lookup <- readxl::read_excel(
  path = use_network_path(
    "2024 SES Index/data/raw_data/crime_rate/Pop by DA and RESP.xlsx"
  ),
  sheet = "DA RESP"
)

DA_RESP_lookup <- DA_RESP_lookup %>%
  filter(!is.na(RESP) & !RESP == 'NULL') %>%
  janitor::clean_names(case = "screaming_snake") # clean the names. We prefer all uppercase

DA_RESP_lookup %>%
  count(DA_2021)
# 3,711
# some DAs cover multiple RESP
# ? this DA_2021 only 4 digits long, so it is not the same as the DA_2021 in the census data
# solution is to get the unique combination of short DA_2021 and RESP from TMF table which also has the long DA_NUM
TMF_file <- use_network_path("2024 SES Index/data/raw_data/TMF/GCS_202406.csv")

TMF <- read_csv(TMF_file)


# standardize the DA number, append the prefix BC code 59, so it is easy to join to other tables.
TMF <- TMF %>%
  mutate(DA_NUM = as.numeric(str_c("59", CD_2021, DA_2021, sep = "")))

TMF_CR <-
  TMF %>%
  janitor::clean_names(case = "screaming_snake") %>%
  count(CD_2021, DA_2021, DA_NUM, RESP)

DA_RESP_lookup_long <- DA_RESP_lookup %>%
  mutate(
    DA_2021 = str_pad(DA_2021, width = 4, pad = "0", side = "left")
  ) %>%
  left_join(
    TMF_CR %>% mutate(RESP = as.character(RESP)),
    by = c("DA_2021" = "DA_2021", "RESP" = "RESP") # the combination of short DA_2021 and RESP is unique, which gives us the unique long DA_NUM
  )

DA_RESP_lookup_long %>%
  count(RESP)
# # 194 RESPs in the lookup table which is close to the number of 193 RESPs in Crime rate data table

# create a table with all possible combinations of REF_DATE, VIOLATIONS, STATISTICS for each RESP and DA.
DA_RESP_lookup_with_year = bc_crime_stats %>%
  distinct(REF_DATE, VIOLATIONS, STATISTICS) %>%
  cross_join(DA_RESP_lookup_long %>% select(RESP, DA_NUM, POP_CNT, PC_CNT))

# one option is to join to da table
bc_da_crime_stats_year = DA_RESP_lookup_with_year %>%
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

# to eliminate the duplicated RESP rate within DAs in which one DAs have multiple RESP rates,
# we use number of postal code regions or population as weights to get weighted average crime rate for each DA

# bc_da_crime_stats_year %>% names %>% paste(collapse = ",")

bc_da_crime_stats_year_weighted_by_pop = bc_da_crime_stats_year %>%
  group_by(
    REF_DATE,
    VIOLATIONS,
    CLASSIFICATION_CODE_FOR_VIOLATIONS,
    STATISTICS,
    DA_NUM
  ) %>% # now only group by DA and year without RESP
  summarise(VALUE = weighted.mean(VALUE, w = POP_CNT))
# since the data has ',' in the cells, we use write.csv2
bc_da_crime_stats_year_weighted_by_pop %>%
  write_csv2(here::here("out/BC_DA_Crime_Rate_DIP.csv"))
# write.csv2(use_network_path(
#   "2024 SES Index/data/output/BC_DA_Crime_Rate_DIP.csv"
# ))

# later, we may calculate the moving average of the crime rate instead of using the observed rate.

##############################################################
# Data Dictionary
#############################################################

crime_rate_dict_labels = c(
  "REF_DATE" = "The year of the observation (in '%Y' format): from 2000 to 2023",
  "VIOLATIONS" = "Violation type and classification, such as violent criminal code violations|homicide|attempted murder|assault|breaking|entering|",
  "CLASSIFICATION_CODE_FOR_VIOLATIONS" = "The classification code for the violation",
  "STATISTICS" = "The statistic being measured, including Rate per 100,000 population, Percentage change in rate",
  "DA_NUM" = "Dessemination area id in 2021 Canadian Census",
  "VALUE" = "VALUE: Rate per 100,000 population or Percentage change in rate"
)

crime_rate_dict = create_dictionary(
  bc_da_crime_stats_year_weighted_by_pop,
  var_labels = crime_rate_dict_labels
)
# write the dictionary to DIP, since the data has ',' in the cells, we use write.csv2
write_csv2(crime_rate_dict, here::here("out/Crime_Rate_Dict_DIP.csv"))
write.csv2(
  crime_rate_dict,
  use_network_path("2024 SES Index/data/output/Crime_Rate_Dict_DIP.csv")
)
