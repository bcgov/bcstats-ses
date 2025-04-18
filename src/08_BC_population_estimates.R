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

# This script is used to download the population estimates for British Columbia from the BC Stats Datacatalogue website
# The data is downloaded in CSV format and saved to the out folder
# updated: 2025-02-20 we need the age groups, so redo this step

pacman::p_load(tidyverse, readxl, safepaths,bcdata)
library(dplyr)
library(tidyr)
library(janitor)
library(datadictionary)

# only need to run once to find the right record id in data catalogue.
# bcdc_search("municipality-population")

# bcdc_browse("86839277-986a-4a29-9f70-fa9b1166f6cb")


bc_sub_provincial_population_estimates_and_projections <- bcdc_get_record("86839277-986a-4a29-9f70-fa9b1166f6cb")

bcdc_tidy_resources('86839277-986a-4a29-9f70-fa9b1166f6cb')
# 7 Regional Districts (Census Divisions) is what we need. 
municipality_population_df <- bcdc_get_data('86839277-986a-4a29-9f70-fa9b1166f6cb', 
                                            resource = '0e15d04d-127c-457a-b999-20800c929927')

# check the length of the regions
municipality_population_df %>%
  count(Region, Region.Name, Region.Type) %>% 
  glimpse()

######################################################################################
# this file only has municipal population estimates for British Columbia
# we need the CSD code to join with other datasets
# CSD code is available in the population estimates and also in TMF file.
# Import TMF file to verify the CSD code in population estimate
######################################################################################

# The GCS 202406 csv file is provided by Econ team and saved in LAN. Need safe network path to get it.
# The network path should be set to a location that could be save in the environment variable SAFEPATHS_NETWORK_PATH or a config file.
# if the safepaths does not work, switch to config file.

stopifnot(Sys.getenv("SAFEPATHS_NETWORK_PATH") != "")

TMF_file  <-  use_network_path("data/raw_data/TMF/GCS_202406.csv")
# TMF_file  <-  file.path(config::get("lan_path"),"2024 SES Index/data/raw_data/TMF/GCS_202406.csv")
TMF <- read_csv(TMF_file)


# standardize the DA number, append the prefix BC code 59, so it is easy to join to other tables.
TMF <- TMF %>% 
  mutate(DA_NUM = as.numeric(str_c("59", CD_2021, DA_2021, sep = "")))

# TMF_names = TMF %>% names() %>% paste(collapse = ",")

# clean the names, one name is not upper-cased. We prefer all uppercase
TMF <- 
  TMF %>% 
  janitor::clean_names(case = "screaming_snake" ) 


# reshape TMF from wide to long format so CDCSD, Municipalities and CSDS are all in one column each.


TMF_CSD <- TMF %>% 
  select(POSTALCODE, CDCSD_2021, CSD_2021, MUNNAME_2021 = MUN_NAME_2021, 
        CDCSD_2016, CSD_2016, MUNNAME_2016 = MUN_NAME_2016,
        CDCSD_2011, CSD_2011, MUNNAME_2011 = MUN_NAME_2011) %>%
  pivot_longer(
    cols = starts_with("CDCSD_") | starts_with("CSD_") | starts_with("MUNNAME_"),
    names_to = c(".value", "YEAR"), # .value: Indicates that the first part of the column name (e.g., "CDCSD", "CSD", "MUN_NAME") should be used as the name of the resulting columns.
    # YEAR: The second part of the column name (e.g., "2011", "2016", "2021") will be stored in a new column named "YEAR".
    names_sep = "_" # names_sep = "_": This specifies that the underscore character ("_") is used to separate the components in the original column names.
  ) %>% 
  count(CDCSD, CSD, MUNNAME, YEAR) %>%
  rename(COUNT_POSTAL_CODE = n)
  

# Transform TMF_CSD data frame to include a continuous YEAR column ranging from 2000 to 2024, and to fill in missing values appropriately,

years <- 2000:2024

complete_data <- TMF_CSD %>%
  distinct( CDCSD, CSD, MUNNAME) %>%
  expand_grid(YEAR = years) %>%  # expand.grid() to create all combinations of POSTALCODE, CDCSD, CSD, MUNNAME, and YEAR.
  mutate(YEAR = as.character(YEAR))

merged_data <- complete_data %>%
  left_join(TMF_CSD, by = c( "CDCSD", "CSD", "MUNNAME", "YEAR"))

# Fill missing COUNT_POSTAL_CODE values backward and forward within each group
final_data <- merged_data %>%
  group_by( CDCSD, CSD, MUNNAME) %>%
  fill(COUNT_POSTAL_CODE, .direction = "up") %>% # the fill() function from the tidyr package to propagate non-missing values backward and forward within each group.
  fill(COUNT_POSTAL_CODE, .direction = "down") %>% # for the last couples of year after 2021, use propagate non-mi  forward 
  ungroup() 
  

#################################################################################

# Join the population estimates with the TMF_CSD file
municipality_population_CSD_df <- municipality_population_df %>%
  janitor::clean_names(case = "screaming_snake" )  %>% 
  # we need the age group, so no select anymore
  # select(Region, Region.Name, Region.Type,Year,Type,Gender, Total ) %>%
  filter(TYPE == "Estimate",
         GENDER == "T",
         YEAR>=2000) %>% 
  mutate(YEAR = as.character(YEAR),
         CDCSD = str_pad(REGION, 5, side = "left", pad = "0")) %>%
  left_join(
    final_data,
    # should use CSD code to join
    by = c("CDCSD" = "CDCSD", "YEAR" = "YEAR")  # Region.Name is not standardized, for example, Langley, District Municipality; Langley, City of
  ) %>%
  select(CDCSD ,  REGION_NAME , MUN_NAME = MUNNAME, YEAR, POPULATION = TOTAL, starts_with("X")) %>% 
  rename_with(.fn = ~ str_replace(string = .x,  
                          pattern ="X", 
                          replacement = "AGE_"),
              .cols = starts_with("X"))




# save to out folder
municipality_population_CSD_df  %>%
  write_csv("out/BC_municipality_CSD_population_estimate.csv")
municipality_population_CSD_df  %>%
  write_csv(use_network_path("data/Output/BC_municipality_CSD_population_estimate.csv"))

# create a dictionary of the data types
#################################################################################################
# Data dictionary for the BC municipality population estimates
#################################################################################################



# 2. Create a dictionary 

municipality_population_CSD_labels = c(
        'CDCSD' = 'Census Subdivision Code',
        'REGION_NAME'= 'Region Name',
        'MUN_NAME'= 'Census Subdivision Name',
        'YEAR' = 'Year',
        'POPULATION'= 'Total Population Estimate'
    )


municipality_population_CSD_dict <- create_dictionary(municipality_population_CSD_df %>% 
                                                        select(-starts_with("AGE")),
                                                            id_var = c("CDCSD", "YEAR"),
                                                            var_labels = municipality_population_CSD_labels)
# 
file_path <- use_network_path("data/Output/BC_municipality_CSD_population_estimate_dict.csv")
write.csv(municipality_population_CSD_dict, file = file_path)
file_path <- "out/BC_municipality_CSD_population_estimate_dict.csv"
write.csv(municipality_population_CSD_dict, file = file_path)
