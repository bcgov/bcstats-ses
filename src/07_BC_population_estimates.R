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

pacman::p_load(tidyverse, readxl, safepaths,bcdata)
library(dplyr)
library(tidyr)
library(janitor)
bcdc_search("municipality-population")

bcdc_browse("86839277-986a-4a29-9f70-fa9b1166f6cb")


bc_sub_provincial_population_estimates_and_projections <- bcdc_get_record("86839277-986a-4a29-9f70-fa9b1166f6cb")

bcdc_tidy_resources('86839277-986a-4a29-9f70-fa9b1166f6cb')

municipality_population_df <- bcdc_get_data('86839277-986a-4a29-9f70-fa9b1166f6cb', resource = '0e15d04d-127c-457a-b999-20800c929927')

# Download the population estimates for British Columbia
# municipality_population_url <- "https://catalogue.data.gov.bc.ca/dataset/86839277-986a-4a29-9f70-fa9b1166f6cb/resource/0e15d04d-127c-457a-b999-20800c929927/download/municipality-population.csv"
# municipality_population_file <- "../../data/municipality-population.csv"
# 
# download.file(municipality_population_url, 
#               destfile=municipality_population_file, 
#               mode='wb')
# 
# municipality_population_df <- read_csv(municipality_population_url)

municipality_population_df %>%
  count(Region, Region.Name, Region.Type) %>% 
  glimpse()

######################################################################################
# this file only has municipal population estimates for British Columbia
# we need the CSD code to join with other datasets
# CSD code is available in TMF file.

######################################################################################

# The GCS 202406 csv file is provided by Econ team and saved in LAN. Need safe network path to get it.
# The network path should be set to a location that could be save in the environment variable SAFEPATHS_NETWORK_PATH or a config file.
stopifnot(Sys.getenv("SAFEPATHS_NETWORK_PATH") != "")

TMF_file  <-  use_network_path("data/raw_data/TMF/GCS_202406.csv")

TMF <- read_csv(TMF_file)


# standardize the DA number, append the prefix BC code 59, so it is easy to join to other tables.
TMF <- TMF %>% 
  mutate(DA_NUM = as.numeric(str_c("59", CD_2021, DA_2021, sep = "")))

# TMF_names = TMF %>% names() %>% paste(collapse = ",")

# clean the names, one name is not upper-cased. We prefer all uppercase
TMF <- 
  TMF %>% 
  janitor::clean_names(case = "screaming_snake" ) 


# To reshape data frame TMF from a wide format to a long format using the pivot_longer() function from the tidyr package in R,
# 
# Explanation:
#   
# cols = starts_with("CDCSD_") | starts_with("CSD_") | starts_with("MUN_NAME_"): This argument selects all columns that start with "CDCSD_", "CSD_", or "MUN_NAME_".
# 
# names_to = c(".value", "YEAR"): This specifies that the column names will be split into two parts:
#   
# .value: Indicates that the first part of the column name (e.g., "CDCSD", "CSD", "MUN_NAME") should be used as the name of the resulting columns.
# YEAR: The second part of the column name (e.g., "2011", "2016", "2021") will be stored in a new column named "YEAR".
# names_sep = "_": This specifies that the underscore character ("_") is used to separate the components in the original column names.

TMF_CSD <- TMF %>% 
  select(POSTALCODE, CDCSD_2021, CSD_2021, MUNNAME_2021 = MUN_NAME_2021, 
        CDCSD_2016, CSD_2016, MUNNAME_2016 = MUN_NAME_2016,
        CDCSD_2011, CSD_2011, MUNNAME_2011 = MUN_NAME_2011) %>%
  pivot_longer(
    cols = starts_with("CDCSD_") | starts_with("CSD_") | starts_with("MUNNAME_"),
    names_to = c(".value", "YEAR"),
    names_sep = "_"
  ) %>% 
  count(CDCSD, CSD, MUNNAME, YEAR) %>%
  rename(COUNT_POSTAL_CODE = n)
  

# To transform TMF_CSD data frame to include a continuous YEAR column ranging from 2000 to 2024, and to fill in missing values appropriately, you can follow these steps in R:
#   
#   Create a Complete Data Frame with All Year Combinations:
#   
#   Generate a sequence of years from 2000 to 2024.
# Use expand.grid() to create all combinations of POSTALCODE, CDCSD, CSD, MUNNAME, and YEAR.
# Merge with the Original Data:
#   
#   Perform a left join between the complete data frame and your original TMF_CSD data to align existing data with the complete set.
# Fill Missing Values:
#   
#   Use the fill() function from the tidyr package to propagate non-missing values backward within each group.

# Step 1: Create a complete data frame with all combinations
years <- 2000:2024

complete_data <- TMF_CSD %>%
  distinct( CDCSD, CSD, MUNNAME) %>%
  expand_grid(YEAR = years) %>% 
  mutate(YEAR = as.character(YEAR))

# Step 2: Left join with the original data
merged_data <- complete_data %>%
  left_join(TMF_CSD, by = c( "CDCSD", "CSD", "MUNNAME", "YEAR"))

# Step 3: Fill missing COUNT_POSTAL_CODE values backward within each group
final_data <- merged_data %>%
  group_by( CDCSD, CSD, MUNNAME) %>%
  fill(COUNT_POSTAL_CODE, .direction = "up") %>%
  fill(COUNT_POSTAL_CODE, .direction = "down") %>%
  ungroup() 
  

# View the final data
print(final_data)

#################################################################################

# Join the population estimates with the TMF_CSD file
municipality_population_CSD_df <- municipality_population_df %>% 
  select(Region, Region.Name, Region.Type,Year,Type,Gender, Total ) %>%
  filter(Type == "Estimate",
         Gender == "T",
         Year>=2000) %>% 
  mutate(YEAR = as.character(Year),
         CDCSD = str_pad(Region, 5, side = "left", pad = "0")) %>%
  left_join(
    final_data,
    # should use CSD code to join
    by = c("CDCSD" = "CDCSD", "YEAR" = "YEAR")  # Region.Name is not standardized, for example, Langley, District Municipality; Langley, City of
  ) %>% 
  select(CDCSD , REGION_ID = Region,  REGION_NAME = Region.Name, MUN_NAME = MUNNAME, YEAR,   POPULATION = Total)

# save to out folder
municipality_population_CSD_df  %>%
  # janitor::clean_names(case = "") %>%
  write_csv("out/BC_municipality_CSD_population_estimate.csv")

# create a dictionary of the data types
#################################################################################################
# Data dictionary for the BC municipality population estimates
#################################################################################################



# 2. Create a dictionary 
library(datadictionary)
municipality_population_CSD_labels = c(
        'CDCSD' = 'Census Subdivision Code',
        'REGION_ID'= 'Region ID',
        'REGION_NAME'= 'Region Name',
        'MUN_NAME'= 'Census Subdivision Name',
        'YEAR' = 'Year',
        'POPULATION'= 'Total Population Estimate'
    )


municipality_population_CSD_dict <- create_dictionary(municipality_population_CSD_df ,
                                                            id_var = c("CDCSD", "YEAR"),
                                                            var_labels = municipality_population_CSD_labels)
# 
file_path <- use_network_path("data/Output/municipality_population_CSD_dict.csv")
write.csv(municipality_population_CSD_dict, file = file_path)
file_path <- "out/municipality_population_CSD_dict.csv"
write.csv(municipality_population_CSD_dict, file = file_path)
