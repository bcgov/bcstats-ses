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


#Setting paths
bc_stats_project_lan <- config$file_path$bc_stats_project
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

db_population_df %>%
  count(Type)

db_population_df %>%
  count(Sex)

age_list = db_population_df %>%
  count(Age)

db_population_df %>%
  filter(Sex == "Total") %>%
  filter(Age == "TOTAL") %>%
  filter(Type == "Census Subdivision" | Type == "Dissemination Area") %>%
  glimpse()

db_population_df %>%
  filter(Sex == "Total") %>%
  filter(Age == "TOTAL") %>%
  filter(Type == "Census Subdivision" | Type == "Dissemination Area") %>%
  select(-Sex, -Age) %>%
  write_csv("out/csd_da_db_population_estimate_from_jonathan.csv")
