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
source("./src/utils.R") # Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()

#Setting paths
bc_ses_project_lan_path <- config$lan_path

bc_ses_exports <- file.path(
  bc_ses_project_lan_path,
  "2024 SES Index/exports/2025-07-31-initial-index-results"
)
# get the trend of total index
longitudinal_total_index <- read_csv(file.path(
  bc_ses_exports,
  "longitudinal-total-index-csd-contribution-from-factors-2025-07-21_masked.csv"
))
# create a trend with error bars for total index
longitudinal_total_index %>%
  mutate(
    # date = as.Date(CALENDAR_YEAR),
    year = as.character(CALENDAR_YEAR),
    # month = month(CALENDAR_YEAR, label = TRUE, abbr = FALSE)
    index = as.numeric(TOTAL_INDEX_0_100)
  ) %>%
  group_by(year) %>%
  summarise(
    mean_index = mean(index, na.rm = TRUE),
    sd_index = sd(index, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    se_index = sd_index / sqrt(n),
    lower_bound = mean_index - se_index,
    upper_bound = mean_index + se_index
  ) %>%
  ggplot(aes(x = year, y = mean_index)) +
  geom_line(group = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.2) +
  labs(
    title = "Trend of Total Index with Error Bars",
    x = "Year",
    y = "Mean Total Index"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# create a map to show the distribution of total index in 2021
library(sf)
library(tmap)
# read in the shapefile for CSDs
csd_sf <- st_read(file.path(
  bc_ses_project_lan_path,
  "2021 Census Boundary Files - 2023 Release/csd_2021.shp"
))  
