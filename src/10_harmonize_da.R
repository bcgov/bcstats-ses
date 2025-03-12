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

# This script is used to harmonize the estimated census variables for each DAs.
# The data is downloaded in CSV format and saved to the out folder
# TODO:
# 1. find the way to replicate StatsCan's approach
# 2. use the 'tongfen' R package to get the better results
# TongFen creates a common geography to make the census data comparable through time on a common geography based on DA level data.
# An alternative method to make data comparable is to use the tongfen_estimate method to generate (dasymetric) area-weighted interpolations of census data on a common geography, but that is likely to introduce systematic biases as density is a common confounding factor of DA geographies.

# Load the required libraries
pacman::p_load(tidyverse, readxl, safepaths, bcdata)
library(dplyr)
library(tidyr)
library(janitor)
library(datadictionary)

library(dplyr)
library(ggplot2)
library(tidyr)
library(cancensus)
library(sf)
library(tongfen)
# cancensus::set_api_key("<your cancensus API key>")
Sys.setenv(
  "tongfen.cache_path" = here::here("data", "census_cache")
)

get_harmonized_census_data <- function(
  variables,
  variable_label,
  level = "DA"
) {
  meta <- meta_for_ca_census_vectors(variables)
  region <- list(PR = "59")
  census_data <- get_tongfen_ca_census(
    regions = region,
    meta = meta,
    level = level,
    quiet = TRUE,
    base_geo = "CA21"
  )

  census_data %>%
    st_drop_geometry() %>%
    write_csv(
      glue::glue("out/census_{variable_label}.csv")
    )
  return(census_data)
}


##############################################################################
# question: find the population change in DAs.

region <- list(PR = "59")

# variables <-
#   c(
#     "Population_2001" = "v_CA01_1",
#     "Population_2006" = "v_CA06_1",
#     "Population_2011" = "v_CA11N_1",
#     "Population_2016" = "v_CA16_1",
#     "Population_2016" = "v_CA21_1"
#   )

# meta <- meta_for_ca_census_vectors(variables)

meta <- meta_for_additive_variables(
  c("CA01", "CA06", "CA11", "CA16"),
  "Population"
)

population_data <- get_tongfen_ca_census(
  regions = region,
  meta = meta,
  level = "DA",
  quiet = TRUE,
  base_geo = "CA11"
)


# population density

# median/average house hold/family income

# lone parents ratio
lone_parent_variables <- c(
  'FAMILIES_TOT_21' = 'v_CA21_499', # Total number of census families in private households
  'LONE_PARENT_TOT_21' = 'v_CA21_507', # Lone Parent - Total
  'FAMILIES_TOT_16' = 'v_CA16_484', # Number of families
  'LONE_PARENT_TOT_16' = 'v_CA16_488', # Lone Parent - Total
  'FAMILIES_TOT_11' = 'v_CA11F_115', # Total number of census families in private households
  'LONE_PARENT_TOT_11' = 'v_CA11F_129', # Lone Parent - Total
  'FAMILIES_TOT_06' = 'v_CA06_55', # Lone Parent - Total
  'LONE_PARENT_TOT_06' = 'v_CA06_69', # Lone Parent - Total
  'FAMILIES_TOT_01' = 'v_CA01_53', #
  'LONE_PARENT_TOT_01' = 'v_CA01_67' # Lone Parent - Total
)


meta <- meta_for_ca_census_vectors(lone_parent_variables)


lone_parent__data <- get_tongfen_ca_census(
  regions = region,
  meta = meta,
  level = "DA",
  quiet = TRUE,
  base_geo = "CA21"
)


lone_parent_census_data <- get_harmonized_census_data(
  lone_parent_variables,
  variable_label = "lone_parent_census_data"
)

# proportion of employed in a management occupation
occu_variables <- c(
  'OCC_TOT_21' = 'v_CA21_6561', # Occupation - Total
  'OCC_MGMT_21' = 'v_CA21_6570', # Occupation - Management
  'OCC_TOT_16' = 'v_CA16_5654', # Occupation - Total
  'OCC_MGMT_16' = 'v_CA16_5663', # Occupation - Management
  'OCC_TOT_11' = 'v_CA11N_2026', # Occupation - Total
  'OCC_MGMT_11' = 'v_CA11N_2035', # Occupation - Management
  'OCC_TOT_06' = 'v_CA06_827', # Occupation - Total
  'OCC_MGMT_06' = 'v_CA06_830', # Occupation - Management
  'OCC_TOT_01' = 'v_CA01_989', # Occupation - Total
  'OCC_MGMT_01' = 'v_CA01_990', # Occupation - Management
)


occu_census_data <- get_harmonized_census_data(
  occu_variables,
  variable_label = "occu_census_data"
)

# labor market participation rate
lmpr_variables <- c(
  'IN_LAB_FORCE_TOT_2021' = 'v_CA21_6492', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES_2021' = 'v_CA21_6495', # Population aged 15 years and over by Labour force status - in labour force
  'LABOUR_PART_RT_2016' = 'v_CA16_5612', # Labour Force - Participation Rate
  'LABOUR_PART_RT_2011' = 'v_CA11N_2002', # Labour Force - Participation Rate
  'LABOUR_PART_RT' = 'v_CA06_580', # Labour Force - Participation Rate
)

lmpr_census_data <- get_harmonized_census_data(
  lmpr_variables,
  variable_label = "lmpr_census_data"
)


# labor market participation rate
lmpr_variables <- c(
  'LABOUR_EMPL_RT' = 'v_CA16_5615', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT' = 'v_CA16_5618', # Labour Force - Unemployment Rate

  'LABOUR_EMPL_RT' = 'v_CA11N_2005', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT' = 'v_CA11N_2008', # Labour Force - Unemployment Rate

  'LABOUR_EMPL_RT' = 'v_CA06_581', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT' = 'v_CA06_582', # Labour Force - Unemployment Rate
)
##############################################################################
# question: find the children age between 0-14 over time.
# Percentage point change in share of children aged 0-14 between 2011 and 2016
# City of Toronto change in number of children under 15 between 2001 to 2021
vsb_regions <- list(
  CSD = c("5915022", "5915803"),
  CT = c("9330069.01", "9330069.02", "9330069.00")
)
variables <- c(
  "2016_0-14" = "v_CA16_4",
  "2011_0-4" = "v_CA11F_8",
  "2011_5-9" = "v_CA11F_11",
  "2011_10-14" = "v_CA11F_14"
)
meta <- meta_for_ca_census_vectors(variables) %>%
  bind_rows(meta_for_additive_variables(
    c("CA11", "CA11", "CA16"),
    "Population"
  ))

children_data <- get_tongfen_ca_census(
  regions = vsb_regions,
  meta = meta,
  level = "DA",
  quiet = TRUE
) %>%
  mutate(
    `2011_0-14` = purrr::reduce(
      select(sf::st_set_geometry(., NULL), starts_with("2011_")),
      `+`
    )
  ) %>%
  mutate(change = `2016_0-14` / Population_CA16 - `2011_0-14` / Population_CA11)


children_data_harmonized <- children_data %>%
  filter(str_length(TongfenUID) > 39)

children_data %>%
  st_drop_geometry() %>%
  write_csv(
    "out/children_data_harmonized_.csv"
  )

ggplot(children_data, aes(fill = change)) +
  geom_sf(size = 0.1) +
  scale_fill_gradient2(labels = scales::percent) +
  coord_sf(datum = NA) +
  labs(
    title = "Percentage point change in share of children aged 0-14 between 2011 and 2016",
    fill = NULL
  )


###################################################################################
# DA level census variables across year

rent_variables <- c(
  rent_2001 = "v_CA01_1667",
  rent_2016 = "v_CA16_4901",
  rent_2011 = "v_CA11N_2292",
  rent_2006 = "v_CA06_2050"
)
meta <- meta_for_ca_census_vectors(rent_variables)

regions = list(PR = "59")
rent_data <- get_tongfen_ca_census(
  regions = regions,
  meta = meta,
  quiet = TRUE,
  method = "estimate",
  level = "CT",
  base_geo = "CA16"
)


periods <- c("2001-2006", "2006-2011", "2011-2016", "2001-2016")
plot_data <- rent_data %>%
  mutate(
    `2001-2006` = rent_2006 / rent_2001 - 1,
    `2006-2011` = rent_2011 / rent_2006 - 1,
    `2011-2016` = rent_2016 / rent_2011 - 1,
    `2001-2016` = rent_2016 / rent_2001 - 1
  ) %>%
  pivot_longer(
    cols = all_of(periods),
    names_to = "Period",
    values_to = "Change"
  ) %>%
  mutate(Period = factor(Period, level = periods)) %>%
  st_sf()
ggplot(plot_data, aes(fill = Change)) +
  geom_sf(size = 0.1) +
  scale_fill_gradient2(labels = scales::percent) +
  coord_sf(datum = NA, xlim = c(-123.3, -122.5), ylim = c(49, 49.42)) +
  facet_wrap("Period", ncol = 2) +
  labs(title = "Change in average gross rent")

###################################################################################
# CSD or CT level
# low income cutoff level total and share
library(tongfen)
meta <- meta_for_ca_census_vectors(c(
  total_2006 = "v_CA06_1982",
  lico_share_2006 = "v_CA06_1984",
  lico_2016 = "v_CA16_2561",
  lico_share_2016 = "v_CA16_2576"
))

lico_data <- get_tongfen_ca_census(
  regions = list(CSD = "5915022"),
  meta,
  level = "CT"
) |>
  mutate(lico_2006 = total_2006 * lico_share_2006 / 100) |>
  mutate(
    `Absolute change` = lico_2016 - lico_2006,
    `Percentage point change` = lico_share_2016 - lico_share_2006
  )


ggplot(lico_data, aes(fill = `Absolute change`)) +
  geom_sf() +
  scale_fill_gradient2() +
  coord_sf(datum = NA) +
  labs(
    title = "Change in number of children under 6 in low income",
    caption = "StatCan Census 2006, 2016"
  )


ggplot(lico_data, aes(fill = `Percentage point change` / 100)) +
  geom_sf() +
  scale_fill_gradient2(labels = scales::percent) +
  coord_sf(datum = NA) +
  labs(
    title = "Change in share of children under 6 in low income",
    fill = "Percentage\npoint change",
    caption = "StatCan Census 2006, 2016"
  )


#################################################################################
meta <- c(
  bachelor_2006 = "v_CA06_1256",
  base_2006 = "v_CA06_1248",
  bachelor_2011 = "v_CA11N_1822",
  base_2011 = "v_CA11N_1801",
  bachelor_2016 = "v_CA16_5123",
  base_2016 = "v_CA16_5096"
) %>%
  meta_for_ca_census_vectors()

education_data <- get_tongfen_ca_census(regions = list(CMA = "59933"), meta) %>%
  mutate(
    share_2006 = bachelor_2006 / base_2006,
    share_2011 = bachelor_2011 / base_2011,
    share_2016 = bachelor_2016 / base_2016
  ) %>%
  mutate(
    `2006-2011` = share_2011 - share_2006,
    `2011-2016` = share_2016 - share_2011,
    `2006-2016` = share_2016 - share_2006
  )

education_data %>%
  pivot_longer(starts_with("20")) %>%
  st_sf() %>%
  ggplot() +
  geom_sf(aes(fill = value), size = 0.1) +
  geom_water(size = 0.1) +
  geom_roads() +
  scale_fill_gradient2(
    labels = function(d) scales::percent(d, suffix = "pp"),
    guide = guide_colourbar(barwidth = 15)
  ) +
  facet_wrap("name") +
  theme(legend.position = "bottom") +
  coord_bbox(metro_van_bbox("tight")) +
  labs(
    title = "Percentage point change in share of 25-64 year olds with bachelor degree or higher",
    fill = "Percentage point\nchange",
    caption = "MountainMath, StatCan Census 2006-2016"
  )
###################################################################################
# To CSD CT summary: aggregate_data
vectors <- c("v_CA16_4836", "v_CA16_4838", "v_CA16_4899")
meta = meta_for_ca_census_vectors(vectors) %>%
  bind_rows(meta_for_additive_variables(
    "CA16",
    c("Population", "Dwellings", "Households")
  ))
vsb_regions <- list(
  CSD = c("5915022", "5915803"),
  CT = c("9330069.01", "9330069.02", "9330069.00")
)
vsb <- get_census(
  "CA16",
  regions = vsb_regions,
  vectors = meta$variable,
  labels = "short"
)
vsb <- aggregate_data_with_meta(vsb, meta) %>%
  mutate(
    Total = v_CA16_4836,
    Renters = v_CA16_4838,
    rent_poor = v_CA16_4899 / 100
  ) %>%
  mutate(rent_share = Renters / Total)

#################################################################################
# Aggregate population from DA level to grouped by CT_UID
# if (FALSE) {
#   geo <- cancensus::get_census(
#     "CA06",
#     regions = list(CSD = "5915022"),
#     level = 'DA'
#   )
#   meta <- meta_for_additive_variables("CA06", "Population")
#   result <- aggregate_data_with_meta(geo %>% group_by(CT_UID), meta)
# }

##############################################################################
geo <- cancensus::get_census(
  "CA06",
  regions = list(PR = "59"),
  level = 'DA'
)
meta <- meta_for_additive_variables("CA06", "Population")
result <- aggregate_data_with_meta(geo %>% group_by(CSD_UID), meta)


# 2021
# 'FAMILIES_TOT' = 'v_CA21_499', # Total number of census families in private households
# 'LONE_PARENT_TOT' = 'v_CA21_507',  # Lone Parent - Total

# Estimate a common geography for 2006 and 2016 dissemination areas in the City of Vancouver
# based on the geographic data.
if (FALSE) {
  regions <- list(CSD = "5915022")

  data_06 <- cancensus::get_census(
    "CA06",
    regions = regions,
    geo_format = 'sf',
    level = "DA"
  ) %>%
    rename(GeoUID_06 = GeoUID)
  data_16 <- cancensus::get_census(
    "CA16",
    regions = regions,
    geo_format = "sf",
    level = "DA"
  ) %>%
    rename(GeoUID_16 = GeoUID)

  correspondence <- estimate_tongfen_correspondence(
    list(data_06, data_16),
    c("GeoUID_06", "GeoUID_16")
  )
}


bc_da_correspondence_01_06_11_16 <- get_tongfen_correspondence_ca_census(
  geo_datasets = c('CA01', 'CA06', 'CA11', 'CA16'),
  regions = list(PR = "59"),
  level = 'DA'
)


bc_da_correspondence_01_06_11_16_21 <- get_tongfen_correspondence_ca_census(
  geo_datasets = c('CA01', 'CA06', 'CA11', 'CA16', 'CA21'),
  regions = list(PR = "59"),
  level = 'DA'
)


bc_da_correspondence_16_21 %>% write_csv("out/bc_da_correspondence_16_21.csv")

bc_da_correspondence_11_16_21 %>%
  write_csv("out/bc_da_correspondence_11_16_21.csv")

bc_da_correspondence_06_11_16_21 %>%
  write_csv("out/bc_da_correspondence_06_11_16_21.csv")

bc_da_correspondence_01_06_11_16 %>%
  write_csv("out/bc_da_correspondence_01_06_11_16.csv")


bc_da_correspondence <- get_tongfen_correspondence_ca_census(
  geo_datasets = c('CA01', 'CA06', 'CA11', 'CA16', 'CA21'),
  regions = list(PR = "59"),
  level = 'DA'
)

bc_da_correspondence_01_06_11_16_21 %>%
  write_csv("out/bc_da_correspondence_01_06_11_16_21.csv")
