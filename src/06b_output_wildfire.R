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

# TODO [REFACTORING]: This script has two completely different approaches (lines 1-493 and 495-716).
# TODO: Consider splitting into separate scripts:
# TODO:   - 06_output_wildfire_approach1.R (BC Data Catalogue API approach)
# TODO:   - 06_output_wildfire_approach2.R (Direct file + SQL approach)
# TODO: This will improve maintainability and reduce cognitive load.

####################################################################
# This script reads wild fire geodata and calculate the events in each DA.
# if wrong character set
# convert character set
# iconv -f ISO-8859-1 -t UTF-8 2021_92-151_X.csv > 2021_92-151_X_iconv.csv
###################################################################

## Set library
pacman::p_load(
  cancensus,
  geojsonsf,
  tidyverse,
  config,
  bcmaps,
  bcdata,
  janitor,
  cansim,
  safepaths,
  arrow,
  duckdb,
  sf,
  jsonlite,
  lwgeom,
  testthat,
  units,
  writexl,
  DBI,
  odbc,
  readr,
  glue
)


## Turn off spherical geometry
sf::sf_use_s2(FALSE)


#################################################################################
# second version from Brett's new approach
#################################################################################

options(scipen = 999)

# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()

# Shared helpers (connect_db, load_year_config; ADR-0009)
source("R/config.R")
source("R/db.R")

# Year-sensitive refresh parameters (GCS snapshot) — tracked in config_year.yml
# load_year_config() also validates the GCS table against refresh_year (ADR-0005).
year_config <- load_year_config()

# see https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical
# Load wildfire, DA spatial and cencus income data

lan_path <- config::get("lan_path")
wildfire_data_path <- config::get("wildfire_data_path")

wildfires <- sf::st_read(
  file.path(
    lan_path,
    wildfire_data_path,
    "Wildfires_v2/PROT_HISTORICAL_FIRE_POLYS_SP.geojson"
  )
) # or GeoJSON, etc.
wildfires |>
  filter(FIRE_YEAR >= 2000) |>
  glimpse()

das <- sf::st_read(
  file.path(
    lan_path,
    file.path(year_config$project_folder, year_config$da_boundary_shp)
  )
)
da_to_csd <- read_csv(
  file.path(lan_path, wildfire_data_path, "Wildfires_v2/DAtoCSD.csv"),
  col_types = cols(.default = "c")
)
csd <- read_csv(
  file.path(lan_path, wildfire_data_path, "Wildfires_v2/CSD.csv"),
  col_types = cols(.default = "c")
)

# Establish database connection using shared helper (normalizes the
# config$database / config$data_server drift; ADR-0009)
con <- connect_db(config)

income <- dbGetQuery(
  con,
  statement = sprintf("
with geo as
(
SELECT distinct
      [MUN_NAME_2021]
      ,[CDCSD_2021]
      ,[CMACA_2021]
      ,[DA_2021]
      ,concat('59',[CD_2021],[DA_2021]) as ALT_DA
  FROM [Population_Labour_Social].[%s].[%s]
  ),
  income as
  (
  SELECT [ALT_GEO_CODE],
  sum(case when CHARACTERISTIC_ID = '115' then [C1_COUNT_TOTAL] end) as med_income_at,
  sum(case when CHARACTERISTIC_ID = '1' then [C1_COUNT_TOTAL] end) as popltn,
  sum(case when CHARACTERISTIC_ID = '6' then [C1_COUNT_TOTAL] end) as popltn_sqr_km
  FROM [Population_Labour_Social].[Prod].[FCT_CENSUS_2021_BC_DA]
  where CHARACTERISTIC_ID in ('1','6', '115')
  and [C1_COUNT_TOTAL] is not null
  and [ALT_GEO_CODE]=[GEO_NAME]
  group by [ALT_GEO_CODE]
)
select [MUN_NAME_2021]
      ,[CDCSD_2021]
      ,[CMACA_2021]
      ,[DA_2021],
      [ALT_GEO_CODE],
      med_income_at,
      popltn,
      popltn_sqr_km,
      popltn*popltn_sqr_km as sqr_km,
      popltn*popltn_sqr_km*1000 as sqr_m
from income left join geo on [ALT_GEO_CODE]=ALT_DA
order by [ALT_GEO_CODE]
", year_config$gcs$schema, year_config$gcs$table)
)

HECTARES_TO_SQM <- 1000000
#normalize data to sq meters in DA file
das <- das %>%
  mutate(land_area_sqm = LANDAREA * HECTARES_TO_SQM) %>%
  filter(land_area_sqm > 0) %>%
  mutate(da_area_m2 = as.numeric(st_area(.)))
# two areas are close but not exactly the same

# Ensure both layers use the same CRS
wildfires <- st_transform(wildfires, st_crs(das))

# check if there are any duplication in wildfire data
wildfires %>%
  st_drop_geometry() %>%
  group_by(FIRE_YEAR, FIRE_NUMBER) %>%
  filter(n() > 1)
# A tibble: 6,240 × 19
# Groups:   FIRE_YEAR, FIRE_NUMBER [2,419]
# if there are duplications, but with different geometries, or geometries are not overlapping, then it's fine.
# but if there are duplications with same geometries, then we need to remove duplicates.

# TODO [DUPLICATE CODE]: This transformation pattern is repeated from approach 1
# TODO: Consider extracting to shared utility function

# if we do not want duplication, we can aggregate first,
# otherwise, one place can be burned multiple times in one year,
# then the duplication could make the area over counted.

# Intersect wildfire polygons with DAs
intersection <- st_intersection(das, wildfires)
intersection |>
  st_drop_geometry() |>
  count(FIRE_YEAR)
# 2025 is in another file on the BC Data Catalogue, need to download and merge if needed:

# Calculate area of intersection and total DA area
intersection_concise <- intersection %>%
  mutate(intersect_area_m2 = as.numeric(st_area(.))) %>%
  mutate(pcnt_fire = intersect_area_m2 / land_area_sqm) %>%
  mutate(pcnt_fire_2 = intersect_area_m2 / da_area_m2) %>%
  select(
    DAUID,
    FIRE_YEAR,
    FIRE_CAUSE,
    intersect_area_m2,
    da_area_m2,
    pcnt_fire_2
  ) %>%
  arrange(desc(pcnt_fire_2))

#--------test distinct DAUID by year---------------------
YEAR_RANGE <- year_config$wildfire$test_year_range[1]:year_config$wildfire$test_year_range[2]
intersection_concise %>%
  filter(substr(DAUID, 1, 2) == "59", FIRE_YEAR %in% YEAR_RANGE) %>%
  group_by(FIRE_YEAR) %>%
  summarise(
    distinct_strings = n_distinct(DAUID)
  )

intersection_concise |> count(FIRE_YEAR)
YEAR_RANGE <- year_config$wildfire$summary_year_range[1]:year_config$wildfire$summary_year_range[2]
#--------final report by year---------------------
intersection_byyear <- intersection_concise %>%
  filter(
    FIRE_YEAR %in% YEAR_RANGE
  ) %>% #filter(FIRE_YEAR %in% c('2025', '2024', '2023', '2022', '2021'))
  group_by(FIRE_YEAR, DAUID, da_area_m2) %>%
  summarise(
    total_intersect_area_m2 = sum(intersect_area_m2, na.rm = TRUE),
    total_pcnt_fire_2 = sum(pcnt_fire_2, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(
    FIRE_YEAR,
    DAUID,
    total_intersect_area_m2,
    da_area_m2,
    total_pcnt_fire_2
  )

# interesting, no total_pcnt_fire_2 over 1 a lot, which means in wildfire gemoetry data, no geometry overlapping.

intersection_byyear %>%
  filter(total_pcnt_fire_2 > 1)
# slightly over 1, could be from calculation precision issue since two areas are the same.

#link for CSD
fire_DA_CSD <- da_to_csd %>%
  left_join(intersection_byyear, by = c("DAuid" = "DAUID")) %>%
  mutate(
    total_pcnt_fire_2 = ifelse(is.na(total_pcnt_fire_2), 0, total_pcnt_fire_2),
    CDCSD = paste0(PRuid, CDcode, CSDcode)
  )


fire_DA_CSDNAME <- fire_DA_CSD %>%
  left_join(csd, by = c("CDCSD" = "CSDuid"))

fire_DA_CSDNAME_final <- fire_DA_CSDNAME %>%
  filter(substr(DAuid, 1, 2) == "59", !is.na(FIRE_YEAR)) %>%
  select(
    FIRE_YEAR,
    DAuid,
    total_intersect_area_m2,
    da_area_m2,
    total_pcnt_fire_2,
    DApop_2021,
    CSDdguid,
    CSDname,
    CSDtype
  )

# check if there are duplications in the final data
fire_DA_CSDNAME_final %>%
  group_by(FIRE_YEAR, DAuid) %>%
  filter(n() > 1)


#final set
write_csv(
  fire_DA_CSDNAME_final,
  file.path(
    lan_path,
    wildfire_data_path,
    'Wildfires_v2/BC_WILDFIRE_2011_2025.csv'
  )
)


#-------------------------------------------------------------------
#income, fire linking
income_DA_fire <- income %>%
  left_join(fire_DA_CSDNAME_final, by = c("ALT_GEO_CODE" = "DAuid")) %>%
  mutate(
    total_pcnt_fire_2 = ifelse(is.na(total_pcnt_fire_2), 0, total_pcnt_fire_2),
    med_income_at = ifelse(is.na(med_income_at), 0, med_income_at),
    total_intersect_area_m2 = ifelse(
      is.na(total_intersect_area_m2),
      0,
      total_intersect_area_m2
    )
  ) %>%
  select(
    MUN_NAME_2021,
    CDCSD_2021,
    ALT_GEO_CODE,
    med_income_at,
    total_intersect_area_m2,
    da_area_m2,
    total_pcnt_fire_2
  )


#test correlation between income and wildfire
income_fire_ols <- lm(
  med_income_at ~ total_pcnt_fire_2 + total_intersect_area_m2,
  data = income_DA_fire
)
summary(income_fire_ols)


#next steps
# get sqr m from census for each DA to compare
# roll up to CSD
# compare with CSD level SEI

# aggregate to CSD level if needed
fire_CSDNAME_final <- fire_DA_CSDNAME_final %>%
  group_by(FIRE_YEAR, CSDdguid, CSDname, CSDtype) %>%
  summarise(
    total_intersect_area_m2 = sum(total_intersect_area_m2, na.rm = TRUE),
    total_da_area_m2 = sum(da_area_m2, na.rm = TRUE),
    total_pcnt_fire_2 = mean(total_pcnt_fire_2, na.rm = TRUE),
    total_DApop_2021 = sum(as.numeric(DApop_2021), na.rm = TRUE)
  ) %>%
  ungroup() |>
  mutate(
    CSDID = substr(CSDdguid, 10, nchar(CSDdguid))
  )


#final set
write_csv(
  fire_CSDNAME_final,
  file.path(
    lan_path,
    wildfire_data_path,
    'Wildfires_v2/BC_CSD_WILDFIRE_2011_2025.csv'
  )
)
