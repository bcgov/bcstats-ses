# Copyright 2024 Province of British Columbia
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

####################################################################
# This script reads distance and drive time data from home address to nearest facilities(Service BC office, school, and hospital) data from geo-location service team in BC Data Service.
# Geodata team has updated the data with DA id and randomly sampled addresses within DA strata in 2025-02-11.
# We need to keep the coordinates columns when we drop the geometry column, or we need to add the coordinates columns back after we drop the geometry column from original data.
# Geodata team has added the DA id in the dataset, so we can skip the step of calculating the DA id using st_join function.
# We will create a DA level summary table: average drive time and distance, and number of address.
# We will create a CSD level summary table: average drive time and distance, and number of address.
# Geodata fixed those addresses that does not have valid coordinates or are not connected to the road network.
# Exclude those addresses from the analysis and resample the addresses.
# 751 CSDs and 7848 DAs in the DA shape file from statsan
# but only 420 CSDUIDs and 6967 DAs in the CSD_DA_list/TMF file from bcstats
# 7326 DAs in distance dataset from geodata team
# solution:
# skip the GCS TMF file since it does not have the right full list of CSD id and DA id.
# Get the Dissemination Geographies Relationship File from statscan, and join the distance dataset to the DA shape file.
# StatsCan’s 2021 Census uses a hierarchical geographic framework. At one level, census subdivisions (CSDs) represent municipalities and similar administrative areas, while at a lower level, dissemination areas (DAs) are created by aggregating dissemination blocks into contiguous areas (typically with 400–700 people) that usually nest within CSD boundaries.

# To help users link these different levels, Statistics Canada provides a lookup table—the Dissemination Geographies Relationship File—which uses unique identifiers (DGUIDs) to connect DAs to higher geographic units such as CSDs, census tracts, and beyond. This file (along with related correspondence files) lets analysts cross-reference and integrate data across the geographic hierarchy.
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21
###################################################################

# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
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
  cancensus
)
library(jsonlite)
library(cowplot) # For aligning multiple plots
library(patchwork)
# Load the rlang package for the bang-bang operator
library(rlang)
source("./src/utils.R") # get the functions for plotting maps
###################################################################
# # boundary files
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
# https://www12.statcan.gc.ca/census-recensement/alternative_alternatif.cfm?l=eng&dispext=zip&teng=lfsa000b21a_e.zip&k=%20%20%20158240&loc=//www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lfsa000b21a_e.zip
# # geographic attribute file
# https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/index-eng.cfm

###################################################################

# To retrieve an option
getOption('timeout')
# [1] 60
# To set an option
options(timeout = 600)

###########################################################################################
# get the Dissemination Geographies Relationship File from statscan
###########################################################################################
geouid_url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/files-fichiers/2021_98260004.zip"

download.file(geouid_url, destfile = "./out/2021_98260004.zip")

unzip("./out/2021_98260004.zip", exdir = "./out/2021_98260004")

daid_file_path = "./out/2021_98260004/2021_98260004.csv"

# read csv all columns as strings instead of numbers
daid_df = read_csv(daid_file_path, col_types = cols(.default = "c"))
daid_df %>% glimpse()
bc_daid_df <- daid_df %>%
  filter(PRDGUID_PRIDUGD == '2021A000259') # BC

bc_daid_df %>%
  count(PRDGUID_PRIDUGD, CDDGUID_DRIDUGD, CSDDGUID_SDRIDUGD) %>%
  glimpse()
# 751 CSDs in BC
bc_csd_daid_df <- bc_daid_df %>%
  count(PRDGUID_PRIDUGD, CDDGUID_DRIDUGD, CSDDGUID_SDRIDUGD, DADGUID_ADIDUGD)
# 7848 DAs in BC
###########################################################################################
# https://www.statcan.gc.ca/en/subjects/standard/sgc/2021/index
# Standard Geographical Classification (SGC) 2021 - Volume I, The Classification
# The standard classification version of the SGC 2021 provides standard names and codes for the geographical regions of Canada, provinces and territories, census divisions (counties, regional county municipalities) and census subdivisions (municipalities).
###########################################################################################

# get the csd file from statscan
SGC_2021_url = "https://www.statcan.gc.ca/en/statistical-programs/document/sgc-cgt-2021-structure-eng.csv"

download.file(SGC_2021_url, destfile = "./out/sgc-cgt-2021-structure-eng.csv")


sgc_2021_df = read_csv(SGC_2021_url, col_types = cols(.default = "c"))
bc_sgc_2021_df = sgc_2021_df %>%
  filter(str_sub(Code, 1, 2) == "59")
bc_csd_sgc_2021_df <- bc_sgc_2021_df %>%
  filter(`Hierarchical structure` %in% c("Census subdivision"))
# 751 CSDs in BC

bc_sgc_2021_df %>%
  filter(`Hierarchical structure` %in% c("Census division")) %>%
  write_csv(("./out/bc_cdid_name_2021.csv"))

bc_csdid_name_daid_2021_df <- bc_csd_daid_df %>%
  select(CDDGUID_DRIDUGD, CSDDGUID_SDRIDUGD, DADGUID_ADIDUGD) %>%
  mutate(
    CDID = str_sub(CDDGUID_DRIDUGD, 10, 13),
    CSDID = str_sub(CSDDGUID_SDRIDUGD, 10, 16),
    DAID = str_sub(DADGUID_ADIDUGD, 10, 17)
  ) %>%
  select(-CDDGUID_DRIDUGD, -CSDDGUID_SDRIDUGD, -DADGUID_ADIDUGD) %>%
  left_join(
    bc_csd_sgc_2021_df %>%
      select(CSDID = Code, MUN_NAME_2021 = `Class title`),
    by = join_by(CSDID)
  )

bc_csdid_name_daid_2021_df %>%
  write_csv(("./out/bc_csdid_name_daid_2021.csv"))
# write_csv(use_network_path("2024 SES Index/data/raw_data/remoteness/bc_csdid_name_daid_2021.csv"))

###########################################################################################
# DA shp file
# since geodata team added DA id in the dataset, we can skip this step now. Previously, we have to calculate the DA id using st_join function.
###########################################################################################

# grab the file from "Statistics Canada", only run once.
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip",
#               destfile=use_network_path("2024 SES Index/data/raw_data/remoteness/lda_000a21a_e.zip"))
# unzip(use_network_path("2024 SES Index/data/raw_data/remoteness/lda_000a21a_e.zip"),
#       exdir = use_network_path("2024 SES Index/data/raw_data/remoteness/lda_000a21a_e"))
# ## READ DISSEMINATION BLOCKS
file_path <- use_network_path(
  "2024 SES Index/data/raw_data/remoteness/lda_000a21a_e/lda_000b21a_e.shp"
)
#
# # Load the dissemination area shapefile
da_shapefile <- st_read(file_path)
da_shapefile <- da_shapefile %>%
  filter(PRUID == '59') %>%
  st_transform(crs = 3005)
###########################################################################################
# CSD shp file
# https://www12.statcan.gc.ca/census-recensement/alternative_alternatif.cfm?l=eng&dispext=zip&teng=lcsd000b21a_e.zip&k=%20%20%20152326&loc=//www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip
###########################################################################################

# grab the file from "Statistics Canada", only run once.
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip",
#               # destfile=use_network_path("2024 SES Index/data/raw_data/remoteness/lcsd000b21a_e.zip")lcsd000b21a_e
#               destfile= "./out/lcsd000b21a_e.zip"
# )
# unzip(use_network_path("2024 SES Index/data/raw_data/remoteness/lcsd000b21a_e.zip"),
#       exdir = use_network_path("2024 SES Index/data/raw_data/remoteness/"))
file_path <- use_network_path(
  "2024 SES Index/data/raw_data/remoteness/lcsd000b21a_e/lcsd000b21a_e.shp"
)
# Load the dissemination area shapefile
csd_shapefile <- st_read(file_path)


csd_shapefile <- csd_shapefile %>%
  filter(PRUID == '59') %>%
  st_transform(crs = 3005)

csd_shapefile %>%
  st_drop_geometry() %>%
  count(CSDUID) %>%
  glimpse()
# 751 CSD
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
# The following datasets are updated and much better than the previous one since they have the right address and adddress id and DA id.
# geodata team added another batch and fixed the invalid address.
# new_da_servicebc_file_path_1 = use_network_path("2024 SES Index/data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_DA_nearest_servicebc_20250207_140020_JOINED_2.zip")
# new_da_servicebc_df1 <-    read_csv(new_da_servicebc_file_path_1)
# new_da_servicebc_df1 %>% glimpse() #Rows: 615,570
# new_da_servicebc_df1 %>%
#   filter(FULL_ADDRESS == "5783 Toby St Edgewater BC")

# geodata team creates another batch for service bc with address id.
# we are going to use this one.
# zik files are just zip files, so read_csv can handle them directly.
new_da_servicebc_file_path_2 = use_network_path(
  "2024 SES Index/data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_DA_nearest_servicebc_20250212_150617.zik"
)
new_da_servicebc_df2 <- read_csv(new_da_servicebc_file_path_2)
new_da_servicebc_df2 %>% glimpse()
# Rows: 614,459

new_da_servicebc_df2 %>%
  filter(tag == 'nan')
# 2,715 ??
# new data for hospital
new_da_hospital_file_path = use_network_path(
  "2024 SES Index/data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_DA_nearest_hospitals_20250219_100047.zik"
)

new_da_hospital_df <- read_csv(new_da_hospital_file_path)
new_da_hospital_df %>% glimpse()
# Rows: 614,459

# new data for school
new_da_school_file_path = use_network_path(
  "2024 SES Index/data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_DA_nearest_schools_20250222_180651.zik"
)

new_da_school_df <- read_csv(new_da_school_file_path)
new_da_school_df %>% glimpse()
# Rows: 614,459
# all have the same numbers of rows, so we can join them together.

# check duplication

new_da_school_df %>%
  # filter(tag == "servicebc") %>%
  select(FULL_ADDRESS) %>%
  count(FULL_ADDRESS) %>%
  filter(n > 1) %>%
  glimpse()

# Previously, there were 2000 invalid rows in each dataset, no coordinates, no DA id, and no address id.
# geodata team fixed them.
# replacement of those addresses that missed coordinates or are not connected to road network
replacement_file_path = use_network_path(
  "2024 SES Index/data/raw_data/remoteness/proximity_analysis_route_salvaging"
)
replacement_files <- list.files(
  replacement_file_path,
  full.names = T,
  recursive = T
) # fs::dir_ls
replacement_df <- map_df(replacement_files, read_csv)
replacement_df %>% glimpse()
# 8,021 rows

# servicbc/hospital/schools duplicates
# "1090A Main St Valemount BC"
# According to getodata team, we can check it using https://geocoder.api.gov.bc.ca/addresses.json?addressString=1090A%20Main%20St%20Valemount%20BC&outputSRS=3005
# and only keep the id 112681, exclude the other one 112684.

new_da_school_df %>%
  filter(FULL_ADDRESS == "1090A Main St Valemount BC")

address_sf_with_da <- new_da_servicebc_df2 %>%
  bind_rows(new_da_hospital_df) %>%
  bind_rows(new_da_school_df) %>%
  bind_rows(replacement_df) %>%
  janitor::clean_names(case = "screaming_snake") %>%
  filter(!FID == 112684) %>% # remove duplicated rows
  filter(TAG != 'nan') %>% # remove the rows without coordinates for facilities
  mutate(ADDRESS_ALBERS_X = SITE_ALBERS_X, ADDRESS_ALBERS_Y = SITE_ALBERS_Y) %>%
  st_as_sf(coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)
# 1841998 features and 13 fields

address_sf_with_da <- address_sf_with_da %>%
  mutate(
    TAG_2 = str_remove_all(TAG, "_v2")
  )


# Save the data to csv file
# This file will be used in the next step for calculating the drive time and distance to the nearest facility
# for each address.
address_sf_with_da %>%
  st_drop_geometry() %>%
  write_csv(use_network_path(
    "2024 SES Index/data/raw_data/remoteness/bc_address_with_da.csv"
  ))


###########################################################################################
#   create a DA level summary.
###########################################################################################

# Create a DA level summary table: average drive time and distance, and number of address.

avg_dist_drvtime_by_da_service <- address_sf_with_da %>%
  st_drop_geometry() %>%
  group_by(DAID, TAG_2) %>%
  summarise(
    AVG_DRV_TIME_SEC = mean(DRV_TIME_SEC),
    AVG_DRV_DIST = mean(DRV_DIST),
    N_ADDRESS = n_distinct(FID)
  )

avg_dist_drvtime_by_da_service %>%
  filter(is.na(AVG_DRV_DIST)) %>%
  # count(DAID) %>%
  glimpse()
# No row missing distance value since all the addresses and DA information are from geodata team by sampling. Not full picture.

avg_dist_drvtime_by_da_service %>%
  write_csv(use_network_path(
    "2024 SES Index/data/output/remoteness/avg_dist_drvtime_by_da_facility.csv"
  ))


###########################################################################################
#  To get how many DAs are missing. And create a CSD level summary.
# This is for econ team, and not for index project.
# skip the GCS TMF file since it does not have the right full list of CSD id and DA id.
###########################################################################################
#
# # Save to a new file this one has all the information but without the geometry column.
# bc_csdid_name_DAID_2021_df %>%
#   count(CSDID) %>% glimpse()
# 751 CSDs
CSD_DA_address_dist_drvtime <- bc_csdid_name_daid_2021_df %>%
  left_join(
    address_sf_with_da %>% mutate(DAID = as.character(DAID)),
    by = join_by("DAID" == "DAID")
  )
# should not inner join, otherwise some CSDs will be dropped.
# 1,842,520 × 17
# since this is left join by DA, some DAs in SGC file are not in the distance dataset.
# 7326 DAs in distance dataset from geodata team
# but 7848 DAs in the DA shape file from statscan
# 7848 - 7326 = 522 DAs are missing in the distance dataset.
# corresondingly, there are CSDs missing in the distance dataset.
# 751 CSDs in BC from statscan

# CSD_DA_address_dist_drvtime %>%
#   count(CSDID) %>% glimpse()

CSD_DA_address_dist_drvtime %>%
  filter(is.na(FID)) %>%
  # count(CSDID) %>%
  glimpse()
# 522 DAs are missing in the distance dataset, but in the DA file from statscan.

# why TAG_2 is NA?
# # a CSD level summary table: average drive time and distance, and number of address.
CSD_REMOTENESS_BY_FACILITY = CSD_DA_address_dist_drvtime %>%
  group_by(CSDID, MUN_NAME_2021, TAG_2) %>%
  summarise(
    AVG_DRV_TIME_SEC = mean(DRV_TIME_SEC, na.rm = T), # data from geodata team
    AVG_DRV_DIST = mean(DRV_DIST, na.rm = T), # data from geodata team
    N_ADDRESS = n_distinct(FID, na.rm = TRUE), # data from statscan
    N_DA = n_distinct(DAID, na.rm = TRUE) # data from statscan
  ) %>%
  ungroup()

# 1,761 × 7

# TAG_2 is NA, just mean that some DAs in that CSD do not have any address in the distance dataset.

# CSD_REMOTENESS_BY_FACILITY %>%
#   count(CSDID) %>% glimpse()
# 751 CSDs
# we should have 751 CSDs, but we have only 467 CSDs have all three facilities.

CSD_REMOTENESS_BY_FACILITY %>%
  # filter(!is.na(AVG_DRV_DIST)) %>%
  count(CSDID) %>%
  filter(n >= 3)
# 467 CSDs have all three facilities,
# 751-467 = 284 CSDs missing.

CSD_REMOTENESS_BY_FACILITY %>%
  count(CSDID) %>%
  filter(n < 3) %>%
  arrange(desc(n)) %>%
  glimpse()
# 284 CSDs do not have any address in the distance dataset.

CSD_REMOTENESS_BY_FACILITY %>%
  count(CSDID) %>%
  filter(n < 3) %>%
  left_join(
    CSD_REMOTENESS_BY_FACILITY
  ) %>%
  glimpse()
# Those CSDs do not have any address in the distance dataset, so they do not have any distance measure.
# Those CSDs are relatively small, only have a few DAs, so they are not in the distance dataset.
# one sample

CSD_REMOTENESS_BY_FACILITY %>%
  filter(CSDID == "5901805") %>%
  glimpse()
# 5901805 is a small CSD, only have 1 DAs, so it is not in the distance dataset.

CSD_REMOTENESS_BY_FACILITY %>%
  write_csv("./out/CSD_REMOTENESS_BY_FACILITY.csv")

CSD_REMOTENESS_BY_FACILITY %>%
  write_csv(use_network_path(
    "2024 SES Index/data/output/remoteness/CSD_REMOTENESS_BY_FACILITY.csv"
  ))


###########################################################################################
# Validation:
###########################################################################################
#
#   Plot the results to visually validate that points are correctly joined to their respective dissemination areas:

# create a sf object for the facility which will be used for plotting with the geometry column.
facility_sf = address_sf_with_da %>%
  st_drop_geometry() %>%
  mutate(DAID = as.character(DAID)) %>%
  # filter(!is.na(COORD_X)) %>% # remove the rows without coordinates for facilities
  left_join(bc_csdid_name_daid_2021_df, by = join_by("DAID" == "DAID")) %>%
  count(
    TAG_2,
    CSDID,
    MUN_NAME_2021,
    NEAREST_FACILITY,
    COORD_X,
    COORD_Y
  ) %>%
  mutate(FACILITY_ALBERS_X = COORD_X, FACILITY_ALBERS_Y = COORD_Y) %>%
  st_as_sf(coords = c("COORD_X", "COORD_Y"), crs = 3005)
facility_sf %>% glimpse()
# 4226 facilities include servicebc, hospitals, and schools

# 7848 unique DAs in the DA shape file
# 7326 unique DAs are joined to shape file from the distance dataset.

# da level average distance with da sf object, append the average distance and drive time to the DA polygon shapefile

CSD_DA_avg_dist_drvtime_by_service_sf <- da_shapefile %>%
  select(-DGUID, -PRUID) %>%
  left_join(
    bc_csdid_name_daid_2021_df,
    by = join_by("DAUID" == "DAID")
  ) %>%
  left_join(
    avg_dist_drvtime_by_da_service %>% mutate(DAID = as.character(DAID)),
    join_by("DAUID" == "DAID")
  )
# 7848 - 7326 = 522 DAs are missing in the distance dataset.
# CSD information plus da level average distance with da sf object, append the average distance and drive time to the DA shapefile
# so there are some DAs which still missing any addresses/red points.

CSD_DA_avg_dist_drvtime_by_service_sf %>%
  filter(is.na(AVG_DRV_DIST)) %>%
  # count(TAG_2)
  glimpse()
#
# 522 DAs in DA shape file are missing distance measure. The TAG is NA, so they are not joined to any DAs in distance dataset
# TAG_2   n                       geometry
# 1  <NA> 522 MULTIPOLYGON (((603198.1 93...
# 522 rows are missing at least one distance measure in geodata team's file, but in the da_shapefile.

# add title to the ggplot object

plot_bc_address_map(
  data = CSD_DA_avg_dist_drvtime_by_service_sf,
  address_sf = address_sf_with_da,
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average Drive Distance",
  facility_name = "servicebc",
  save_png = T
)

# the same addresses of families are plotted on the map, and the dissemination areas are shown in light blue.
# The red points represent the addresses of families that have been successfully joined to their respective dissemination areas.

plot_bc_address_map(
  data = CSD_DA_avg_dist_drvtime_by_service_sf,
  address_sf = address_sf_with_da,
  fill_var = "AVG_DRV_TIME_SEC",
  fill_var_name = "Average Drive Time (seconds)",
  facility_name = "hospitals",
  save_png = T
)

plot_bc_address_map(
  data = CSD_DA_avg_dist_drvtime_by_service_sf,
  address_sf = address_sf_with_da,
  fill_var = "AVG_DRV_TIME_SEC",
  fill_var_name = "Average Drive Time (seconds)",
  facility_name = "schools",
  save_png = T
)

################################################################
# Create a color map plot using ggplot2 to visualize the average distance to the nearest facility by csd.
################################################################

csd_avg_address_dist_sf = csd_shapefile %>%
  left_join(CSD_REMOTENESS_BY_FACILITY, join_by("CSDUID" == "CSDID"))

# the color in the CSD show the level of the remoteness
plot_csd_avg_map_fn(
  csd_avg_address_dist_sf,
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average Drive Distance",
  facility_name = "servicebc",
  save_png = T
)

plot_csd_avg_map_fn(
  csd_avg_address_dist_sf,
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average Drive Distance",
  facility_name = "schools",
  save_png = T
)

plot_csd_avg_map_fn(
  csd_avg_address_dist_sf,
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average Drive Distance",
  facility_name = "hospitals",
  save_png = T
)


################################################################
# interesting these results weren't what I was as expecting for the least remote.
# assuming longest drive time = most remote, then Smithers is less remote than Vancouver by this definition.
# we can plot them in the map
################################################################

################################################################

compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Smithers",
  facility = "servicebc",
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average distance",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf,
  label_var1 = F,
  label_var2 = T,
  save_png = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Smithers",
  facility = "servicebc",
  fill_var = "AVG_DRV_TIME_SEC",
  fill_var_name = "Average drive time in second",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf,
  label_var1 = F,
  label_var2 = T,
  save_png = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Smithers",
  facility = "hospitals",
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average distance",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf,
  label_var1 = F,
  label_var2 = T,
  save_png = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Smithers",
  facility = "schools",
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average distance",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf,
  label_var1 = F,
  label_var2 = T,
  save_png = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Fernie",
  facility = "servicebc",
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average distance",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf,
  label_var1 = F,
  label_var2 = T,
  save_png = T
)
