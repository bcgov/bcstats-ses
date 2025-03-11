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
###################################################################

# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
pacman::p_load(cancensus, geojsonsf, tidyverse, config, bcmaps, bcdata, janitor, cansim, safepaths, arrow, duckdb, cancensus)
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
options(timeout=600)

###########################################################################################
# DA shp file
# since geodata team added DA id in the dataset, we can skip this step now. Prevously, we have to calculate the DA id using st_join function.
###########################################################################################

# grab the file from "Statistics Canada", only run once.
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip",
#               destfile=use_network_path("2024 SES Index/data/raw_data/remoteness/lda_000a21a_e.zip"))
# unzip(use_network_path("2024 SES Index/data/raw_data/remoteness/lda_000a21a_e.zip"),
#       exdir = use_network_path("2024 SES Index/data/raw_data/remoteness/lda_000a21a_e"))
# ## READ DISSEMINATION BLOCKS
file_path <- use_network_path("2024 SES Index/data/raw_data/remoteness/lda_000a21a_e/lda_000b21a_e.shp")
# 
# # Load the dissemination area shapefile
da_shapefile <- st_read(file_path)
da_shapefile <- da_shapefile %>%
    filter(PRUID == '59') %>%
    st_transform(crs=3005)
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
# ## READ DISSEMINATION BLOCKS
file_path <- use_network_path("2024 SES Index/data/raw_data/remoteness/lcsd000b21a_e/lcsd000b21a_e.shp")
# Load the dissemination area shapefile
csd_shapefile <- st_read(file_path)


csd_shapefile <- csd_shapefile %>%
  filter(PRUID == '59') %>%
  st_transform(crs=3005)

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
new_da_servicebc_file_path_2 = use_network_path("2024 SES Index/data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_DA_nearest_servicebc_20250212_150617.zik")
new_da_servicebc_df2 <-    read_csv(new_da_servicebc_file_path_2)
new_da_servicebc_df2 %>% glimpse()
# Rows: 614,459

new_da_servicebc_df2 %>% 
  filter(tag == 'nan')
# 2,715 ??
# new data for hospital 
new_da_hospital_file_path = use_network_path("2024 SES Index/data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_DA_nearest_hospitals_20250219_100047.zik")

new_da_hospital_df <-    read_csv(new_da_hospital_file_path)
new_da_hospital_df %>% glimpse()
# Rows: 614,459

# new data for school 
new_da_school_file_path = use_network_path("2024 SES Index/data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_DA_nearest_schools_20250222_180651.zik")

new_da_school_df <-    read_csv(new_da_school_file_path)
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
replacement_file_path = use_network_path("2024 SES Index/data/raw_data/remoteness/proximity_analysis_route_salvaging")
replacement_files <- list.files(replacement_file_path, full.names = T, recursive =T) # fs::dir_ls
replacement_df <-    map_df(replacement_files, read_csv)
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
  filter(TAG!='nan') %>% # remove the rows without coordinates for facilities
  mutate(ADDRESS_ALBERS_X = SITE_ALBERS_X,
         ADDRESS_ALBERS_Y = SITE_ALBERS_Y) %>% 
  st_as_sf(coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)
# 1841998 features and 13 fields

# What is the nan in TAG?
address_sf_with_da %>% 
  count(TAG)

address_sf_with_da <- address_sf_with_da %>% 
  mutate(TAG_2 = case_when(
    TAG == "servicebc_v2" ~ "servicebc",
    TAG == "hospitals_v2" ~ "hospitals",
    TAG == "schools_v2" ~ "schools",
    TRUE ~ TAG
  ))

# Save the data to csv file
# This file will be used in the next step for calculating the drive time and distance to the nearest facility
# for each address.
address_sf_with_da %>% 
  st_drop_geometry() %>% 
  write_csv(use_network_path("2024 SES Index/data/raw_data/remoteness/bc_address_with_da.csv"))


###########################################################################################
#   create a DA level summary. 
###########################################################################################

# Create a DA level summary table: average drive time and distance, and number of address. 



avg_dist_drvtime_by_da_service <- address_sf_with_da %>% 
  st_drop_geometry() %>% 
  group_by(DAID , TAG_2) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(DRV_TIME_SEC ),
    AVG_DRV_DIST = mean(DRV_DIST ),
    N_ADDRESS = n_distinct(FID)
  )

avg_dist_drvtime_by_da_service %>% 
  filter(is.na(AVG_DRV_DIST)) %>%
  # count(DAID) %>%
  glimpse()
# No row missing distance value, 

avg_dist_drvtime_by_da_service %>% 
  write_csv(use_network_path("2024 SES Index/data/output/remoteness/avg_dist_drvtime_by_da_facility.csv"))




###########################################################################################
#  To get how many DAs are missing. And create a CSD level summary. 
# This is for econ team, and not for index project.
###########################################################################################

# Get a DA2021 list from TMF.
TMF_file  <-  use_network_path("2024 SES Index/data/raw_data/TMF/GCS_202406.csv")

TMF <- read_csv(TMF_file)

# standardize the DA number, append the prefix BC code 59, so it is easy to join to other tables.
TMF <- TMF %>% 
  mutate(DA_NUM = as.numeric(str_c("59", CD_2021, DA_2021, sep = "")))

# TMF_names = TMF %>% names() %>% paste(collapse = ",")

# clean the names, one name is not upper-cased. We prefer all uppercase
TMF <- 
  TMF %>% 
  janitor::clean_names(case = "screaming_snake" )

CSD_DA_list <-  TMF %>% 
  filter(ACTIVE =="Y") %>% 
  mutate(CDUID = str_sub(DA_NUM, 1, 4),
         CSDUID = str_c(CDUID, CSD_2021)) %>%
  group_by(CDUID, CSDUID, MUN_NAME_2021, CSD_2021, DA_2021, DA_NUM) %>% 
  summarise(
    N_POSTALCODE = n()
  ) %>% 
  ungroup()


# Save to a new file this one has all the information but without the geometry column.
CSD_DA_address_dist_drvtime <- CSD_DA_list %>% 
  inner_join(address_sf_with_da, by = join_by("DA_NUM" == "DAID") )
# should inner join, otherwise some CSDs have missed distance values.



# check if there is duplication
# CSD_DA_list %>%
#   count(DA_NUM) %>%
#   filter(n>1)
# no it is fine.

# Left join table together



# a CSD level summary table: average drive time and distance, and number of address.
CSD_REMOTENESS_BY_FACILITY = CSD_DA_address_dist_drvtime %>% 
  group_by(CDUID, CSDUID,MUN_NAME_2021,TAG_2) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(DRV_TIME_SEC, na.rm = T),
    AVG_DRV_DIST = mean(DRV_DIST, na.rm = T),
    N_ADDRESS =  n_distinct(FID),
    N_DA =  n_distinct(DA_NUM)
  ) %>% 
  ungroup()

CSD_REMOTENESS_BY_FACILITY %>% 
  filter(is.na(AVG_DRV_DIST)) %>% glimpse()
  count(CSDUID) %>%
  glimpse()
# No CSDs are missing at least one distance value.

CSD_REMOTENESS_BY_FACILITY %>% 
write_csv("./out/CSD_REMOTENESS_BY_FACILITY.csv")

CSD_REMOTENESS_BY_FACILITY %>% 
  write_csv(use_network_path("2024 SES Index/data/output/remoteness/CSD_REMOTENESS_BY_FACILITY.csv"))



###########################################################################################
# Validation:
###########################################################################################
#   
#   Plot the results to visually validate that points are correctly joined to their respective dissemination areas:

# create a sf object for the facility which will be used for plotting with the geometry column.
facility_sf = address_sf_with_da %>% 
  st_drop_geometry() %>%
  filter(!is.na(COORD_X)) %>%  # remove the rows without coordinates for facilities
  left_join(CSD_DA_list, by = join_by("DAID" == "DA_NUM") ) %>%
  count(TAG_2,CDUID, CSDUID,MUN_NAME_2021, CSD_2021 , NEAREST_FACILITY, COORD_X ,COORD_Y) %>% 
  mutate(FACILITY_ALBERS_X = COORD_X,
         FACILITY_ALBERS_Y = COORD_Y) %>%
  st_as_sf(coords = c("COORD_X", "COORD_Y"), crs = 3005)

# da level average distance with da sf object, append the average distance and drive time to the DA shapefile
address_avg_dist_da_shapefile <- da_shapefile %>% 
  left_join(
  avg_dist_drvtime_by_da_service %>% mutate(DAUID = as.character(DAID)),
  join_by("DAUID")
) 
# 22500 features and 9 fields

# CSD information plus da level average distance with da sf object, append the average distance and drive time to the DA shapefile
CSD_DA_avg_dist_drvtime_by_service_sf <- address_avg_dist_da_shapefile %>% 
  left_join(
            CSD_DA_list,
            by = join_by("DAID" == "DA_NUM")
  )
# 22500 features and 15 fields


# There are some DAs which still missing any addresses/red points.

address_avg_dist_da_shapefile %>% 
  filter(is.na(AVG_DRV_DIST)) %>%
  # count(TAG_2)
  glimpse()

# 522 DAs are missing. The TAG is NA, so they are not joined to any addresses.
# TAG_2   n                       geometry
# 1  <NA> 522 MULTIPOLYGON (((603198.1 93...
# 522 rows are missing at least one distance measure in geodata team's file, but in the da_shapefile. 

# add title to the ggplot object
address_avg_dist_da_servicebc_p <- ggplot(data = address_avg_dist_da_shapefile %>% filter(TAG_2 == "servicebc")) +
  geom_sf(
    aes(fill = AVG_DRV_DIST),
    color = "gray"
  ) +
  scale_fill_viridis_c(option = "viridis") +
  geom_sf(
    data = address_sf_with_da %>% filter(TAG_2 == "servicebc"),
    color = "red",
    size = 1,
    fill = "red",
    alpha = 0.5
  ) +
  theme_minimal() +
  labs(title = "Families Addresses Distance to ServiceBC within Dissemination Areas")

print(address_avg_dist_da_servicebc_p)
# Save the plot
ggsave( "./out/address_servicebc_dist_with_da_map.png", address_avg_dist_da_servicebc_p, width = 10, height = 8, dpi = 300)
# the same addresses of families are plotted on the map, and the dissemination areas are shown in light blue.
# The red points represent the addresses of families that have been successfully joined to their respective dissemination areas.

###########################################################################################
# Validation: color map
###########################################################################################


my_map_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

################################################################
# Create a color map plot using ggplot2 to visualize the average distance to the nearest facility by dissemination area.
################################################################


csd_avg_address_dist_sf = csd_shapefile %>% 
  left_join( CSD_REMOTENESS_BY_SERVICE ,
             join_by(CSDUID ) )
  
csd_avg_address_dist_sf_servicebc_p <- ggplot(data = csd_avg_address_dist_sf %>% filter(TAG_2 == 'servicebc')
       ) +
  geom_sf(aes(fill = AVG_DRV_DIST), color = "gray") +
  scale_fill_viridis_c(option = "viridis" ) +
  labs(
    title = "Avg. Distance to the Facility by Region",
    subtitle = "Remoteness measurement in BC",
    x = "Longitude",
    y = "Latitude"
  ) +
  my_map_theme+
  guides(fill=guide_legend(title="Avg. Distance to the Facilities"))

print(csd_avg_address_dist_sf_servicebc_p)

# Save the plot
ggsave("./out/csd_avg_address_dist_servicebc_sf.png", width = 10, height = 8, dpi = 300)
ggsave(use_network_path("2024 SES Index/data/output/remoteness/csd_avg_address_dist_servicebc_sf.png"), width = 10, height = 8, dpi = 300)


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
  facility_sf ,
  label_var1 = F ,
  label_var2 = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Smithers",
  facility = "servicebc",
  fill_var = "AVG_DRV_TIME_SEC",
  fill_var_name = "Average drive time in second",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf ,
  label_var1 = F ,
  label_var2 = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Smithers",
  facility = "hospitals",
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average distance",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf ,
  label_var1 = F ,
  label_var2 = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Smithers",
  facility = "schools",
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average distance",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf ,
  label_var1 = F ,
  label_var2 = T
)


compare_two_csd_in_map(
  CSD1 = "Vancouver",
  CSD2 = "Fernie",
  facility = "servicebc",
  fill_var = "AVG_DRV_DIST",
  fill_var_name = "Average distance",
  csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
  facility_sf ,
  label_var1 = F ,
  label_var2 = T
)




