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
# This data is link to the shape file for Dessimination Block/Dessimination Area to get a DB/DA id for the home, so we can calculate the DB/DA level average distance and drive time. 
###################################################################



## Set library
pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb, cancensus)

library(rgdal)
library(sf)
library(jsonlite)
library(lwgeom )



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
###########################################################################################

# grab the file from "Statistics Canada", only run once.
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip",
#               destfile=use_network_path("data/raw_data/remoteness/lda_000a21a_e.zip"))
# unzip(use_network_path("data/raw_data/remoteness/lda_000a21a_e.zip"),
#       exdir = use_network_path("data/raw_data/remoteness/lda_000a21a_e"))
# ## READ DISSEMINATION BLOCKS
file_path <- use_network_path("data/raw_data/remoteness/lda_000a21a_e/lda_000b21a_e.shp")
# Load the dissemination area shapefile
da_shapefile <- st_read(file_path)


da_shapefile <- da_shapefile %>% 
    filter(PRUID == '59') %>% 
    st_transform(crs=3005)
  # glimpse()


###########################################################################################
# FSA shp file
###########################################################################################

# grab the file from "Statistics Canada", only run once.
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lfsa000b21a_e.zip",
#               destfile=use_network_path("data/raw_data/remoteness/lda_000a21a_e.zip")
#               # destfile= "./out/lfsa000b21a_e.zip"
#               )
# unzip(use_network_path("data/raw_data/remoteness/lfsa000b21a_e.zip"),
#       exdir = use_network_path("data/raw_data/remoteness/"))
# ## READ DISSEMINATION BLOCKS
file_path <- use_network_path("data/raw_data/remoteness/lfsa000b21a_e/lfsa000b21a_e.shp")
# Load the dissemination area shapefile
fsa_shapefile <- st_read(file_path)


fsa_shapefile <- fsa_shapefile %>% 
  filter(PRUID == '59') %>% 
  st_transform(crs=3005)
# glimpse()


###########################################################################################
# CSD shp file
# https://www12.statcan.gc.ca/census-recensement/alternative_alternatif.cfm?l=eng&dispext=zip&teng=lcsd000b21a_e.zip&k=%20%20%20152326&loc=//www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip
###########################################################################################

# grab the file from "Statistics Canada", only run once.
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip",
#               # destfile=use_network_path("data/raw_data/remoteness/lcsd000b21a_e.zip")lcsd000b21a_e
#               destfile= "./out/lcsd000b21a_e.zip"
# )
# unzip(use_network_path("data/raw_data/remoteness/lcsd000b21a_e.zip"),
#       exdir = use_network_path("data/raw_data/remoteness/"))
# ## READ DISSEMINATION BLOCKS
file_path <- use_network_path("data/raw_data/remoteness/lcsd000b21a_e/lcsd000b21a_e.shp")
# Load the dissemination area shapefile
csd_shapefile <- st_read(file_path)


csd_shapefile <- csd_shapefile %>% 
  filter(PRUID == '59') %>% 
  st_transform(crs=3005)
# glimpse()

###########################################################################################
# Service BC and Hospitals
###########################################################################################

servicebc_file_path = use_network_path("data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_nearest_servicebc_20241212_101844/30_percent_site_Hybrid_geocoder_nearest_servicebc_20241212_101844.csv")

hospital_file_path = use_network_path("data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_nearest_hospitals_20241218_081633/30_percent_site_Hybrid_geocoder_nearest_hospitals_20241218_081633.csv")


address_data_path_list = c(
  servicebc_file_path,
  hospital_file_path
)

address_dist_data <- address_data_path_list %>%  
  read_csv() %>% 
  bind_rows()

# Convert to an sf object (BC Albers projection: EPSG 3005)
address_dist_sf <- st_as_sf(address_dist_data, coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)


###########################################################################################
# Service BC
###########################################################################################
# Load the address CSV file
# updated the Geocoder address list to allow us to choose between BC Albers or WGS84 (lat/long) for the result file.
address_dist_servicebc_data <- read_csv(use_network_path("data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_nearest_servicebc_20241212_101844/30_percent_site_Hybrid_geocoder_nearest_servicebc_20241212_101844.csv"))

# address_servicebc_data <- address_dist_servicebc_data %>% 
#   count(FULL_ADDRESS,	SITE_ALBERS_X,	SITE_ALBERS_Y, coord_x,	coord_y)

# Convert to an sf object (BC Albers projection: EPSG 3005)
address_servicebc_sf <- st_as_sf(address_dist_servicebc_data, coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)


# da_shapefile <- st_read(use_network_path("data/raw_data/Wildfire/Wildfires_DB/Input/lda_000a21a_e/lda_000b21a_e.shp"))


# Check CRS
st_crs(address_servicebc_sf)
st_crs(da_shapefile)

# Why Use a Projected CRS?
#   Accuracy:
#   
#   In geographic CRS (e.g., EPSG:4326), distances and areas are distorted because they are based on spherical coordinates (degrees).
# Projected CRSs, like BC Albers (EPSG:3005), preserve distance and area measurements accurately for specific regions.
# Alignment with Dissemination Areas:
#   
#   Dissemination area shapefiles from Statistics Canada are typically in NAD83 / Statistics Canada Lambert (EPSG:3347). Reprojecting both datasets to a local projected CRS (e.g., BC Albers) ensures better alignment and reduces potential mismatches at boundaries.
# Spatial Join Precision:
#   
#   Operations like st_join() rely on spatial relationships (e.g., points inside polygons). Using a projected CRS improves the precision of these calculations.

# Reproject if necessary
if (st_crs(address_servicebc_sf) != st_crs(da_shapefile)) {
  # address_sf <- st_transform(address_sf, st_crs(da_shapefile))
  da_shapefile <- st_transform(da_shapefile, st_crs(address_servicebc_sf))
}

# Efficiency:
#   
#   For large datasets, consider spatial indexing using st_join(..., largest = TRUE) to speed up operations.

# Spatial join: append dissemination area IDs to address points
address_servicebc_with_da <- st_join(address_servicebc_sf, da_shapefile, left = TRUE)


# View the resulting dataset
print(address_servicebc_with_da)

# Save to a new file (optional)
st_write(address_servicebc_with_da, 
         "./out/address_servicebc_with_da.geojson"
         # use_network_path("data/raw_data/remoteness/address_servicebc_with_da.geojson")
         )

address_servicebc_with_da %>% 
  st_drop_geometry() %>% 
  # inner_join(address_dist_data) %>% 
  write_csv(use_network_path("data/raw_data/remoteness/address_servicebc_with_da.csv"))





# Create a DA level summary table: average drive time and distance, and number of address. 
avg_dis_time_servicebc_by_da <- address_servicebc_with_da %>% 
  st_drop_geometry() %>% 
  group_by(DAUID) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(drv_time_sec),
    AVG_DRV_DIST = mean(drv_dist),
    N_ADDRESS = n()
  )


###########################################################################################
# Hospital
###########################################################################################
# Load the address CSV file
# updated the Geocoder address list to allow us to choose between BC Albers or WGS84 (lat/long) for the result file.
address_dist_hospital_data <- read_csv(use_network_path("data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_nearest_hospitals_20241218_081633/30_percent_site_Hybrid_geocoder_nearest_hospitals_20241218_081633.csv"))

# address_hospital_data <- address_dist_hospital_data %>% 
#   count(FULL_ADDRESS,	SITE_ALBERS_X,	SITE_ALBERS_Y, coord_x,	coord_y)

# Convert to an sf object (BC Albers projection: EPSG 3005)
address_hospital_sf <- st_as_sf(address_dist_hospital_data, coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)


# da_shapefile <- st_read(use_network_path("data/raw_data/Wildfire/Wildfires_DB/Input/lda_000a21a_e/lda_000b21a_e.shp"))


# Check CRS
st_crs(address_hospital_sf)
st_crs(da_shapefile)

# Why Use a Projected CRS?
#   Accuracy:
#   
#   In geographic CRS (e.g., EPSG:4326), distances and areas are distorted because they are based on spherical coordinates (degrees).
# Projected CRSs, like BC Albers (EPSG:3005), preserve distance and area measurements accurately for specific regions.
# Alignment with Dissemination Areas:
#   
#   Dissemination area shapefiles from Statistics Canada are typically in NAD83 / Statistics Canada Lambert (EPSG:3347). Reprojecting both datasets to a local projected CRS (e.g., BC Albers) ensures better alignment and reduces potential mismatches at boundaries.
# Spatial Join Precision:
#   
#   Operations like st_join() rely on spatial relationships (e.g., points inside polygons). Using a projected CRS improves the precision of these calculations.

# Reproject if necessary
if (st_crs(address_hospital_sf) != st_crs(da_shapefile)) {
  # address_sf <- st_transform(address_sf, st_crs(da_shapefile))
  da_shapefile <- st_transform(da_shapefile, st_crs(address_hospital_sf))
}

# Efficiency:
#   
#   For large datasets, consider spatial indexing using st_join(..., largest = TRUE) to speed up operations.

# Spatial join: append dissemination area IDs to address points
address_hospital_with_da <- st_join(address_hospital_sf, da_shapefile, left = TRUE)


# View the resulting dataset
print(address_hospital_with_da)

# Save to a new file (optional)
st_write(address_hospital_with_da, 
         "./out/address_hospital_with_da.geojson"
         # use_network_path("data/raw_data/remoteness/address_hospital_with_da.geojson")
         )

address_hospital_with_da %>% 
  st_drop_geometry() %>% 
  # inner_join(address_dist_data) %>% 
  write_csv(use_network_path("data/raw_data/remoteness/address_hospital_with_da.csv"))



###########################################################################################
#   create a DA level summary. 
###########################################################################################

# Create a DA level summary table: average drive time and distance, and number of address. 

address_hospital_servicebc_with_da <- address_servicebc_with_da %>% 
  mutate(across(
    .cols = c(PRUID, DAUID, DGUID),
    .fns = as.character
  )) %>% 
  bind_rows(address_hospital_with_da )

avg_dis_time_hospital_servicebc_by_da <- address_hospital_servicebc_with_da %>% 
  st_drop_geometry() %>% 
  group_by(DAUID, tag) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(drv_time_sec),
    AVG_DRV_DIST = mean(drv_dist),
    N_ADDRESS = n()
  )

avg_dis_time_hospital_servicebc_by_da %>% 
  write_csv(use_network_path("data/raw_data/remoteness/avg_dis_time_hospital_servicebc_by_da.csv"))


###########################################################################################
#  To get how many DAs are missing. And create a CSD level summary. 
###########################################################################################

# Get a DA2021 list from TMF.
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

CSD_DA_list <-  TMF %>% 
  filter(ACTIVE =="Y") %>% 
  group_by(MUN_NAME_2021, CSD_2021, DA_2021, DA_NUM) %>% 
  summarise(
    N_POSTALCODE = n()
  ) %>% 
  ungroup()

# check if there is duplication
# CSD_DA_list %>% 
#   count(DA_NUM) %>% 
#   filter(n>1)
# no it is fine.

# Left join table together



CSD_DA_REMOTENESS = CSD_DA_list %>% 
  mutate(DAUID = as.character(DA_NUM)) %>% 
  left_join(avg_dis_time_hospital_servicebc_by_da, by = c( "DAUID")) %>% 
  mutate(DIST_IS_NA = if_else(is.na(AVG_DRV_DIST), 'Distance data is na',"Distance data is available"),
         ADDRESS_IS_NA = if_else(is.na(N_ADDRESS), 'Address data is na',"Address data is available")
  )

# how many missing value in DAs. 
CSD_DA_REMOTENESS_DISTANCE_AVAILABILITY = CSD_DA_REMOTENESS %>% 
  count(DIST_IS_NA)

CSD_DA_REMOTENESS_ADDRESS_AVAILABILITY = CSD_DA_REMOTENESS %>% 
  count(ADDRESS_IS_NA)


# a CSD level summary
CSD_REMOTENESS_BY_SERVICE = CSD_DA_REMOTENESS %>% 
  group_by(CSD_2021,MUN_NAME_2021,tag) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(AVG_DRV_TIME_SEC, na.rm = T),
    AVG_DRV_DIST = mean(AVG_DRV_DIST, na.rm = T),
    N_ADDRESS = sum(N_ADDRESS, na.rm = T),
    N_DA = n()
  ) %>% 
  ungroup()

CSD_REMOTENESS_BY_SERVICE %>% 
  write_csv("./out/CSD_REMOTENESS_BY_SERVICE.csv")

CSD_REMOTENESS_BY_SERVICE %>% 
  write_csv(use_network_path("data/output/remoteness/CSD_REMOTENESS_BY_SERVICE.csv"))

# a CSD level summary
CSD_REMOTENESS_ALL_SERVICE = CSD_DA_REMOTENESS %>% 
  group_by(CSD_2021,MUN_NAME_2021) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(AVG_DRV_TIME_SEC, na.rm = T),
    AVG_DRV_DIST = mean(AVG_DRV_DIST, na.rm = T),
    N_ADDRESS = sum(N_ADDRESS, na.rm = T),
    N_DA = n()
  ) %>% 
  ungroup()

CSD_REMOTENESS %>% 
  write_csv("./out/CSD_REMOTENESS.csv")

CSD_REMOTENESS %>% 
  write_csv(use_network_path("data/output/remoteness/CSD_REMOTENESS.csv"))


###########################################################################################
# Another approach for CSD level summary

# I used two different way to calculate the CSV average. First, I calculate the DA average, and then use TMF to join DA to CSD, and get the CSD average. I think this approach applies different weight on the address. (1/n)*(1/m), n is the nubmer address in DA, m is the number of DA  in CSD. 
# 
# So I downloaded the CSD boundary file and directly calculated the CSD average after join CSD boundary to Brian's address data. 
#  
###########################################################################################

# Spatial join: append dissemination area IDs to address points
csd_sf_address_dist <- st_join(csd_shapefile,address_dist_df, left = TRUE)

csd_avg_address_dist_by_service = csd_sf_address_dist %>% 
  st_drop_geometry() %>%
  group_by(CSDUID,CSDNAME, tag) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(drv_time_sec, na.rm = T),
    AVG_DRV_DIST = mean(drv_dist, na.rm = T),
    N_ADDRESS = n(),
    N_NA = sum(if_else(is.na(drv_dist), 1, 0))
  ) %>% 
  ungroup()

csd_avg_address_dist_by_service %>% 
  write_csv(use_network_path("data/output/remoteness/csd_avg_address_dist_by_service.csv"))


csd_avg_address_dist = csd_sf_address_dist %>% 
  st_drop_geometry() %>%
  group_by(CSDUID,CSDNAME) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(drv_time_sec, na.rm = T),
    AVG_DRV_DIST = mean(drv_dist, na.rm = T),
    N_ADDRESS = n(),
    N_NA = sum(if_else(is.na(drv_dist), 1, 0))
  ) %>% 
  ungroup()

csd_avg_address_dist %>% 
  write_csv(use_network_path("data/output/remoteness/csd_avg_address_dist.csv"))



###########################################################################################
# Validation:
###########################################################################################
#   
#   Plot the results to visually validate that points are correctly joined to their respective dissemination areas:

library(ggplot2)
ggplot() +
  geom_sf(data = da_shapefile, fill = "lightblue", color = "gray") +
  geom_sf(data = address_servicebc_with_da, color = "red", size = 2) +
  theme_minimal()

# Save the plot
ggsave("./out/address_servicebc_with_da_map.png",  width = 10, height = 8, dpi = 300)

library(ggplot2)
ggplot() +
  geom_sf(data = da_shapefile, fill = "lightblue", color = "gray") +
  geom_sf(data = address_hospital_with_da, color = "red", size = 2) +
  theme_minimal()

# Save the plot
ggsave("./out/address_hospital_with_da.png", width = 10, height = 8, dpi = 300)


###########################################################################################
# Validation: color map
###########################################################################################


# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)

################################################################
# Create a color map plot using ggplot2
################################################################

csd_avg_address_dist_sf = csd_shapefile %>% 
  # filter(tag == "servicebc") %>% 
  left_join( csd_avg_address_dist, join_by(CSDUID, CSDNAME) )

ggplot(data = csd_avg_address_dist_sf
       ) +
  geom_sf(aes(fill = AVG_DRV_DIST), color = "white") +
  scale_fill_viridis_c(option = "viridis", name = "Avg. Distance to the Facilities") +
  labs(
    title = "Avg. Distance to the Facility by Region",
    subtitle = "Remoteness measurement in BC",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


# Save the plot
ggsave("./out/csd_avg_address_dist_sf.png", width = 10, height = 8, dpi = 300)
ggsave(use_network_path("data/output/remoteness/csd_avg_address_dist_sf.png"), width = 10, height = 8, dpi = 300)
################################################################
# servicebc
################################################################

csd_avg_address_dist_servicebc_sf = csd_shapefile %>% 
  left_join( csd_avg_address_dist_by_service  %>%  filter(tag == "servicebc")  , 
             join_by(CSDUID, CSDNAME) )

ggplot(data = csd_avg_address_dist_servicebc_sf
) +
  geom_sf(aes(fill = AVG_DRV_DIST), color = "white") +
  scale_fill_viridis_c(option = "viridis", name = "Avg. Distance to ServiceBC") +
  labs(
    title = "Avg. Distance to ServiceBC by Region",
    subtitle = "Remoteness measurement in BC",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


# Save the plot
ggsave("./out/csd_avg_address_dist_servicebc_sf.png", width = 10, height = 8, dpi = 300)
ggsave(use_network_path("data/output/remoteness/csd_avg_address_dist_servicebc_sf.png"), width = 10, height = 8, dpi = 300)
################################################################
# Hospital
################################################################

csd_avg_address_dist_hospitals_sf = csd_shapefile %>% 
  left_join( csd_avg_address_dist_by_service  %>%  filter(tag == "hospitals")  , 
             join_by(CSDUID, CSDNAME) )

ggplot(data = csd_avg_address_dist_hospitals_sf
) +
  geom_sf(aes(fill = AVG_DRV_DIST), color = "white") +
  scale_fill_viridis_c(option = "viridis", name = "Avg. Distance to Hospitals") +
  labs(
    title = "Avg. Distance to Hospitals by Region",
    subtitle = "Remoteness measurement in BC",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


# Save the plot
ggsave("./out/csd_avg_address_dist_hospitals_sf.png", width = 10, height = 8, dpi = 300)
ggsave(use_network_path("data/output/remoteness/csd_avg_address_dist_hospitals_sf.png"), width = 10, height = 8, dpi = 300)

################################################################
################################################################
# Handle Missing Matches:
#   
#   Check for NA values in the dissemination area ID column (address_with_da$DA_ID) to identify points that do not fall within any dissemination area.
# Use st_is_within_distance() with a small tolerance to match points near boundaries if required.

unmatched <- address_with_da %>% filter(is.na(DA_ID))

# 3. Handle Edge Cases (Optional)
# If there are unmatched points (NA values in the dissemination area ID column), you can inspect and handle them separately. For instance, use st_is_within_distance() to include points near boundaries:
#   r
# Find points near boundaries
near_boundary <- st_is_within_distance(address_sf, da_shapefile, dist = 1)  # Adjust distance as needed

