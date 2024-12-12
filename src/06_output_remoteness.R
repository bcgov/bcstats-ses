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
# 
# # geographic attribute file
# https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/index-eng.cfm
###################################################################




# To retrieve an option

getOption('timeout')
# [1] 60

# To set an option

options(timeout=600)



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
    filter(PRUID == '59')
  # glimpse()

# Load the address CSV file
address_dist_data <- read.csv(use_network_path("data/raw_data/remoteness/format_1.csv"))

address_data <- address_dist_data %>% 
  count(FULL_ADDRESS,	SITE_ALBERS_X,	SITE_ALBERS_Y, coord_x,	coord_y)

# Convert to an sf object (BC Albers projection: EPSG 3005)
address_sf <- st_as_sf(address_data, coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)


# da_shapefile <- st_read(use_network_path("data/raw_data/Wildfire/Wildfires_DB/Input/lda_000a21a_e/lda_000b21a_e.shp"))


# Check CRS
st_crs(address_sf)
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
if (st_crs(address_sf) != st_crs(da_shapefile)) {
  # address_sf <- st_transform(address_sf, st_crs(da_shapefile))
  da_shapefile <- st_transform(da_shapefile, st_crs(address_sf))
}

# Efficiency:
#   
#   For large datasets, consider spatial indexing using st_join(..., largest = TRUE) to speed up operations.

# Spatial join: append dissemination area IDs to address points
address_with_da <- st_join(address_sf, da_shapefile, left = TRUE)


# View the resulting dataset
print(address_with_da)

# Save to a new file (optional)
st_write(address_with_da, use_network_path("data/raw_data/remoteness/address_with_dis_area.geojson"))

address_with_da %>% 
  st_drop_geometry() %>% 
  inner_join(address_dist_data) %>% 
  write_csv(use_network_path("data/raw_data/remoteness/address_with_dis_area.csv"))

# Validation:
#   
#   Plot the results to visually validate that points are correctly joined to their respective dissemination areas:

library(ggplot2)
ggplot() +
  geom_sf(data = da_shapefile, fill = "lightblue", color = "gray") +
  geom_sf(data = address_sf, color = "red", size = 2) +
  theme_minimal()


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

