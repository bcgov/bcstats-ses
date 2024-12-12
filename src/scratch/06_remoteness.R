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

# grab the file from "Statistics Canada", only run once.
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/ldb_000a21a_e.zip",
#               destfile=use_network_path("data/Wildfires_DB/Input/Blocks_2021/ldb_000a21a_e.zip"))
# download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip",
#               destfile=use_network_path("data/raw_data/Wildfire/Wildfires_DB/Input/Blocks_2021/lda_000a21a_e.zip"))
# unzip(use_network_path("data/Wildfires_DB/Input/Blocks_2021/ldb_000a21a_e.zip"))
## READ DISSEMINATION BLOCKS
file_path <- use_network_path("data/raw_data/Wildfire/Wildfires_DB/Input/Blocks_2021/ldb_000b21a_e.shp")

blocks <- st_read(file_path) %>%
  filter(PRUID == '59') 

# blocks_area =  blocks %>%
#   # st_transform(, crs = st_crs(BC_wildfire_perimeter_historic)) %>%
#   mutate(DB_AREA = st_area(.)) 
# 
# # DB_filtered <- blocks %>%
# #   select(c(DBUID, DB_AREA)) %>%
# #   st_drop_geometry()

file_path <- use_network_path("data/raw_data/Wildfire/Wildfires_DB/Input/Blocks_2021/ldb_000b21a_e.shp")

das <- st_read(file_path) %>%
  filter(PRUID == '59') 



