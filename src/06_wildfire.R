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
# This script reads stored interest rates, housing prices and income
# from the LAN. Process the data, cleaning the data, imputing 
# missing values for housing prices and using an ARIMA model
# to predict missing income values in the last years of data.
# Municipalities with too many missing values are ignored.
# Then proceeds to estimate the mortage payment as a percentage
# of income. All of this by municipality and housing type.
###################################################################

## Source local configurations
config <- config::get()
lan_path <- config$lan_path

## Set library
pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb)

library(rgdal)
library(sf)
library(jsonlite)
library(lwgeom )

## Turn off spherical geometry
sf::sf_use_s2(FALSE)

## READ HISTORIC WILDFIRES
file_path <- use_network_path("data/Wildfires_DB/Input/BC Wildfire Fire Perimeters - Historical GEOJSON/PROT_HISTORICAL_FIRE_POLYS_SP.geojson")
BC_wildfire_perimeter_historic <- sf::st_read(file_path) %>%
  filter(FIRE_YEAR>=2000)

# Check current CRS
print(st_crs(BC_wildfire_perimeter_historic))

# The best practice is to reproject your data into a projected CRS before applying st_union() to avoid distortions and ensure accurate results. Always ensure geometries are valid and use simplification if necessary for performance reasons.


## FIX CASES WITH FIRES SPLIT INTO SEVERAL PARTS (DUPLICATED FIRES)
dups <- unique(BC_wildfire_perimeter_historic$FIRE_LABEL[which(duplicated(BC_wildfire_perimeter_historic$FIRE_LABEL))])
BC_dups <- BC_wildfire_perimeter_historic[BC_wildfire_perimeter_historic$FIRE_LABEL %in% dups, ] 

# Before performing st_union(), reproject the data to a suitable projected coordinate system (e.g., UTM or an equal-area projection). This ensures that the union operation is done in a coordinate system that respects spatial distances and areas.
# Reproject to UTM (adjust to your area)
projected_BC_dups <- st_transform(BC_dups, crs = 32633)

# Ensure geometries are valid
valid_projected_BC_dups <- st_make_valid(projected_BC_dups)

# Perform the union
# st_union could union geometry to get an accurate range of the fire
union_valid_projected_BC_dups <- valid_projected_BC_dups %>%
  group_by(FIRE_LABEL) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  st_as_sf()

# Reproject back to geographic CRS if needed
dups_merge <- st_transform(union_valid_projected_BC_dups, crs = st_crs(BC_dups))

library(units)
# ensure that both datasets have the same units for the var column. You can use units::set_units() to assign the appropriate units:
# or you can strip the units from the column with the units::drop_units()
BC_dups <- BC_dups %>%
  st_drop_geometry(.) %>%
  group_by(FIRE_LABEL) %>% 
  summarise(across(.cols = everything(),
                   .fns = max)) %>% 
  # FEATURE_LENGTH_M , OBJECTID etc. are different across the same FIRE_LABEL
  # distinct(FIRE_LABEL, .keep_all = TRUE) %>% #keep all variables in .data. If a combination of FIRE_LABEL is not distinct, this keeps the first row of values.
  # arrange(FIRE_LABEL) %>% 
  # remove area variables before we calculate the new area variables using union geometry.
  merge(., dups_merge) %>%
  mutate(
    FEATURE_AREA_SQM = st_area(geometry) ,
    FIRE_SIZE_HECTARES = (FEATURE_AREA_SQM)/10000
    # FEATURE_AREA_SQM = units::set_units(st_area(geometry),"m^2") ,
    # FIRE_SIZE_HECTARES = units::drop_units(FEATURE_AREA_SQM)/10000
    
  ) %>%
  distinct(FIRE_LABEL, .keep_all = TRUE) %>%
  st_as_sf(., crs=st_crs(BC_wildfire_perimeter_historic))
# 70 unique elements are previously duplicated 


BC_wildfire_perimeter_historic <- BC_wildfire_perimeter_historic[!BC_wildfire_perimeter_historic$FIRE_LABEL %in% dups, ] %>%
  bind_rows(
    .,
    BC_dups %>%
      mutate(
        FEATURE_AREA_SQM  = units::drop_units(FEATURE_AREA_SQM),
        FIRE_SIZE_HECTARES  = units::drop_units(FIRE_SIZE_HECTARES)
      )
  )

## READ DISSEMINATION BLOCKS
file_path <- use_network_path("data/Wildfires_DB/Input/Blocks_2021/ldb_000b21a_e.shp")
blocks <- st_read(file_path) %>%
  st_transform(, crs = st_crs(BC_wildfire_perimeter_historic)) %>%
  filter(PRUID == '59') %>%
  mutate(DB_AREA = st_area(.)) 

DB_filtered <- blocks %>%
  select(c(DBUID, DB_AREA)) %>%
  st_drop_geometry()

# Reproject datasets to a suitable CRS
BC_wildfire_perimeter_historic_projected <- st_transform(BC_wildfire_perimeter_historic, crs = 32633)  # UTM Zone 33N (example)
blocks_projected <- st_transform(blocks, crs = 32633)

# Ensure geometries are valid
BC_wildfire_perimeter_historic_projected <- st_make_valid(BC_wildfire_perimeter_historic_projected)
blocks_projected <- st_make_valid(blocks_projected)

## COMBINE BLOCKS AND FIRES # Perform the intersection
BC_fire_perimeter_projected <- st_intersection(blocks_projected, BC_wildfire_perimeter_historic_projected) %>%
  select(c(DBUID, FIRE_LABEL, FIRE_NUMBER, FIRE_YEAR, FIRE_CAUSE, FIRE_DATE, FIRE_SIZE_HECTARES,
           FEATURE_AREA_SQM))
##. Reproject back to original CRS if needed
BC_fire_perimeter <- st_transform(BC_fire_perimeter_projected, st_crs(BC_wildfire_perimeter_historic))



BC_fire_perimeter <- merge(BC_fire_perimeter, DB_filtered)
BC_fire_perimeter <- dplyr::rename(BC_fire_perimeter, FIRE_AREA_SQM = FEATURE_AREA_SQM) %>%  
  mutate(FIRE_DB_AREA_SQM = st_area(geometry))  

BC_fire_perimeter <- BC_fire_perimeter %>%
  mutate(FIRE_PERCENT_DB = 100*as.numeric(FIRE_DB_AREA_SQM)/as.numeric(DB_AREA),
         FIRE_PERCENT_FIRE = 100*as.numeric(FIRE_DB_AREA_SQM)/as.numeric(FIRE_AREA_SQM),
         FIRE_PERCENT_FIRE = ifelse(FIRE_PERCENT_FIRE>100, 100, FIRE_PERCENT_FIRE))

BC_fire_list <- split(BC_fire_perimeter, BC_fire_perimeter$FIRE_YEAR)

## CURRENT WILDFIRES
file_path <- use_network_path("data/Wildfires_DB/Input/BC Wildfire Fire Perimeters - Current GEOJSON/PROT_CURRENT_FIRE_POLYS_SP.geojson")
BC_wildfire_perimeter_current <- st_read(file_path) %>%
  filter(FIRE_YEAR>=2024) %>%
  mutate(FIRE_LABEL = paste(FIRE_YEAR, FIRE_NUMBER, sep='-'))




BC_fire_perimeter <- st_intersection(blocks, BC_wildfire_perimeter_current) %>%
  select(c(DBUID, FIRE_LABEL, FIRE_NUMBER, FIRE_YEAR, FIRE_STATUS, TRACK_DATE, FIRE_SIZE_HECTARES,
           FEATURE_AREA_SQM))

## COMBINE CURRENT FIRES AND BLOCKS
BC_fire_perimeter <- merge(BC_fire_perimeter, DB_filtered)
BC_fire_perimeter <- BC_fire_perimeter %>%
  dplyr::rename(FIRE_AREA_SQM = FEATURE_AREA_SQM) %>%
  mutate(FIRE_DB_AREA_SQM = st_area(.))

BC_fire_perimeter <- BC_fire_perimeter %>%
  mutate(FIRE_PERCENT_DB = 100*as.numeric(FIRE_DB_AREA_SQM)/as.numeric(DB_AREA),
         FIRE_PERCENT_FIRE = 100*as.numeric(FIRE_DB_AREA_SQM)/as.numeric(FIRE_AREA_SQM),
         FIRE_PERCENT_FIRE = ifelse(FIRE_PERCENT_FIRE>100, 100, FIRE_PERCENT_FIRE))

BC_fire_list[['2024']] <- BC_fire_perimeter

## COMBINE CURRENT AND HISTORICAL INTO SINGLE DATASET
BC_fire <- bind_rows(BC_fire_list) %>%
  st_as_sf(.) %>%
  st_drop_geometry(.)

BC_fire <- BC_fire %>%
  mutate(ESTIMATED_AREA = ifelse(FIRE_LABEL %in% dups, 1, 0),
         FIRE_LABEL_DB = paste(FIRE_LABEL, DBUID, sep='-')
  ) %>%
  select(c(FIRE_LABEL_DB, FIRE_LABEL, DBUID, FIRE_NUMBER, FIRE_YEAR, 
           FIRE_CAUSE, FIRE_DATE, TRACK_DATE, FIRE_STATUS, FIRE_SIZE_HECTARES, 
           FIRE_AREA_SQM, DB_AREA, FIRE_DB_AREA_SQM, FIRE_PERCENT_DB, FIRE_PERCENT_FIRE,
           ESTIMATED_AREA))

BC_fire <- BC_fire[order(BC_fire$FIRE_LABEL_DB), ]
file_path <- paste0(lan_path, 'Output/BC_wildfires_2000_2024.csv')
write.csv(BC_fire, file = file_path)

## EXTENDED VERSION
## ESTIMATE NEIGHBORS FOR EACH BLOCK
neighbors <- st_touches(blocks)
fire_neighbors <- tibble()
for(x in 1:nrow(BC_fire)){
  DB <- BC_fire$DBUID[x]
  match <- which(blocks$DBUID==DB)
  DB_neighbors <- neighbors[[match]]
  aux <- tibble(
    DBUID = blocks$DBUID[DB_neighbors],
    FIRE_LABEL = BC_fire$FIRE_LABEL[x],
    FIRE_NUMBER = BC_fire$FIRE_NUMBER[x],
    FIRE_YEAR = BC_fire$FIRE_YEAR[x],
    FIRE_CAUSE = BC_fire$FIRE_CAUSE[x],
    FIRE_DATE = BC_fire$FIRE_DATE[x],
    TRACK_DATE = BC_fire$TRACK_DATE[x],
    FIRE_STATUS = BC_fire$FIRE_STATUS[x],
    FIRE_SIZE_HECTARES = BC_fire$FIRE_SIZE_HECTARES[x],
    FIRE_AREA_SQM = BC_fire$FIRE_AREA_SQM[x],
    NEIGHBOR = 1
  )
  fire_neighbors <- rbind(fire_neighbors, aux)
  rm(aux)
}

#plot(blocks$geometry[neighbors[[which(blocks$DBUID=='59410459089')]]], col='yellow')
#plot(blocks$geometry[which(blocks$DBUID=='59410459089')], add=T, col='red')

BC_fire <- bind_rows(BC_fire, fire_neighbors) %>%
  mutate(NEIGHBOR = ifelse(is.na(NEIGHBOR), 0, NEIGHBOR),
         FIRE_LABEL_DB = ifelse(is.na(FIRE_LABEL_DB), 
                                paste(FIRE_LABEL, DBUID, sep='-'),
                                FIRE_LABEL_DB)
  )
BC_fire <- BC_fire[order(BC_fire$FIRE_LABEL_DB), ]
file_path <- paste0(lan_path, 'Output/BC_wildfires_2000_2024_extended.csv')
write.csv(BC_fire, file = file_path)


#################################################################################################
# Data dictionary
#################################################################################################
# 2. Create a dictionary 
library(datadictionary)


BC_fire_labels  <- c(
  "FIRE_LABEL_DB" = "Unique ID for each element of the data, combination of FIRE_LABEL and DBUID.",
  "FIRE_LABEL" = "Wildfire unique ID, combination of FIRE_NUMBER and FIRE_YEAR.",
  "DBUID" = "Dissemination block unique ID",
  "FIRE_NUMBER" = "Wildfire number (some fires in the same area but different years may share the same fire number)",
  "FIRE_YEAR" = "Year of the Wildfire",
  "FIRE_CAUSE" = "Cause of the wildfire (Missing for current wildfires)",
  "FIRE_DATE" = "Estimated date of the Wildfire (Missing for current wildfires)",
  "TRACK_DATE" = "Date when the wildfire was tracked (only for current wildfires)",
  "FIRE_STATUS" = "Status of the wildfire (Off, under control, etc.) only for current wildfires",
  "FIRE_SIZE_HECTARES" = "Size of the wildfire in Hectares (reported)",
  "FIRE_AREA_SQM" = "Size of the wildfire in square meters (reported/estimated)",
  "DB_AREA" = "Size of the dissemination block in square meters (estimated)",
  "FIRE_DB_AREA_SQM" = "Size of the portion of the wildfire that belongs to the dissemination block in square meters (estimated)",
  "FIRE_PERCENT_DB" = "Size of the fire (portion that match with dissemination block) as a percent of the dissemination block. (Estimated)",
  "FIRE_PERCENT_FIRE" = "Size of the wildfire portion as a percentage of the total Wildfire (estimated)",
  "ESTIMATED_AREA" = "Dummy variable equal to 1 if the FIRE_AREA_SQM was estimated rather than keep the reported value (only for cases where the FIRE originally was reported in separated parts)",
  "NEIGHBOR" = "Dummy variable equal to 1 if the observation associated with the fire is a neighbor of the fire happening (is an adjacent dissemination block). For these cases there are no percentages or areas estimated since there is no fire."
)

# Print the label vector
print(label_vector)


BC_fire_dict <- create_dictionary(BC_fire,
                             id_var = "POSTALCODE",
                             var_labels = wildfire_labels)