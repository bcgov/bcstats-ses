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
# This script reads wild fire geodata and calculate the events in each DB. 
# if wrong character sdet
# convert character set
# iconv -f ISO-8859-1 -t UTF-8 2021_92-151_X.csv > 2021_92-151_X_iconv.csv
###################################################################


## Set library
pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb, cancensus)

library(rgdal)
library(sf)
library(jsonlite)
library(lwgeom )
library(testthat)


## Turn off spherical geometry
sf::sf_use_s2(FALSE)

###################################################################
# BC Wildfire Fire Perimeters - Historical
# https://catalogue.data.gov.bc.ca/dataset/22c7cb44-1463-48f7-8e47-88857f207702
###################################################################


## READ HISTORIC WILDFIRES
# file_path <- use_network_path("data/Wildfires_DB/Input/BC Wildfire Fire Perimeters - Historical GEOJSON/PROT_HISTORICAL_FIRE_POLYS_SP.geojson")
# BC_wildfire_perimeter_historic <- sf::st_read(file_path) %>%
#   filter(FIRE_YEAR>=2000)

bcdc_get_record("22c7cb44-1463-48f7-8e47-88857f207702")

# Access the full 'Resources' data frame using:
bcdc_tidy_resources('22c7cb44-1463-48f7-8e47-88857f207702')
# Query and filter this data using: 

BC_wildfire_perimeter_historic <- bcdc_query_geodata('22c7cb44-1463-48f7-8e47-88857f207702') %>%
  filter(FIRE_YEAR>=2000) %>% 
  collect()

# show all the features
BC_wildfire_perimeter_historic %>% glimpse()

# Check current CRS
print(st_crs(BC_wildfire_perimeter_historic))
# "NAD83 / BC Albers"
# 
# **EPSG Code**: `3005` (NAD83 / BC Albers)
# - **Description**: A custom projection for British Columbia, using an Albers Equal Area Conic projection.
# **EPSG Code**: `4326` (WGS 84) or `4269` (NAD 83)
# - **Description**: These are unprojected coordinate systems based on latitude and longitude.
# **Limitations**: Distances, areas, and intersections will not be accurate due to distortion.

# The best practice is to reproject your data into a projected CRS before applying st_union() to avoid distortions and ensure accurate results. Always ensure geometries are valid and use simplification if necessary for performance reasons.

### Recommendation
# 
# - For **local/regional work**: Use the **UTM zone** that corresponds to your area.
# - For **province-wide work**: Use **NAD83 / BC Albers (EPSG:3005)**.
# - For **Canada-wide analyses**: Use **Canada Atlas Lambert (EPSG:3347)**.


## FIX CASES WITH FIRES SPLIT INTO SEVERAL PARTS (DUPLICATED FIRES)
dups <- unique(BC_wildfire_perimeter_historic$FIRE_LABEL[which(duplicated(BC_wildfire_perimeter_historic$FIRE_LABEL))])
BC_dups <- BC_wildfire_perimeter_historic[BC_wildfire_perimeter_historic$FIRE_LABEL %in% dups, ] 

# Before performing st_union(), reproject the data to a suitable projected coordinate system (e.g., UTM or an equal-area projection). This ensures that the union operation is done in a coordinate system that respects spatial distances and areas.
# Reproject to UTM (adjust to your area)
# BC spans several **UTM (Universal Transverse Mercator)** zones, and UTM is commonly used for regional-scale analyses because it minimizes distortions for smaller areas.
# 
# - **UTM Zones in BC**:
#   
# - Southern BC: **Zone 10N** (`EPSG:32610`)
# - Central BC: **Zone 9N** (`EPSG:32609`)
# - Northern BC: **Zone 8N** (`EPSG:32608`)

projected_BC_dups <- st_transform(BC_dups, crs = 32610)

# Ensure geometries are valid
valid_projected_BC_dups <- st_make_valid(projected_BC_dups)

# Perform the union
# st_union could union geometry to get an accurate range of the fire
union_valid_projected_BC_dups <- valid_projected_BC_dups %>%
  group_by(FIRE_LABEL) %>%
  summarize(geometry = sf::st_union(geometry)) 

# Reproject back to geographic CRS if needed
dups_merge <- st_transform(union_valid_projected_BC_dups, crs = st_crs(BC_wildfire_perimeter_historic))

library(units)
# ensure that both datasets have the same units for the var column. You can use units::set_units() to assign the appropriate units:
# or you can strip the units from the column with the units::drop_units()
BC_dups <- BC_dups %>%
  st_drop_geometry() %>%   # remove area variables before we calculate the new area variables using union geometry.
  group_by(FIRE_LABEL) %>% # FIRE_LABEL is combination of fire_number and year, so basically group by year and fire_number
  summarise(across(.cols = everything(),
                   .fns = max)) %>% 
  # FEATURE_LENGTH_M , OBJECTID etc. are different across the same FIRE_LABEL
  inner_join(dups_merge, by = join_by(FIRE_LABEL)) %>%
  mutate(
    FEATURE_AREA_SQM = st_area(geometry) ,
    FIRE_SIZE_HECTARES = (FEATURE_AREA_SQM)/10000
  ) %>%
  distinct(FIRE_LABEL, .keep_all = TRUE)   #this will keep the first row within group/distinct value
# 70 unique elements are previously duplicated 


BC_wildfire_perimeter_historic <- BC_wildfire_perimeter_historic %>% 
  filter(!FIRE_LABEL %in% dups) %>% 
  bind_rows(
    BC_dups %>%
      mutate(
        FEATURE_AREA_SQM  = units::drop_units(FEATURE_AREA_SQM),
        FIRE_SIZE_HECTARES  = units::drop_units(FIRE_SIZE_HECTARES)
      )
  )


###################################################################
# BC Wildfire Fire Perimeters - Current
# https://catalogue.data.gov.bc.ca/dataset/cdfc2d7b-c046-4bf0-90ac-4897232619e1
###################################################################
## CURRENT WILDFIRES


bcdc_get_record("cdfc2d7b-c046-4bf0-90ac-4897232619e1")

# Available Resources (1):
# 1. View WMS getCapabilities request details (wms)
# Access the full 'Resources' data frame using:
bcdc_tidy_resources('cdfc2d7b-c046-4bf0-90ac-4897232619e1')
# Query and filter this data using: bcdc_query_geodata('cdfc2d7b-c046-4bf0-90ac-4897232619e1')

BC_wildfire_perimeter_current <-  bcdc_query_geodata('cdfc2d7b-c046-4bf0-90ac-4897232619e1') %>%
  filter(FIRE_YEAR >= 2024) %>% # The data in BC data has one row FIRE_NUMBER == 'G51564' for 2023 which is duplicated since it is in historic dataset.
  collect() %>%
  mutate(FIRE_LABEL = paste(FIRE_YEAR, FIRE_NUMBER, sep = '-'))

print(st_crs(BC_wildfire_perimeter_current))

# The same as the historic wild fire data

# BC_wildfire_perimeter_current %>% 
#   group_by(FIRE_LABEL) %>% 
#   filter(n()>1)
# no duplicated FIRE_LABEL, no need for dealing with duplication like historic dataset.

# Columns unique to df1
unique_to_df1 <- setdiff(colnames(BC_wildfire_perimeter_historic), colnames(BC_wildfire_perimeter_current))

# Columns unique to df2
unique_to_df2 <- setdiff(colnames(BC_wildfire_perimeter_current), colnames(BC_wildfire_perimeter_historic))

# Common columns
common_columns <- intersect(colnames(BC_wildfire_perimeter_historic), colnames(BC_wildfire_perimeter_current))

# Print differences
print(unique_to_df1)  # ["a"]
print(unique_to_df2)  # ["c"]
print(common_columns) # ["b"]


# Bind rows
BC_wildfire_perimeter = bind_rows(BC_wildfire_perimeter_historic, BC_wildfire_perimeter_current) %>% 
  arrange(FIRE_LABEL) 


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
# unzip(use_network_path("data/Wildfires_DB/Input/Blocks_2021/ldb_000a21a_e.zip"))
## READ DISSEMINATION BLOCKS
file_path <- use_network_path("data/raw_data/Wildfire/Wildfires_DB/Input/Blocks_2021/ldb_000b21a_e.shp")

blocks <- st_read(file_path) %>%
  filter(PRUID == '59') %>%
  st_transform(, crs = st_crs(BC_wildfire_perimeter_historic)) %>%
  mutate(DB_AREA = st_area(.)) 

# DB_filtered <- blocks %>%
#   select(c(DBUID, DB_AREA)) %>%
#   st_drop_geometry()


# Reproject datasets to a suitable CRS
BC_wildfire_perimeter_projected <- st_transform(BC_wildfire_perimeter, crs = 32610)  
blocks_projected <- st_transform(blocks, crs = 32610)

# Ensure geometries are valid
BC_wildfire_perimeter_projected <- st_make_valid(BC_wildfire_perimeter_projected)
blocks_projected <- st_make_valid(blocks_projected)

## COMBINE BLOCKS AND FIRES # Perform the intersection
BC_DB_wildfire_perimeter_projected <- st_intersection(blocks_projected, BC_wildfire_perimeter_projected) %>% 
  select(DBUID, DB_AREA,
         FIRE_LABEL, FIRE_NUMBER, FIRE_YEAR,
         FIRE_CAUSE, FIRE_DATE, FIRE_STATUS, TRACK_DATE, FIRE_SIZE_HECTARES,FEATURE_AREA_SQM,
         everything()) %>% 
  arrange(DBUID, FIRE_LABEL)
  # select(c(DBUID, FIRE_LABEL, FIRE_NUMBER, FIRE_YEAR, FIRE_CAUSE, FIRE_DATE, FIRE_STATUS, TRACK_DATE, FIRE_SIZE_HECTARES,
  #          FEATURE_AREA_SQM))

##. Reproject back to original CRS 
BC_DB_wildfire_perimeter <- st_transform(BC_DB_wildfire_perimeter_projected, st_crs(BC_wildfire_perimeter))


# Get back the DB area, only areas that cover fires, one DB could have three fires in one year
# BC_DB_wildfire_perimeter_perimeter <- DB_filtered %>% inner_join(BC_DB_wildfire_perimeter_perimeter, by = join_by(DBUID) )

# ? should we group by DB to get the total percent? or if the fire areas are overlapped, should we st_union them to avoid double counting?

# Group Data by Year and Region: Use dplyr::group_by() to group the spatial data by the year and region.
# Identify Overlaps Within Groups:
#   Use st_intersects() to identify spatial intersections between fire areas.
# Check if any intersecting geometries have different timestamps within the same group.
# Summarize Results: For each group, determine if overlaps exist and how many.


# Group data by year and region, then identify overlaps

overlap_results <- BC_DB_wildfire_perimeter %>%
  group_by(FIRE_YEAR, DBUID) %>%
  summarise(
    overlaps = any(sapply(
      seq_len(n()),
      function(i) {
        # Work with the current group only
        current_group <-  st_as_sf(pick(everything()))  # Ensure it's an sf object # Access only rows within the group
        current_group <- st_transform(current_group, crs =  st_crs(BC_wildfire_perimeter))  # BC Albers projection
        others <- current_group[-i, ]  # Exclude the i-th row within the group
        overlaps <- st_intersects(current_group[i, ], others, sparse = FALSE)
        # Check if overlaps occur with different times, skip time this time. 
        # any(overlaps & current_group$time[-i] != current_group$time[i])
        any(overlaps)
      }
    ))
  )


# it seems that some 93 DBs have the overlaps, but not sure if they are significant. 
overlap_results %>% 
  filter(overlaps)

# Reproject data to a projected CRS for accurate area calculations

# Group by year and region, then union fire areas
BC_DB_wildfire_perimeter_grouped <- BC_DB_wildfire_perimeter_projected %>%
  group_by(FIRE_YEAR, DBUID, DB_AREA, LANDAREA) %>%
  summarise(
    N_FIRE = n(),
    FIRE_NUMBER = str_c(FIRE_NUMBER, collapse = ", ", na.rm = TRUE),
    across(
      .cols = c(FIRE_SIZE_HECTARES ,FEATURE_AREA_SQM),
      .fns = sum,
      .names = "SIMPLE_SUM_{col}"
    ),
    geometry = st_union(geometry),  # Union geometries within each group
    .groups = "drop"
  ) %>%
  mutate(TOTAO_FIRE_AREA = st_area(geometry))  # Calculate the total area  
 

BC_DB_wildfire_perimeter_grouped = st_transform(BC_DB_wildfire_perimeter_grouped,  st_crs(BC_wildfire_perimeter)) 
# 6736 rows

# Get percentage of fire area
BC_DB_wildfire_perimeter_grouped <- BC_DB_wildfire_perimeter_grouped %>%
  mutate(FIRE_PERCENT_DB = 100*as.numeric(TOTAO_FIRE_AREA)/as.numeric(DB_AREA), # one DB could have three fires in one year, so the percent could small but the total percent could be large.  
         FIRE_PERCENT_FIRE = 100*as.numeric(TOTAO_FIRE_AREA)/as.numeric(SIMPLE_SUM_FEATURE_AREA_SQM   ),
         FIRE_PERCENT_FIRE = ifelse(FIRE_PERCENT_FIRE>100, 100, FIRE_PERCENT_FIRE)) # why it could be larger than 100? due geometry computation?


###################################################################
# save grouped dataset
###################################################################


BC_DB_wildfire_perimeter_grouped <- BC_DB_wildfire_perimeter_grouped %>% 
  arrange(FIRE_YEAR, DBUID, DB_AREA, LANDAREA)
# file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_grouped_wildfires_2000_2024.csv")
# write.csv(BC_DB_wildfire_perimeter_grouped, file = file_path)
file_path <- "out/BC_DB_grouped_wildfires_2000_2024.csv"
write.csv(BC_DB_wildfire_perimeter_grouped, file = file_path)

# the geospatial data could be saved ad geojson. 
file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_grouped_wildfires_2000_2024.geojson")
st_write(BC_DB_wildfire_perimeter_grouped, dsn = file_path)
file_path <- "out/BC_DB_grouped_wildfires_2000_2024.geojson"
st_write(BC_DB_wildfire_perimeter_grouped, dsn = file_path)

#################################################################################################
# Data dictionary for grouped dataset
#################################################################################################
# 2. Create a dictionary 
library(datadictionary)

BC_DB_wildfire_perimeter_grouped_labels  <- c(
  "FIRE_YEAR" = "Year of the Wildfire",
  "DBUID" = "Dissemination block unique ID",
  "DB_AREA" = "Size of the dissemination block in square meters (estimated)",
  "LANDAREA" = "Size of the land in dissemination block in hectares (estimated)",
  "FIRE_NUMBER" = "Wildfire number (some fires in the same area but different years may share the same fire number)",
  "N_FIRE" = "The total numbers the wildfire",
  "SIMPLE_SUM_FIRE_SIZE_HECTARES" = "The simple total size of all wildfire in the DB within the year in Hectares (reported)",
  "SIMPLE_SUM_FEATURE_AREA_SQM" = "The simple total size of all wildfire in the DB within the year in square meters (reported/estimated)",
  "TOTAO_FIRE_AREA" = "The total size of all wildfire in the DB within the year in square meters (reported/estimated without overlapped areas)", 
  "FIRE_PERCENT_DB" = "Size of the fire (portion that match with dissemination block) as a percent of the dissemination block. (Estimated)",
  "FIRE_PERCENT_FIRE" = "Size of the wildfire portion as a percentage of the total Wildfire (estimated)"
)

# Print the label vector
print(BC_DB_wildfire_perimeter_grouped_labels)
names(BC_DB_wildfire_perimeter_grouped_labels)
names(BC_DB_wildfire_perimeter_grouped)
setdiff(names(BC_DB_wildfire_perimeter_grouped_labels), names(BC_DB_wildfire_perimeter_grouped))
# only difference is the geometry
# test_that("calculates correctly", {
#   expect_equal(names(BC_DB_wildfire_perimeter_grouped), names(BC_DB_wildfire_perimeter_grouped_labels))          # Basic case
# })



BC_DB_wildfire_perimeter_grouped_dict <- create_dictionary(BC_DB_wildfire_perimeter_grouped %>% 
                                                             st_drop_geometry() %>% 
                                                             select(names(BC_DB_wildfire_perimeter_grouped_labels)),
                                  id_var = c("FIRE_YEAR", "DBUID"),
                                  var_labels = BC_DB_wildfire_perimeter_grouped_labels)


# file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_wildfire_perimeter_grouped_dict.csv")
# write.csv(BC_DB_wildfire_perimeter_grouped_dict, file = file_path)
file_path <- "out/BC_DB_wildfire_perimeter_grouped_dict.csv"
write.csv(BC_DB_wildfire_perimeter_grouped_dict, file = file_path)


###################################################################
# The full wild fire table with details of each wild fire.
###################################################################

BC_DB_wildfire_perimeter <- BC_DB_wildfire_perimeter %>%
  mutate(FIRE_DB_AREA_SQM = st_area(geometry)) %>%
  rename(FIRE_AREA_SQM = FEATURE_AREA_SQM) %>% 
  mutate(ESTIMATED_AREA = ifelse(FIRE_LABEL %in% dups, 1, 0),
         FIRE_LABEL_DB = paste(FIRE_LABEL, DBUID, sep='-')
  ) 

# Get percentage of fire area
BC_DB_wildfire_perimeter<- BC_DB_wildfire_perimeter %>%
  mutate(FIRE_PERCENT_DB = 100*as.numeric(FIRE_DB_AREA_SQM)/as.numeric(DB_AREA), # one DB could have three fires in one year, so the percent could small but the total percent could be large.  
         FIRE_PERCENT_DB = ifelse(FIRE_PERCENT_DB>100, 100, FIRE_PERCENT_DB) , # should be capped by 100% 
         FIRE_PERCENT_FIRE = 100*as.numeric(FIRE_DB_AREA_SQM)/as.numeric(FIRE_DB_AREA_SQM   ),
         FIRE_PERCENT_FIRE = ifelse(FIRE_PERCENT_FIRE>100, 100, FIRE_PERCENT_FIRE)) %>% # why it could be larger than 100? due geometry computation?
  select(c(FIRE_LABEL_DB, FIRE_LABEL, DBUID, FIRE_NUMBER, FIRE_YEAR, 
           FIRE_CAUSE, FIRE_DATE, TRACK_DATE, FIRE_STATUS, FIRE_SIZE_HECTARES, 
           FIRE_AREA_SQM, DB_AREA, FIRE_DB_AREA_SQM, FIRE_PERCENT_DB, FIRE_PERCENT_FIRE,
           ESTIMATED_AREA))

file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_wildfire_perimeter_2000_2004.csv")
write.csv(BC_DB_wildfire_perimeter, file = file_path)
file_path <- "out/BC_DB_wildfire_perimeter_2000_2004.csv"
write.csv(BC_DB_wildfire_perimeter, file = file_path)

# the geospatial data could be saved ad geojson. 
file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_wildfires_2000_2024.geojson")
st_write(BC_DB_wildfire_perimeter, dsn = file_path)
file_path <- "out/BC_DB_wildfires_2000_2024.geojson"
st_write(BC_DB_wildfire_perimeter, dsn = file_path)
#################################################################################################
# Data dictionary for full dataset
#################################################################################################
# 2. Create a dictionary 
library(datadictionary)

names(BC_DB_wildfire_perimeter)
BC_DB_wildfire_perimeter_labels  <- c(
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
  "ESTIMATED_AREA" = "Dummy variable equal to 1 if the FIRE_AREA_SQM was estimated rather than keep the reported value (only for cases where the FIRE originally was reported in separated parts)"
)

# Print the label vector
print(BC_DB_wildfire_perimeter_labels)
names(BC_DB_wildfire_perimeter_labels)
names(BC_DB_wildfire_perimeter)
setdiff(names(BC_DB_wildfire_perimeter_labels), names(BC_DB_wildfire_perimeter))
setdiff( names(BC_DB_wildfire_perimeter), names(BC_DB_wildfire_perimeter_labels))
# only difference is the geometry
# test_that("calculates correctly", {
#   expect_equal(names(BC_DB_wildfire_perimeter), names(BC_DB_wildfire_perimeter_labels))          # Basic case
# })


BC_DB_wildfire_perimeter <- BC_DB_wildfire_perimeter %>% 
  mutate(
    across(
      .cols = c(FIRE_CAUSE,FIRE_STATUS ),
      .fns = as_factor
    )
  )

BC_DB_wildfire_perimeter_dict <- create_dictionary(BC_DB_wildfire_perimeter %>% 
                                                     st_drop_geometry(),
                                  id_var = "FIRE_LABEL_DB",
                                  var_labels = BC_DB_wildfire_perimeter_labels)

# file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_wildfire_perimeter_dict.csv")
# write.csv(BC_DB_wildfire_perimeter_dict, file = file_path)
file_path <- "out/BC_DB_wildfire_perimeter_dict.csv"
write.csv(BC_DB_wildfire_perimeter_dict, file = file_path)


#################################################################################################
## EXTENDED VERSION
## ESTIMATE NEIGHBORS FOR EACH BLOCK
#################################################################################################

# in the extended version we include the DB that do not have a fire but are some how in contact with a  DB where there was a fire

# Reproject your data into a projected CRS before using st_touches. This ensures that the geometries are treated as planar, and operations like st_touches work accurately.

# Reproject data to a suitable projected CRS (e.g., UTM Zone 33N)
neighbors <- st_touches(st_transform(blocks, crs = 32610))
# blocks %>% glimpse()
# Rows: 52,387
# ST_Touches(A, B) returns true if A and B have at least one point in common, but their interiors do not intersect.
# ST_Touches tests whether two geometries touch at their boundaries, but do not intersect in their interiors
# neighbors has 52387 rows as well.
# It is something like
# Sparse geometry binary predicate list of length 52387, where the predicate was `touches'
# first 10 elements:
#  1: 2, 3, 10, 14, 18, 19, 20
# It like row 1: touch 2, 3, 10, 14, 18, 19, 20 rows

fire_neighbors <- tibble()
# append the neighbours as a column to the BC_DB_wildfire_perimeter, so each DB gets many rows based on how many neighbours they have.
# In BC_DB_wildfire_perimeter, some DBs have multiple rows within one year due to multiple wild fires within one year. 
# BC_DB_wildfire_perimeter is the inner join of wildfire area and DB area so it does not have all DBs in BC.
# the key in this BC_DB_wildfire_perimeter is FIRE_LABEL_DB, year + fire number + db id.
for(x in 1:nrow(BC_DB_wildfire_perimeter)){
  DB <- BC_DB_wildfire_perimeter$DBUID[x]
  match <- which(blocks$DBUID==DB)
  DB_neighbors <- neighbors[[match]] # get all the row number of rows' that st touch the row x. It like row 1: touch 2, 3, 10, 14, 18, 19, 20
  aux <- tibble(
    DBUID = blocks$DBUID[DB_neighbors],
    FIRE_LABEL = BC_DB_wildfire_perimeter$FIRE_LABEL[x],
    FIRE_NUMBER = BC_DB_wildfire_perimeter$FIRE_NUMBER[x],
    FIRE_YEAR = BC_DB_wildfire_perimeter$FIRE_YEAR[x],
    FIRE_CAUSE = BC_DB_wildfire_perimeter$FIRE_CAUSE[x],
    FIRE_DATE = BC_DB_wildfire_perimeter$FIRE_DATE[x],
    TRACK_DATE = BC_DB_wildfire_perimeter$TRACK_DATE[x],
    FIRE_STATUS = BC_DB_wildfire_perimeter$FIRE_STATUS[x],
    FIRE_SIZE_HECTARES = BC_DB_wildfire_perimeter$FIRE_SIZE_HECTARES[x],
    FIRE_AREA_SQM = BC_DB_wildfire_perimeter$FIRE_AREA_SQM[x],
    NEIGHBOR = 1
  )
  fire_neighbors <- rbind(fire_neighbors, aux)
  rm(aux)
}


#plot(blocks$geometry[neighbors[[which(blocks$DBUID=='59410459089')]]], col='yellow')
#plot(blocks$geometry[which(blocks$DBUID=='59410459089')], add=T, col='red')


# append rows for those neighours.
BC_DB_wildfire_perimeter_extended <- bind_rows(BC_DB_wildfire_perimeter, fire_neighbors) %>%
  mutate(NEIGHBOR = ifelse(is.na(NEIGHBOR), 0, NEIGHBOR),
         FIRE_LABEL_DB = ifelse(is.na(FIRE_LABEL_DB), 
                                paste(FIRE_LABEL, DBUID, sep='-'),
                                FIRE_LABEL_DB)
  )
BC_DB_wildfire_perimeter_extended <- BC_DB_wildfire_perimeter_extended %>% 
  arrange(FIRE_LABEL_DB)

# file_path <- use_network_path("data/Output/Wildfires_DB/BC_wildfires_2000_2024_extended.csv")
# write.csv(BC_DB_wildfire_perimeter_extended, file = file_path)
file_path <- "out/BC_wildfires_2000_2024_extended.csv"
write.csv(BC_DB_wildfire_perimeter_extended, file = file_path)

# the geospatial data could be saved ad geojson. 
file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_wildfire_perimeter_extended.geojson")
st_write(BC_DB_wildfire_perimeter_extended, dsn = file_path)
file_path <- "out/BC_DB_wildfire_perimeter_extended.geojson"
st_write(BC_DB_wildfire_perimeter_extended, dsn = file_path)

#################################################################################################
# Data dictionary for full enxtened dataset
#################################################################################################
# 2. Create a dictionary 
library(datadictionary)

names(BC_DB_wildfire_perimeter_extended)
BC_DB_wildfire_perimeter_extended_labels  <- c(
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
print(BC_DB_wildfire_perimeter_extended_labels)
names(BC_DB_wildfire_perimeter_extended_labels)
names(BC_DB_wildfire_perimeter_extended)
setdiff(names(BC_DB_wildfire_perimeter_extended_labels), names(BC_DB_wildfire_perimeter_extended))
setdiff( names(BC_DB_wildfire_perimeter_extended), names(BC_DB_wildfire_perimeter_extended_labels))

test_that("calculates correctly", {
  expect_equal(names(BC_DB_wildfire_perimeter_extended), names(BC_DB_wildfire_perimeter_extended_labels))          # Basic case
})

BC_DB_wildfire_perimeter_extended <- BC_DB_wildfire_perimeter_extended %>% 
  mutate(
    across(
      .cols = c(FIRE_CAUSE,FIRE_STATUS ),
      .fns = as_factor
    )
  )

BC_DB_wildfire_perimeter_extended_dict <- create_dictionary(BC_DB_wildfire_perimeter_extended %>% 
                                                     st_drop_geometry(),
                                                   id_var = "FIRE_LABEL_DB",
                                                   var_labels = BC_DB_wildfire_perimeter_extended_labels)
# 
# file_path <- use_network_path("data/Output/Wildfires_DB/BC_DB_wildfire_perimeter_extended_dict.csv")
# write.csv(BC_DB_wildfire_perimeter_extended_dict, file = file_path)
file_path <- "out/BC_DB_wildfire_perimeter_extended_dict.csv"
write.csv(BC_DB_wildfire_perimeter_extended_dict, file = file_path)