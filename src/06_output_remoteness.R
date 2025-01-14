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

school_file_path = use_network_path("data/raw_data/remoteness/30_percent_site_Hybrid_geocoder_nearest_schools_20241225_180826/30_percent_site_Hybrid_geocoder_nearest_schools_20241225_180826.csv")

address_data_path_list = c(
  servicebc_file_path,
  hospital_file_path,
  school_file_path
)

address_dist_data <- address_data_path_list %>%  
  read_csv() %>% 
  bind_rows()

# Convert to an sf object (BC Albers projection: EPSG 3005)
address_dist_sf <- st_as_sf(address_dist_data, coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)

address_dist_data %>% 
  count(tag)


# Efficiency:
#   
#   For large datasets, consider spatial indexing using st_join(..., largest = TRUE) to speed up operations.

# Spatial join: append dissemination area IDs to address points
address_sf_with_da <- st_join(address_dist_sf, da_shapefile, left = TRUE)


# View the resulting dataset
print(address_sf_with_da)

# Save to a new file (optional)
st_write(address_sf_with_da, 
         "./out/address_sf_with_da.geojson"
         # use_network_path("data/raw_data/remoteness/address_servicebc_with_da.geojson")
         )

address_sf_with_da %>% 
  st_drop_geometry() %>% 
  # inner_join(address_dist_data) %>% 
  # write_csv(use_network_path("data/raw_data/remoteness/address_sf_with_da.csv"))
  write_csv(("./out/address_sf_with_da.csv")) 





###########################################################################################
#   create a DA level summary. 
###########################################################################################

# Create a DA level summary table: average drive time and distance, and number of address. 



avg_dist_drvtime_by_da_service <- address_sf_with_da %>% 
  st_drop_geometry() %>% 
  group_by(DAUID, tag) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(drv_time_sec),
    AVG_DRV_DIST = mean(drv_dist),
    N_ADDRESS = n()
  )

avg_dist_drvtime_by_da_service %>% 
  write_csv(use_network_path("data/raw_data/remoteness/avg_dist_drvtime_by_da_service.csv"))


avg_dist_drvtime_by_da<- address_sf_with_da %>% 
  st_drop_geometry() %>% 
  group_by(DAUID) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(drv_time_sec),
    AVG_DRV_DIST = mean(drv_dist),
    N_ADDRESS = n()
  )

avg_dist_drvtime_by_da %>% 
  write_csv(use_network_path("data/raw_data/remoteness/avg_dist_drvtime_by_da.csv"))


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


# Save to a new file (optional)
CSD_DA_address_dist_drvtime <- CSD_DA_list %>% mutate(DAUID = as.character(DA_NUM)) %>% 
  right_join(address_sf_with_da, by = join_by(DAUID) )

CSD_DA_address_dist_drvtime <- CSD_DA_address_dist_drvtime %>% 
  st_as_sf()
  
st_write(CSD_DA_address_dist_drvtime, 
         "./out/CSD_DA_address_dist_drvtime.geojson"
         # use_network_path("data/raw_data/remoteness/address_servicebc_with_da.geojson")
)

# check if there is duplication
# CSD_DA_list %>% 
#   count(DA_NUM) %>% 
#   filter(n>1)
# no it is fine.

# Left join table together



CSD_DA_REMOTENESS = CSD_DA_list %>% 
  mutate(DAUID = as.character(DA_NUM)) %>% 
  left_join(avg_dist_drvtime_by_da_service, by = c( "DAUID")) %>% 
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

CSD_REMOTENESS_ALL_SERVICE %>% 
  write_csv("./out/CSD_REMOTENESS.csv")

CSD_REMOTENESS_ALL_SERVICE %>% 
  write_csv(use_network_path("data/output/remoteness/CSD_REMOTENESS.csv"))


###########################################################################################
# Another approach for CSD level summary

# I used two different way to calculate the CSV average. First, I calculate the DA average, and then use TMF to join DA to CSD, and get the CSD average. I think this approach applies different weight on the address. (1/n)*(1/m), n is the nubmer address in DA, m is the number of DA  in CSD. 
# 
# So I downloaded the CSD boundary file and directly calculated the CSD average after join CSD boundary to Brian's address data. 
#  
###########################################################################################

# Spatial join: append dissemination area IDs to address points
csd_sf_address_dist <- st_join(csd_shapefile,address_dist_sf, left = TRUE)

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
my_map_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

################################################################
# Create a color map plot using ggplot2
################################################################

csd_avg_address_dist_sf = csd_shapefile %>% 
  # filter(tag == "servicebc") %>% 
  left_join( csd_avg_address_dist, join_by(CSDUID, CSDNAME) )

ggplot(data = csd_avg_address_dist_sf
       ) +
  geom_sf(aes(fill = AVG_DRV_DIST), color = "white") +
  scale_fill_viridis_c(option = "viridis" ) +
  labs(
    title = "Avg. Distance to the Facility by Region",
    subtitle = "Remoteness measurement in BC",
    x = "Longitude",
    y = "Latitude"
  ) +
  my_map_theme+
  guides(fill=guide_legend(title="Avg. Distance to the Facilities"))


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
  my_map_theme


# Save the plot
ggsave("./out/csd_avg_address_dist_servicebc_sf.png", width = 10, height = 8, dpi = 300)
ggsave(use_network_path("data/output/remoteness/csd_avg_address_dist_servicebc_sf.png"), width = 10, height = 8, dpi = 300)


################################################################
# interesting these results weren't what I was as expecting for the least remote. assuming longest drive time = most remote, then Smithers is less remote than Vancouver by this definition.
# we can plot them in the map
################################################################

two_csd_avg_address_dist_servicebc_sf = da_shapefile %>% 
  inner_join( CSD_DA_REMOTENESS  %>%  filter(tag == "servicebc") , 
             join_by("DAUID") ) %>% 
  filter(MUN_NAME_2021 %in% c("Smithers", "Vancouver"))

CSD_DA_address_dist_drvtime %>% 
  filter(MUN_NAME_2021 %in% c( "Vancouver")) 
  

  
########## 
#  A map plotting function for a CSD region
##########  
# data, sf, ; filter: tag/service, CSD; 
map_data_fn <- function(df, CSD, service, sf) {
  df %>%
    filter(tag == service, MUN_NAME_2021 == CSD) %>%
    inner_join(sf, join_by("DAUID"))%>% 
    st_as_sf() 
}

# sf; fill_var; CSD; Service 
map_plot_fn <- function(sf, fill_var,fill_var_name, CSD, service){
  sf %>% ggplot() +
    geom_sf(aes(fill = {{fill_var}}), color = "white")+
    scale_fill_viridis_c(option = "viridis") +
    labs(
      # title = "Avg. Distance to ServiceBC by Region",
      subtitle = glue::glue("Remoteness measurement in {CSD}"),
      x = "Longitude",
      y = "Latitude"
    ) +
    my_map_theme+ 
    guides(fill=guide_legend(title=glue::glue("Avg. {stringr::str_to_title(fill_var_name)} to the {service}")))
}

map_dot_plot_fn <- function(sf, fill_var,fill_var_name, CSD, service){
  sf %>% ggplot() +
    geom_sf(aes(fill = {{fill_var}}), color = "white")+
    scale_fill_viridis_c(option = "viridis") +
    labs(
      # title = "Avg. Distance to ServiceBC by Region",
      subtitle = glue::glue("Remoteness measurement in {CSD}"),
      x = "Longitude",
      y = "Latitude"
    ) +
    my_map_theme+ 
    guides(fill=guide_legend(title=glue::glue("Avg. {stringr::str_to_title(fill_var_name)} to the {service}")))
}




library(cowplot) # For aligning multiple plots
library(patchwork )
plot_a = map_data_fn(df = CSD_DA_REMOTENESS, CSD = "Vancouver", service = "servicebc", sf = da_shapefile) %>% 
  map_plot_fn(sf =., fill_var = AVG_DRV_DIST, fill_var_name = "Distance", CSD = "Vancouver", service = "ServiceBC")


plot_b = map_data_fn(df = CSD_DA_REMOTENESS, CSD = "Smithers", service = "servicebc", sf = da_shapefile) %>% 
  map_plot_fn(sf =., fill_var = AVG_DRV_DIST, fill_var_name = "Distance", CSD = "Smithers", service = "ServiceBC")
  
  



# Combine the two plots side by side
# Combine the two plots using patchwork and add a shared title
combined_plot <- (plot_a + plot_b +
                    plot_layout(ncol = 2, guides = "collect") &
                    theme(legend.position = "bottom")) +
  plot_annotation(
    title = "Remoteness measurement: Vancouver vs Smithers",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  )


# combined_plot <- plot_a + plot_b +
#   plot_layout(ncol = 2, guides = "collect") &
#   theme(legend.position = "bottom") # Align legends at the bottom
# 



# Display the combined plot
print(combined_plot)



# Save the combined plot
ggsave(use_network_path("data/output/remoteness/two_csd_avg_address_dist_servicebc_sf_by_da.png"),
       plot = combined_plot,
       width = 14, height = 7, dpi = 300)

################################################################
# Hospital
################################################################

csd_avg_address_dist_hospitals_sf = csd_shapefile %>% 
  left_join( csd_avg_address_dist_by_service  %>%  filter(tag == "hospitals")  , 
             join_by(CSDUID, CSDNAME) )

csd_avg_address_dist_hospitals_sf %>% 
  filter(AVG_DRV_DIST > 400)
  summary()
  
  # Stikine Region

plot_csd_remoteness <-  function(data, remoteness_metric, remoteness_metric_name,service_name){
  ggplot(data 
  ) +
    geom_sf(aes(fill = {{remoteness_metric}}), color = "white") +
    scale_fill_viridis_c(option = "viridis", name = glue::glue("Avg. {remoteness_metric_name} to {service_name}")) +
    labs(
      title = glue::glue("Avg. {remoteness_metric_name} to {service_name} by Region"),
      subtitle = "Remoteness measurement in BC",
      x = "Longitude",
      y = "Latitude"
    ) +
    my_map_theme
}

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
  my_map_theme


# Save the plot
ggsave("./out/csd_avg_address_dist_hospitals_sf.png", width = 10, height = 8, dpi = 300)
ggsave(use_network_path("data/output/remoteness/csd_avg_address_dist_hospitals_sf.png"), width = 10, height = 8, dpi = 300)


################################################################
# School
################################################################

csd_avg_address_dist_schools_sf = csd_shapefile %>% 
  left_join( csd_avg_address_dist_by_service  %>%  filter(tag == "schools")  , 
             join_by(CSDUID, CSDNAME) )


csd_avg_address_dist_schools_sf %>% 
  filter(AVG_DRV_DIST > 100)
# North Tacla Lake
summary()

ggplot(data = csd_avg_address_dist_schools_sf %>% mutate(AVG_DRV_DIST = if_else( AVG_DRV_DIST < 100, AVG_DRV_DIST, 100))
) +
  geom_sf(aes(fill = AVG_DRV_DIST), color = "white") +
  scale_fill_viridis_c(option = "viridis", name = "Avg. Distance to Schools") +
  labs(
    title = "Avg. Distance to Schools by Region",
    subtitle = "Remoteness measurement in BC",
    x = "Longitude",
    y = "Latitude"
  ) +
  my_map_theme


# Save the plot
ggsave("./out/csd_avg_address_dist_schools_sf.png", width = 10, height = 8, dpi = 300)
ggsave(use_network_path("data/output/remoteness/csd_avg_address_dist_schools_sf.png"), width = 10, height = 8, dpi = 300)

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


