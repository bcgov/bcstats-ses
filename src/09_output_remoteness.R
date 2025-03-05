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
# Based on the DB/DA level data, we can calculate the CSD level average distance and drive time.
# Brian updated the data with DA id and randomly sampled addresses within DA strata in 2025-02-11. 
# We have two sets of data, one is the original data which do not have DA id and randomly sampled within locality id strata, and the other is the updated data.
# Previously, we added DA id into the original data using st_join function, and now we will combine the original data with the updated data.
# Therefore, we need to deal with potential duplicated row issues. 
# TODO: previous, when we drop the geometry from a sf object, we lost the coordinates columns. 
# We need to keep the coordinates columns when we drop the geometry column, or we need to add the coordinates columns back after we drop the geometry column from original data.
###################################################################


# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb, cancensus)
library(jsonlite)
library(cowplot) # For aligning multiple plots
library(patchwork )
# Load the rlang package for the bang-bang operator
library(rlang)

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
# we can jump to line 251
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
# 
# 
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

# servicbc/hospital/schools duplicates
# "1090A Main St Valemount BC"
# According to getodata team, we can check it using https://geocoder.api.gov.bc.ca/addresses.json?addressString=1090A%20Main%20St%20Valemount%20BC&outputSRS=3005
# and only keep the id 112681

new_da_servicebc_df2 %>% 
  filter(FULL_ADDRESS == "1090A Main St Valemount BC")

address_sf_with_da <- new_da_servicebc_df2 %>% 
  bind_rows(new_da_hospital_df) %>% 
  bind_rows(new_da_school_df) %>% 
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(ADDRESS_ALBERS_X = SITE_ALBERS_X,
         ADDRESS_ALBERS_Y = SITE_ALBERS_Y) %>% 
  st_as_sf(coords = c("SITE_ALBERS_X", "SITE_ALBERS_Y"), crs = 3005)




# What is the nan in TAG?
new_da_servicebc_df2 %>% 
  bind_rows(new_da_hospital_df) %>% 
  bind_rows(new_da_school_df) %>% 
  janitor::clean_names(case = "screaming_snake") %>% 
  count(TAG)


new_da_servicebc_df2 %>% 
  bind_rows(new_da_hospital_df) %>% 
  bind_rows(new_da_school_df) %>% 
  janitor::clean_names(case = "screaming_snake") %>% 
  filter(TAG == "nan")

new_da_servicebc_df2 %>% 
  bind_rows(new_da_hospital_df) %>% 
  bind_rows(new_da_school_df) %>% 
  janitor::clean_names(case = "screaming_snake") %>%
  rename(ADDRESS_ALBERS_X = SITE_ALBERS_X,
         ADDRESS_ALBERS_Y = SITE_ALBERS_Y)  %>% 
  write_csv(use_network_path("2024 SES Index/data/raw_data/remoteness/bc_address_with_da.csv"))

###########################################################################################
#   create a DA level summary. 
###########################################################################################

# Create a DA level summary table: average drive time and distance, and number of address. 



avg_dist_drvtime_by_da_service <- address_sf_with_da %>% 
  st_drop_geometry() %>% 
  group_by(DAID , TAG) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(DRV_TIME_SEC ),
    AVG_DRV_DIST = mean(DRV_DIST ),
    N_ADDRESS = n()
  )

avg_dist_drvtime_by_da_service %>% 
  filter(is.na(AVG_DRV_DIST)) %>%
  count(DAID) %>%
  glimpse()

avg_dist_drvtime_by_da_service %>% 
  write_csv(use_network_path("2024 SES Index/data/output/remoteness/avg_dist_drvtime_by_da_service.csv"))




###########################################################################################
#  To get how many DAs are missing. And create a CSD level summary. 
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
  group_by(MUN_NAME_2021, CSD_2021, DA_2021, DA_NUM) %>% 
  summarise(
    N_POSTALCODE = n()
  ) %>% 
  ungroup()


# Save to a new file this one has all the information but without the geometry column.
CSD_DA_address_dist_drvtime <- CSD_DA_list %>% 
  left_join(address_sf_with_da, by = join_by("DA_NUM" == "DAID") )


# create a sf object for the facility which will be used for plotting with the geometry column.
facility_sf = address_sf_with_da %>% 
  st_drop_geometry() %>%
  filter(!is.na(COORD_X)) %>%  # remove the rows without coordinates for facilities
  left_join(CSD_DA_list, by = join_by("DAID" == "DA_NUM") ) %>%
  count(TAG,MUN_NAME_2021, CSD_2021 , NEAREST_FACILITY, COORD_X ,COORD_Y) %>% 
  mutate(FACILITY_ALBERS_X = COORD_X,
         FACILITY_ALBERS_Y = COORD_Y) %>%
  st_as_sf(coords = c("COORD_X", "COORD_Y"), crs = 3005)

# check if there is duplication
# CSD_DA_list %>%
#   count(DA_NUM) %>%
#   filter(n>1)
# no it is fine.

# Left join table together



# a CSD level summary
CSD_REMOTENESS_BY_SERVICE = CSD_DA_address_dist_drvtime %>% 
  mutate(CDUID = str_sub(DA_NUM, 1, 4),
         CSDUID = str_c(CDUID, CSD_2021)) %>% 
  group_by(CSDUID,MUN_NAME_2021,TAG) %>% 
  summarise(
    AVG_DRV_TIME_SEC = mean(DRV_TIME_SEC, na.rm = T),
    AVG_DRV_DIST = mean(DRV_DIST, na.rm = T),
    N_ADDRESS =  n()
  ) %>% 
  ungroup()

CSD_REMOTENESS_BY_SERVICE %>% 
  filter(is.na(AVG_DRV_DIST)) %>%
  count(CSDUID) %>%
  glimpse()

CSD_REMOTENESS_BY_SERVICE %>% 
  write_csv("./out/CSD_REMOTENESS_BY_SERVICE.csv")

CSD_REMOTENESS_BY_SERVICE %>% 
  write_csv(use_network_path("2024 SES Index/data/output/remoteness/CSD_REMOTENESS_BY_SERVICE.csv"))



###########################################################################################
# Validation:
###########################################################################################
#   
#   Plot the results to visually validate that points are correctly joined to their respective dissemination areas:


address_avg_dist_da_shapefile <- da_shapefile %>% left_join(
  avg_dist_drvtime_by_da_service %>% mutate(DAUID = as.character(DAID)),
  join_by("DAUID")
)

library(ggplot2)
# add title to the ggplot object
address_avg_dist_da_servicebc_p <- ggplot(data = address_avg_dist_da_shapefile %>% filter(TAG == "servicebc")) +
  geom_sf(
    aes(fill = AVG_DRV_DIST),
    color = "gray"
  ) +
  scale_fill_viridis_c(option = "viridis") +
  geom_sf(
    data = address_sf_with_da %>% filter(TAG == "servicebc"),
    color = "red",
    size = 2
  ) +
  theme_minimal() +
  labs(title = "Families Addresses Distance to ServiceBC within Dissemination Areas")

print(address_avg_dist_da_servicebc_p)
# Save the plot
ggsave( "./out/address_servicebc_dist_with_da_map.png", address_avg_dist_da_servicebc_p, width = 10, height = 8, dpi = 300)
# the same addresses of families are plotted on the map, and the dissemination areas are shown in light blue.
# The red points represent the addresses of families that have been successfully joined to their respective dissemination areas.
# There are some DAs which still missing any addresses/red points.

address_avg_dist_da_shapefile %>% 
  filter(is.na(AVG_DRV_DIST)) %>%
  count(TAG)
  glimpse()

# 810 DAs are missing. The TAG is NA, so they are not joined to any addresses.
#    TAG   n                       geometry
# 1  nan 290 MULTIPOLYGON (((594038.4 86...
# 2 <NA> 520 MULTIPOLYGON (((603200.1 93...
# 290 rows have nan in TAG
  # 520 rows are missing in geodata team's file, but in the TMF. 
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
# Create a color map plot using ggplot2
################################################################


csd_avg_address_dist_sf = csd_shapefile %>% 
  left_join( CSD_REMOTENESS_BY_SERVICE ,
             join_by(CSDUID ) )
  
  
  test3 = csd_avg_address_dist_sf %>% 
    filter(MUN_NAME_2021 %in% c("Vancouver", "Smithers")) %>% 
    filter(!is.na(AVG_DRV_DIST))
  

csd_avg_address_dist_sf_servicebc_p <- ggplot(data = csd_avg_address_dist_sf %>% filter(TAG == 'servicebc')
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
# interesting these results weren't what I was as expecting for the least remote. assuming longest drive time = most remote, then Smithers is less remote than Vancouver by this definition.
# we can plot them in the map
################################################################

CSD_DA_avg_dist_drvtime_by_service_sf <- address_avg_dist_da_shapefile %>% 
  left_join(CSD_DA_list, by = join_by("DAID" == "DA_NUM") )

  
CSD = "Vancouver"
facility = "servicebc"
test4 <-  facility_sf %>% 
  filter(TAG == service) %>% 
  filter(MUN_NAME_2021 == CSD) 


########## 
#  A map plotting function for a CSD region
# plot DAs in a CSD region with the average distance to the nearest servicebc/schools/hospitals
##########  
# add a bright red dots for the facilities. 
# 
# To ensure that the same data values map to the same colors in both maps, 
# use identical fill scales (same limits, breaks, and palette) in both ggplot objects. 
# Then, when assemble them with patchwork, set the guides to "collect" so that a single shared legend is used. 

min_val<- 0 #range(CSD_DA_avg_dist_drvtime_by_service_sf$AVG_DRV_DIST, na.rm = T)[1]
max_val <- 12 #range(CSD_DA_avg_dist_drvtime_by_service_sf$AVG_DRV_DIST, na.rm = T)[2] 
step  <- 1.5

#' Plot a map for a given Census Subdivision (CSD) showing the average distance to a specific facility (e.g., servicebc, schools, hospitals).
#' 
#' This function generates a map for a specified Census Subdivision (CSD) that displays the average distance to a given facility (e.g., servicebc, schools, hospitals). The map includes the CSD boundaries, with each area colored based on the average distance to the service. Additionally, the locations of the facilityfacilities are marked with red dots. The map can optionally include labels showing the average distance values.
#' 
#' @param CSD Character string specifying the name of the Census Subdivision (CSD) to plot.
#' @param facility Character string specifying the type of facility to plot (e.g., "servicebc", "schools", "hospitals").
#' @param fill_var Character string specifying the variable name in the data frame that contains the average distance values.
#' @param fill_var_name Character string specifying the name of the variable to use in the legend and labels.
#' @param csd_sf Simple Features object containing the CSD boundaries and average distance data.
#' @param facility_sf Simple Features object containing the locations of the service facilities.
#' @param min_val Numeric value specifying the minimum value for the fill scale.
#' @param max_val Numeric value specifying the maximum value for the fill scale.
#' @param step Numeric value specifying the step size for the fill scale breaks.
#' @param label_var Logical value indicating whether to include labels showing the average distance values. Default is FALSE.
#' 
#' @return A ggplot object representing the map.
#' 
#' @examples
#' plot_a <- plot_csd_facility_map_fn(CSD = "Vancouver", 
#'                                    facility = "servicebc", 
#'                                    fill_var = "AVG_DRV_DIST", 
#'                                    fill_var_name = "Average distance",
#'                                    csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf, 
#'                                    facility_sf = facility_sf,  
#'                                    min_val = 0, max_val = 12, step = 1.5,
#'                                    label_var = FALSE)
#' print(plot_a)
plot_csd_facility_map_fn <- function(CSD, facility, fill_var,fill_var_name, csd_sf, facility_sf, min_val, max_val, step,  label_var = FALSE){
  
  csd_sf <- csd_sf %>% 
    filter(TAG == facility) %>% 
    filter(MUN_NAME_2021 == CSD)
  
  facility_sf <- facility_sf %>% 
    filter(TAG == facility) %>% 
    filter(MUN_NAME_2021 == CSD) 
  
  P <- csd_sf %>% 
    ggplot() +
    # geom_sf(aes_string(fill = fill_var), color = "white")+ # or use 
    geom_sf(aes(fill = !!sym(fill_var)))+
    scale_fill_viridis_c(limits = c(min_val, max_val), breaks = seq(min_val, max_val, by = step))
  
  if (label_var) {
    P <- P + geom_sf_label(aes(label = format(!!sym(fill_var), digits =2)), colour = "black")
  }
  
  P <- P +
    labs(
      subtitle = glue::glue("{fill_var_name} to the nearest {facility} in {CSD}"),
      x = "Longitude",
      y = "Latitude"
    ) + 
    my_map_theme+ 
    guides(fill=guide_legend(title=glue::glue("Avg. {stringr::str_to_title(fill_var_name)} to the {facility}")))
  
  P <- P + geom_point(
    data = facility_sf,aes(geometry = geometry),
    shape = 23,  
    color = "red",
    fill = "red",
    size = 5,
    alpha = 0.5,
    stat = "sf_coordinates"
  ) 
  
  return(P)
}

plot_a = plot_csd_facility_map_fn(CSD = "Vancouver", 
                     facility = "servicebc", 
                     fill_var = "AVG_DRV_DIST", fill_var_name = "Average distance",
                     csd_sf =CSD_DA_avg_dist_drvtime_by_service_sf, facility_sf ,  
                     min_val, max_val, step,
                     label_var = FALSE )

print(plot_a)
plot_b = plot_csd_facility_map_fn(CSD = "Smithers",
                     facility = "servicebc", 
                     fill_var = "AVG_DRV_DIST", fill_var_name = "Average distance",
                     csd_sf =CSD_DA_avg_dist_drvtime_by_service_sf, facility_sf ,  
                     min_val, max_val, step,
                     label_var = T )

print(plot_b)



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

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave(use_network_path("2024 SES Index/data/output/remoteness/two_csd_avg_address_dist_servicebc_sf_by_da.png"),
       plot = combined_plot,
       width = 14, height = 7, dpi = 300)


################################################################
# create a function for the map plot
#' Compare Two CSDs in a Map
#'
#' This function compares two Census Subdivisions (CSDs) by plotting their remoteness measurement on a map. It generates two separate plots for each CSD, displaying the average distance to a specified service (e.g., Service BC office), and combines them into a single plot with a shared title.
#'
#' @param CSD1 The name of the first Census Subdivision to compare.
#' @param CSD2 The name of the second Census Subdivision to compare.
#' @param facility The name of the facility to measure remoteness from (e.g., "servicebc").
#' @param fill_var The variable name in the csd_sf data frame that represents the remoteness measurement (e.g., "AVG_DRV_DIST").
#' @param fill_var_name A descriptive name for the fill_var variable to use in the plot legend.
#' @param csd_sf A simple features data frame containing the Census Subdivision boundaries and remoteness measurement data.
#' @param facility_sf A simple features data frame containing the facility locations.
#' @param label_var1 A logical indicating whether to label the first CSD plot with facility names.
#' @param label_var2 A logical indicating whether to label the second CSD plot with facility names.
#'
#' @return This function returns a combined plot of the two CSDs, displaying their remoteness measurement, and saves the plot to a file.
#' @export
compare_two_csd_in_map <- function(CSD1 = "Vancouver",
                                   CSD2 = "Smithers",
                                   facility = "servicebc",
                                   fill_var = "AVG_DRV_DIST",
                                   fill_var_name = "Average distance",
                                   csd_sf = CSD_DA_avg_dist_drvtime_by_service_sf,
                                   facility_sf = facility_sf ,
                                   label_var1 = F ,
                                   label_var2 = T) {
  color_range  <- csd_sf %>% 
    filter(MUN_NAME_2021 %in% c(CSD1, CSD2)) %>% 
    filter(!is.na(!!sym(fill_var))) %>% 
    filter(TAG %in% c(service)) %>% 
    pull(!!sym(fill_var)) %>% 
    range()
  
  min_val<- floor(color_range[1]) #
  
  max_val <- ceiling(color_range[2]) #
  step  <- (max_val - min_val)/(12-1)
  
  plot_a = plot_csd_facility_map_fn(
    CSD = CSD1,
    facility ,
    fill_var ,
    fill_var_name ,
    csd_sf ,
    facility_sf  ,
    min_val, max_val, step,
    label_var = FALSE
  )
  
  print(plot_a)
  plot_b = plot_csd_facility_map_fn(
    CSD = CSD2,
    facility ,
    fill_var ,
    fill_var_name ,
    csd_sf ,
    facility_sf  ,
    min_val, max_val, step,
    label_var = T
  )
  
  print(plot_b)
  
  # Combine the two plots using patchwork and add a shared title
  combined_plot <- (
    plot_a + plot_b +
      plot_layout(ncol = 2, guides = "collect") &
      theme(legend.position = "bottom")
  ) +
    plot_annotation(
      title = glue::glue("Remoteness measurement: {CSD1} vs {CSD2}"),
      theme = theme(plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5
      ))
    )
  
  # Display the combined plot
  print(combined_plot)
  
  # Save the combined plot
  ggsave(
    use_network_path(
      glue::glue(
        "2024 SES Index/data/output/remoteness/two_csd_{CSD1}_{CSD2}_avg_address_dist_{facility}_by_da.png"
      )
    ),
    plot = combined_plot,
    width = 14,
    height = 7,
    dpi = 300
  )
}


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


