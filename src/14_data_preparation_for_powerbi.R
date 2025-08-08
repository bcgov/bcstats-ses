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

library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(glue)
library(dplyr)
library(dbplyr)
library(datadictionary)
source("./src/utils.R") # Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()

# https://www.statcan.gc.ca/en/subjects/standard/sgc/2021/index
sgc_element <- readr::read_csv(
  "https://www.statcan.gc.ca/en/statistical-programs/document/sgc-cgt-2021-element-eng.csv"
)
sgc_structure <- readr::read_csv(
  "https://www.statcan.gc.ca/en/statistical-programs/document/sgc-cgt-2021-structure-eng.csv"
)

sgc_structure |>
  filter(
    `Hierarchical structure` == "Province and territory"
  ) |>
  glimpse()

sgc_structure_cd <- sgc_structure |>
  filter(
    `Hierarchical structure` == "Census division"
  ) |>
  rename(
    CD_ID = Code,
    CD_NAME = `Class title`
  ) |>
  mutate(CD_ID = as.character(CD_ID))

sgc_structure_csd <- sgc_structure |>
  filter(
    `Hierarchical structure` == "Census subdivision"
  ) |>
  rename(
    CSD_ID = Code,
    CSD_NAME = `Class title`
  ) |>
  mutate(
    CD_ID = stringr::str_sub(CSD_ID, start = 1, end = 4),
    PROVINCE_ID = stringr::str_sub(CSD_ID, start = 1, end = 2)
  )


sgc_structure_csd_BC <- sgc_structure_csd |>
  left_join(
    sgc_structure_cd |>
      select(CD_ID, CD_NAME)
  ) |>
  filter(PROVINCE_ID == "59")

sgc_structure_csd_BC |>
  readr::write_csv(file.path(
    bc_ses_project_lan_path,
    "2024 SES Index\\data\\other\\StatsCAN_sgc\\sgc_structure_csd_BC.csv"
  ))


library(bcmaps)
# The package also contains a few handy utility functions:
#
#   transform_bc_albers() for transforming any sf object to BC Albers projection
# bc_area() to get the total area of British Columbia in various units
# bc_bbox() to get an extend/bounding box for British Columbia

library(geojsonio)
library(sf)
library(dplyr)

# Load the TopoJSON file
chsa <- geojson_read(
  file.path(
    bc_ses_project_lan_path,
    "2024 SES Index\\data\\other\\StatsCAN_sgc\\chsa_2022_wgs.json"
  ),
  what = "sp"
)

# Convert to sf object
chsa_sf <- st_as_sf(chsa)

# Define Lower Mainland CHSA names
lower_mainland_chsas <- c(
  "Downtown Vancouver",
  "West End",
  "Fairview",
  "Downtown Eastside",
  "Northeast False Creek",
  "Grandview-Woodland",
  "Cedar Cottage",
  "Hastings-Sunrise",
  "Renfrew-Collingwood",
  "Shaughnessy/Arbutus Ridge/Kerrisdale",
  "West Point Grey/Dunbar-Southlands",
  "University of British Columbia",
  "Kitsilano",
  "Kensington",
  "Mount Pleasant",
  "South Cambie/Riley Park",
  "Killarney",
  "Oakridge/Marpole",
  "Sunset",
  "Victoria-Fraserview",
  "Brentwood/Willingdon/Parkcrest",
  "Burnaby Mountain/Lougheed",
  "Garden Village/Cascade/Douglas/Gilpin",
  "Buckingham/Lakeview/Cariboo/Second Street",
  "Burnaby Heights/Capital Hill",
  "Westridge/Sperling/Gov't Road",
  "Kingsway/Edmonds",
  "South Slope/Big Bend",
  "Metrotown/Marlborough/Windsor",
  "Richmond City Centre",
  "Blundell",
  "Broadmoor",
  "Steveston",
  "East and West Cambie/Bridgeport",
  "Thompson/Seafair",
  "Gilmore/Shellmont/East/Hamilton",
  "Cloverdale",
  "Panorama",
  "East Newton",
  "Fleetwood",
  "Guildford",
  "West Newton",
  "North Surrey",
  "Whalley",
  "South Surrey East",
  "South Surrey West",
  "North Delta",
  "Tsawwassen",
  "Ladner",
  "New Westminster - East",
  "New Westminster - Downtown",
  "New Westminster - West/Queensborough",
  "New Westminster - Central",
  "North Vancouver City - East",
  "North Vancouver City - West",
  "North Vancouver DM - Central",
  "North Vancouver DM - East",
  "North Vancouver DM - West",
  "West Vancouver - Lower",
  "West Vancouver - Upper",
  "North Coquitlam",
  "Southwest Coquitlam",
  "Southeast Coquitlam",
  "Port Coquitlam",
  "Port Moody South",
  "Port Moody North/Anmore/Belcarra",
  "City of Langley",
  "Willoughby",
  "Brookswood/Murrayville",
  "Aldergrove/Otter",
  "North Langley Township",
  "South Langley Township",
  "Walnut Grove/Fort Langley",
  "Haney",
  "Pitt Meadows",
  "Maple Ridge Rural",
  "North Mission",
  "South Mission",
  "Central Abbotsford",
  "East Abbotsford",
  "West Abbotsford",
  "Abbotsford Rural",
  "South Chilliwack",
  "North Chilliwack"
)

# Filter the CHSAs
filtered_chsa <- chsa_sf %>%
  filter(CHSA_Name %in% lower_mainland_chsas)

geojson_data <- geojson_json(filtered_chsa)
topojson_data <- geo2topo(geojson_data)

writeLines(
  topojson_data,
  file.path(
    bc_ses_project_lan_path,
    "2024 SES Index\\data\\other\\StatsCAN_sgc\\lower_mainland_chsa.json"
  )
)

# Export to TopoJSON
geojson_write(
  filtered_chsa,
  file = file.path(
    bc_ses_project_lan_path,
    "2024 SES Index\\data\\other\\StatsCAN_sgc\\lower_mainland_chsa.topojson"
  ),
  convert_to_topojson = TRUE
)


# Save as TopoJSON
geojson_write(
  filtered_chsa,
  file = file.path(
    bc_ses_project_lan_path,
    "2024 SES Index\\data\\other\\StatsCAN_sgc\\lower_mainland_chsa.topo.json"
  ),
  convert_to_topojson = TRUE
)


# Save as GeoJSON (Power BI compatible)
st_write(
  filtered_chsa,
  file.path(
    bc_ses_project_lan_path,
    "2024 SES Index\\data\\other\\StatsCAN_sgc\\lower_mainland_chsa.geojson"
  ),
  driver = "GeoJSON"
)
