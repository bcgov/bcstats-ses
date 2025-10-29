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
library(bcmaps)
library(geojsonio)
library(sf)
# The package also contains a few handy utility functions:
##   transform_bc_albers() for transforming any sf object to BC Albers projection
# bc_area() to get the total area of British Columbia in various units
# bc_bbox() to get an extend/bounding box for British Columbia
source("./src/utils.R") # Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()

bc_ses_project_lan_path = config$lan_path

#################################################################
# # get csd map
# # bc_csd_map <- bcmaps::census_subdivision(ask = interactive(), force = FALSE)

# # create a indigenous CSD flag
# # •	Exclude indigenous CSD and CHSA,
# # •	Flag indigenous CSD
# # # In Canada, a census subdivision (CSD) with a CSD_Type value of "IRI" represents an Indian reserve / Réserve indienne. If the CSD_Type value is "NL", it stands for Newfoundland and Labrador community (local service district). These designations are used by Statistics Canada to classify different types of census subdivisions within the Canadian geographic hierarchy
# #
# bc_csd_map |>
#   # remove the geometry column
#   st_drop_geometry(-geometry) |>
#   select(
#     CENSUS_SUBDIVISION_ID,
#     CENSUS_SUBDIVISION_NAME,
#     CENSUS_SUBDIVISION_TYPE_CODE,
#     CENSUS_DIVISION_ID,
#     CENSUS_DIVISION_NAME,
#     CENSUS_METRO_AREA_ID,
#     CENSUS_METRO_AREA_NAME,
#     ECONOMIC_REGION_ID,
#     ECONOMIC_REGION_NAME,
#     ING_CSD_FLAG,
#     REGION_NAME
#   ) |>
#   write_csv(
#     file.path(
#       bc_ses_project_lan_path,
#       "2024 SES Index\\data\\other\\StatsCAN_sgc\\bc_csd_lookup.csv"
#     ),
#   )
# bc_csd_map |> plot()
# ###########################################################################
# # bcmaps csd map is too simplified. try statscan's shp file or topojson
# bc_csd_map <- st_read(
#   file.path(
#     bc_ses_project_lan_path,
#     "2024 SES Index\\data\\other\\StatsCAN_sgc\\lcsd000b21a_e\\lcsd000b21a_e.shp"
#   ),
#   quiet = TRUE
# ) |>
#   filter(PRUID == "59")

# bc_csd_map <- bc_csd_map |>
#   mutate(
#     ING_CSD_FLAG = case_when(
#       !CENSUS_SUBDIVISION_TYPE_CODE %in% c("IRI", "NL") ~ "Non-indigenous",
#       TRUE ~ "Indigenous"
#     )
#   ) |>
#   mutate(
#     REGION_NAME = CENSUS_SUBDIVISION_NAME
#   )

# bc_csd_map |> glimpse()
# bc_csd_map |>
#   filter(PRUID == "59") |>
#   plot()

# #
# # # Transform to WGS84 (EPSG:4326) for web compatibility
# bc_csd_map_wgs84 <- bc_csd_map |>
#   filter(PRUID == "59") |>
#   st_transform(4326) |>
#   # Simplify geometry to reduce file size and improve Power BI performance
#   # st_simplify(dTolerance = 0.001) |>
#   # Ensure valid geometries
#   st_make_valid()

# bc_csd_map_wgs84 |> plot()
# #
# # # Save as GeoJSON first for Power BI compatibility
# st_write(
#   bc_csd_map_wgs84,
#   file.path(
#     bc_ses_project_lan_path,
#     "2024 SES Index\\data\\other\\StatsCAN_sgc\\bc_csd.geojson"
#   ),
#   driver = "GeoJSON",
#   delete_dsn = TRUE
# )

# None of them work, so have to upload to mapreshaper.org to do it.

##########################################
# Load the TopoJSON file
# https://open.canada.ca/data/en/dataset/68f2f577-28a7-46b4-bca9-7e9770f2f357
chsa <- geojson_read(
  file.path(
    bc_ses_project_lan_path,
    "2024 SES Index\\data\\other\\StatsCAN_sgc\\chsa_2022_wgs.json"
  ),
  what = "sp"
)

# Convert to sf object
chsa_sf <- st_as_sf(chsa)

chsa_sf |> glimpse()

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
#
#
# # Save as GeoJSON (Power BI compatible)
# st_write(
#   filtered_chsa,
#   file.path(
#     bc_ses_project_lan_path,
#     "2024 SES Index\\data\\other\\StatsCAN_sgc\\lower_mainland_chsa_test.geojson"
#   ),
#   driver = "GeoJSON"
# )

# look up table from CHSA to HA

library(bcdata)
bcdc_search("community-health-service-areas-boundaries")
chsa = bcdc_get_record("68f2f577-28a7-46b4-bca9-7e9770f2f357")
chsa_tidy = bcdc_tidy_resources('68f2f577-28a7-46b4-bca9-7e9770f2f357')
chsa_tbl = bcdc_get_data(
  '68f2f577-28a7-46b4-bca9-7e9770f2f357',
  resource = "874aa151-afe6-400c-876c-aef1ce55102e"
)


chsa_tbl |> glimpse()

#######################
##########################################################
# create a long format table by staking CSD and CHSA level indices
data_path <- file.path(
  bc_ses_project_lan_path,
  "2024 SES Index/exports/2025-07-31-initial-index-results"
)

index_data_path <- list.files(data_path, full.names = TRUE) |>
  as.data.frame() |>
  filter(str_detect(
    `list.files(data_path, full.names = TRUE)`,
    "weighted-scores"
  ))

index_data_df_ls <- list()

for (i in 1:nrow(index_data_path)) {
  csv_file <- index_data_path[i, 1]
  file_name <- basename(csv_file)

  # Read the CSV file
  df <- read_csv(
    file.path(
      csv_file
    ),
    col_types = cols(
      AVERAGE_AGE = col_double(),
      POPULATION_ESTIMATE = col_double(),
      TOTAL_INDEX_0_100 = col_double(),
      SEI_INDEX_0_100 = col_double(),
      ECON_0_100 = col_double(),
      EDUC_0_100 = col_double(),
      HEALTH_0_100 = col_double(),
      COMMUNITY_0_100 = col_double()
    )
  )

  # Add model_type flag
  df$MODEL_TYPE <- ifelse(
    str_detect(file_name, "longitudinal"),
    "Longitudinal",
    ifelse(str_detect(file_name, "robust"), "Detail", NA)
  )

  # Add region_level flag
  df$REGION_LEVEL <- ifelse(
    str_detect(file_name, "csd"),
    "CSD",
    ifelse(str_detect(file_name, "chsa"), "CHSA", NA)
  )

  # Rename columns
  names(df)[str_detect(names(df), "UID")] <- "UID"
  names(df)[str_detect(names(df), "_NAME")] <- "REGION_NAME"

  index_data_df_ls[[i]] <- df
}

index_data_combined <- bind_rows(index_data_df_ls)

index_data_combined |>
  filter(UID == 5909815) |>
  glimpse()

index_data_combined |>
  pivot_longer(
    cols = contains("_0_100"),
    names_to = "INDEX_TYPE",
    values_to = "INDEX_VALUE"
  ) |>
  filter(is.na(REGION_NAME)) |>
  glimpse()

index_data_combined |>
  pivot_longer(
    cols = contains("_0_100"),
    names_to = "INDEX_TYPE",
    values_to = "INDEX_VALUE"
  ) |>
  mutate(
    INDEX_LABEL = case_when(
      INDEX_TYPE == "TOTAL_INDEX_0_100" ~ "Total Index",
      INDEX_TYPE == "SEI_INDEX_0_100" ~ "SEI Index",
      INDEX_TYPE == "ECON_0_100" ~ "Economy",
      INDEX_TYPE == "EDUC_0_100" ~ "Education",
      INDEX_TYPE == "HEALTH_0_100" ~ "Health",
      INDEX_TYPE == "COMMUNITY_0_100" ~ "Community",
      TRUE ~ INDEX_TYPE
    )
  ) |>
  filter(!is.na(REGION_NAME)) |>
  filter(!is.na(INDEX_VALUE)) |>
  write_csv(file.path(
    data_path,
    "long_format_index_all.csv"
  ))

index_data_combined |>
  pivot_longer(
    cols = contains("_0_100"),
    names_to = "INDEX_TYPE",
    values_to = "INDEX_VALUE"
  ) |>
  mutate(
    INDEX_LABEL = case_when(
      INDEX_TYPE == "TOTAL_INDEX_0_100" ~ "Total Index",
      INDEX_TYPE == "SEI_INDEX_0_100" ~ "SEI Index",
      INDEX_TYPE == "ECON_0_100" ~ "Economy",
      INDEX_TYPE == "EDUC_0_100" ~ "Education",
      INDEX_TYPE == "HEALTH_0_100" ~ "Health",
      INDEX_TYPE == "COMMUNITY_0_100" ~ "Community",
      TRUE ~ INDEX_TYPE
    )
  ) |>
  filter(!is.na(REGION_NAME)) |>
  filter(!is.na(INDEX_VALUE)) |>
  distinct(MODEL_TYPE, REGION_LEVEL, CALENDAR_YEAR, REGION_NAME) |>
  write_csv(file.path(
    data_path,
    "long_format_index_distinct_MODEL_TYPE_REGION_LEVEL_CALENDAR_YEAR_REGION_NAME_filter_b.csv"
  ))


#################################################################
# since we have a long format index data frame, we also need a long format data frame for contribution, so we can link two dataframe together.
index_contribution_data_path <- list.files(data_path, full.names = TRUE) |>
  as.data.frame() |>
  filter(str_detect(
    `list.files(data_path, full.names = TRUE)`,
    "contribution-from-factors"
  ))

index_contribution_data_df_ls <- list()

for (i in 1:nrow(index_contribution_data_path)) {
  csv_file <- index_contribution_data_path[i, 1]
  file_name <- basename(csv_file)

  # Read the CSV file
  df <- read_csv(
    csv_file
  ) %>%
    mutate(
      across(ends_with(c("UID", "NAME", "YEAR", "NAME_2021")), as.character),
      across(!ends_with(c("UID", "NAME", "YEAR", "NAME_2021")), as.numeric)
    ) %>%
    pivot_longer(
      cols = ends_with("_0_100"),
      names_to = "INDEX_TYPE",
      values_to = "TOTAL"
    ) %>%
    mutate(
      "DEVIATION_CONTRIBUTION_BENCHMARK" = 50
    ) |>
    pivot_longer(
      cols = starts_with("DEVIATION_CONTRIBUTION_"),
      names_to = "FACTOR",
      values_to = "CONTRIBUTION_VALUE"
    ) %>%
    mutate(
      FACTOR = str_remove(FACTOR, "DEVIATION_CONTRIBUTION_")
    )

  # Add model_type flag
  df$MODEL_TYPE <- ifelse(
    str_detect(file_name, "longitudinal"),
    "Longitudinal",
    ifelse(str_detect(file_name, "robust"), "Detail", NA)
  )

  # Add region_level flag
  df$REGION_LEVEL <- ifelse(
    str_detect(file_name, "csd"),
    "CSD",
    ifelse(str_detect(file_name, "chsa"), "CHSA", NA)
  )

  # Rename columns
  names(df)[str_detect(names(df), "UID")] <- "UID"
  names(df)[str_detect(names(df), "_NAME")] <- "REGION_NAME"

  index_contribution_data_df_ls[[i]] <- df
}

index_contribution_data_combined <- bind_rows(index_contribution_data_df_ls)

index_contribution_data_combined |>
  filter(UID == 5909815) |>
  glimpse()

index_contribution_data_combined |>
  filter(!is.na(REGION_NAME)) |>
  filter(!is.na(CONTRIBUTION_VALUE)) |>
  glimpse()


#################################################################
# need a better name for those factors. load from a data dictionary
# Load data dictionary
longitudinal_model_input_data_dictionary <- read_csv(file.path(
  bc_ses_project_lan_path,
  "2024 SES Index/exports/2025-07-31-initial-index-results",
  "longitudinal_model_input_data_dictionary_2025-07-21.csv"
))

detail_model_input_data_dictionary <- read_csv(file.path(
  bc_ses_project_lan_path,
  "2024 SES Index/exports/2025-07-31-initial-index-results",
  "robust_model_input_data_dictionary_2025-07-21.csv"
))

data_dictionary <- bind_rows(
  longitudinal_model_input_data_dictionary,
  detail_model_input_data_dictionary
) %>%
  distinct()

# Create a new column 'Short Label' for easier plotting
data_dictionary <- data_dictionary %>%
  mutate(
    `Short Label` = case_when(
      `Variable Name` == "ECON_DEPENDENCY_RATIO" ~ "Dependency Ratio",
      `Variable Name` == "ECON_HOUSING_SPENDING_PCT" ~ "Housing Spend Pct",
      `Variable Name` == "ECON_INCOME_ASSISTANCE_PCT" ~ "Income Assist %",
      `Variable Name` == "ECON_MEDIAN_HH_INCOME" ~ "Median HH Income",
      `Variable Name` == "ECON_UNEMPLOYMENT_RATE_PCT" ~ "Unemployment %",
      `Variable Name` == "EDUC_NO_CERTIFICATE_PCT" ~ "No Certificate %",
      `Variable Name` == "HEALTH_LIFE_EXPECTANCY" ~ "Life Expectancy",
      `Variable Name` == "COMMUNITY_CRIME_RATE" ~ "Crime Rate",
      `Variable Name` == "ECON_INC_BT_HHS_MED" ~ "Median HH Income (Census)",
      `Variable Name` == "EDUC_NO_HIGHSCH_RATIO" ~ "No High School Pct",
      `Variable Name` == "EDUC_HIGHSCH_RATIO" ~ "High School Only Pct",
      `Variable Name` == "EDUC_POSTSEC_RATIO" ~ "Post-Secondary Pct",
      `Variable Name` == "ECON_LABOUR_EMPL_RT" ~ "Employment Rate (Census)",
      `Variable Name` == "ECON_LABOUR_UNEM_RT" ~ "Unemployment Rate (Census)",
      `Variable Name` == "ECON_HOME_OWN_RENT_RATIO" ~ "Home Owner/Renter Ratio",
      `Variable Name` == "ECON_REPAIRS_MAJOR_RATIO" ~ "Major Repairs Needed Pct",
      `Variable Name` == "COMMUNITY_SING_PARENT_RATIO" ~ "Single Parent Families Pct",
      `Variable Name` == "COMMUNITY_AVG_LONE_PARENT_AT_BIRTH" ~ "Lone Parent at Birth Pct",
      `Variable Name` == "HEALTH_ASTHMA_RATE" ~ "Asthma Rate",
      `Variable Name` == "HEALTH_DIABETES_RATE" ~ "Diabetes Rate",
      `Variable Name` == "HEALTH_HYPERTENSION_RATE" ~ "Hypertension Rate",
      `Variable Name` == "HEALTH_OSTEOARTHRITIS_RATE" ~ "Osteoarthritis Rate",
      `Variable Name` == "HEALTH_MOOD_ANX_RATE" ~ "Mood/Anxiety Rate",
      `Variable Name` == "HEALTH_PCT_PAID" ~ "Paid Healthcare Pct",
      `Variable Name` == "EDUC_COURSE_MARK_PERCENT_OF_A_ENGLISH_LANGUAGE_ARTS" ~ "Grade A English Pct",
      `Variable Name` == "EDUC_COURSE_MARK_PERCENT_OF_A_MATHEMATICS" ~ "Grade A Math Pct",
      `Variable Name` == "EDUC_COURSE_MARK_PERCENT_OF_A_SOCIAL_STUDIES" ~ "Grade A Social Studies Pct",
      `Variable Name` == "EDUC_COURSE_MARK_PERCENT_OF_A_SCIENCES" ~ "Grade A Science Pct",
      `Variable Name` == "EDUC_FSA_AVG_SCORE_PERCENT_04_RE" ~ "FSA G4 Reading Score",
      `Variable Name` == "EDUC_FSA_AVG_SCORE_PERCENT_04_NU" ~ "FSA G4 Numeracy Score",
      `Variable Name` == "EDUC_FSA_AVG_SCORE_PERCENT_07_RE" ~ "FSA G7 Reading Score",
      `Variable Name` == "EDUC_FSA_AVG_SCORE_PERCENT_07_NU" ~ "FSA G7 Numeracy Score",
      `Variable Name` == "EDUC_ECC_GRADE12_GRADUATION_RATE" ~ "G12 Graduation Rate",
      `Variable Name` == "COMMUNITY_CYIC_PER_1000" ~ "Children in Care Rate",
      `Variable Name` == "COMMUNITY_TA_PROP" ~ "Temp. Assistance Pct",
      `Variable Name` == "COMMUNITY_DA_PROP" ~ "Disability Assistance Pct",
      `Variable Name` == "COMMUNITY_ALL_CRIME_EXCEPT_TRAFFIC_RATE_PER_100000" ~ "Crime Rate (non-traffic)",
      `Variable Name` == "COMMUNITY_HOMICIDE_RATE_PER_100000" ~ "Homicide Rate",
      `Variable Name` == "ECON_POP_PCT_CHANGE" ~ "Population Growth Pct",
      `Variable Name` == "ECON_OCC_MGMT_RATIO" ~ "Management Occupations Pct",
      `Variable Name` == "ECON_OCC_NAT_APP_SCI_RATIO" ~ "Science Occupations Pct",
      `Variable Name` == "ECON_OCC_HLTH_RATIO" ~ "Health Occupations Pct",
      `Variable Name` == "ECON_LIM_AT_PREVALENCE" ~ "Low Income Pct (LIM-AT)",
      `Variable Name` == "ECON_MEDIAN_INCOME_MEDIAN" ~ "Median Income (Finance)",
      `Variable Name` == "ECON_LFS_UNEMPLOYMENT_RATE_MEAN" ~ "Unemployment Rate (LFS)",
      `Variable Name` == "ECON_LFS_EMPLOYMENT_RATE_MEAN" ~ "Employment Rate (LFS)",
      `Variable Name` == "ECON_MEDIAN_HOUSE_TOTAL_VALUE" ~ "Median House Value",
      `Variable Name` == "EDUC_SLS_POSITIVE_NEGATIVE_RESPONSE_PCT_NUTRI_BREAKFAST" ~ "SLS: Had Breakfast Pct",
      `Variable Name` == "EDUC_SLS_POSITIVE_NEGATIVE_RESPONSE_PCT_LIKE_SCHOOL" ~ "SLS: Like School Pct",
      `Variable Name` == "EDUC_SLS_POSITIVE_NEGATIVE_RESPONSE_PCT_FEEL_WELCOME" ~ "SLS: Feel Welcome Pct",
      TRUE ~ `Variable Name` # Fallback to variable name if no match
    )
  )
data_dictionary |> rename(FACTOR = `Variable Name`, FACTOR_LABEL = `Short Label`) |> write_csv(file.path(data_path, "data_dictionary.csv"))

# Join data dictionary to contribution data to get better factor names
index_contribution_data_combined <- index_contribution_data_combined %>%
  left_join(
    data_dictionary %>%     
      select(`Variable Name`, `Description`, `Short Label`) %>%
      rename(FACTOR = `Variable Name`, FACTOR_LABEL = `Short Label`),
    by = "FACTOR"
  ) |>
  mutate(
    FACTOR_LABEL = case_when(
      FACTOR == 'BENCHMARK' ~ "BC Benchmark",
      FACTOR == 'NORMALIZED_HEALTH' ~ "Health",
      FACTOR == 'NORMALIZED_EDUC' ~ "Education",
      FACTOR == 'NORMALIZED_ECON' ~ "Economy",
      FACTOR == 'NORMALIZED_COMMUNITY' ~ "Community",
      T ~ FACTOR_LABEL
    )
  )

index_contribution_data_combined <- index_contribution_data_combined %>%
  mutate(
    INDEX_LABEL = case_when(
      INDEX_TYPE == "TOTAL_INDEX_0_100" ~ "Total Index",
      INDEX_TYPE == "SEI_INDEX_0_100" ~ "SEI Index",
      INDEX_TYPE == "ECON_0_100" ~ "Economy",
      INDEX_TYPE == "EDUC_0_100" ~ "Education",
      INDEX_TYPE == "HEALTH_0_100" ~ "Health",
      INDEX_TYPE == "COMMUNITY_0_100" ~ "Community",
      TRUE ~ INDEX_TYPE
    )
  )

index_contribution_data_combined |>
  filter(!is.na(REGION_NAME)) |>
  filter(!is.na(CONTRIBUTION_VALUE)) |>
  write_csv(file.path(
    data_path,
    "long_format_index_contribution_data_combined_all.csv"
  ))
#################################################################
# create a long format data frame for all input factor values
index_factor_values_data_path <- list.files(data_path, full.names = TRUE) |>
  as.data.frame() |>
  filter(str_detect(
    `list.files(data_path, full.names = TRUE)`,
    "weighted-factors"
  ))

index_factor_values_data_df_ls <- list()

for (i in 1:nrow(index_factor_values_data_path)) {
  csv_file <- index_factor_values_data_path[i, 1]
  file_name <- basename(csv_file)

  # Read the CSV file
  df <- read_csv(
    csv_file
  ) %>%
    mutate(
      across(ends_with(c("UID", "NAME", "YEAR", "NAME_2021")), as.character),
      across(!ends_with(c("UID", "NAME", "YEAR", "NAME_2021")), as.numeric)
    ) %>%
    pivot_longer(
      cols = starts_with(c("ECON", "EDUC", "HEALTH", "COMMUNITY")),
      names_to = "FACTOR",
      values_to = "FACTOR_VALUE"
    )

  # Add model_type flag
  df$MODEL_TYPE <- ifelse(
    str_detect(file_name, "longitudinal"),
    "Longitudinal",
    ifelse(str_detect(file_name, "robust"), "Detail", NA)
  )

  # Add region_level flag
  df$REGION_LEVEL <- ifelse(
    str_detect(file_name, "csd"),
    "CSD",
    ifelse(str_detect(file_name, "chsa"), "CHSA", NA)
  )

  # Rename columns
  names(df)[str_detect(names(df), "UID")] <- "UID"
  names(df)[str_detect(names(df), "_NAME")] <- "REGION_NAME"

  index_factor_values_data_df_ls[[i]] <- df
}

index_factor_values_data_combined <- bind_rows(index_factor_values_data_df_ls)


# Join data dictionary to contribution data to get better factor names
index_factor_values_data_combined <- index_factor_values_data_combined %>%
  left_join(
    data_dictionary %>%
      select(`Variable Name`, `Description`, `Short Label`) %>%
      rename(FACTOR = `Variable Name`, FACTOR_LABEL = `Short Label`),
    by = "FACTOR"
  )

index_factor_values_data_combined |>
  filter(UID == 5909815) |>
  glimpse()

index_factor_values_data_combined |>
  filter(!is.na(REGION_NAME)) |>
  filter(!is.na(FACTOR_VALUE)) |>
  glimpse()

index_factor_values_data_combined |>
  filter(!is.na(REGION_NAME)) |>
  filter(!is.na(FACTOR_VALUE)) |>
  write_csv(file.path(
    data_path,
    "long_format_index_factor_values_data_combined_all.csv"
  ))


###################################################################
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

#########################################################################################
#
