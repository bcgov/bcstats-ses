# this file is used for outputting data for importing to DIP

pacman::p_load(
  cancensus,
  geojsonsf,
  tidyverse,
  config,
  bcmaps,
  bcdata,
  janitor,
  cansim,
  safepaths,
  arrow,
  duckdb
)
library(dplyr)
library(datadictionary)
library(readr)
library(readxl)
######################################################################################
#
# Translation Master File: a table with different levels of geography to link the data
# https://www2.gov.bc.ca/assets/gov/health/forms/5512datadictionary.pdf
# TRANSLATION MASTER FILE – GEOCODES: DATA DICTIONARY
# Census Divisions (CD) are the principal geographic areas for the country-wide census and are created by Statistics
# Canada. There are currently 28 CDs in B.C. (2011 Census). Census Division boundaries in Canada follow counties,
# regional districts, regional municipalities, and five other types of geographic areas made up of groups of Census
# Subdivisions (CSDs). In British Columbia the CD is comprised of one unorganized region (Stikine) and 27 Regional
# Districts which have local government functions. CD boundaries follow Regional Districts so they are affected by
# Regional District boundary changes, such as extensions, reductions, name changes, and newly created Regional
# Districts.
# Census Subdivisions (CSD) aggregate to Census Divisions (CDs), which in turn aggregate to a province or territory
# (the province code for British Columbia is 59). This relationship is reflected in the 7-digit Standard Geographical
# Classification code (SGC). The TMF SGC is slightly different from the Statistics Canada SGC due to the exclusion of the
# province code. Therefor the TMF value for Census Division and Subdivision is referred to as the two digit CD code and
# a three digit CSD code.
# it is important to note that not all of the 837 CSD’s in the province will be represented on the TMF, as the appearance of
# a CSD on the TMF is dependent upon whether or not there is a postal code geocoded to that area. A large number of
# the RDEA’s and the vast majority of IRs are not represented on the TMF

# Census Subdivision (CSD) CSDs are municipalities (as determined by provincial legislation) or areas treated as municipal equivalents
#
######################################################################################

# The GCS 202406 csv file is provided by Econ team and saved in LAN. Need safe network path to get it.
stopifnot(Sys.getenv("SAFEPATHS_NETWORK_PATH") != "")

TMF_file <- use_network_path("2024 SES Index/data/raw_data/TMF/GCS_202406.csv")

TMF <- read_csv(TMF_file)


# standardize the DA number, append the prefix BC code 59, so it is easy to join to other tables.
TMF <- TMF %>%
  mutate(DA_NUM = as.numeric(str_c("59", CD_2021, DA_2021, sep = "")))

# TMF_names = TMF %>% names() %>% paste(collapse = ",")

# clean the names, one name is not upper-cased. We prefer all uppercase
TMF <-
  TMF %>%
  janitor::clean_names(case = "screaming_snake")

# 1. BC Translation_Master_File
TMF %>% readr::write_csv(here::here("out", "Translation_Master_File_DIP.csv"))

#################################################################################################
# Data dictionary
#################################################################################################
# 2. Create a dictionary for BC Translation_Master_File

TMF <- TMF %>%
  mutate(across(
    .cols = c(
      PROV,
      SOURCE,
      ACTIVE # leave other ids as number since there are too many items/elements in those factors.
    ),
    .fns = as.factor
  ))

TMF_dict = create_dictionary(TMF, id_var = "POSTALCODE", var_labels = NULL)

# f2 <- "https://www2.gov.bc.ca/assets/gov/health/forms/5512datadictionary.pdf"
# a better data dictionary in Geocoding Self-Service (GCS) User Guide Prepared by BC Stats March 2020, not online, provided by Econ team
# f3 <-  use_network_path("docs/GCS_User_Guide.pdf")

# manually create a item field in this detail dataframe to join the TMF_dict dataframe

TMF_dict_detail <- read_csv(use_network_path("docs/TMF_data_dict.csv"))
View(TMF_dict_detail)


create_item <- function(x) {
  # Use case_match for value mapping
  case_match(
    x,
    "Postal Code" ~ "POSTALCODE",
    "Birth Date" ~ "BIRTH_DATE",
    "Retired Date" ~ "RET_DATE",
    "Latitude" ~ "LATITUDE",
    "Longitude" ~ "LONGITUDE",
    "Census Division" ~ "CD",
    "Census Subdivision" ~ "CSD",
    "Census Subdivision Name" ~ "MUN_NAME",
    "Census Metropolitan Area or Census Agglomeration Area" ~ "CMACA",
    "Dissemination Area (DA)" ~ "DA",
    "Census Tract (CT)" ~ "CT",
    "Dissemination Block (DB)" ~ "DB",
    "Designated Place Name (DPL)" ~ "DPL",
    "Population Centre (POPCTR)" ~ "POPCTR",
    "Development Region (DR)" ~ "DR",
    "Health Authority (HA)" ~ "HA",
    "Health Service Delivery Area (HSDA)" ~ "HSDA",
    "Local Health Area (LHA)" ~ "LHA",
    "Community Health Service Area (CHSA)" ~ "CHSA",
    "Pre-2018 Local Health Area" ~ "LHA_PRE_2018",
    "Micro Health Area" ~ "MHA",
    "Ministry of Children & Families Region (MCFD)" ~ "MCFD",
    "Ministry of Children & Families Service Delivery Area (MCFD_SDA)" ~
      "MCFD_SDA",
    "Ministry of Children & Families Local Service Area (MCFD_LSA)" ~
      "MCFD_LSA",
    "College Region" ~ "CR",
    "School District (SD)" ~ "SD",
    "School District Trustee Electoral Area (TEA)" ~ "TEA",
    "Provincial Electoral District (PED)" ~ "PED",
    "Federal Electoral District (FED)" ~ "FED",
    "Police Services Respondent Area (RESP)" ~ "RESP",
    "Tourism Region" ~ "TOURISM",
    "Games Zone" ~ "GZ",
    "Community Name" ~ "COMM_NAME",
    "Modified Census Subdivision Name" ~ "Modified_MUN_NAME",
    "Modified Full Census Subdivision" ~ "CDCSD",
    .default = NA_character_ # Default case if no match
  )
}


TMF_dict_detail <- TMF_dict_detail %>%
  mutate(item_short = create_item(`Field Name`))

stopifnot(sum(is.na(TMF_dict_detail$item_short)) == 0)

# for the datadictionary created from datadictionary function, we also need to create a shorten item name since some items have year as sufix such as CD_2021.
TMF_dict <- TMF_dict %>%
  mutate(item_short = str_remove(item, "_\\d{4}$"))


# TMF_dict's summary for numerica number does not mean a lot since it is a fact table, and all dimension tables are in another lookup.xlsx excel file.
# so we could simplify the TMF_dict
# or it does not matter, we can add value label later using our dimension table
TMF_dict <- TMF_dict %>%
  left_join(TMF_dict_detail, join_by(item_short))


TMF_dict %>%
  readr::write_csv(here::here("out", "Translation_Master_File_Dict_DIP.csv"))


#################################################################################################
# Save the dimension table aka lookup tables separately
#################################################################################################

# Path to the Excel file
file_path <- use_network_path("data/raw_data/TMF/GCS_Lookup_Table.xlsx")

# Specify the prefix for the CSV files
prefix <- "Translation_Master_File_Lookup_"

# Get the sheet names
sheet_names <- excel_sheets(file_path)

# Loop through each sheet and save as CSV with a prefix
for (sheet in sheet_names) {
  # Read the sheet
  data <- read_excel(file_path, sheet = sheet)

  # Create the CSV file name with the prefix
  csv_file_name <- here::here("out", paste0(prefix, sheet, ".csv"))

  # Save the sheet as a CSV file
  write.csv(data, csv_file_name, row.names = FALSE)

  # Print message for confirmation
  message(paste("Saved:", csv_file_name))
}
