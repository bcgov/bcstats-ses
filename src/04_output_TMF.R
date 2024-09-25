# this file is used for outputting data for importing to DIP

pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb)

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

# The GCS 202406 csv file is provided by Brett's team and saved in LAN. Need safe network path to get it.

TMF_file  <-  use_network_path("data/GCS_202406.csv")

TMF <- read_csv(TMF_file)


# standardize the DA number, append the prefix BC code 59, so it is easy to join to other tables.
TMF <- TMF %>% 
  mutate(DA_NUM = as.numeric(str_c("59", CD_2021, DA_2021, sep = "")))

# TMF_names = TMF %>% names() %>% paste(collapse = ",")

# clean the names, one name is not upper-cased. We prefer all uppercase
TMF <- 
  TMF %>% 
  janitor::clean_names(case = "screaming_snake" ) 

# 1. BC Translation_Master_File
TMF %>% readr::write_csv(here::here("out", "Translation_Master_File_DIP.csv"))

# 2. Create a dictionary for BC Translation_Master_File
library(datadictionary)

TMF <- TMF %>% 
  mutate(
    across(.cols = c(PROV, SOURCE, ACTIVE),
           .fns = as.factor)
  )


TMF_dict = create_dictionary(TMF,
                  id_var = "POSTALCODE",
                  # file = "GCS202406_DICT.xlsx",
                  var_labels = NULL)


# f2 <- "https://www2.gov.bc.ca/assets/gov/health/forms/5512datadictionary.pdf"
# a better data dictionary in Geocoding Self-Service (GCS) User Guide Prepared by BC Stats March 2020, not online, provided by Brett.
# f3 <-  use_network_path("docs/GCS_User_Guide.pdf")

# it does not work well using pdf tools in R. so we had to manually make a csv file for it.

# manually create a item field in this detail dataframe to join the TMF_dict dataframe
# it is too hard to do it mannually. Chatgpt did it.

library(readr)
TMF_dict_detail <- read_csv("G:/Operations/Data Science and Analytics/2024 SES Index/docs/TMF_data_dict.csv")
View(TMF_dict_detail)

# 

TMF_dict_detail %>% pull(`Field Name`)

# Your vector of strings
strings <- TMF_dict_detail %>% pull(`Field Name`)

# Create the mapping
mapping <- setNames(strings, strings)

# Custom print function
formatted_output <- paste0('"', names(mapping), '" = "', mapping, '"', collapse = ", ")

# Print the formatted string
cat(formatted_output)

# then I can copy the results to our new switch function
# maybe better just do it in CSV file.
# or I should use those as labels in create_dictionary function. liek c(POSTAL_CODE = "Postal code")
create_item = function(x) {
  y = switch(
    x,
    "Postal code" = "POSTALCODE",
    "Birth Date"  = "BIRTH_DATE",
    "Retired Date" = "RET_DATE",
    "Latitude" =  "LATITUDE",
    "Longitude" = "LONGITUDE",
    "Census Division" = "CD",
    "Census Subdivision" = "CSD",
    "Census Subdivision Name" = "MUN_NAME",
    "Census Metropolitan Area or Census Agglomeration Area" = "CMACA",
    "Dissemination Area (DA)" = "DA",
    "Census Tract (CT)" = "CT",
    "Dissemination Block (DB)" = "DB",
    "Designated Place Name (DPL)" = "DPL",
    "Population Centre (POPCTR)" = "POPCTR",
    "Development Region (DR)" = "DR",
    "Health Authority (HA)" = "HA",
    "Health Service Delivery Area (HSDA)" = "HSDA",
    "Local Health Area (LHA)" = "LHA",
    "Community Health Service Area (CHSA)" = "CHSA",
    "Pre-2018 Local Health Area" = "LHA_PRE_2018",
    "Micro Health Area" = "MHA",
    "Ministry of Children & Families Region (MCFD)" = "MCFD",
    "Ministry of Children & Families Service Delivery Area (MCFD_SDA)" = "MCFD_SDA",
    "Ministry of Children & Families Local Service Area (MCFD_LSA)" = "MCFD_LSA",
    "College Region" = "CR",
    "School District (SD)" = "SD",
    "School District Trustee Electoral Area (TEA)" = "TEA",
    "Provincial Electoral District (PED)" = "PED",
    "Federal Electoral District (FED)" = "FED",
    "Police Services Respondent Area (RESP)" = "RESP",
    "Tourism Region" = "TOURISM",
    "Games Zone" = "GZ",
    "Community Name" = "COMM_NAME",
    "Modified Census Subdivision Name" = "MUN_NAME",
    "Modified Full Census Subdivision" = "CDCSD"

  )
  return(y)
}

TMF_dict_detail <- TMF_dict_detail %>%
  mutate(item = sapply(`Field Name`, create_item))
  
# for the datadictionary created from datadictionary function, we also need to create a shorten item name since some items have year as sufix such as CD_2021. 
TMF_dict

str_remove(input_string, "_\\d{4}")

