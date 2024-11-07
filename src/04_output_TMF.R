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

#################################################################################################
# Data dictionary
#################################################################################################
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
TMF_dict_detail <- read_csv( use_network_path("docs/TMF_data_dict.csv"))
View(TMF_dict_detail)


# TMF_dict_detail %>% pull(`Field Name`)
# 
# # Your vector of strings
# strings <- TMF_dict_detail %>% pull(`Field Name`)
# 
# # Create the mapping
# mapping <- setNames(strings, strings)
# 
# # Custom print function
# formatted_output <- paste0('"', names(mapping), '" = "', mapping, '"', collapse = ", ")
# 
# # Print the formatted string
# cat(formatted_output)

# then I can copy the results to our new switch function
# maybe better just do it in CSV file.
# or I should use those as labels in create_dictionary function. liek c(POSTAL_CODE = "Postal code")
# finally use chatgpt to translate the switch function to a case_when
create_item = function(x) {
  # Using case_when to replace the switch statement
  y <- case_when(
    x == "Postal code" ~ "POSTALCODE",
    x == "Birth Date" ~ "BIRTH_DATE",
    x == "Retired Date" ~ "RET_DATE",
    x == "Latitude" ~ "LATITUDE",
    x == "Longitude" ~ "LONGITUDE",
    x == "Census Division" ~ "CD",
    x == "Census Subdivision" ~ "CSD",
    x == "Census Subdivision Name" ~ "MUN_NAME",
    x == "Census Metropolitan Area or Census Agglomeration Area" ~ "CMACA",
    x == "Dissemination Area (DA)" ~ "DA",
    x == "Census Tract (CT)" ~ "CT",
    x == "Dissemination Block (DB)" ~ "DB",
    x == "Designated Place Name (DPL)" ~ "DPL",
    x == "Population Centre (POPCTR)" ~ "POPCTR",
    x == "Development Region (DR)" ~ "DR",
    x == "Health Authority (HA)" ~ "HA",
    x == "Health Service Delivery Area (HSDA)" ~ "HSDA",
    x == "Local Health Area (LHA)" ~ "LHA",
    x == "Community Health Service Area (CHSA)" ~ "CHSA",
    x == "Pre-2018 Local Health Area" ~ "LHA_PRE_2018",
    x == "Micro Health Area" ~ "MHA",
    x == "Ministry of Children & Families Region (MCFD)" ~ "MCFD",
    x == "Ministry of Children & Families Service Delivery Area (MCFD_SDA)" ~ "MCFD_SDA",
    x == "Ministry of Children & Families Local Service Area (MCFD_LSA)" ~ "MCFD_LSA",
    x == "College Region" ~ "CR",
    x == "School District (SD)" ~ "SD",
    x == "School District Trustee Electoral Area (TEA)" ~ "TEA",
    x == "Provincial Electoral District (PED)" ~ "PED",
    x == "Federal Electoral District (FED)" ~ "FED",
    x == "Police Services Respondent Area (RESP)" ~ "RESP",
    x == "Tourism Region" ~ "TOURISM",
    x == "Games Zone" ~ "GZ",
    x == "Community Name" ~ "COMM_NAME",
    x == "Modified Census Subdivision Name" ~ "Modified_MUN_NAME",
    x == "Modified Full Census Subdivision" ~ "CDCSD",
    TRUE ~ NA_character_  # Default case if no match
  )
  
  return(y)
}

TMF_dict_detail <- TMF_dict_detail %>%
  mutate(item_short = create_item(`Field Name`))
  
# for the datadictionary created from datadictionary function, we also need to create a shorten item name since some items have year as sufix such as CD_2021. 
TMF_dict <- TMF_dict %>% 
  mutate( item_short = str_remove(item, "_\\d{4}")) 
  

# TMF_dict's summary for numerica number does not mean a lot since it is a fact table, and all dimension tables are in another lookup.xlsx excel file. 
# so we could simplify the TMF_dict
# TMF_dict_simple <- TMF_dict %>% 
#   group_by(item) %>% 
# slice_head(n = 1)
# filter(row_number() == 1)

# or it does not matter, we can add value label later using our dimension table
TMF_dict <- TMF_dict %>% 
  left_join(TMF_dict_detail, join_by( item_short))


TMF_dict %>% readr::write_csv(here::here("out", "Translation_Master_File_Dict_DIP.csv"))


#################################################################################################
# Now we deal with the dimension table aka lookup tables
#################################################################################################

# Load necessary libraries
library(readxl)

# Path to the Excel file
file_path <- use_network_path("data/GCS_Lookup_Table.xlsx")

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

