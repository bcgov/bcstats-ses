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

# 1. **Configuration Management**:
#   - Uses the `config` package to load database connection parameters and table names
# - Supports different environments through configuration profiles
#
# 2. **Data Sources**:
#   - Connects to SQL Server database using parameters from config.yml
#   - Access the following database tables:
#   - `bca_folio_addresses`: Contains property addresses with postal codes
#   - `bca_property_values`: Contains property valuation data
#   - `GCS`: Geographic Correspondence System table that links postal codes to dissemination areas
#   - `FIN_MEDINCOME_6DIG_PC`: Contains income data by postal code
#
# 3. **Data Processing**:
#   - Cleans and standardizes postal codes
#   - Filters for residential properties with values over $50,000
#   - Links properties to geographic identifiers (DA)
#   - Uses dbplyr to perform SQL operations in R syntax
#   - Calculates property statistics at the DA level:
#     - Count of properties
#   - Average improvement values
#   - Average land values
#   - Average and median total property values
#   - Minimum and maximum property values
#   - Joins with 2021 median income data by DA
#
# 4. **Output**:
#   - Produces a CSV file with timestamp in the filename
#   - Contains all property value metrics alongside average median income by DA
#   - File is saved in the path specified in the configuration file
#   - Final dataset includes DA, jurisdiction, property counts, value metrics, and income data

library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(glue)
library(dbplyr)
library(datadictionary)
source("./src/utils.R") # get the functions for plotting maps

# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()
# use this config <- config::get(config = "development" ) to switch to development environment.

# Extract settings from config
bca_addresses_table <- config$tables$bca_addresses


# Define property value tables for multiple years
# Extract property value tables from config
bca_property_values_table <- config$tables$bca_property_values


gcs_table <- config$tables$gcs
income_table <- config$tables$income

# Establish database connection using config values
tryCatch(
  {
    con <- dbConnect(
      odbc(),
      Driver = config$database$driver,
      Server = config$database$server,
      Database = config$database$database,
      trusted_connection = config$database$trusted_connection
    )
    cat("Successfully connected to the database\n")
  },
  error = function(e) {
    stop(glue("Failed to connect to database: {e$message}"))
  }
)

# Validate tables exist before proceeding
check_table <- function(table_name) {
  exists <- tolower(table_name) %in% tolower(dbListTables(con, schema = "Prod"))
  if (!exists) {
    warning(glue(
      "Table {table_name} not found in {config$database$database}.Prod schema"
    ))
  }
  return(exists)
}

# Check schema and structure of tables
print("Checking tables existence...")
tables_to_check <- c(
  bca_addresses_table,
  unlist(bca_property_values_table),
  gcs_table,
  income_table
)
table_check_results <- sapply(tables_to_check, check_table)

if (!all(table_check_results)) {
  warning("Some tables are missing. Check your configuration.")
}

# Get the column info for address table to confirm structure
tryCatch(
  {
    cat("Checking address table structure...\n")
    addr_cols <- dbListFields(
      con,
      Id(schema = "Prod", table = bca_addresses_table)
    )
    cat(
      "Address table columns:",
      paste(head(addr_cols), collapse = ", "),
      "...\n"
    )

    # Check if key fields exist - case-insensitive comparison for MS SQL Server
    required_fields <- c("FOLIO_ID", "postal_code")
    addr_cols_upper <- toupper(addr_cols)
    required_fields_upper <- toupper(required_fields)
    missing_fields <- required_fields[
      !required_fields_upper %in% addr_cols_upper
    ]

    if (length(missing_fields) > 0) {
      warning(glue(
        "Missing required fields in address table: {paste(missing_fields, collapse=', ')}"
      ))
    }
  },
  error = function(e) {
    warning(glue("Failed to check address table structure: {e$message}"))
  }
)


# list tables in the database
# dbListTables(con)

# Step 1: Clean postal codes by removing empty spaces in the middle position (4th character)
# This ensures postal codes are in the correct format for joining with geographic data
# There are many NAs in the postal_code column, so we handle them appropriately
# use dbplyr to check how many NAs are present
# dbGetQuery(con, "SELECT COUNT(*) FROM [database].[schema].[%s] WHERE postal_code IS NULL", bca_addresses_table)
# 380017 with NAs in postal_code
# check out what is the proportion of NAs overall in the table
# dbGetQuery(con, "SELECT COUNT(*) FROM [database].[schema].[%s]", bca_addresses_table)
# 380017/2279095 = 0.1667403
# 16% of the postal_code values are NA, so we will filter them out later
# Get the addresses and clean postal codes

cat("Retrieving address data...\n")
bca_addrs <- tbl(con, in_schema("Prod", bca_addresses_table)) %>%
  mutate(
    PC = case_when(
      substr(POSTAL_CODE, 4, 4) != "" ~ POSTAL_CODE,
      TRUE ~ paste0(substr(POSTAL_CODE, 1, 3), substr(POSTAL_CODE, 5, 7))
    )
  ) %>%
  select(FOLIO_ID, STREET_NUMBER, STREET_NAME, PC)


# Create GCS data reference

cat("Retrieving GCS data...\n")
gcs_data <- tbl(con, in_schema("Prod", gcs_table)) %>%
  filter(ACTIVE == 'Y') %>%
  mutate(DA = paste0(CDCSD_2021, DA_2021)) %>%
  select(POSTALCODE, DA)


# ms sql server has an issue with long column: https://www.nickvasile.com/2020/05/13/error-invalid-descriptor-index/; Short story is this is a bug in the Microsoft Driver with varchar(max) columns and they need to be at the end of the select query to work.
# the new odbc package says it already fixed this issue, but we still got it here for 2023 and 2024 tables.

# Process each property value table
# for 2023, we need to solve the "Invalid Descriptor Index", and for 2024, we need to convert the char to number

tbl_type_23 = DBI::dbGetQuery(
  con,
  glue_sql(
    "
SELECT column_name, data_type, CHARACTER_MAXIMUM_LENGTH
FROM information_schema.columns 
WHERE table_schema = 'Prod' AND table_name = '{SQL(bca_property_values_table$`2023`)}'
ORDER BY ordinal_position
"
  )
)

# 	JURISDICTION and GEN_PROPERTY_CLASS_DESC have length -1 !!!!!!!!!!!!!!!!!!!!!!!!!
# a column with Length = -1 is very likely causing your "Invalid Descriptor Index" error. In SQL Server, -1 indicates MAX length data types such as:
# VARCHAR(MAX)
# NVARCHAR(MAX)
# VARBINARY(MAX)
# XML
# Deprecated types like TEXT/NTEXT
#
# These can store up to 2GB of data, which often causes problems with ODBC drivers.

# Initialize combined dataframes before the loop
combined_ave_p <- NULL
combined_med_p <- NULL

# start working on the property value tables
for (year_name in names(bca_property_values_table)) {
  # Get the table name for this year
  current_table <- bca_property_values_table[[year_name]]

  cat(sprintf(
    "Processing property values for year %s from table %s\n",
    year_name,
    current_table
  ))

  # Step 2: Get property details for the current year
  # tbl(con, in_catalog(config$database$database, "Prod", current_table))
  # bca_properties <-tbl(con, in_schema("Prod", current_table))

  bca_properties <- tbl_long_cols_mssql(
    con,
    "Prod",
    current_table
  )

  bca_properties <- bca_properties %>%
    mutate(
      GEN_GROSS_IMPROVEMENT_VALUE = as.numeric(GEN_GROSS_IMPROVEMENT_VALUE),
      GEN_GROSS_LAND_VALUE = as.numeric(GEN_GROSS_LAND_VALUE)
    ) %>%
    dplyr::filter(
      GEN_PROPERTY_SUBCLASS_DESC %in%
        c('Residential Single Family', 'Residential Strata'),
      GEN_GROSS_IMPROVEMENT_VALUE > 50000,
      GEN_GROSS_LAND_VALUE > 50000
    ) %>%
    mutate(
      YR = year_name, # Use the year from our loop
      TOTAL_VALUE = GEN_GROSS_IMPROVEMENT_VALUE + GEN_GROSS_LAND_VALUE
    )

  # Join the tables to create folio data for this year
  folio_data <- bca_properties %>%
    left_join(bca_addrs, by = "FOLIO_ID") %>%
    left_join(gcs_data, by = c("PC" = "POSTALCODE"))

  # Step 3: Calculate average property statistics by DA and jurisdiction
  ave_p <- folio_data %>%
    group_by(YR, DA) %>%
    summarize(
      N_PROPERTY = n_distinct(FOLIO_ID),
      AVE_IMPROVEMENT_VALUE = mean(GEN_GROSS_IMPROVEMENT_VALUE, na.rm = TRUE),
      AVE_LAND_VALUE = mean(GEN_GROSS_LAND_VALUE, na.rm = TRUE),
      AVE_TOTAL_VALUE = mean(TOTAL_VALUE, na.rm = TRUE),
      MIN_TOTAL_VALUE = min(TOTAL_VALUE, na.rm = TRUE),
      MAX_TOTAL_VALUE = max(TOTAL_VALUE, na.rm = TRUE),
      .groups = "drop"
    )

  ave_p <- ave_p %>%
    collect()

  # Step 4: Calculate median property values: median function is not available in dbplyr with group by and summarise, so we need to use mutate and distinct
  med_p <- folio_data %>%
    group_by(YR, DA) %>%
    mutate(
      MEDIAN_IMPROVEMENT_VALUE = median(
        GEN_GROSS_IMPROVEMENT_VALUE,
        na.rm = TRUE
      ),
      MEDIAN_LAND_VALUE = median(GEN_GROSS_LAND_VALUE, na.rm = TRUE),
      MEDIAN_TOTAL_VALUE = median(TOTAL_VALUE, na.rm = TRUE)
    ) %>%
    distinct(
      YR,
      DA,
      MEDIAN_TOTAL_VALUE,
      MEDIAN_LAND_VALUE,
      MEDIAN_IMPROVEMENT_VALUE
    ) %>%
    collect()

  # Store the year's results in the list
  # After calculating ave_p and med_p, directly append to combined dataframes
  combined_ave_p <- bind_rows(combined_ave_p, ave_p)
  combined_med_p <- bind_rows(combined_med_p, med_p)
}


# Final step: Combine all statistics

# Check if we have any data before continuing
if (
  !is.null(combined_ave_p) &&
    nrow(combined_ave_p) > 0 &&
    !is.null(combined_med_p) &&
    nrow(combined_med_p) > 0
) {
  # Final step: Combine all statistics
  final_data <- combined_ave_p %>%
    left_join(combined_med_p, by = c("DA", "YR")) %>%
    arrange(YR, DA)

  # Count records with and without income data
  cat(glue("Total records: {nrow(final_data)}\n"))

  # View the first few rows of the result
  print(head(final_data))
} else {
  warning("No property data was processed successfully.")
}


# Save the final data to a CSV file with date and time in the filename
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Make sure output directory exists
output_dir <- dirname(config$output$house_file_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(glue("Created output directory: {output_dir}\n"))
}

# Save two versions - one with all data, one filtered for records with income data
write_csv(
  final_data,
  glue(
    "{config$output$house_file_path}/property_values_by_da_all_{current_time}.csv"
  )
)


cat(glue("Files saved to {config$output$house_file_path}\n"))

# Close database connection
dbDisconnect(con)


#################################################################################################
# Data dictionary
#################################################################################################
housing_value_dict_labels = c(
  "YR" = "The year of the observation (in '%Y' format)",

  "DA" = "Census Dissemination Area unique ID.",

  "N_PROPERTY" = "The number of the houses/properties in a DA",

  "AVE_IMPROVEMENT_VALUE" = "Average value of structures built on the lots",

  "AVE_LAND_VALUE" = "Average lot value.",

  "AVE_TOTAL_VALUE" = "Average; Total value is the sum of the Land and Improvement Value (total value of land plus structures built on it).",
  "MIN_TOTAL_VALUE" = "Minimum; Total value is the sum of the Land and Improvement Value (total value of land plus structures built on it).",
  "MAX_TOTAL_VALUE" = "Maximum; Total value is the sum of the Land and Improvement Value (total value of land plus structures built on it).",

  "MEDIAN_IMPROVEMENT_VALUE" = "Median value of structures built on the lots",

  "MEDIAN_LAND_VALUE" = "Median lot value.",

  "MEDIAN_TOTAL_VALUE" = "Median total value is the sum of the Land and Improvement Value (total value of land plus structures built on it)."
)

housing_value_dict = create_dictionary(
  final_data,
  var_labels = housing_value_dict_labels
)

# since there are comma "," in the labels so sometimes we use write.csv2 with semicolon ";" as delimiter.
write.csv(housing_value_dict, here::here("out/Housing_Value_Dict_DIP.csv"))
