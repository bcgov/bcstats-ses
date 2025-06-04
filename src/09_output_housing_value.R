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

# This scipt analyzes housing assessed values in relation to income data
# - Access the following database tables:
# - `address table`: Contains property addresses with postal codes
# - `property_values_table`: Contains property valuation data
# - `GCS_table`: Geographic Correspondence System table that links postal codes to dissemination areas
# - `MEDINCOME_table`: Contains income data by postal code
#
# 3. **Data Processing**:
#   - Cleans and standardizes postal codes
# - Filters for residential properties with values over $50,000
# - Links properties to geographic identifiers (DA)
# - Uses dbplyr to perform SQL operations in R syntax
# - Calculates property statistics at the DA level:
#   - Count of properties
# - Average improvement values
# - Average land values
# - Average and median total property values
# - Minimum and maximum property values
# - Joins with 2021 median income data by DA
#
# 4. **Output**:
#   - Produces a CSV file with timestamp in the filename
# - Contains all property value metrics alongside average median income by DA
# - File is saved in the path specified in the configuration file
# - Final dataset includes DA, jurisdiction, property counts, value metrics, and income data
# TODO: extend the analysis for year 2023, 2024

library(tidyverse)
library(DBI)
library(odbc)


# Load configuration using config package
# This will automatically look for a file named config.yml in the current or parent directory
config <- config::get()

# Extract settings from config
bca_addresses_table <- config$tables$bca_addresses
bca_property_values_table <- config$tables$bca_property_values
gcs_table <- config$tables$gcs
income_table <- config$tables$income
gcs_income_join_table <- config$tables$gcs_income_join

# Establish database connection using config values
con <- dbConnect(
  odbc(),
  Driver = config$database$driver,
  Server = config$database$server,
  Database = config$database$database,
  trusted_connection = config$database$trusted_connection
)
# list tables in the database
# dbListTables(con)

# Step 1: Clean postal codes by removing empty spaces in the middle position (4th character)
# This ensures postal codes are in the correct format for joining with geographic data
# There are many NAs in the postal_code column, so we handle them appropriately
# use dbplyr to check how many NAs are present
# 380017 with NAs in postal_code
# check out what is the proportion of NAs overall in the table
# 380017/2279095 = 0.1667403
# 16% of the postal_code values are NA, so we will filter them out later
# Get the addresses and clean postal codes
bca_addrs <- tbl(
  con,
  sql(sprintf(
    "
    SELECT *, 
        CASE WHEN SUBSTRING(postal_code,4,1) <> '' 
             THEN postal_code 
             ELSE CONCAT(SUBSTRING(postal_code,1,3),SUBSTRING(postal_code,5,3)) 
        END AS PC
    FROM [%s].[Prod].[%s]
",
    config$database$database,
    bca_addresses_table
  ))
) %>%
  select(FOLIO_ID, STREET_NUMBER, STREET_NAME, PC)
# %>%     filter(!is.na(PC)) # Filter out rows with NA in cleaned postal code

# Step 2: Get property details with addresses and dissemination areas
# Filtering for:
# 1. Only residential properties (single family and strata)
# 2. Properties with improvement value > $50,000
# 3. Properties with land value > $50,000
# 4. Only active properties
# Step 2: Get property details with addresses and dissemination areas using dbplyr

# Create tbl object for property values
bca_properties <- tbl(
  con,
  sql(sprintf(
    "SELECT * FROM [%s].[Prod].[%s]",
    config$database$database,
    bca_property_values_table
  ))
)

# Create tbl object for property values
bca_properties <- bca_properties %>%
  dplyr::filter(
    GEN_PROPERTY_SUBCLASS_DESC %in%
      c('Residential Single Family', 'Residential Strata'),
    GEN_GROSS_IMPROVEMENT_VALUE > 50000,
    GEN_GROSS_LAND_VALUE > 50000
  ) %>%
  mutate(
    YR = substring(WHEN_CREATED, 1, 4),
    TOTAL_VALUE = GEN_GROSS_IMPROVEMENT_VALUE + GEN_GROSS_LAND_VALUE
  )

# Use existing bca_addrs object (already created earlier in the code)

# Create tbl object for geographic correspondence table
# gcs <- tbl(con, sql(sprintf("SELECT * FROM [%s].[Prod].[%s]", config$database$database, gcs_table))) %>%
#     dplyr::filter(ACTIVE == 'Y') %>% # Filter out empty postal codes
#     mutate(DA = paste0(CDCSD_2021, DA_2021))  %>%
#     select(POSTALCODE, DA)

gcs_data <- tbl(
  con,
  sql(sprintf(
    "
    SELECT [POSTALCODE], [CDCSD_2021], [DA_2021], [ACTIVE]
    FROM [%s].[Prod].[%s]
",
    config$database$database,
    gcs_income_join_table
  ))
) %>%
  dplyr::filter(ACTIVE == 'Y') %>% # Filter out empty postal codes
  mutate(DA = paste0(CDCSD_2021, DA_2021)) %>%
  select(POSTALCODE, DA)


# Join the three tables
folio_data <- bca_properties %>%
  left_join(bca_addrs, by = "FOLIO_ID") %>%
  left_join(gcs_data, by = c("PC" = "POSTALCODE"))

folio_data <- folio_data %>%
  dplyr::select(
    YR,
    DA,
    GEN_PROPERTY_SUBCLASS_DESC,
    FOLIO_ID,
    POSTALCODE = PC,
    STREET_NUMBER,
    STREET_NAME,
    JURISDICTION,
    JURISDICTION_CODE,
    GEN_GROSS_IMPROVEMENT_VALUE,
    GEN_GROSS_LAND_VALUE,
    TOTAL_VALUE
  )


# Step 3: Calculate average property statistics by DA and jurisdiction
# This includes property count, average improvement value, land value, total value
# Also calculates min and max total values
# Calculate median property values
# Uses quantile function for accurate median calculation
ave_p <- folio_data %>%
  group_by(YR, DA, JURISDICTION, JURISDICTION_CODE) %>%
  summarize(
    properties = n_distinct(FOLIO_ID),
    AVE_IMPROVEMENT_VALUE = mean(GEN_GROSS_IMPROVEMENT_VALUE, na.rm = TRUE),
    AVE_LAND_VALUE = mean(GEN_GROSS_LAND_VALUE, na.rm = TRUE),
    AVE_TOTAL_VALUE = mean(TOTAL_VALUE, na.rm = TRUE),
    MIN_TOTAL_VALUE = min(TOTAL_VALUE, na.rm = TRUE),
    MAX_TOTAL_VALUE = max(TOTAL_VALUE, na.rm = TRUE),
    .groups = "drop"
  )


# Step 4: Calculate median property values
# Uses quantile function for accurate median calculation
# Grouped by year, DA, and jurisdiction
# ! Translation of `median()` in `summarise()` is not supported for SQL Server.
# ℹ Use a combination of `distinct()` and `mutate()` for the same result:
#   `mutate(<col> = median(TOTAL_VALUE, na.rm = TRUE)) %>% distinct(<col>)`

med_p <- folio_data %>%
  group_by(YR, DA, JURISDICTION) %>%
  mutate(
    MEDIAN_TOTAL_VALUE = median(TOTAL_VALUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  distinct(YR, DA, JURISDICTION, MEDIAN_TOTAL_VALUE)


# Step 5: Get average median income by DA for 2021 tax year
# Links income data with geographic identifiers
income_data <- tbl(
  con,
  sql(sprintf(
    "
    SELECT [tax_year],
           [postal_code],
           [median_income]
    FROM [%s].[Prod].[%s] 
    WHERE [tax_year]='2021'
",
    config$database$database,
    income_table
  ))
)


inc <- income_data %>%
  left_join(gcs_data, by = c("postal_code" = "POSTALCODE")) %>%
  group_by(tax_year, DA) %>%
  summarize(
    AVE_MED_INCOME = mean(median_income, na.rm = TRUE),
    .groups = "drop"
  )


# Final step: Combine all statistics
# Links average values, median values, and income data
# Filters out areas where income data is not available
# Orders results by DA and jurisdiction for easy reading
final_data <- ave_p %>%
  left_join(med_p, by = c("DA", "JURISDICTION", "YR")) %>%
  left_join(inc, by = "DA") %>%

  arrange(DA, JURISDICTION)

final_data %>% filter(!is.na(AVE_MED_INCOME)) %>% glimpse()

final_data <- final_data %>% collect()


# View the first few rows of the result
print(head(final_data))

# Save the final data to a CSV file with date and time in the filename
library(lubridate)
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Write to CSV
write_csv(
  final_data,
  glue::glue(
    "{config$output$house_file_path}/property_values_by_da_{current_time}.csv"
  )
)

# Close database connection
dbDisconnect(con)
