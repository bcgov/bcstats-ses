library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(glue)
library(dbplyr)

# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()

# Extract settings from config
bca_addresses_table <- config$tables$bca_addresses


# Define property value tables for multiple years
# Extract property value tables from config
bca_property_values_table <- config$tables$bca_property_values


gcs_table <- config$tables$gcs
income_table <- config$tables$income

# Establish database connection using config values

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
# dbGetQuery(con, "SELECT COUNT(*) FROM [Population_Labour_Social].[Prod].[%s] WHERE postal_code IS NULL", bca_addresses_table)
# 380017 with NAs in postal_code
# check out what is the proportion of NAs overall in the table
# dbGetQuery(con, "SELECT COUNT(*) FROM [Population_Labour_Social].[Prod].[%s]", bca_addresses_table)
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

# Create GCS data reference
gcs_data <- tbl(
  con,
  sql(sprintf(
    "
    SELECT [POSTALCODE], [CDCSD_2021], [DA_2021], [ACTIVE]
    FROM [%s].[Prod].[%s]
",
    config$database$database,
    gcs_table
  ))
) %>%
  dplyr::filter(ACTIVE == 'Y') %>%
  mutate(DA = paste0(CDCSD_2021, DA_2021)) %>%
  select(POSTALCODE, DA)

# Initialize combined dataframes before the loop
combined_ave_p <- NULL
combined_med_p <- NULL
year_name = "2023"
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
#
#   VARCHAR(MAX)
# NVARCHAR(MAX)
# VARBINARY(MAX)
# XML
# Deprecated types like TEXT/NTEXT
#
# These can store up to 2GB of data, which often causes problems with ODBC drivers.

tbl_long_cols_mssql <- function(con, schema, table) {
  # Build table name as "schema.table"
  table_full <- DBI::Id(schema = schema, table = table)

  # Get columns info including data type and character length
  cols_info <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT 
         COLUMN_NAME as name, 
         DATA_TYPE as data_type,
         CHARACTER_MAXIMUM_LENGTH as column_size
       FROM INFORMATION_SCHEMA.COLUMNS
       WHERE TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'",
      schema,
      table
    )
  )

  # Create a sorting field with 3 priority groups:
  # 1. Numeric columns (first)
  # 2. Regular character columns (middle)
  # 3. MAX length columns (-1) (last)
  cols_sorted <- cols_info %>%
    dplyr::mutate(
      # Group 1: Numeric types (int, decimal, etc.)
      # Group 2: Normal character columns
      # Group 3: MAX length columns (-1)
      priority_group = dplyr::case_when(
        # Numeric types (these have NULL/NA in column_size)
        is.na(column_size) ~ 1,
        # MAX length types
        column_size == -1 ~ 3,
        # Regular character types
        TRUE ~ 2
      ),
      # Within each priority group, sort by actual size (or 0 for numeric)
      sort_size = dplyr::if_else(
        is.na(column_size),
        0, # Numeric columns get size 0
        as.numeric(column_size)
      )
    ) %>%
    # First sort by priority group, then by size within group
    dplyr::arrange(priority_group, sort_size) %>%
    dplyr::pull(name)

  # Return the table with columns ordered by our defined logic
  dplyr::tbl(con, dbplyr::in_schema(schema, table)) %>%
    dplyr::select(dplyr::all_of(cols_sorted))
}

# ?????????????????????????????????????????????????????????????????????????????
# do we need the JURISDICTION? It causes many problem. We only need the DA.
# ?????????????????????????????????????????????????????????????????????????????

# start working on the property value tables
for (year_name in names(bca_property_values_table)[1]) {
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

  bca_properties %>%
    dplyr::select(
      GEN_PROPERTY_SUBCLASS_DESC,
      FOLIO_ID,
      GEN_GROSS_IMPROVEMENT_VALUE,
      GEN_GROSS_LAND_VALUE,
      JURISDICTION
    ) %>%
    glimpse()

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

  # folio_data <- folio_data %>%
  #   dplyr::select(
  #     YR,
  #     DA,
  #     GEN_PROPERTY_SUBCLASS_DESC,
  #     FOLIO_ID,
  #     POSTALCODE = PC,
  #     STREET_NUMBER,
  #     STREET_NAME,
  #     GEN_GROSS_IMPROVEMENT_VALUE,
  #     GEN_GROSS_LAND_VALUE,
  #     TOTAL_VALUE,
  #     JURISDICTION
  #   )
  # folio_data %>% glimpse()
  # Step 3: Calculate average property statistics by DA and jurisdiction
  ave_p <- folio_data %>%
    group_by(YR, DA, JURISDICTION) %>%
    summarize(
      N_PROPERTY = n_distinct(FOLIO_ID),
      AVE_IMPROVEMENT_VALUE = mean(GEN_GROSS_IMPROVEMENT_VALUE, na.rm = TRUE),
      AVE_LAND_VALUE = mean(GEN_GROSS_LAND_VALUE, na.rm = TRUE),
      AVE_TOTAL_VALUE = mean(TOTAL_VALUE, na.rm = TRUE),
      MIN_TOTAL_VALUE = min(TOTAL_VALUE, na.rm = TRUE),
      MAX_TOTAL_VALUE = max(TOTAL_VALUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(-JURISDICTION, everything(), JURISDICTION)

  # ave_p %>% show_query() %>% as.character()
  #
  # test <- tbl_long_cols_query(con, ave_p %>% show_query() %>% as.character())

  ave_p <- ave_p %>%
    collect()

  # ave_p %>% count(DA) %>% filter(n > 1)
  # ave_p %>%
  #   filter(DA == "010120219")
  # 1063 DAs in 2023 has at least two JURISDICTIONs. It causes another problem.

  # Step 4: Calculate median property values
  med_p <- folio_data %>%
    group_by(YR, DA, JURISDICTION) %>%
    mutate(
      MEDIAN_TOTAL_VALUE = median(TOTAL_VALUE, na.rm = TRUE)
    ) %>%
    distinct(YR, DA, MEDIAN_TOTAL_VALUE, JURISDICTION) %>%
    select(-JURISDICTION, everything(), JURISDICTION) %>%
    collect()

  # Store the year's results in the list
  # After calculating ave_p and med_p, directly append to combined dataframes
  combined_ave_p <- bind_rows(combined_ave_p, ave_p)
  combined_med_p <- bind_rows(combined_med_p, med_p)
}

# Step 5: Get average median income by DA for 2021 tax year. There are many years of income data from 2016 to 2021. 2021 is the most recent year.
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

# Combine data from all years
combined_ave_p <- bind_rows(lapply(all_years_data, function(year_data) {
  (year_data$ave_p)
}))


combined_med_p <- bind_rows(lapply(all_years_data, function(year_data) {
  (year_data$med_p)
}))

# Collect the income data
inc_collected <- collect(inc)

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
    left_join(combined_med_p, by = c("DA", "JURISDICTION", "YR")) %>%
    arrange(YR, DA, JURISDICTION)

  final_data_with_income <- final_data %>%
    left_join(inc, by = "DA") %>%
    arrange(YR, DA, JURISDICTION)

  # Count records with and without income data
  income_count <- sum(!is.na(final_data_with_income$AVE_MED_INCOME))
  cat(glue("Total records: {nrow(final_data)}\n"))
  cat(glue("Records with income data: {income_count}\n"))

  # View the first few rows of the result
  print(head(final_data_with_income))

  # Save the final data to a CSV file with date and time in the filename
  current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Make sure output directory exists
  output_dir <- dirname(config$output$file_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(glue("Created output directory: {output_dir}\n"))
  }

  # Save two versions - one with all data, one filtered for records with income data
  write_csv(
    final_data,
    glue(
      "{config$output$file_path}/property_values_by_da_all_{current_time}.csv"
    )
  )
  write_csv(
    final_data_with_income,
    glue(
      "{config$output$file_path}/property_values_by_da_with_income_{current_time}.csv"
    )
  )

  cat(glue("Files saved to {config$output$file_path}\n"))
} else {
  warning("No property data was processed successfully.")
}

# Close database connection
dbDisconnect(con)
