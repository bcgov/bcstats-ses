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

# Get years for file naming
last_year <- as.character(max(as.numeric(format(LFS$REF_DATE, "%Y"))))
current_year <- format(Sys.Date(), "%Y")

pacman::p_load(
  cancensus,
  geojsonsf,
  tidyverse,
  config,
  bcmaps,
  bcdata,
  janitor,
  cansim,
  # safepaths,
  arrow,
  duckdb,
  datadictionary,
  futile.logger
)

## Logging setup
log_file <- "./R/execution_log.txt"
flog.appender(appender.file(log_file), name = "file_logger")
flog.threshold(INFO, name = "file_logger")

log_info <- function(msg) {
  flog.info(msg, name = "file_logger")
  print(paste(Sys.time(), "|", msg))
}

# Load configs (ADR-0005). NOTE: lan_path previously leaked from another
# script's session; this script now loads its own.
config <- config::get()
source("R/config.R")
year_config <- load_year_config()
lan_path <- config$lan_path

# Load LFS data from StatsCan
cansim_id <- "14-10-0457-01"
log_info(glue::glue("Loading LFS data from StatsCan table {cansim_id}..."))
LFS_raw <- cansim::get_cansim_connection(cansim_id, format = 'sqlite') |>
  collect()
log_info(glue::glue("Loaded LFS raw data: {nrow(LFS_raw)} rows"))

# Filter to only include British Columbia data
LFS <- LFS_raw |>
  filter(str_detect(GEO, "British Columbia")) |>
  mutate(GEO = str_replace(GEO, ", British Columbia", ""))
log_info(glue::glue("Filtered to BC: {nrow(LFS)} rows"))

# Clean column names to screaming snake case
LFS <- LFS |>
  janitor::clean_names(case = "screaming_snake")

# Recode STATUS: if VALUE is NA, set STATUS to NA, otherwise keep original STATUS
LFS <- LFS |> mutate(STATUS = case_when(is.na(VALUE) ~ NA, T ~ STATUS))

# REF_DATE is monthly so we can use zoo's yearmon() for easier manipulation
LFS <- LFS |>
  mutate(REF_DATE = zoo::as.yearmon(LFS$REF_DATE))

# Remove columns that are entirely NA
LFS <- LFS |> select(where(~ !all(is.na(.))))

# Remove SCALAR_FACTOR and SCALAR_ID columns as they have no variance
LFS <- LFS |>
  select(
    LFS |>
      summarise(across(everything(), n_distinct)) |>
      pivot_longer(cols = everything()) |>
      filter(value > 1) |>
      pull(name)
  )

# Filter for estimates only, excluding confidence intervals or other statistics
LFS <- LFS |>
  filter(STATISTICS == "Estimate") |>
  select(-STATISTICS)
log_info(glue::glue("Filtered to estimates only: {nrow(LFS)} rows"))

# Extract GEO_TYPE from square brackets in GEO and clean the REGION name
LFS <- LFS |>
  mutate(
    GEO_TYPE = str_extract(GEO, "\\[(.+)\\]", group = 1),
    .after = 'GEO'
  ) |>
  mutate(REGION = str_remove(GEO, " \\[.+\\]")) |>
  select(-GEO)

# Verify geography columns look correct
LFS$GEO_TYPE |> unique()
LFS$REGION |> unique()

# Drop redundant columns: UOM, UOM_ID, and DECIMALS
LFS <- select(LFS, -UOM, -UOM_ID, -DECIMALS)

# Select final set of columns for analysis
LFS <- select(
  LFS,
  REF_DATE,
  GEO_TYPE,
  REGION,
  LABOUR_FORCE_CHARACTERISTICS,
  VALUE,
  STATUS
)

# Convert categorical columns to factors
LFS <- LFS |>
  mutate(across(c(GEO_TYPE, LABOUR_FORCE_CHARACTERISTICS, STATUS), as.factor))

# Inspect data summary
log_info(glue::glue("LFS cleaned: {nrow(LFS)} rows, {ncol(LFS)} columns"))
print(LFS)

# Load SLA geography linkage file
SLA_file <- glue::glue(
  "{lan_path}/{year_config$project_folder}/{year_config$sla_file}"
)
log_info(glue::glue("Loading SLA linkage file from: {SLA_file}"))
SLA_raw <- readxl::read_excel(SLA_file)
log_info(glue::glue("Loaded SLA raw data: {nrow(SLA_raw)} rows"))

# Filter for BC records only (PR == "59")
SLA <- SLA_raw |>
  filter(PR == "59") |>
  select(-PR)

# Clean names using janitor
SLA <- SLA |>
  rename(CSDName = CSDname, CSDType = CSDtype) |>
  janitor::clean_names(case = "screaming_snake")

# Remove the '59' prefix from CSD identifiers
SLA$CSD <- substr(SLA$CSD, 3, nchar(SLA$CSD))

# Lookup table to fill in missing SLA_NAMEs based on CMA mapping
SLA_lookup <- tribble(
  ~CMA                   ,
  ~SLA_NAME              ,
                     905 ,
  "Cranbrook"            ,
                     907 ,
  "Nelson"               ,
                     910 ,
  "Trail"                ,
                     913 ,
  "Penticton"            ,
                     915 ,
  "Kelowna"              ,
                     918 ,
  "Vernon"               ,
                     920 ,
  "Salmon Arm"           ,
                     925 ,
  "Kamloops"             ,
                     930 ,
  "Chilliwack"           ,
                     932 ,
  "Abbotsford - Mission" ,
                     933 ,
  "Vancouver"            ,
                     934 ,
  "Squamish"             ,
                     935 ,
  "Victoria"             ,
                     936 ,
  "Ladysmith"            ,
                     937 ,
  "Duncan"               ,
                     938 ,
  "Nanaimo"              ,
                     939 ,
  "Parksville"           ,
                     940 ,
  "Port Alberni"         ,
                     943 ,
  "Courtenay"            ,
                     944 ,
  "Campbell River"       ,
                     945 ,
  "Powell River"         ,
                     950 ,
  "Williams Lake"        ,
                     952 ,
  "Quesnel"              ,
                     955 ,
  "Prince Rupert"        ,
                     965 ,
  "Terrace"              ,
                     970 ,
  "Prince George"        ,
                     975 ,
  "Dawson Creek"         ,
                     977 ,
  "Fort St. John"
)

# Merge lookup to fill in blanks in SLA_NAME
SLA <- SLA |>
  left_join(SLA_lookup, by = "CMA") |>
  mutate(
    SLA_NAME = case_match(SLA_NAME.x, NA ~ SLA_NAME.y, .default = SLA_NAME.x)
  ) |>
  select(-SLA_NAME.x, -SLA_NAME.y)

# Ensure no missing values remain in SLA table
stopifnot(SLA |> is.na() |> sum() == 0)
log_info(glue::glue(
  "SLA lookup complete: {nrow(SLA)} records, no missing values"
))

# Convert to character to match TMF format expectations
SLA <- SLA |>
  mutate(across(everything(), as.character))

# Ensure SLA_CODE is NA if SLA_NAME is missing
SLA <- SLA |>
  mutate(SLA_CODE = case_when(is.na(SLA_NAME) ~ NA_character_, T ~ SLA_CODE))

# Final selection of columns for SLA
SLA <- SLA |>
  select(SLA_NAME, CSD, CSD_NAME, CMA)

# Join LFS with SLA using REGION/SLA_NAME mapping (many-to-many)
log_info("Joining LFS with SLA geography lookup...")
LFS <- LFS |>
  inner_join(
    SLA,
    by = join_by("REGION" == "SLA_NAME"),
    relationship = 'many-to-many'
  )
log_info(glue::glue("LFS joined with SLA: {nrow(LFS)} rows"))

# Export LFS data with year-stamped filename
lfs_output_file <- here::here(
  "out",
  paste0("Labour_Force_Survey_DIP_", last_year, "_", current_year, ".csv")
)
log_info(glue::glue("Writing LFS output to: {lfs_output_file}"))
readr::write_csv(LFS, lfs_output_file)
log_info("LFS output written successfully")

#################################################################################################
# Data dictionary
#################################################################################################

LFS_dict_labels <- c(
  "REF_DATE" = "The month and year of the observation (in '%b %Y' format)",

  "GEO_TYPE" = "A census metropolitan area (CMA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CMA must have a total population of at least 100,000 of which 50,000 or more must live in the core. \n\n  A census agglomeration (CA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CA must have a core population of at least 10,000 based on data from the previous Census of Population Program. \n\n  A self-contained labour area (SLA) is a functional area composed of census subdivisions which are not already included in a CMA or CA. All three types of regions are determined using commuting flows derived from census program place of work data.",
  "REGION" = "Self-contained Labour Areas (SLA) name. SLA are functional areas composed of Census Subdivisions (CSD) grouped according according to commuting patterns (OECD, 2020).",
  "LABOUR_FORCE_CHARACTERISTICS" = "The employment rate is the small area estimate of the number of employed persons expressed as a percentage of the population 15 years of age and older. Estimates are percentages, rounded to the nearest tenth.\n\n  The unemployment rate is the number of unemployed people as a percentage of the labour force (employed and unemployed). The unemployment rate is the number of unemployed persons expressed as a percentage of the labour force. Unemployed persons are those who were without work, had looked for work in the past four weeks, and were available for work. Those persons on layoff or who had a new job to start in four weeks or less are also considered unemployed. The labour force is all civilian, non-institutionalized persons 15 years or age and older who were employed or unemployed. Estimates are percentages, rounded to the nearest tenth.\n\n  Employment is the small area estimate of the number of persons who worked for pay or profit, or had a job but were not at work due to own illness or disability, personal or family responsibilities, labour dispute, vacation, or other reason. Estimates are rounded to the nearest ten.",
  "VALUE" = "The value of the observation",
  "STATUS" = "One of 'E' (use with caution) or 'F' (too unreliable to be published).",
  "CSD" = "Census subdivision (CSD) is the general term for municipalities (as determined by provincial/territorial legislation) or areas treated as municipal equivalents for statistical purposes (e.g., Indian reserves, Indian settlements and unorganized territories).",
  "CSD_NAME" = "Name of the CSD",
  "CMA" = "A census metropolitan area (CMA) or a census agglomeration (CA) is formed by one or more adjacent municipalities centred on a population centre (known as the core). A CMA must have a total population of at least 100,000 of which 50,000 or more must live in the core, based on adjusted data from the previous census. A CA must have a core population of at least 10,000, also based on data from the previous census. To be included in the CMA or CA, other adjacent municipalities must have a high degree of integration with the core, as measured by commuting flows derived from data on place of work from the previous census."
)

log_info("Creating LFS data dictionary...")
LFS_dict <- create_dictionary(LFS, var_labels = LFS_dict_labels)

# Export dictionary using semicolon delimiter for compatibility with comma-heavy labels
lfs_dict_output_file <- here::here(
  "out",
  paste0("Labour_Force_Survey_Dict_DIP_", last_year, "_", current_year, ".csv")
)
log_info(glue::glue("Writing LFS dictionary to: {lfs_dict_output_file}"))
write.csv2(LFS_dict, lfs_dict_output_file)
log_info("LFS dictionary written successfully")
