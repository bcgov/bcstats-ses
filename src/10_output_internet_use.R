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

# This script is used to download statscan data on internet use in Canada and create a summary table of the data

#Load required packages for use in this R session (HIDE OUTPUT)
library(haven) #to import SPSS .sav file
library(dplyr) #for data manipulation
library(srvyr) #survey-specific functions
library(gtsummary) #create summary tables
library(ggplot2) #create plots
library(flextable) # create tables
library(officer) # manipulate word docs and power points
pacman::p_load(tidyverse, readxl, safepaths, bcdata)
library(dplyr)
library(tidyr)
library(janitor)
library(datadictionary)


## Public Use Microdata Files (PUMFs)

# _Public Use Microdata Files_ (PUMFs) is the term Statistics Canada uses for non-aggregated data it releases for public use. PUMFs are released under the [Statistics Canada Open Licence](https://statcan.gc.ca/reference/licence){:target="_blank"} and can be used with very few restrictions, as long as Statistics Canada is acknowledged as the source.
#
# PUMFs take precautions to protect respondent privacy:
#
#   - Some variables are suppressed _(e.g. smaller geographic areas)_
# - Some variables are aggregated _(e.g. age, income)_
# - Might only be a sample of responses _(e.g. Census of Canada PUMFs)_

# The PUMFs come with _user guides_, _codebooks_, and/or _questionnaires_ that will help you interpret the data and avoid mistakes in your analysis.
#
# All PUMFs will have some way to identify variables, whether it's a _codebook_, _data dictionary_ or other reference file. _User guides_ and _questionnaires_ are not always present.
# {: .note}
#
# ### User guide
# The user guide usually begins with a description of the survey and includes sections on definitions, methodology, data collection, and more. It will also include a section on the data file structure, which will help you understand the data file and how to use it.
#

### Codebook
# A _codebook_ lists all the variables in the PUMF along with:
#
#   - answer categories _(possible answers to the question)_
# - frequencies _(the number of responses for each category)_
# - weighted frequencies _(population estimate for each category)_

## Weights
# The codebook frequencies lead us to a key concept: the survey *weight*. In a Statistics Canada PUMF each response represents a certain number of people in the target population. That number is represented by the weight variable.
# An analysis based on the number of responses will tell us about the survey _sample_ (e.g. X people who answered the survey are current internet user). More often we want to produce estimates of the population (e.g. X people in the target population are current ). That is, weights are used to make the data more representative of the target population.
#
# **WTPP** is the weight variable for the survey In most cases our analysis should be based on the _sum_ of the WTPP variable, not on the count of records.
#

# To retrieve an option
getOption('timeout')
# [1] 60
# To set an option
options(timeout = 600)

download.file(
  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2022.zip",
  "CIUS_2022.zip",
  mode = "wb"
)

download.file(
  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2020.zip",
  use_network_path(file.path(
    "2024 SES Index/data/raw_data/internet_connectivity",
    "CIUS_2020.zip"
  )),
  mode = "wb"
)

cius_file_urls = c(
  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2022.zip",

  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2020.zip",

  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2018-eng.zip",
  # individual-level data
  "https://www150.statcan.gc.ca/n1/pub/56m0005x/2013000/56M0005X.zip",
  # household-level data
  "https://www150.statcan.gc.ca/n1/en/pub/56m0004x/2013000/56M0004X.zip",

  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2009-eng.zip",

  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2007-eng.zip",

  "https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2005-eng.zip"
)


cius_file_names = c(
  "CIUS_2022.zip",
  "CIUS_2020.zip",
  "CIUS_2018.zip",
  # individual-level data
  "CIUS_2013_ind.zip",
  # household-level data
  "CIUS_2013.zip",
  "CIUS_2009.zip",
  "CIUS_2007.zip",
  "CIUS_2005.zip"
)

for (i in 3:length(cius_file_urls)) {
  download.file(
    cius_file_urls[i],
    use_network_path(file.path(
      "2024 SES Index/data/raw_data/internet_connectivity",
      cius_file_names[i]
    )),
    mode = "wb"
  )
}

extract_nested_zips <- function(zip_file_path) {
  ""
  "
  Extracts all nested zip files within a given zip file, using the main zip file's
  name as the output directory.

  Args:
    zip_file_path: The path to the main zip file.

  Returns:
    None. Prints messages indicating the progress of the extraction.
  "
  ""

  if (!file.exists(zip_file_path)) {
    stop(paste("Zip file not found:", zip_file_path))
  }

  temp_dir <- tempdir()

  tryCatch(
    {
      unzip(zip_file_path, exdir = temp_dir)

      zip_files <- list.files(temp_dir, pattern = "\\.zip$", full.names = TRUE)

      # Use the main zip file's name (without extension) as the output directory
      output_dir <- tools::file_path_sans_ext(basename(zip_file_path))
      dir.create(output_dir, showWarnings = FALSE)

      for (zip_file in zip_files) {
        file_name <- basename(zip_file)
        output_inner_dir <- file.path(
          output_dir,
          tools::file_path_sans_ext(file_name)
        )
        dir.create(output_inner_dir, showWarnings = FALSE)

        unzip(zip_file, exdir = output_inner_dir)

        cat(paste("Extracted:", file_name, "to", output_inner_dir, "\n"))
      }

      cat("Extraction process complete.\n")
    },
    error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")
    },
    finally = {
      # Clean up the temporary directory
      unlink(temp_dir, recursive = TRUE)
    }
  )
}

use_network_path(file.path(
  "2024 SES Index/data/raw_data/internet_connectivity",
  cius_file_names[2]
))

# Example usage:
# extract_nested_zips("path/to/your/main.zip")

# Example usage:
zip_file_path <- use_network_path(file.path(
  "2024 SES Index/data/raw_data/internet_connectivity",
  cius_file_names[2]
))
extract_nested_zips(
  zip_file_path
)

unzip("CTNS_2020.zip")
unzip("CTNS_2022.zip")


library(haven)


##########################################################################################
# 2022
# PROVINCE     = "PROVINCE"
# LUC_RST      = "Population centre"

# Answer Categories Code Frequency Weighted Frequency %
# Larger Urban Centres (CMA/CA) 01 18,444 27,482,983 84.3
# Rural and Small Town (non-CMA/CA) 02 5,610 4,986,521 15.3
# Prince Edward Island 03 1,064 143,193 0.4
# Population centres are classified into three groups, depending on the size of their population:
#   - small population centres, with a population between 1,000 and 29,999
# - medium population centres, with a population between 30,000 and 99,999
# - large urban population centres, with a population of 100,000 or more.

# Information derived using postal codes.
# A population centre (POPCTR) has a population of at least 1,000 and a population
# density of 400 persons or more per square kilometre, based on population counts from
# the current Census of Population. All areas outside population centres are classified as
# rural areas.

# RRS_G12      = "Number of person in the household"
# AGE_GRP      = "Age Groups - Derived variable"
# GENDER       = "Gender - Derived variable"
# VALUE     SEX001F
# 1 = "Male"
# 2 = "Female"
# 6 = "Valid skip"
# 7 = "Don't know"
# 8 = "Refusal"
# 9 = "Not stated"
# ;

# AC_010A      = "Access to the Internet through a mobile data plan"
# AC_030A      = "Access to the Internet at home"
# VALUE     YESN01F
# 1 = "Yes"
# 2 = "No"
# 6 = "Valid skip"
# 7 = "Don't know"
# 8 = "Refusal"
# 9 = "Not stated"
# ;

##########################################################################################

cius_pumf_2022 <- read_sas(
  "G:/Operations/Data Science and Analytics/2024 SES Index/data/raw_data/internet_connectivity/2022/4432_CIUS_ECUI_2022/Data/cius_pumf.sas7bdat",
  NULL
)

BC_cius_pumf <- cius_pumf %>%
  filter(PROVINCE == 59)

BC_cius_pumf %>%
  select(
    PUMFID,
    PROVINCE,
    LUC_RST,
    RRS_G12,
    AGE_GRP,
    AC_010A,
    AC_030A
  ) %>%
  glimpse()

##########################################################################################
# 2005
##########################################################################################

##########################################################################################

# Define variable specifications from SAS code.
var_specs_2005 <- list(
  names = c(
    "PUMFID",
    "REFYEAR",
    "PROVINCE",
    "REGION",
    "G_URBRUR",
    "GCAGEGR6",
    "CSEX",
    "GMARSTAT",
    "G_CEDUC",
    "G_CSTUD",
    "G_CLFSST",
    "GCOWMAIN",
    "FTPTMAIN",
    "GFAMTYPE",
    "G_HHSIZE",
    "HCONNECT",
    "G_HEDUC",
    "G_HSTUD",
    "EV_Q01",
    "EV_Q02",
    "PU_Q01",
    "PU_Q02",
    "PU_Q03",
    "PU_Q06A",
    "PU_Q06E",
    "PU_Q06J",
    "PU_Q06K",
    "PU_G06",
    "LU_Q01",
    "LU_Q02",
    "LU_G03",
    "LU_Q04",
    "LU_G05",
    "LU_G06A",
    "LU_G06B",
    "LU2_G06",
    "IU_Q01A",
    "IU_Q01B",
    "IU_Q01G",
    "IU_G01",
    "IU_Q02A",
    "IU_Q02B",
    "IU_G02",
    "IU_Q02E",
    "IU_Q03",
    "IU_Q04",
    "IU_G05",
    "IU_Q06",
    "SU_Q01",
    "SU_Q02",
    "SU_Q03",
    "SU_Q04",
    "SU_Q05",
    "SU_G06",
    "SU_Q07",
    "SU_Q08",
    "SU_Q09",
    "SU_Q10",
    "SU_Q11",
    "SU_Q12",
    "SU_Q13",
    "SU_Q14",
    "SU_Q15",
    "SU_Q16",
    "SU_Q17",
    "SU_Q18",
    "SU_Q19",
    "SU_Q20",
    "SU_G21",
    "SU_Q22",
    "SU_Q23",
    "GL_Q01A",
    "GL_Q01B",
    "GL_Q01C",
    "GL_Q01D",
    "GL_Q01E",
    "GL_Q01F",
    "GL_Q01G",
    "GL_Q01I",
    "GL_Q01J",
    "GL_G01",
    "GL_Q02",
    "GL_Q03",
    "GL_Q04A",
    "GL_Q04B",
    "GL_Q04C",
    "GL_G05",
    "MH_Q01A",
    "MH_Q01B",
    "MH_Q01C",
    "MH_Q01D",
    "MH_Q01E",
    "MH_Q01F",
    "MH_Q01G",
    "MH_Q01H",
    "MH_G02",
    "MH_Q03",
    "EU_G01A",
    "EU_G01B",
    "EU_G01C",
    "EU_G01D",
    "EU_G01",
    "SC_Q01",
    "SC_Q02",
    "SC_Q03",
    "EC_Q01",
    "EC_Q02A",
    "EC_Q02B",
    "EC_Q02C",
    "EC_Q02D",
    "EC_Q02E",
    "EC_Q02F",
    "EC_Q02I",
    "EC_Q02J",
    "EC_Q02K",
    "EC_Q02L",
    "EC_Q02M",
    "EC_Q02N",
    "EC_Q02O",
    "EC_Q02P",
    "EC_Q02Q",
    "EC_G02",
    "EC_Q03",
    "EC_Q04",
    "EC_Q05",
    "EC_Q06",
    "EC_Q07A",
    "EC_Q07B",
    "EC_G07",
    "EC_Q08",
    "EC_Q09A",
    "EC_Q09B",
    "EC_Q09C",
    "EC_Q09D",
    "EC_Q09E",
    "EC_Q09F",
    "EC_Q09J",
    "EC_Q09K",
    "EC_Q09L",
    "EC_Q09M",
    "EC_Q09N",
    "EC_Q09O",
    "EC_Q09P",
    "EC_Q09Q",
    "EC_Q09R",
    "EC_G09",
    "EC_Q10",
    "NU_Q01",
    "NU_Q02A",
    "NU_Q02B",
    "NU_Q02D",
    "NU_G02",
    "NU_Q03",
    "NU_Q04",
    "NU_Q05A",
    "NU_Q05B",
    "NU_G05",
    "NU_Q06A",
    "NU_G06",
    "NU_Q07A",
    "NU_Q07B",
    "NU_Q07E",
    "NU_Q07F",
    "NU_Q07J",
    "NU_G07",
    "NU_G08",
    "NU_Q09",
    "PS_Q01",
    "PS_Q02",
    "PS_Q03",
    "PS_Q04",
    "PS_Q05",
    "G_HQUINT",
    "WTPP"
  ),

  widths = c(
    5,
    4,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    3,
    6,
    3,
    6,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    12
  ), # Variable widths

  positions = c(
    001,
    006,
    010,
    012,
    013,
    014,
    015,
    016,
    017,
    018,
    019,
    020,
    021,
    022,
    023,
    024,
    025,
    026,
    027,
    028,
    029,
    030,
    031,
    032,
    033,
    034,
    035,
    036,
    037,
    038,
    039,
    040,
    041,
    042,
    043,
    044,
    045,
    046,
    047,
    048,
    049,
    050,
    051,
    052,
    053,
    054,
    056,
    057,
    058,
    059,
    060,
    061,
    062,
    063,
    064,
    065,
    066,
    067,
    068,
    069,
    070,
    071,
    072,
    073,
    074,
    075,
    076,
    077,
    078,
    079,
    080,
    081,
    082,
    083,
    084,
    085,
    086,
    087,
    088,
    089,
    090,
    091,
    092,
    093,
    094,
    095,
    096,
    098,
    099,
    100,
    101,
    102,
    103,
    104,
    105,
    106,
    107,
    108,
    109,
    110,
    111,
    112,
    113,
    114,
    115,
    116,
    117,
    118,
    119,
    120,
    121,
    122,
    123,
    124,
    125,
    126,
    127,
    128,
    129,
    130,
    131,
    132,
    133,
    136,
    142,
    145,
    151,
    152,
    153,
    154,
    155,
    156,
    157,
    158,
    159,
    160,
    161,
    162,
    163,
    164,
    165,
    166,
    167,
    168,
    169,
    170,
    171,
    172,
    173,
    174,
    175,
    176,
    177,
    178,
    179,
    180,
    181,
    182,
    183,
    184,
    185,
    186,
    187,
    188,
    189,
    190,
    192,
    193,
    194,
    195,
    196,
    197,
    198,
    199
  )
)

# Read fixed-width file
cius2005 <- read.fwf(
  file = use_network_path(
    "2024 SES Index/data/raw_data/internet_connectivity/CIUS_2005/4432_CIUS_ECUI_2005/data/cius2005_PUMF.txt"
  ),
  widths = var_specs_2005$widths,
  col.names = var_specs_2005$names,
  colClasses = "character", # Read all as character initially
  n = -1, # Read all lines
  buffersize = 2000
)

BC_cius2005 <- cius2005 %>%
  filter(PROVINCE == "59")

#############################################################################
# codebook
# LABEL PUMFID    = "Respondent's identification number";
# LABEL REFYEAR   = "Reference Year";
# LABEL PROVINCE  = "Province";
# LABEL REGION    = "Regions of Canada";
# LABEL G_URBRUR  = "Urban Rural";
# LABEL GFAMTYPE  = "Type of household";
# LABEL G_HHSIZE  = "Number persons in household";
# LABEL HCONNECT  = "Household connected to Internet";
# LABEL EV_Q01    = "Used Internet from any location";
# LABEL  G_HQUINT = "Household Income quintile"
# LABEL WTPP = "Survey weight of a person"

# /* PROVINCE format applies to: PROVINCE  */
#   VALUE PROVINCE
# 10 = "Newfoundland and Labrador"
# 11 = "Prince Edward Island"
# 12 = "Nova Scotia"
# 13 = "New Brunswick"
# 24 = "Quebec"
# 35 = "Ontario"
# 46 = "Manitoba"
# 47 = "Saskatchewan"
# 48 = "Alberta"
# 59 = "British Columbia";
# /* REGION format applies to: REGION  */
#   VALUE REGION
# 1 = "Atlantic Region"
# 2 = "Quebec"
# 3 = "Ontario"
# 4 = "Manitoba/Saskatchewan"
# 5 = "Alberta"
# 6 = "British Columbia";

# /* sYesNo format applies to:
#   G_CSTUD  HCONNECT  G_HSTUD  */
#   VALUE sYesNo
# 1 = "Yes"
# 2 = "No";

# /* EV_Q01F format applies to: EV_Q01  */
#   VALUE EV_Q01F
# 1 = "Yes"
# 2 = "No"
# 6 = "Valid skip"
# 7 = "Don't know"
# 8 = "Refusal"
# 9 = "Not stated";

# /* G_HQUINT format applies to: G_HQUINT  */
# VALUE G_HQUINT
# 1 = "Quintile 1 - <=$21,000"
# 2 = "Quintile 2 - $21,001 - $37,999"
# 3 = "Quintile 3 - $38,000 - $59,999"
# 4 = "Quintile 4 - $60,000 - $85,999"
# 5 = "Quintile 5 - $86,000 +";

#############################################################################
BC_cius2005 %>%
  select(
    PUMFID,
    REFYEAR,
    PROVINCE,
    REGION,
    G_URBRUR,
    GFAMTYPE,
    G_HHSIZE,
    HCONNECT,
    EV_Q01,
    G_HEDUC,
    G_HSTUD,
    G_HQUINT,
    WTPP
  ) %>%
  glimpse()


# Convert WTPP to numeric with 4 decimal places
cius2005$WTPP <- as.numeric(cius2005$WTPP) / 10000

# Convert other numeric columns as needed
numeric_cols <- c("PUMFID", "REFYEAR", "PROVINCE") # Add other numeric columns as needed
cius2005[numeric_cols] <- lapply(cius2005[numeric_cols], as.numeric)

# Save processed data
saveRDS(cius2005, "cius2005.rds")


### srvyr package

# In the code above we used the `as_survey` function in the *srvyr* package. We set the weight to be the [WTPP variable](https://ubc-library-rc.github.io/r-microdata/content/orientation_ctns.html#weights). After this point, the data will automatically be weighted for graphs and analyses.
## Create summary tables

### gtsummary package

# There are many ways to create tables in R. The `tbl_svysummary` function from the *gtsummary* package takes advantage of the survey weight we configured earlier and produces easy-to-read tables with little effort.

# Here's a sample table with one variable...

#Table estimating population by smoking status, CTNS 2020

tbl_svysummary(ctns2020, include = DV_SSR)


#Table estimating population by smoking status and province, CTNS 2020
tbl_svysummary(ctns2020, include = DV_SSR, by = PROV_C)
