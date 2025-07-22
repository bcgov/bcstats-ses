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

# this file is used for downloading data from StatsCan and other source.
# library("remotes")
# install_github("bcgov/safepaths")
pacman::p_load(
  cancensus,
  geojsonsf,
  sf,
  tidyverse,
  config,
  bcmaps,
  bcdata,
  janitor,
  cansim,
  safepaths,
  arrow,
  duckdb,
  datadictionary
)
if (!fs::dir_exists("out")) fs::dir_create("out")
######################################################################################
# Census data
# 2025-04-11 add proportion of indigenous population DA level
######################################################################################
# see census/background.txt to get information for installation and background.

# if you get the cache for cancensus package working, set use_cache flag as TRUE, then it will pull data from cache.
# Otherwise, keep the use_cache flag as FALSE, then it will download the data from statscan. It may take a few minutes.
use_cache = TRUE


# Put all the variable strings together:
CA21_VECTORS = c(
  # 'POP'             = 'v_CA21_1',  # Population, 2021

  'POP_PCT_CHANGE' = 'v_CA21_3', # Population percentage change, 2016 to 2021

  'POP_DENS' = 'v_CA21_6', # Population density

  'DWELL_TOT' = 'v_CA21_434', # Dwellings - Total
  'DWELL_HOUS' = 'v_CA21_435', # Dwellings - Single detached house
  'DWELL_APT_LRG' = 'v_CA21_440', # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML' = 'v_CA21_439', # Dwellings - Apartment < 5 stories
  'DWELL_MOVE' = 'v_CA21_442', # Dwellings - Movable Done

  ###########################################################################################################################################
  # Indigenous
  # 25% data Indigenous identity, status, and ancestry
  # Total - Indigenous identity for the population in private households
  "INDIG_TOT" = 'v_CA21_4201',
  # Indigenous identity - Total
  "INDIG" = 'v_CA21_4204', # Indigenous identity
  ###########################################################################################################################################
  # Family

  'FAMILIES_TOT' = 'v_CA21_499', # Total number of census families in private households
  'LONE_PARENT_TOT' = 'v_CA21_507', # Lone Parent - Total
  'LONE_PARENT_F' = 'v_CA21_508', # Lone Parent - Female
  'LONE_PARENT_M' = 'v_CA21_509', # Lone Parent - Male Done

  ###########################################################################################################################################
  # Income
  # Household income
  # Income statistics for private households
  # Median total income of household in 2020 ($)
  # Median after-tax income of household in 2020 ($)
  # Income statistics for one-person private households
  # Income statistics for two-or-more-persons private households
  # Household total income groups in 2020 for private households
  # Household after-tax income groups in 2020 for private households

  'INC_BT_HHS_MED' = 'v_CA21_906', # Median total income of household in 2020 ($)
  'INC_AT_HHS_MED' = 'v_CA21_907', # Median after-tax income of household in 2020 ($)

  'INC_BT_IND_MED' = 'v_CA21_560', # Total Individual Income Before Tax - Median -  Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2020 among recipients ($)
  # 'INC_BT_IND_MED_19'  = 'v_CA21_818', # Total Individual Income Before Tax - Median -Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2019 among recipients ($)
  'INC_BT_IND_AVG' = 'v_CA21_605', # Average total income in 2020 among recipients ($) - Total Individual Income Before Tax - Average ? Number of total income recipients aged 15 years and over in private households in 2020 Average total income in 2020 among recipients ($) - 35%

  'INC_AT_LONE_PARENT_MED' = 'v_CA21_978', # Median after-tax income of lone-parent economic families : Income statistics for one-parent economic families in private households: Median after-tax income of one-parent economic families in 2020 ($)
  'INC_AT_CPL_W_CHILD_MED' = 'v_CA21_974', # Median after-tax income of couple economic families with children: in 21 couple-only and couple-with-children: Median after-tax income of couple-with-children economic families in 2020 ($) Done

  # 25% Data
  # Income
  # Economic family income
  # Income statistics for economic families in private households
  # Average total income of economic family in 2020 ($)
  # Average after-tax income of economic family in 2020 ($)

  'INC_BT_FAM_MED' = 'v_CA21_965', # Total Family Income Before Tax - Median
  'INC_AT_FAM_MED' = 'v_CA21_966', # Total Family Income After Tax - Median

  # Inequality measures for the population in private households
  # 'GINI_INDEX_         # Gini index on adjusted household total income
  # 'GINI_INDEX_         # Gini index on adjusted household market income
  'GINI_INDEX_AT_INC' = 'v_CA21_1142', # Gini index on adjusted household after-tax income

  # v_CA21_1143
  # Total
  # P90/P10 ratio on adjusted household after-tax income
  # Income; Inequality measures for the population in private households; P90/P10 ratio on adjusted household after-tax income

  'P90P10_RATIO' = 'v_CA21_1143', # P90/P10 ratio on adjusted household after-tax income Done

  # CA 2021 Census
  # 100% data
  # Income
  # Low income status
  # LIM-AT
  # In low income based on the Low-income measure, after tax (LIM-AT)
  # Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)
  # LICO
  # In low income based on the Low-income cut-offs, after tax (LICO-AT)

  # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)

  'LICO_AT_PREVALENCE' = 'v_CA21_1085', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%): Share of people in low income (LICO-AT)
  'LIM_AT_PREVALENCE' = 'v_CA21_1040', # Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)

  # LICO-AT is a hybrid measure, originally relative in nature but now functioning as an "anchored" indicator of low income. Its thresholds are based on 1992 family expenditure patterns and are updated annually only for inflation using the Consumer Price Index (CPI). Consequently, while mathematically consistent, LICO-AT measures progress against a standard of living that becomes less relevant with each passing year. In contrast, LIM-AT is a purely relative measure, defined as 50% of the contemporary median adjusted household income. It is recalculated annually, ensuring it reflects current societal income distributions and making it the international standard for such comparisons.
  # LICO-AT answers the question, "Are the poor better off than they were in 1992?" while LIM-AT answers, "Are the poor keeping pace with the middle of society today?"
  ###########################################################################################################################################
  # Occupation from Labour force stats
  # 25% Data
  # Work
  # Total - Labour force aged 15 years and over by occupation - Broad category - National Occupational Classification (NOC) 2021
  # Occupation - not applicable
  # All occupations
  # 0 Legislative and senior management occupations
  # 1 Business, finance and administration occupations
  # 2 Natural and applied sciences and related occupations
  # 3 Health occupations
  # 4 Occupations in education, law and social, community and government services
  # 5 Occupations in art, culture, recreation and sport
  # 6 Sales and service occupations
  # 7 Trades, transport and equipment operators and related occupations
  # 8 Natural resources, agriculture and related production occupations
  # 9 Occupations in manufacturing and utilities

  # 'OCC_TOT' = 'v_CA21_6561', # Occupation - Total - Labour force aged 15 years and over by occupation - Broad category - National Occupational Classification (NOC) 2021
  'OCC_TOT' = 'v_CA21_6567', # Occupation - All occupations, should be the same as previous line, except excluding Occupation - not applicable
  'OCC_MGMT' = 'v_CA21_6570', # Occupation - 0 Legislative and senior management occupations <- Management, in 2021 StatsCAN change the definition of this item
  'OCC_BUS_FIN_AD' = 'v_CA21_6573', # Occupation - 1 Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA21_6576', # Occupation - 2 Natural and applied sciences and related occupations
  'OCC_HLTH' = 'v_CA21_6579', # Occupation - 3 Health
  'OCC_SOCI_SERV' = 'v_CA21_6582', # Occupation - 4 Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA21_6585', # Occupation - 5 Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV' = 'v_CA21_6588', # Occupation - 6 Sales and Service
  'OCC_TRADES' = 'v_CA21_6591', # Occupation - 7 Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC' = 'v_CA21_6594', # Occupation - 8 Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL' = 'v_CA21_6597', # Occupation - 9 Manufacturing and Utilities Done

  # 25% Data
  # Education
  # TODO: we many only top 1,2,3 numbers or , 1, 2, 3.1 and 3.2
  # Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households
  # 1 No certificate, diploma or degree
  # 2 High (secondary) school diploma or equivalency certificate
  # 3 Postsecondary certificate, diploma or degree
  #   3.1 Postsecondary certificate or diploma below bachelor level
  #     3.1.1 Apprenticeship or trades certificate or diploma
  #     3.1.2 College, CEGEP or other non-university certificate or diploma
  #     3.1.3 University certificate or diploma below bachelor level
  #   3.2 Bachelor's degree or higher
  #     3.2.1 Bachelor's degree
  #     3.2.2 University certificate or diploma above bachelor level
  #     3.2.3 Degree in medicine, dentistry, veterinary medicine or optometry
  #     3.2.4 Master's degree
  #     3.2.5 Earned doctorate
  # from 2021
  'EDUC_TOT' = 'v_CA21_5865', # Education for population aged 25 - 64 - Total
  'EDUC_NONE' = 'v_CA21_5868', # 1 Education for population aged 25 - 64 - No certificate, diploma or degree
  'EDUC_HIGHSCH' = 'v_CA21_5871', # 2 Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate

  'EDUC_POSTSEC' = 'v_CA21_5874', # 3 Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree
  # following are the subsection of the 5874: two parts: below bachelor level 5877 ; above bachelor level5895
  'EDUC_POSTSEC_BELOW_BACH' = 'v_CA21_5877', # 3.1 Education for population aged 25 - 64 - Postsecondary certificate or diploma below bachelor level
  # following are subsection of 5877
  'EDUC_TRADES' = 'v_CA21_5880', # 3.1.1 Education for population aged 25 - 64 - Apprenticeship or trades certificate or diploma
  'EDUC_COLLEGE' = 'v_CA21_5889', # 3.1.2 Education for population aged 25 - 64 - College, CEGEP or other non-university certificate or diploma

  'EDUC_BACHELOR_HIGHER' = 'v_CA21_5895', # 3.2 Education for population aged 25 - 64 - Bachelor's degree or higher
  # following are the subsection of the 5895 Bachelor's degree or higher
  'EDUC_BACHELOR' = 'v_CA21_5898', # 3.2.1 Education for population aged 25 - 64 - Bachelor's degree

  'EDUC_CRT_ABV_BACH' = 'v_CA21_5901', # 3.2.2 Education for population aged 25 - 64 - University certificate or diploma above bachelor level
  'EDUC_MEDICAL' = 'v_CA21_5904', # 3.2.3 Education for population aged 25 - 64 - Degree in medicine, dentistry, veterinary medicine or optometry
  'EDUC_MASTERS' = 'v_CA21_5907', # 3.2.4 Education for population aged 25 - 64 - Master's degree
  'EDUC_PHD' = 'v_CA21_5910', # 3.2.5 Education for population aged 25 - 64 - Earned doctorate Done

  ###########################################################################################################################################
  # labor market
  # 25% Data
  # Work
  # Total - Population aged 15 years and over by labour force status
  # In the labour force
  # Employed
  # Unemployed
  # Not in the labour force

  'IN_LAB_FORCE_TOT' = 'v_CA21_6492', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES' = 'v_CA21_6495', # Population aged 15 years and over by Labour force status - in labour force
  'IN_LAB_FORCE_NO' = 'v_CA21_6504', # Population aged 15 years and over by Labour force status - not in labour force Done
  'LABOUR_EMPL_CNT' = 'v_CA21_6498',
  'LABOUR_UNEM_CNT' = 'v_CA21_6501', # Need to calculate the rate cnt/total Done

  ###########################################################################################################################################
  # Housing
  # 25% Data
  # Housing
  # Total - Private households by tenure
  # Owner
  # Renter
  # Dwelling provided by the local government, First Nation or Indian band
  'HOME_OWN_TOT' = 'v_CA21_4237', # Home ownership - Total
  'HOME_OWN_OWN' = 'v_CA21_4238', # Home ownership - Owner
  'HOME_OWN_RENT' = 'v_CA21_4239', # Home ownership - Renter
  'HOME_OWN_BAND' = 'v_CA21_4240', # Home ownership - Band Housing Done

  # 25% Data
  # Housing
  # Total - Owner households in non-farm, non-reserve private dwellings
  # % of owner households with a mortgage
  # % of owner households spending 30% or more of its income on shelter costs
  # % in core housing need
  # Median monthly shelter costs for owned dwellings ($)
  # Average monthly shelter costs for owned dwellings ($)
  # Median value of dwellings ($)
  # Average value of dwellings ($)

  'DWELLING_VALUE_AVG' = 'v_CA21_4312', # Average value of dwelling
  'DWELLING_VALUE_MED' = 'v_CA21_4311', # Median value of dwelling
  'DWELLING_COST_OWN' = 'v_CA21_4310', # Average monthly shelter costs for owned dwellings Done

  # 25% Data
  # Housing
  # Total - Tenant households in non-farm, non-reserve private dwellings
  # Median monthly shelter costs for rented dwellings ($)
  # Average monthly shelter costs for rented dwellings ($)

  'DWELLING_RENT_AVG' = 'v_CA21_4318', # Average monthly shelter costs for rented dwellings
  'DWELLING_RENT_MED' = 'v_CA21_4317', # Median monthly shelter costs for rented dwellings Done

  # 25% Data
  # Housing
  # Total - Occupied private dwellings by dwelling condition
  # Only regular maintenance and minor repairs needed
  # Major repairs needed

  'REPAIRS_TOT' = 'v_CA21_4272', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA21_4273', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA21_4274', # Occupied private dwellings by dwelling condition	- Major repairs needed Done

  # 25% Data
  # Housing
  # Total - Private households by housing suitability
  # Suitable
  # Not suitable
  # Housing suitability
  # refers to whether a private household is living in suitable accommodations according to the National Occupancy Standard (NOS); that is, whether the dwelling has enough bedrooms for the size and composition of the household. A household is deemed to be living in suitable accommodations if its dwelling has enough bedrooms, as calculated using the
  # NOS .
  'HOUSING_SUITABLE_TOT' = 'v_CA21_4260', # Private households by housing suitability - Total
  'HOUSING_SUITABLE_YES' = 'v_CA21_4261', # Private households by housing suitability - Suitable
  'HOUSING_SUITABLE_NO' = 'v_CA21_4262', # Private households by housing suitability - Not suitable Done

  # 25% Data
  # Commute
  # Total - Time leaving for work for the employed labour force aged 15 years and over with a usual place of work or no fixed workplace address
  # Between 5 a.m. and 5:59 a.m.
  # Between 6 a.m. and 6:59 a.m.
  # Between 7 a.m. and 7:59 a.m.
  # Between 8 a.m. and 8:59 a.m.
  # Between 9 a.m. and 11:59 a.m.
  # Between 12 p.m. and 4:59 a.m.

  'GO_TO_WORK_TOT' = 'v_CA21_7674', # Total - Time leaving for work for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address
  'GO_TO_WORK_5AM_6AM' = 'v_CA21_7677', # Between 5 a.m. and 5:59 a.m.
  'GO_TO_WORK_6AM_7AM' = 'v_CA21_7680', # Between 6 a.m. and 6:59 a.m.
  'GO_TO_WORK_7AM_8AM' = 'v_CA21_7683', # Between 7 a.m. and 7:59 a.m.
  'GO_TO_WORK_8AM_9AM' = 'v_CA21_7686', # Between 8 a.m. and 8:59 a.m.
  'GO_TO_WORK_9AM_12PM' = 'v_CA21_7689', # Between 9 a.m. and 11:59 a.m.
  'GO_TO_WORK_12PM_4AM' = 'v_CA21_7692' # Between 12 p.m. and 4:59 a.m.
)


# this function extracts information of vector/variable from StatsCan website.
create_census_meta_data <- function(
  CENSUS_NUMBER = "CA21",
  VECTORS = CA21_VECTORS
) {
  # convert the vector to a dataframe, so we can  join to other table
  VECTORS_DF <- data.frame(
    name = names(VECTORS),
    vector = as.character(VECTORS)
  )

  vector_list <- list_census_vectors(CENSUS_NUMBER)

  VECTORS_DF_DICT <- VECTORS_DF %>%
    left_join(vector_list, by = join_by(vector))

  return(VECTORS_DF_DICT)
}


CA21_META_DATA <- create_census_meta_data(
  CENSUS_NUMBER = "CA21",
  VECTORS = CA21_VECTORS
)

CA21_META_DATA %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_21_BC_META_DATA_DIP.csv"))

# since we have cache on the LAN, we need vpn2 and connect to LAN
CA21_DATA <- get_census(
  dataset = 'CA21',
  regions = list(PR = "59"),
  vectors = CA21_VECTORS,
  level = 'DA',
  quiet = TRUE,
  geo_format = NA,
  use_cache = use_cache,
  labels = 'short'
)


CA21_DATA = CA21_DATA %>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(REGION_NAME = as.character(REGION_NAME))

CA21_DATA %>%
  mutate(CENSUS_YEAR = 2021) %>%
  select(CENSUS_YEAR, everything()) %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_21_BC_DA_DIP.csv"))

str(CA21_DATA)
attr(CA21_DATA, "last_updated")


CENSUS_COMMON_LABELS <- c(
  'GEO_UID' = "Geographic unique ID",
  'TYPE' = 'Geographic type: DA or CSD',
  'REGION_NAME' = 'Geographic name; in 2021 it is DA id; but in other waves of survey; it was region name.',
  'AREA_SQ_KM' = 'Aear in squared kilometer',

  'POPULATION' = "Population",
  'DWELLINGS' = "Number of dwellings",
  'HOUSEHOLDS' = 'Number of households',

  'CSD_UID' = 'CSD_UID',
  'CD_UID' = 'CD_UID',
  'CT_UID' = 'CT_UID',
  'CMA_UID' = 'CMA_UID'
)


# # this function creates labels for vector/variable and for the datadictionary function.
create_census_data_label <- function(DATA, VECTORS, CENSUS_COMMON_LABELS) {
  DATA_VECTOR_DETAIL <- attr(DATA, "census_vectors")

  DATA_VECTOR_DETAIL <- DATA_VECTOR_DETAIL %>%
    janitor::clean_names(case = "screaming_snake") %>%
    left_join(tibble(VARIABLE_NAME = names(VECTORS), VECTOR = VECTORS))

  DATA_VECTOR_DETAIL <- tibble(
    VARIABLE_NAME = names(CENSUS_COMMON_LABELS),
    DETAIL = CENSUS_COMMON_LABELS
  ) %>%
    bind_rows(DATA_VECTOR_DETAIL)

  # Opposite to enframe() , the function deframe() converts a two-column tibble to a named vector or list, using the first column as name and the second column as value.

  DATA_labels <- deframe(DATA_VECTOR_DETAIL %>% select(VARIABLE_NAME, DETAIL))

  return(DATA_labels)
}


CA21_DATA_labels <- create_census_data_label(
  CA21_DATA,
  CA21_VECTORS,
  CENSUS_COMMON_LABELS
)


# add label to dictionary, which is similar to decription.
skimr::skim(CA21_DATA) # this is for exploring purpose
# lowest complete rate is 0.946
# Number of rows             7848
# Number of columns          87

# Create labels as a named vector.

CA21_DATA_dict = create_dictionary(
  CA21_DATA,
  # id_var = GEO_UID,
  var_labels = CA21_DATA_labels
)

CA21_DATA_dict %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_21_BC_DA_DICT_DIP.csv"))


# ********************************************************************************
# PULL 2016 DATA
# ********************************************************************************

CA16_VECTORS_LIST = list_census_vectors('CA16')

CA16_VECTORS_LIST %>%
  filter(str_detect(string = details, pattern = "lone"))

CA16_FAMILY_VECTORS_LIST <- find_census_vectors(
  "family",
  dataset = "CA16",
  type = "total",
  query_type = "exact"
)

CA16_VECTORS = c(
  'DWELL_TOT' = 'v_CA16_408', # Dwellings - Total
  'DWELL_HOUS' = 'v_CA16_409', # Dwellings - Single detached house
  'DWELL_APT_LRG' = 'v_CA16_410', # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML' = 'v_CA16_415', # Dwellings - Apartment < 5 stories
  'DWELL_MOVE' = 'v_CA16_417', # Dwellings - Movable
  'FAMILIES_TOT' = 'v_CA16_484', # Number of families
  'LONE_PARENT_TOT' = 'v_CA16_488', # Lone Parent - Total
  'LONE_PARENT_F' = 'v_CA16_489', # Lone Parent - Female
  'LONE_PARENT_M' = 'v_CA16_490', # Lone Parent - Male
  'INC_BT_IND_MED' = 'v_CA16_2207', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG' = 'v_CA16_4957', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED' = 'v_CA16_2447', # Total Family Income Before Tax - Median Median total income of economic families in 2015 ($)
  'INC_BT_FAM_AVG' = 'v_CA16_4994', # Total Family Income Before Tax - Average / Average total income of economic families in 2015 ($)
  'INC_AT_FAM_MED' = 'v_CA16_2448', # Total Family Income After Tax - Median Median after-tax income of economic families in 2015 ($)
  'INC_AT_FAM_AVG' = 'v_CA16_4995', # Total Family Income After Tax - Average / Average after-tax income of economic families in 2015 ($)
  'INC_BT_HHS_MED' = 'v_CA16_2397', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG' = 'v_CA16_4985', # Total Household Income Before Tax - Average
  'LICO_AT_PREVALENCE' = 'v_CA16_2570', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
  'LIM_AT_PREVALENCE' = 'v_CA16_2540', # Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)

  # 'OCC_TOT' = 'v_CA16_5654', # Occupation - Total includes: not applicable +   All occupations
  'OCC_TOT' = 'v_CA16_5660', # Occupation - All occupations
  'OCC_MGMT' = 'v_CA16_5663', # Occupation - Management -> before StatsCAN changed it in 2021
  'OCC_BUS_FIN_AD' = 'v_CA16_5666', # Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA16_5669', # Occupation - Natural and Applied Sciences
  'OCC_HLTH' = 'v_CA16_5672', # Occupation - Health
  'OCC_SOCI_SERV' = 'v_CA16_5675', # Occupation - Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA16_5678', # Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV' = 'v_CA16_5681', # Occupation - Sales and Service
  'OCC_TRADES' = 'v_CA16_5684', # Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC' = 'v_CA16_5687', # Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL' = 'v_CA16_5690', # Occupation - Manufacturing and Utilities

  # Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data
  'EDUC_TOT' = 'v_CA16_5096', # 0 Education for population aged 25 - 64 - Total
  'EDUC_NONE' = 'v_CA16_5099', # 1 Education for population aged 25 - 64 - No certificate, diploma or degree
  'EDUC_HIGHSCH' = 'v_CA16_5102', # 2 Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate
  'EDUC_POSTSEC' = 'v_CA16_5105', # 3 Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree
  # no 3.1 in 2016
  'EDUC_TRADES' = 'v_CA16_5108', # 3.1.1 Education for population aged 25 - 64 - Apprenticeship or trades certificate or diploma
  'EDUC_COLLEGE' = 'v_CA16_5117', # 3.1.2 Education for population aged 25 - 64 - College, CEGEP or other non-university certificate or diploma

  # 3.2 Education for population aged 25 - 64 - Bachelor's degree or higher
  'EDUC_BACHELOR_HIGHER' = 'v_CA16_5123', # 3.2 Education for population aged 25 - 64 - University certificate, diploma or degree at bachelor level or above
  'EDUC_BACHELOR' = 'v_CA16_5126', # 3.2.1 Education for population aged 25 - 64 - Bachelor's degree
  'EDUC_CRT_ABV_BACH' = 'v_CA16_5129', # 3.2.2 Education for population aged 25 - 64 - University certificate or diploma above bachelor level
  'EDUC_MEDICAL' = 'v_CA16_5132', # 3.2.3 Education for population aged 25 - 64 - Degree in medicine, dentistry, veterinary medicine or optometry
  'EDUC_MASTERS' = 'v_CA16_5135', # 3.2.4 Education for population aged 25 - 64 - Master's degree
  'EDUC_PHD' = 'v_CA16_5138', #  3.2.5 Education for population aged 25 - 64 - Earned doctorate

  'LABOUR_PART_RT' = 'v_CA16_5612', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT' = 'v_CA16_5615', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT' = 'v_CA16_5618', # Labour Force - Unemployment Rate
  'IN_LAB_FORCE_TOT' = 'v_CA16_5597', # Population aged 15 years and over by Labour force status - Total

  'IN_LAB_FORCE_YES' = 'v_CA16_5600', # Population aged 15 years and over by Labour force status - in labour force
  # following two numbers are under In the labour force
  'LABOUR_EMPL_CNT' = 'v_CA16_5603', # Labour force - Employed
  'LABOUR_UNEM_CNT' = 'v_CA16_5606', # Labour force - Unemployed

  'IN_LAB_FORCE_NO' = 'v_CA16_5609', # Population aged 15 years and over by Labour force status - not in labour force

  'HOME_OWN_TOT' = 'v_CA16_4836', # Home ownership - Total
  'HOME_OWN_OWN' = 'v_CA16_4837', # Home ownership - Owner
  'HOME_OWN_RENT' = 'v_CA16_4838', # Home ownership - Renter
  'HOME_OWN_BAND' = 'v_CA16_4839', # Home ownership - Band Housing
  'DWELLING_VALUE_AVG' = 'v_CA16_4896', # Average value of dwelling
  'DWELLING_VALUE_MED' = 'v_CA16_4895', # Median value of dwelling
  'DWELLING_COST_OWN' = 'v_CA16_4894', # Average monthly shelter costs for owned dwellings
  'DWELLING_RENT_AVG' = 'v_CA16_4901', # Average monthly shelter costs for rented dwellings
  'DWELLING_RENT_MED' = 'v_CA16_4900', # Median monthly shelter costs for rented dwellings
  'SUBSIDIZED_HOUS' = 'v_CA16_4898', # % of tenant households in subsidized housing
  'HOUSING_SUITABLE_TOT' = 'v_CA16_4859', # Private households by housing suitability - Total
  'HOUSING_SUITABLE_YES' = 'v_CA16_4860', # Private households by housing suitability - Suitable
  'HOUSING_SUITABLE_NO' = 'v_CA16_4861', # Private households by housing suitability - Not suitable
  'REPAIRS_TOT' = 'v_CA16_4870', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA16_4871', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA16_4872', # Occupied private dwellings by dwelling condition	- Major repairs needed
  'GO_TO_WORK_TOT' = 'v_CA16_5831', # Total - Time leaving for work for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address
  'GO_TO_WORK_5AM_6AM' = 'v_CA16_5834', # Between 5 a.m. and 5:59 a.m.
  'GO_TO_WORK_6AM_7AM' = 'v_CA16_5837', # Between 6 a.m. and 6:59 a.m.
  'GO_TO_WORK_7AM_8AM' = 'v_CA16_5840', # Between 7 a.m. and 7:59 a.m.
  'GO_TO_WORK_8AM_9AM' = 'v_CA16_5843', # Between 8 a.m. and 8:59 a.m.
  'GO_TO_WORK_9AM_12PM' = 'v_CA16_5846', # Between 9 a.m. and 11:59 a.m.
  'GO_TO_WORK_12PM_4AM' = 'v_CA16_5849' # Between 12 p.m. and 4:59 a.m.
)

CA16_META_DATA <- create_census_meta_data(
  CENSUS_NUMBER = "CA16",
  VECTORS = CA16_VECTORS
)


CA16_META_DATA %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_16_BC_META_DATA_DIP.csv"))


CA16_DATA <- get_census(
  dataset = "CA16",
  regions = list(PR = "59"),
  vectors = CA16_VECTORS,
  level = 'DA',
  quiet = TRUE,
  geo_format = NA,
  use_cache = use_cache,
  labels = 'short'
)


CA16_DATA = CA16_DATA %>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(REGION_NAME = as.character(REGION_NAME))


CA16_DATA %>%
  mutate(CENSUS_YEAR = 2016) %>%
  select(CENSUS_YEAR, everything()) %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_16_BC_DA_DIP.csv"))

str(CA16_DATA)
# tibble [7,617 × 84]

CA16_DATA_labels <- create_census_data_label(
  CA16_DATA,
  CA16_VECTORS,
  CENSUS_COMMON_LABELS
)

str(CA16_DATA_labels)
# 84

# add label to dictionary, which is similar to description.
skimr::skim(CA16_DATA) # this is for exploring purpose
# Create labels as a named vector.

CA16_DATA_dict = create_dictionary(
  CA16_DATA,
  # id_var = GEO_UID,
  var_labels = CA16_DATA_labels
)

CA16_DATA_dict %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_16_BC_DA_DICT_DIP.csv"))


# ********************************************************************************
# PULL 2011 DATA
# ********************************************************************************

CA11_VECTORS = c(
  'DWELL_TOT' = 'v_CA11F_199', # Dwellings - Total
  'DWELL_HOUS' = 'v_CA11F_200', # Dwellings - Single detached house
  'DWELL_APT_LRG' = 'v_CA11F_201', # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML' = 'v_CA11F_207', # Dwellings - Apartment < 5 stories
  'DWELL_MOVE' = 'v_CA11F_202', # Dwellings - Movable

  'FAMILIES_TOT' = 'v_CA11F_115', # Total number of census families in private households
  'LONE_PARENT_TOT' = 'v_CA11F_129', # Lone Parent - Total
  'LONE_PARENT_F' = 'v_CA11F_130', # Lone Parent - Female
  'LONE_PARENT_M' = 'v_CA11F_134', # Lone Parent - Male
  'INC_BT_IND_MED' = 'v_CA11N_2341', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG' = 'v_CA11N_2344', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED' = 'v_CA11N_2456', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG' = 'v_CA11N_2457', # Total Family Income Before Tax - Average
  'INC_AT_FAM_MED' = 'v_CA11N_2458', # Total Family Income After Tax - Median
  'INC_AT_FAM_AVG' = 'v_CA11N_2459', # Total Family Income After Tax - Average
  'INC_BT_HHS_MED' = 'v_CA11N_2562', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG' = 'v_CA11N_2563', # Total Household Income Before Tax - Average
  'INC_FAM_TOP' = 'v_CA11N_2515', # Total - Economic family income - In the top half of the distribution
  'INC_FAM_BOTTOM' = 'v_CA11N_2497', # Total - Economic family income - In the bottom half of the distribution
  'INC_FAM_DEC_10' = 'v_CA11N_2530', # Total - Economic family income - In the top decile
  'INC_HHS_GRP_TOT' = 'v_CA11N_2547', # Total - Household after-tax income groups - Total
  'INC_HHS_100k_125k' = 'v_CA11N_2559', # Total - Household after-tax income groups - $100,000 to $124,999
  'INC_AT_LONE_PARENT_AVG' = 'v_CA11N_2477', # Average after-tax income of lone-parent economic families
  'INC_AT_CPL_W_CHILD_AVG' = 'v_CA11N_2471', # Average after-tax income of couple economic families with children
  'INC_AT_LONE_PARENT_MED' = 'v_CA11N_2476', # Median after-tax income of lone-parent economic families
  'INC_AT_CPL_W_CHILD_MED' = 'v_CA11N_2470', # Median after-tax income of couple economic families with children
  'LIM_AT_PREVALENCE' = 'v_CA11N_2606', # Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)
  # 'LICO_AT_PREVALENCE' = '', # Not in census table, but in NHS 2011 and only Canada value no BC or DA in CANSIM Frequency: Occasional  Table: 11-10-0195-01 (formerly CANSIM 206-0092)
  # https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=fam019
  # https://www12.statcan.gc.ca/nhs-enm/2011/ref/dict/fam019-eng.cfm

  # 'OCC_TOT' = 'v_CA11N_2026', # Occupation - Total
  'OCC_TOT' = 'v_CA11N_2032', # Occupation - All occupations
  'OCC_MGMT' = 'v_CA11N_2035', # Occupation - Management
  'OCC_BUS_FIN_AD' = 'v_CA11N_2038', # Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA11N_2041', # Occupation - Natural and Applied Sciences
  'OCC_HLTH' = 'v_CA11N_2044', # Occupation - Health
  'OCC_SOCI_SERV' = 'v_CA11N_2047', # Occupation - Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA11N_2050', # Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV' = 'v_CA11N_2053', # Occupation - Sales and Service
  'OCC_TRADES' = 'v_CA11N_2056', # Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC' = 'v_CA11N_2059', # Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL' = 'v_CA11N_2062', # Occupation - Manufacturing and Utilities

  # Total population aged 25 to 64 years by highest certificate, diploma or degree
  'EDUC_TOT' = 'v_CA11N_1801', # 0 Education for population aged 25 - 64 - Total
  'EDUC_NONE' = 'v_CA11N_1804', # 1 Education for population aged 25 - 64 - No certificate, diploma or degree
  'EDUC_HIGHSCH' = 'v_CA11N_1807', # 2 Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate

  'EDUC_POSTSEC' = 'v_CA11N_1810', # 3 Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree
  'EDUC_TRADES' = 'v_CA11N_1813', # 3.1 Education for population aged 25 - 64 - Apprenticeship or trades certificate or diploma
  'EDUC_COLLEGE' = 'v_CA11N_1816', # 3.2 Education for population aged 25 - 64 - College, CEGEP or other non-university certificate or diploma
  'EDUC_CRT_BELOW_BACH' = 'v_CA11N_1819', # 3.3 University certificate or diploma below bachelor level

  'EDUC_BACHELOR_HIGHER' = 'v_CA11N_1822', # 3.4 Education for population aged 25 - 64 - University certificate, diploma or degree at bachelor level or above
  'EDUC_BACHELOR' = 'v_CA11N_1825', # 3.4.1 Education for population aged 25 - 64 - Bachelor's degree
  'EDUC_CRT_ABV_BACH' = 'v_CA11N_1828', # 3.4.2 Education for population aged 25 - 64 - University certificate or diploma above bachelor level

  # Canada Census 2011
  # CA 2011 NHS
  # Work
  # Labour force status
  # Total population aged 15 years and over by labour force status
  'IN_LAB_FORCE_TOT' = 'v_CA11N_1987', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES' = 'v_CA11N_1990', # Population aged 15 years and over by Labour force status - in labour force
  'LABOUR_EMPL_CNT' = 'v_CA11N_1993', # Labour force - in labour force - Employed
  'LABOUR_UNEM_CNT' = 'v_CA11N_1996', # Labour force - in labour force - Unemployed
  'IN_LAB_FORCE_NO' = 'v_CA11N_1999', # Population aged 15 years and over by Labour force status - not in labour force

  'LABOUR_PART_RT' = 'v_CA11N_2002', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT' = 'v_CA11N_2005', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT' = 'v_CA11N_2008', # Labour Force - Unemployment Rate

  'HOME_OWN_TOT' = 'v_CA11N_2252', # Home ownership - Total
  'HOME_OWN_OWN' = 'v_CA11N_2253', # Home ownership - Owner
  'HOME_OWN_RENT' = 'v_CA11N_2254', # Home ownership - Renter
  'HOME_OWN_BAND' = 'v_CA11N_2255', # Home ownership - Band Housing
  'DWELLING_VALUE_AVG' = 'v_CA11N_2287', # Average value of dwelling
  'DWELLING_VALUE_MED' = 'v_CA11N_2286', # Median value of dwelling
  'DWELLING_COST_OWN' = 'v_CA11N_2285', # Average monthly shelter costs for owned dwellings
  'DWELLING_RENT_AVG' = 'v_CA11N_2292', # Average monthly shelter costs for rented dwellings
  'DWELLING_RENT_MED' = 'v_CA11N_2291', # Median monthly shelter costs for rented dwellings
  'SUBSIDIZED_HOUS' = 'v_CA11N_2289', # % of tenant households in subsidized housing
  'HOUSING_SUITABLE_TOT' = 'v_CA11N_2274', # Private households by housing suitability - Total
  'HOUSING_SUITABLE_YES' = 'v_CA11N_2275', # Private households by housing suitability - Suitable
  'HOUSING_SUITABLE_NO' = 'v_CA11N_2276', # Private households by housing suitability - Not suitable

  'REPAIRS_TOT' = 'v_CA11N_2230', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA11N_2231', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA11N_2232', # Occupied private dwellings by dwelling condition	- Major repairs needed

  'GO_TO_WORK_TOT' = 'v_CA11N_2218', # Total employed population aged 15 years and over by time leaving for work
  'GO_TO_WORK_5AM_7AM' = 'v_CA11N_2221', # Total employed population aged 15 years and over by time leaving for work - Between 5 and 6:59 a.m.
  'GO_TO_WORK_7AM_9AM' = 'v_CA11N_2224', # Total employed population aged 15 years and over by time leaving for work - Between 7 and 9:00 a.m.
  'GO_TO_WORK_AFTER_9AM' = 'v_CA11N_2227' # Total employed population aged 15 years and over by time leaving for work - Anytime after 9:00 a.m.
)


CA11_META_DATA <- create_census_meta_data(
  CENSUS_NUMBER = "CA11",
  VECTORS = CA11_VECTORS
)

CA11_META_DATA %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_11_BC_META_DATA_DIP.csv"))


CA11_DATA <- get_census(
  dataset = "CA11",
  regions = list(PR = "59"),
  vectors = CA11_VECTORS,
  level = 'DA',
  quiet = TRUE,
  geo_format = NA,
  use_cache = use_cache,
  labels = 'short'
)


CA11_DATA = CA11_DATA %>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(REGION_NAME = as.character(REGION_NAME))


CA11_DATA %>%
  mutate(CENSUS_YEAR = 2011) %>%
  select(CENSUS_YEAR, everything()) %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_11_BC_DA_DIP.csv"))

CENSUS_COMMON_LABELS_11 <- c(
  'GEO_UID' = "Geographic unique ID",
  'TYPE' = 'Geographic type: DA or CSD',
  'REGION_NAME' = 'Geographic name; in 2021 it is DA id, but in other waves of survey; it was region name.',
  'AREA_SQ_KM' = 'Aear in squared kilometer',

  'POPULATION' = "Population",
  'DWELLINGS' = "Number of dwellings",
  'HOUSEHOLDS' = 'Number of households',

  'CSD_UID' = 'CSD_UID',
  'CD_UID' = 'CD_UID',
  'CT_UID' = 'CT_UID',
  'CMA_UID' = 'CMA_UID',
  'NHS_NON_RETURN_RATE' = 'NHS_NON_RETURN_RATE'
)

CA11_DATA_labels <- create_census_data_label(
  CA11_DATA,
  CA11_VECTORS,
  CENSUS_COMMON_LABELS_11
)

str(CA11_DATA)
# [7,582 × 88]
names(CA11_DATA)
# 88
str(CA11_DATA_labels)
# 88
names(CA11_DATA_labels)

# add label to dictionary, which is similar to decription.
skimr::skim(CA11_DATA) # this is for exploring purpose
# Create labels as a named vector.

CA11_DATA_dict = create_dictionary(
  CA11_DATA,
  # id_var = GEO_UID,
  var_labels = CA11_DATA_labels
)

CA11_DATA_dict %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_11_BC_DA_DICT_DIP.csv"))


# ********************************************************************************
# PULL 2006 DATA
# ********************************************************************************

CA06_VECTORS = c(
  'DWELL_TOT' = 'v_CA06_119', # Dwellings - Total
  'DWELL_HOUS' = 'v_CA06_120', # Dwellings - Single detached house
  'DWELL_APT_LRG' = 'v_CA06_124', # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML' = 'v_CA06_125', # Dwellings - Apartment < 5 stories
  'DWELL_MOVE' = 'v_CA06_127', # Dwellings - Movable

  'FAMILIES_TOT' = 'v_CA06_55', # Lone Parent - Total
  'LONE_PARENT_TOT' = 'v_CA06_69', # Lone Parent - Total
  'LONE_PARENT_F' = 'v_CA06_70', # Lone Parent - Female
  'LONE_PARENT_M' = 'v_CA06_74', # Lone Parent - Male
  'INC_BT_IND_MED' = 'v_CA06_1583', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG' = 'v_CA06_1584', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED' = 'v_CA06_1741', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG' = 'v_CA06_1742', # Total Family Income Before Tax - Average
  'INC_AT_FAM_MED' = 'v_CA06_1785', # Total Family Income After Tax - Median
  'INC_AT_FAM_AVG' = 'v_CA06_1786', # Total Family Income After Tax - Average
  'INC_BT_HHS_MED' = 'v_CA06_2000', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG' = 'v_CA06_2001', # Total Household Income Before Tax - Average
  'LICO_AT_PREVALENCE' = 'v_CA06_1981', # Prevalence of low income after tax in 2005 % / Total persons in private households

  # 'OCC_TOT' = 'v_CA06_827', # Occupation - Total # statsCAN change it in 2011 as well, so data in 2006 is different from data in 2011.
  'OCC_TOT' = 'v_CA06_829', # All occupations
  'OCC_MGMT' = 'v_CA06_830', # A Occupation - Management
  'OCC_BUS_FIN_AD' = 'v_CA06_835', # B Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA06_842', # C Occupation - Natural and Applied Sciences
  'OCC_HLTH' = 'v_CA06_845', # D Occupation - Health
  'OCC_SOCI_SERV' = 'v_CA06_850', # E Occupation - E Occupations in social science, education, government service and religion
  'OCC_ART_CUL_REC' = 'v_CA06_854', # F Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV' = 'v_CA06_857', # G Occupation - Sales and Service
  'OCC_TRADES' = 'v_CA06_868', # H Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC' = 'v_CA06_878', # I Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL' = 'v_CA06_882', # J Occupation - Manufacturing and Utilities

  # Total population 25 to 64 years by highest certificate, diploma or degree
  # 1 No certificate, diploma or degree
  # Certificate, diploma or degree
  #   2 High school certificate or equivalent
  #   3.1 Apprenticeship or trades certificate or diploma
  #   3.2 College, CEGEP or other non-university certificate or diploma
  #   3.3 University certificate, diploma or degree
  #     3.3.1 University certificate or diploma below bachelor level
  #     3.3.2 University certificate or degree
  #       3.2.3.1 Bachelor's degree
  #       3.2.3.2 University certificate or diploma above bachelor level
  #       3.2.3.3 Degree in medicine, dentistry, veterinary medicine or optometry
  #       3.2.3.4 Master's degree
  #       3.2.3.5 Earned doctorate

  # Total population 25 to 64 years by highest certificate, diploma or degree # also different from 2011,
  'EDUC_TOT' = 'v_CA06_1248', # Education for population aged 25 - 64 - Total
  'EDUC_NONE' = 'v_CA06_1249', # 1 Education for population aged 25 - 64 - No certificate, diploma or degree
  # following is similar to 2 highschool and equivalency
  'EDUC_HIGHSCH' = 'v_CA06_1251', # 2 Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate
  # following is similar to 3 postsecondary and equivalency
  # 'EDUC_POSTSEC' = EDUC_TRADE +  EDUC_COLLEGE+ EDUC_UNI, # 3 Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree
  'EDUC_TRADE' = 'v_CA06_1252', # Total	Apprenticeship or trades certificate or diploma
  'EDUC_COLLEGE' = 'v_CA06_1253', # 3.2 Total College, CEGEP or other non-university certificate or diploma

  'EDUC_UNI' = 'v_CA06_1254', # Total	University certificate, diploma or degree
  # following are subcategories of 1254, similar to postsecondary and equivalency
  'EDUC_CRT_BELOW_BACH' = 'v_CA06_1255', # 3.1 Education for population aged 25 - 64 - University certificate or diploma below bachelor level
  'EDUC_BACHELOR_HIGHER' = 'v_CA06_1256', # 3.2 Education for population aged 25 - 64 - University certificate, diploma or degree at bachelor level or above
  # following are subcategories of 1256
  'EDUC_BACHELOR' = 'v_CA06_1257', # 3.2.1 Education for population aged 25 - 64 - Bachelor's degree
  'EDUC_CRT_ABV_BACH' = 'v_CA06_1258', # 3.2.2 Education for population aged 25 - 64 - University certificate or diploma above bachelor level
  'EDUC_MEDICAL' = 'v_CA06_1259', # Education for population aged 25 - 64 - Degree in medicine, dentistry, veterinary medicine or optometry
  'EDUC_MASTERS' = 'v_CA06_1260', # Education for population aged 25 - 64 - Master's degree
  'EDUC_PHD' = 'v_CA06_1261', # Education for population aged 25 - 64 - Earned doctorate

  # Census 2006
  # 20% data
  # Labour Force Activity
  # Population 15 years and over by labour force activity
  # Total population 15 years and over by labour force activity
  'IN_LAB_FORCE_TOT' = 'v_CA06_575', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES' = 'v_CA06_576', # Population aged 15 years and over by Labour force status - in labour force
  'LABOUR_EMPL_CNT' = 'v_CA06_577', # Labour force - in labour force - Employed
  'LABOUR_UNEM_CNT' = 'v_CA06_578', # Labour force - in labour force - Unemployed
  'IN_LAB_FORCE_NO' = 'v_CA06_579', # Population aged 15 years and over by Labour force status - not in labour force
  # Labour Force Activity
  'LABOUR_PART_RT' = 'v_CA06_580', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT' = 'v_CA06_581', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT' = 'v_CA06_582', # Labour Force - Unemployment Rate

  'HOME_OWN_TOT' = 'v_CA06_101', # Home ownership - Total
  'HOME_OWN_OWN' = 'v_CA06_102', # Home ownership - Owner
  'HOME_OWN_RENT' = 'v_CA06_103', # Home ownership - Renter
  'HOME_OWN_BAND' = 'v_CA06_104', # Home ownership - Band Housing
  'DWELLING_VALUE_AVG' = 'v_CA06_2054', # Average value of dwelling
  'DWELLING_COST_OWN' = 'v_CA06_2055', # Average owner major payments
  'DWELLING_RENT_AVG' = 'v_CA06_2050', # Average gross rent
  'REPAIRS_TOT' = 'v_CA06_105', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_ONLY_REGULAR' = 'v_CA06_106', # Occupied private dwellings by dwelling condition	- Only regular maintenance
  'REPAIRS_ONLY_MINOR' = 'v_CA06_107', # Occupied private dwellings by dwelling condition	- Only minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA06_108' # Occupied private dwellings by dwelling condition	- Major repairs needed
)

CA06_META_DATA <- create_census_meta_data(
  CENSUS_NUMBER = "CA06",
  VECTORS = CA06_VECTORS
)

CA06_META_DATA %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_06_BC_META_DATA_DIP.csv"))

# CA_06_EDUC_POSTSEC_VECTORS = c(
#   'EDUC_APPRENT' = 'v_CA06_1252', # Total	Apprenticeship or trades certificate or diploma
#   'EDUC_COLLEGE' = 'v_CA06_1253', # Total College, CEGEP or other non-university certificate or diploma
#   'EDUC_UNI'     = 'v_CA06_1254'  # Total	University certificate, diploma or degree
# )

CA06_DATA <- get_census(
  dataset = "CA06",
  regions = list(PR = "59"),
  vectors = CA06_VECTORS,
  level = 'DA',
  quiet = TRUE,
  geo_format = NA,
  use_cache = use_cache,
  labels = 'short'
)


# ----------- fix the EDUC_POSTSEC category -----------

# merge with CA06_DATA
CA06_DATA <- add_column(
  CA06_DATA,
  EDUC_POSTSEC = rowSums(CA06_DATA[, c(
    'EDUC_TRADE',
    'EDUC_COLLEGE',
    'EDUC_UNI'
  )])
) # according to the statsCAN's definition, this is the postsecondary education.

CA06_DATA = CA06_DATA %>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(REGION_NAME = as.character(REGION_NAME))


CA06_DATA %>%
  mutate(CENSUS_YEAR = 2006) %>%
  select(CENSUS_YEAR, everything()) %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_06_BC_DA_DIP.csv"))

CA06_DATA_labels <- create_census_data_label(
  CA06_DATA,
  CA06_VECTORS,
  CENSUS_COMMON_LABELS
)

CA06_DATA_labels = c(
  CA06_DATA_labels,
  "EDUC_POSTSEC" = "Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree"
)

str(CA06_DATA)
# tibble [7,469 × 73]
str(CA06_DATA_labels)
# 73 after we add occupation and education labels

# add label to dictionary, which is similar to description.
skimr::skim(CA06_DATA) # this is for exploring purpose
# Create labels as a named vector.

CA06_DATA_dict = create_dictionary(
  CA06_DATA,
  # id_var = GEO_UID,
  var_labels = CA06_DATA_labels
)

CA06_DATA_dict %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_06_BC_DA_DICT_DIP.csv"))


# ********************************************************************************
# PULL 2001 DATA
# ********************************************************************************

CA01_VECTORS = c(
  # 'POP'             = 'v_CA01_2',    # Population, 2001
  'DWELL_TOT' = 'v_CA01_112', # Dwellings - Total
  'DWELL_HOUS' = 'v_CA01_113', # Dwellings - Single detached house
  'DWELL_APT_LRG' = 'v_CA01_117', # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML' = 'v_CA01_118', # Dwellings - Apartment < 5 stories
  'DWELL_MOVE' = 'v_CA01_120', # Dwellings - Movable

  'FAMILIES_TOT' = 'v_CA01_53', #
  'LONE_PARENT_TOT' = 'v_CA01_67', # Lone Parent - Total
  'LONE_PARENT_F' = 'v_CA01_68', # Lone Parent - Female
  'LONE_PARENT_M' = 'v_CA01_72', # Lone Parent - Male
  'INC_BT_IND_MED' = 'v_CA01_1449', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG' = 'v_CA01_1448', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED' = 'v_CA01_1508', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG' = 'v_CA01_1507', # Total Family Income Before Tax - Average
  # no income after tax family
  'INC_BT_HHS_MED' = 'v_CA01_1634', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG' = 'v_CA01_1633', # Total Household Income Before Tax - Average
  'LICO_AT_INCIDENCE' = 'v_CA01_1620', # Incidence of low income in 2000 %
  'LICO_AT_TOTAL' = 'v_CA01_1617', # Total - Population in private households
  # 'LICO_AT_PREVALENCE' = 'v_CA01_1620', # Prevalence of low income in 2000 %

  # Canada Census 2001
  # 20% Sample Data
  # Occupation and industry
  # Total labour force 15 years and over by occupation - 2001 National Occupational Classification for Statistics
  'OCC_TOT' = 'v_CA01_989', # Occupation - All occupations # this is right
  'OCC_MGMT' = 'v_CA01_990', # A Occupation - Management
  'OCC_BUS_FIN_AD' = 'v_CA01_995', # B Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA01_1002', # C Occupation - Natural and Applied Sciences
  'OCC_HLTH' = 'v_CA01_1005', # D Occupation - Health
  'OCC_SOCI_SERV' = 'v_CA01_1010', # E Occupations in social science, education, government service and religion
  'OCC_ART_CUL_REC' = 'v_CA01_1014', # F Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV' = 'v_CA01_1017', # G Occupation - Sales and Service
  'OCC_TRADES' = 'v_CA01_1028', # H Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC' = 'v_CA01_1038', # I  Occupations unique to primary industry -> similar to natural resources and agriculture: I0 Occupations unique to agriculture, excluding labourers; I1 Occupations unique to forestry operations, mining, oil and gas extraction and fishing, excluding labourers
  'OCC_MAN_UTIL' = 'v_CA01_1042', # J Occupations unique to processing, manufacturing and utilities

  # Canada Census 2001
  # 20% Sample Data
  # Education
  # Totally different from 2006
  # Total population 20 years and over by highest level of schooling
  #   Less than grade 9
  #   Trades certificate or diploma
  #   College
  #     Without certificate or diploma
  #     With certificate or diploma
  #   University
  #     Without degree
  #     Without certificate or diploma
  #     With certificate or diploma
  #     With bachelor's degree or higher
  #   Grades 9 to 13
  #     Without high school graduation certificate
  #     With high school graduation certificate
  'EDUC_TOT' = 'v_CA01_1384', # Total population 20 years and over by highest level of schooling

  'EDUC_LESS_THAN_GRADE9' = 'v_CA01_1385', # No certificate, diploma or degree
  # 'EDUC_GRADE9_13' = 'v_CA01_1386', # High School - Grades 9 to 13 include 1387 + 1388
  'EDUC_WO_HS' = 'v_CA01_1387', # High School - Without high school graduation certificate
  # 1 'EDUC_NONE' = 'EDUC_LESS_THAN_GRADE9' + 'EDUC_WO_HS' #b 1
  'EDUC_HIGHSCH' = 'v_CA01_1388', # 2 High School -With high school graduation certificate

  # following is similar to 3 postsecondary and equivalency
  # 'EDUC_POSTSEC' = 'EDUC_TRADES' +  EDUC_COLLEGE+ EDUC_UNI, # 3 Education for population aged 20 - 64 - Postsecondary certificate, diploma or degree
  'EDUC_TRADES' = 'v_CA01_1389', # Trades certificate or diploma
  'EDUC_COLLEGE' = 'v_CA01_1390', # College
  # 'EDUC_WO_COL_CERT' = 'v_CA01_1391', # College - Without certificate or diploma
  # 'EDUC_COLLEGE_W_CERT' = 'v_CA01_1392', # College - With certificate or diploma

  'EDUC_UNI' = 'v_CA01_1393', # University
  # following are subcategories of 1393

  # 'EDUC_UNI_WO_UNI_DEGREE' = 'v_CA01_1394', # University - Without degree
  # 'EDUC_UNI_WO_UNI_CERT' = 'v_CA01_1395', # University - Without certificate or diploma
  # 'EDUC_UNI_W_UNI_CERT' = 'v_CA01_1396', # University - With certificate or diploma
  'EDUC_BACHELOR_HIGHER' = 'v_CA01_1397', # University - With bachelor's degree or higher
  # Labour force
  # Labour force status
  # Labour force activity by presence of children
  # Total population 15 years and over in private households by presence of children and labour force activity
  #   In the labour force
  'IN_LAB_FORCE_TOT' = 'v_CA01_735', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES' = 'v_CA01_736', # Population aged 15 years and over by Labour force status - in labour force
  'LABOUR_EMPL_CNT' = 'v_CA01_737', # Labour force - in labour force - Employed
  'LABOUR_UNEM_CNT' = 'v_CA01_738', # Labour force - in labour force - Unemployed
  'IN_LAB_FORCE_NO' = 'v_CA01_739', # Population aged 15 years and over by Labour force status - not in labour force
  # Labour force activity
  'LABOUR_PART_RT' = 'v_CA01_740', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT' = 'v_CA01_741', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT' = 'v_CA01_742', # Labour Force - Unemployment Rate

  'HOME_OWN_TOT' = 'v_CA01_96', # Home ownership - Total  the same as repair_tot
  'HOME_OWN_OWN' = 'v_CA01_99', # Home ownership - Owner
  'HOME_OWN_RENT' = 'v_CA01_100', # Home ownership - Renter
  'HOME_OWN_BAND' = 'v_CA01_101', # Home ownership - Band Housing

  'DWELLING_VALUE_AVG' = 'v_CA01_1674', # Average value of dwelling
  'DWELLING_COST_OWN' = 'v_CA01_1671', # Average owner major payments
  'DWELLING_RENT_AVG' = 'v_CA01_1667', # Average gross rent

  # 'REPAIRS_TOT' = 'v_CA01_96', # Occupied private dwellings by dwelling condition	- Total the same as 'HOME_OWN_TOT'
  'REPAIRS_ONLY_REGULAR' = 'v_CA01_102', # Occupied private dwellings by dwelling condition	- Only regular maintenance
  'REPAIRS_ONLY_MINOR' = 'v_CA01_103', # Occupied private dwellings by dwelling condition	- Only minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA01_104' # Occupied private dwellings by dwelling condition	- Major repairs needed
)

CA01_META_DATA <- create_census_meta_data(
  CENSUS_NUMBER = "CA01",
  VECTORS = CA01_VECTORS
)

CA01_META_DATA %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_01_BC_META_DATA_DIP.csv"))

CA01_DATA <- get_census(
  dataset = "CA01",
  regions = list(PR = "59"),
  vectors = CA01_VECTORS,
  level = 'DA',
  quiet = TRUE,
  geo_format = NA,
  use_cache = use_cache,
  labels = 'short'
)

# fix some issues in terms of definition of indicator changes
CA01_DATA <- CA01_DATA %>%
  mutate(
    EDUC_NONE = EDUC_WO_HS +
      EDUC_LESS_THAN_GRADE9
  )

# DONE: check the EDUC_POSTSEC category
CA01_DATA <- CA01_DATA %>%
  mutate(
    EDUC_POSTSEC = EDUC_TRADES +
      EDUC_COLLEGE +
      EDUC_UNI
  )

CA01_DATA <- CA01_DATA %>%
  mutate(
    REPAIRS_TOT = HOME_OWN_TOT
  )

CA01_DATA <- CA01_DATA %>%
  mutate(
    LICO_AT_PREVALENCE = LICO_AT_INCIDENCE / LICO_AT_TOTAL
  )


CA01_DATA <- CA01_DATA %>%
  janitor::clean_names(case = 'screaming_snake') %>%
  mutate(REGION_NAME = as.character(REGION_NAME))


CA01_DATA %>%
  mutate(CENSUS_YEAR = 2001) %>%
  select(CENSUS_YEAR, everything()) %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_01_BC_DA_DIP.csv"))

CA01_DATA_labels <- create_census_data_label(
  CA01_DATA,
  CA01_VECTORS,
  CENSUS_COMMON_LABELS
)

str(CA01_DATA)
# tibble [7,436 × 67]
str(CA01_DATA_labels)
# 64 ?

CA01_DATA_labels <- c(
  CA01_DATA_labels,
  "EDUC_NONE" = "Education for population aged 20 years and over - No certificate, diploma or degree",
  "EDUC_POSTSEC" = "Education for population aged 20 years and over - Postsecondary certificate, diploma or degree",
  'REPAIRS_TOT' = 'Occupied private dwellings by dwelling condition	- Total' # Occupied private dwellings by dwelling condition	- Total the same as 'HOME_OWN_TOT'
)

CA01_DATA = CA01_DATA %>%
  mutate(REGION_NAME = as.character(REGION_NAME))

# A simple dictionary printed to console
create_dictionary(CA01_DATA)

# add label to dictionary, which is similar to description.
skimr::skim(CA01_DATA) # this is for exploring purpose
# lower complete rate than more recent census
# Create labels as a named vector.

CA01_DATA_dict = create_dictionary(
  CA01_DATA,
  # id_var = GEO_UID,
  var_labels = CA01_DATA_labels
)

CA01_DATA_dict %>%
  readr::write_csv(here::here("out", "StatsCAN_Census_01_BC_DA_DICT_DIP.csv"))
