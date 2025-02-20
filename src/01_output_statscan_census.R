# Copyright 2024 Province of British Columbia
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
pacman::p_load(cancensus,geojsonsf, sf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb, datadictionary)
if (!fs::dir_exists("out")) fs::dir_create("out")
######################################################################################
# Census data
######################################################################################
# see census/background.txt to get informaiton for installation and background.

# if you get the cache for cancensus package working, set use_cache flag as TRUE, then it will pull data from cache.
# Otherwise, keep the use_cache flag as FALSE, then it will download the data from statscan. It may take a few minutes. 
use_cache = TRUE


# Put all the variable strings together: 
CA21_VECTORS = c(
  # 'POP'             = 'v_CA21_1',  # Population, 2021
  
  'POP_PCT_CHANGE'  = 'v_CA21_3',  # Population percentage change, 2016 to 2021
  
  'POP_DENS' = 'v_CA21_6',  # Population density
  
  
  'DWELL_TOT'       = 'v_CA21_434',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA21_435',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA21_440',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA21_439',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA21_442',  # Dwellings - Movable Done
  
  
  ###########################################################################################################################################
  # Family
  
  'FAMILIES_TOT' = 'v_CA21_499', # Total number of census families in private households
  'LONE_PARENT_TOT' = 'v_CA21_507',  # Lone Parent - Total
  'LONE_PARENT_F'   = 'v_CA21_508',  # Lone Parent - Female
  'LONE_PARENT_M'   = 'v_CA21_509',  # Lone Parent - Male Done
  
  
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
  
  
  'INC_BT_HH_MED'  = 'v_CA21_906', # Median total income of household in 2020 ($)
  'INC_AT_HH_MED'  = 'v_CA21_907', # Median after-tax income of household in 2020 ($)
  

  'INC_BT_IND_MED'  = 'v_CA21_560', # Total Individual Income Before Tax - Median -  Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2020 among recipients ($)
  # 'INC_BT_IND_MED_19'  = 'v_CA21_818', # Total Individual Income Before Tax - Median -Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2019 among recipients ($)
  'INC_BT_IND_AVG'  = 'v_CA21_605', # Average total income in 2020 among recipients ($) - Total Individual Income Before Tax - Average ? Number of total income recipients aged 15 years and over in private households in 2020 Average total income in 2020 among recipients ($) - 35%
  
  
  # 25% Data
  # Income
  # Economic family income
  # Income statistics for economic families in private households
  # Average total income of economic family in 2020 ($)
  # Average after-tax income of economic family in 2020 ($)
  
  'INC_BT_FAM_MED'  = 'v_CA21_965', # Total Family Income Before Tax - Median
  'INC_AT_FAM_MED'  = 'v_CA21_966', # Total Family Income After Tax - Median
  
  # Inequality measures for the population in private households
  # 'GINI_INDEX_         # Gini index on adjusted household total income
  # 'GINI_INDEX_         # Gini index on adjusted household market income
  'GINI_INDEX_AT_INC' = 'v_CA21_1142',         # Gini index on adjusted household after-tax income
  
  # v_CA21_1143
  # Total
  # P90/P10 ratio on adjusted household after-tax income
  # Income; Inequality measures for the population in private households; P90/P10 ratio on adjusted household after-tax income
  
  'P90P10_RATIO'      = 'v_CA21_1143',         # P90/P10 ratio on adjusted household after-tax income Done
  
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
  
  # 'DEP_RAT1' = 'v_CA21_35',
  # # DEP_RAT1
  # 'DEP_RAT2' = 'v_CA21_37',
  # # DEP_RAT2
  # 
  # 'DET_HOMES1' = 'v_CA21_41',
  # # DET_HOMES1
  # 'DET_HOMES2' = 'v_CA21_42',
  # # DET_HOMES2
  
  ###########################################################################################################################################
  # Occupation
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
  
  
  'OCC_TOT'         = 'v_CA21_6561', # Occupation - Total
  'OCC_MGMT'        = 'v_CA21_6570', # Occupation - Management
  
  
  ###########################################################################################################################################
  # labor market
  # 25% Data
  # Work
  # Total - Population aged 15 years and over by labour force status
  # In the labour force
  # Employed
  # Unemployed
  # Not in the labour force
  
  'IN_LAB_FORCE_TOT'  = 'v_CA21_6492', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES'  = 'v_CA21_6495', # Population aged 15 years and over by Labour force status - in labour force
  'IN_LAB_FORCE_NO'  = 'v_CA21_6504', # Population aged 15 years and over by Labour force status - not in labour force Done
  # 'LABOUR_PART_RT'  = 'v_CA21_5612', # Labour Force - Participation Rate  in yes tot/tot
  # 'LABOUR_EMPL_RT'  = 'v_CA21_5615', # Labour Force - Employment Rate  need to calculate emp cnt/in labor
  'LABOUR_EMPL_CNT'  = 'v_CA21_6498',
  'LABOUR_UNEM_CNT'  = 'v_CA21_6501',  # Need to calculate the rate cnt/total Done
  
  ###########################################################################################################################################
  # Housing
  # 25% Data
  # Housing
  # Total - Private households by tenure
  # Owner
  # Renter
  # Dwelling provided by the local government, First Nation or Indian band
  'HOME_OWN_TOT'    = 'v_CA21_4237', # Home ownership - Total
  'HOME_OWN_OWN'    = 'v_CA21_4238', # Home ownership - Owner
  'HOME_OWN_RENT'   = 'v_CA21_4239', # Home ownership - Renter
  'HOME_OWN_BAND'   = 'v_CA21_4240', # Home ownership - Band Housing Done
  
  
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
  
  'DWELLING_VALUE_AVG'  = 'v_CA21_4312', # Average value of dwelling
  'DWELLING_VALUE_MED'  = 'v_CA21_4311', # Median value of dwelling
  'DWELLING_COST_OWN'   = 'v_CA21_4310', # Average monthly shelter costs for owned dwellings Done
  
  # 25% Data
  # Housing
  # Total - Tenant households in non-farm, non-reserve private dwellings
  # Median monthly shelter costs for rented dwellings ($)
  # Average monthly shelter costs for rented dwellings ($)
  
  'DWELLING_RENT_AVG'   = 'v_CA21_4318', # Average monthly shelter costs for rented dwellings
  'DWELLING_RENT_MED'   = 'v_CA21_4317', # Median monthly shelter costs for rented dwellings Done
  
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
  'HOUSING_SUITABLE_NO'  = 'v_CA21_4262' # Private households by housing suitability - Not suitable Done
  
)


# this function extracts information of vector/variable from StatsCan website. 
create_census_meta_data <- function( CENSUS_NUMBER = "CA21", VECTORS = CA21_VECTORS ){
  # convert the vector to a dataframe, so we can  join to other table
  VECTORS_DF <- data.frame(
    name = names(VECTORS),
    vector = as.character(VECTORS)
  )
  
  vector_list  <-  list_census_vectors(CENSUS_NUMBER)
  
  VECTORS_DF_DICT <-  VECTORS_DF %>%
    left_join(vector_list, by = join_by(vector))
  
  return(VECTORS_DF_DICT)
}


CA21_META_DATA <- create_census_meta_data( CENSUS_NUMBER = "CA21", VECTORS = CA21_VECTORS )
  
CA21_META_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_21_BC_META_DATA_DIP.csv")   )

# since we have cache on the LAN, we need vpn2 and connect to LAN 
CA21_DATA <- get_census(dataset='CA21', 
                    regions=list(PR="59"),
                    vectors=CA21_VECTORS, 
                    level='DA', 
                    quiet = TRUE, 
                    geo_format = NA,
                    use_cache = use_cache,
                    labels = 'short')



CA21_DATA = CA21_DATA %>% 
  janitor::clean_names(case = "screaming_snake" ) 


CA21_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_21_BC_DA_DIP.csv")   )

str(CA21_DATA)
attr(CA21_DATA, "last_updated")


CENSUS_COMMON_LABELS <- c(
  'GEO_UID'  = "Geographic unique ID",
  'TYPE' = 'Geographic type: DA or CSD',
  'REGION_NAME' = 'Geographic name; in 2021 it is DA id, but in other waves of survey; it was region name.',
  'AREA_SQ_KM' = 'Aear in squared kilometer',
  
  'POPULATION' =  "Population",
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
  
  
  DATA_VECTOR_DETAIL <-  DATA_VECTOR_DETAIL %>% janitor::clean_names(case = "screaming_snake") %>%
    left_join(tibble(VARIABLE_NAME = names(VECTORS), VECTOR = VECTORS))
  

  
  DATA_VECTOR_DETAIL <- tibble(VARIABLE_NAME = names(CENSUS_COMMON_LABELS),
                                    DETAIL = CENSUS_COMMON_LABELS) %>%
    bind_rows(DATA_VECTOR_DETAIL)
  
  # Opposite to enframe() , the function deframe() converts a two-column tibble to a named vector or list, using the first column as name and the second column as value.
  
  DATA_labels  <- deframe(DATA_VECTOR_DETAIL %>% select(VARIABLE_NAME, DETAIL))
  
  return(DATA_labels)
}


CA21_DATA_labels <- create_census_data_label(CA21_DATA, CA21_VECTORS)
  

# add label to dictionary, which is similar to decription.
skimr::skim(CA21_DATA)  # this is for exploring purpose
# Create labels as a named vector.

CA21_DATA_dict = create_dictionary(CA21_DATA,
                               # id_var = GEO_UID,
                               var_labels = CA21_DATA_labels)

CA21_DATA_dict %>% readr::write_csv(here::here("out", "StatsCAN_Census_21_BC_DA_DICT_DIP.csv")   )


# ********************************************************************************
# PULL 2016 DATA
# ********************************************************************************

CA16_VECTORS_LIST = list_census_vectors('CA16')

CA16_VECTORS_LIST %>% 
  filter(str_detect(string = details, pattern = "lone"))

CA16_FAMILY_VECTORS_LIST <- find_census_vectors("family", dataset = "CA16", type = "total", query_type = "exact")

CA16_VECTORS = c(
  'DWELL_TOT'       = 'v_CA16_408',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA16_409',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA16_410',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA16_415',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA16_417',  # Dwellings - Movable
  'FAMILIES_TOT' = 'v_CA16_484',  # Number of families
  'LONE_PARENT_TOT' = 'v_CA16_488',  # Lone Parent - Total
  'LONE_PARENT_F'   = 'v_CA16_489',  # Lone Parent - Female
  'LONE_PARENT_M'   = 'v_CA16_490',  # Lone Parent - Male
  'INC_BT_IND_MED'  = 'v_CA16_2207', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG'  = 'v_CA16_4957', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED'  = 'v_CA16_2447', # Total Family Income Before Tax - Median Median total income of economic families in 2015 ($)
  'INC_BT_FAM_AVG'  = 'v_CA16_4994', # Total Family Income Before Tax - Average / Average total income of economic families in 2015 ($)
  'INC_AT_FAM_MED'  = 'v_CA16_2448', # Total Family Income After Tax - Median Median after-tax income of economic families in 2015 ($)
  'INC_AT_FAM_AVG'  = 'v_CA16_4995', # Total Family Income After Tax - Average / Average after-tax income of economic families in 2015 ($)
  'INC_BT_HHS_MED'  = 'v_CA16_2397', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG'  = 'v_CA16_4985', # Total Household Income Before Tax - Average
  'LICO_AT_PREVALENCE' = 'v_CA16_2570', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
  'LIM_AT_PREVALENCE' = 'v_CA16_2540', # Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)
  'OCC_TOT'         = 'v_CA16_5654', # Occupation - Total
  'OCC_MGMT'        = 'v_CA16_5663', # Occupation - Management
  'LABOUR_PART_RT'  = 'v_CA16_5612', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT'  = 'v_CA16_5615', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT'  = 'v_CA16_5618', # Labour Force - Unemployment Rate
  'IN_LAB_FORCE_TOT'  = 'v_CA16_5597', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES'  = 'v_CA16_5600', # Population aged 15 years and over by Labour force status - in labour force
  'IN_LAB_FORCE_NO'  = 'v_CA16_5609', # Population aged 15 years and over by Labour force status - not in labour force
  'HOME_OWN_TOT'    = 'v_CA16_4836', # Home ownership - Total
  'HOME_OWN_OWN'    = 'v_CA16_4837', # Home ownership - Owner
  'HOME_OWN_RENT'   = 'v_CA16_4838', # Home ownership - Renter
  'HOME_OWN_BAND'   = 'v_CA16_4839', # Home ownership - Band Housing
  'DWELLING_VALUE_AVG'  = 'v_CA16_4896', # Average value of dwelling
  'DWELLING_VALUE_MED'  = 'v_CA16_4895', # Median value of dwelling
  'DWELLING_COST_OWN'   = 'v_CA16_4894', # Average monthly shelter costs for owned dwellings
  'DWELLING_RENT_AVG'  = 'v_CA16_4901', # Average monthly shelter costs for rented dwellings
  'DWELLING_RENT_MED'  = 'v_CA16_4900', # Median monthly shelter costs for rented dwellings
  'SUBSIDIZED_HOUS' = 'v_CA16_4898', # % of tenant households in subsidized housing
  'HOUSING_SUITABLE_TOT' = 'v_CA16_4859', # Private households by housing suitability - Total
  'HOUSING_SUITABLE_YES' = 'v_CA16_4860', # Private households by housing suitability - Suitable
  'HOUSING_SUITABLE_NO' = 'v_CA16_4861', # Private households by housing suitability - Not suitable
  'REPAIRS_TOT' = 'v_CA16_4870', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA16_4871', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA16_4872' # Occupied private dwellings by dwelling condition	- Major repairs needed
  
)

CA16_META_DATA <- create_census_meta_data( CENSUS_NUMBER = "CA16", VECTORS = CA16_VECTORS )

CA16_META_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_16_BC_META_DATA_DIP.csv")   )


CA16_DATA <- get_census(dataset="CA16",
                        regions=list(PR="59"),
                        vectors=
                          CA16_VECTORS,
                        level='DA')


CA16_DATA = CA16_DATA %>% 
  janitor::clean_names(case = "screaming_snake" ) 

CA16_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_16_BC_DA_DIP.csv")   )

str(CA16_DATA)
# tibble [7,848 × 54]

CA16_DATA_labels <- create_census_data_label(CA16_DATA, CA16_VECTORS)

str(CA16_DATA_labels)
# 54 

# add label to dictionary, which is similar to description.
skimr::skim(CA16_DATA)  # this is for exploring purpose
# Create labels as a named vector.

CA16_DATA_dict = create_dictionary(CA16_DATA,
                                   # id_var = GEO_UID,
                                   var_labels = CA16_DATA_labels)

CA16_DATA_dict %>% readr::write_csv(here::here("out", "StatsCAN_Census_16_BC_DA_DICT_DIP.csv")   )


# ********************************************************************************
# PULL 2011 DATA
# ********************************************************************************

CA11_VECTORS = c(

  
  'DWELL_TOT'       = 'v_CA11F_199',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA11F_200',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA11F_201',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA11F_207',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA11F_202',  # Dwellings - Movable
  
  'FAMILIES_TOT' = 'v_CA11F_115',  # Total number of census families in private households
  'LONE_PARENT_TOT' = 'v_CA11F_129', # Lone Parent - Total
  'LONE_PARENT_F'   = 'v_CA11F_130', # Lone Parent - Female
  'LONE_PARENT_M'   = 'v_CA11F_134', # Lone Parent - Male
  'INC_BT_IND_MED'  = 'v_CA11N_2341', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG'  = 'v_CA11N_2344', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED'  = 'v_CA11N_2456', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG'  = 'v_CA11N_2457', # Total Family Income Before Tax - Average
  'INC_AT_FAM_MED'  = 'v_CA11N_2458', # Total Family Income After Tax - Median
  'INC_AT_FAM_AVG'  = 'v_CA11N_2459', # Total Family Income After Tax - Average
  'INC_BT_HHS_MED'  = 'v_CA11N_2562', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG'  = 'v_CA11N_2563', # Total Household Income Before Tax - Average
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
  'OCC_TOT'         = 'v_CA11N_2026', # Occupation - Total
  'OCC_MGMT'        = 'v_CA11N_2035', # Occupation - Management
  'LABOUR_PART_RT'  = 'v_CA11N_2002', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT'  = 'v_CA11N_2005', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT'  = 'v_CA11N_2008', # Labour Force - Unemployment Rate
  'IN_LAB_FORCE_TOT'  = 'v_CA11N_1987', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES'  = 'v_CA11N_1990', # Population aged 15 years and over by Labour force status - in labour force
  'IN_LAB_FORCE_NO'  = 'v_CA11N_1999', # Population aged 15 years and over by Labour force status - not in labour force
  'HOME_OWN_TOT'    = 'v_CA11N_2252', # Home ownership - Total
  'HOME_OWN_OWN'    = 'v_CA11N_2253', # Home ownership - Owner
  'HOME_OWN_RENT'   = 'v_CA11N_2254', # Home ownership - Renter
  'HOME_OWN_BAND'   = 'v_CA11N_2255', # Home ownership - Band Housing
  'DWELLING_VALUE_AVG'  = 'v_CA11N_2287', # Average value of dwelling
  'DWELLING_VALUE_MED'  = 'v_CA11N_2286', # Median value of dwelling
  'DWELLING_COST_OWN'   = 'v_CA11N_2285', # Average monthly shelter costs for owned dwellings
  'DWELLING_RENT_AVG'  = 'v_CA11N_2292', # Average monthly shelter costs for rented dwellings
  'DWELLING_RENT_MED'  = 'v_CA11N_2291', # Median monthly shelter costs for rented dwellings
  'SUBSIDIZED_HOUS' = 'v_CA11N_2289', # % of tenant households in subsidized housing
  'HOUSING_SUITABLE_TOT' = 'v_CA11N_2274', # Private households by housing suitability - Total
  'HOUSING_SUITABLE_YES' = 'v_CA11N_2275', # Private households by housing suitability - Suitable
  'HOUSING_SUITABLE_NO' = 'v_CA11N_2276', # Private households by housing suitability - Not suitable
  'REPAIRS_TOT' = 'v_CA11N_2230', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA11N_2231', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA11N_2232' # Occupied private dwellings by dwelling condition	- Major repairs needed
)

CA11_META_DATA <- create_census_meta_data( CENSUS_NUMBER = "CA11", VECTORS = CA11_VECTORS )

CA11_META_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_11_BC_META_DATA_DIP.csv")   )


CA11_DATA <- get_census(dataset="CA11",
                        regions=list(PR="59"),
                        vectors=
                          CA11_VECTORS,
                        level='DA')


CA11_DATA = CA11_DATA %>% 
  janitor::clean_names(case = "screaming_snake" ) 

CA11_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_11_BC_DA_DIP.csv")   )

CENSUS_COMMON_LABELS_11 <- c(
  'GEO_UID'  = "Geographic unique ID",
  'TYPE' = 'Geographic type: DA or CSD',
  'REGION_NAME' = 'Geographic name; in 2021 it is DA id, but in other waves of survey; it was region name.',
  'AREA_SQ_KM' = 'Aear in squared kilometer',
  
  'POPULATION' =  "Population",
  'DWELLINGS' = "Number of dwellings",
  'HOUSEHOLDS' = 'Number of households',
  
  'CSD_UID' = 'CSD_UID',
  'CD_UID' = 'CD_UID',
  'CT_UID' = 'CT_UID',
  'CMA_UID' = 'CMA_UID',
  'NHS_NON_RETURN_RATE' = 'NHS_NON_RETURN_RATE'
)

CA11_DATA_labels <- create_census_data_label(CA11_DATA, CA11_VECTORS,CENSUS_COMMON_LABELS_11)

str(CA11_DATA)
# tibble [7,848 × 58]
str(CA11_DATA_labels)
# 58

# add label to dictionary, which is similar to decription.
skimr::skim(CA11_DATA)  # this is for exploring purpose
# Create labels as a named vector.


CA11_DATA_dict = create_dictionary(CA11_DATA,
                                   # id_var = GEO_UID,
                                   var_labels = CA11_DATA_labels)

CA11_DATA_dict %>% readr::write_csv(here::here("out", "StatsCAN_Census_11_BC_DA_DICT_DIP.csv")   )



# ********************************************************************************
# PULL 2006 DATA
# ********************************************************************************

CA06_VECTORS = c(
  'DWELL_TOT'       = 'v_CA06_119',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA06_120',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA06_124',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA06_125',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA06_127',  # Dwellings - Movable
  
  'FAMILIES_TOT' = 'v_CA06_55',   # Lone Parent - Total
  'LONE_PARENT_TOT' = 'v_CA06_69',   # Lone Parent - Total
  'LONE_PARENT_F'   = 'v_CA06_70',   # Lone Parent - Female
  'LONE_PARENT_M'   = 'v_CA06_74',   # Lone Parent - Male
  'INC_BT_IND_MED'  = 'v_CA06_1583', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG'  = 'v_CA06_1584', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED'  = 'v_CA06_1741', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG'  = 'v_CA06_1742', # Total Family Income Before Tax - Average
  'INC_AT_FAM_MED'  = 'v_CA06_1785', # Total Family Income After Tax - Median
  'INC_AT_FAM_AVG'  = 'v_CA06_1786', # Total Family Income After Tax - Average
  'INC_BT_HHS_MED'  = 'v_CA06_2000', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG'  = 'v_CA06_2001', # Total Household Income Before Tax - Average
  'LICO_AT_PREVALENCE' = 'v_CA06_1981', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
  'OCC_TOT'         = 'v_CA06_827', # Occupation - Total
  'OCC_MGMT'        = 'v_CA06_830', # Occupation - Management
  'LABOUR_PART_RT'  = 'v_CA06_580', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT'  = 'v_CA06_581', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT'  = 'v_CA06_582', # Labour Force - Unemployment Rate
  'IN_LAB_FORCE_TOT'  = 'v_CA06_575', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES'  = 'v_CA06_576', # Population aged 15 years and over by Labour force status - in labour force
  'IN_LAB_FORCE_NO'  = 'v_CA06_579', # Population aged 15 years and over by Labour force status - not in labour force
  'HOME_OWN_TOT'    = 'v_CA06_101', # Home ownership - Total
  'HOME_OWN_OWN'    = 'v_CA06_102', # Home ownership - Owner
  'HOME_OWN_RENT'   = 'v_CA06_103', # Home ownership - Renter
  'HOME_OWN_BAND'   = 'v_CA06_104', # Home ownership - Band Housing
  'DWELLING_VALUE_AVG'  = 'v_CA06_2054', # Average value of dwelling
  'DWELLING_COST_OWN'   = 'v_CA06_2055', # Average owner major payments
  'DWELLING_RENT_AVG'  = 'v_CA06_2050', # Average gross rent
  'REPAIRS_TOT' = 'v_CA06_105', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_ONLY_REGULAR' = 'v_CA06_106', # Occupied private dwellings by dwelling condition	- Only regular maintenance
  'REPAIRS_ONLY_MINOR' = 'v_CA06_107', # Occupied private dwellings by dwelling condition	- Only minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA06_108' # Occupied private dwellings by dwelling condition	- Major repairs needed
)

CA06_META_DATA <- create_census_meta_data( CENSUS_NUMBER = "CA06", VECTORS = CA06_VECTORS )

CA06_META_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_06_BC_META_DATA_DIP.csv")   )


CA06_DATA <- get_census(dataset="CA06",
                        regions=list(PR="59"),
                        vectors=
                          CA06_VECTORS,
                        level='DA')



CA06_DATA = CA06_DATA %>% 
  janitor::clean_names(case = "screaming_snake" ) 

CA06_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_06_BC_DA_DIP.csv")   )

CA06_DATA_labels <- create_census_data_label(CA06_DATA, CA06_VECTORS,CENSUS_COMMON_LABELS)

str(CA06_DATA)
# tibble [7,848 × 48]
str(CA06_DATA_labels)
# 48

# add label to dictionary, which is similar to description.
skimr::skim(CA06_DATA)  # this is for exploring purpose
# Create labels as a named vector.



CA06_DATA_dict = create_dictionary(CA06_DATA,
                                   # id_var = GEO_UID,
                                   var_labels = CA06_DATA_labels)

CA06_DATA_dict %>% readr::write_csv(here::here("out", "StatsCAN_Census_06_BC_DA_DICT_DIP.csv")   )


# ********************************************************************************
# PULL 2001 DATA
# ********************************************************************************

CA01_VECTORS = c(
  # 'POP'             = 'v_CA01_2',    # Population, 2001
  'DWELL_TOT'       = 'v_CA01_112',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA01_113',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA01_117',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA01_118',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA01_120',  # Dwellings - Movable
  
  'FAMILIES_TOT' = 'v_CA01_53',   # 
  'LONE_PARENT_TOT' = 'v_CA01_67',   # Lone Parent - Total
  'LONE_PARENT_F'   = 'v_CA01_68',   # Lone Parent - Female
  'LONE_PARENT_M'   = 'v_CA01_72',   # Lone Parent - Male
  'INC_BT_IND_MED'  = 'v_CA01_1449', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG'  = 'v_CA01_1448', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED'  = 'v_CA01_1508', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG'  = 'v_CA01_1507', # Total Family Income Before Tax - Average
  # no income after tax family
  'INC_BT_HHS_MED'  = 'v_CA01_1634', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG'  = 'v_CA01_1633', # Total Household Income Before Tax - Average
  'LICO_AT_PREVALENCE' = 'v_CA01_1620', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
  'OCC_TOT'         = 'v_CA01_989', # Occupation - Total
  'OCC_MGMT'        = 'v_CA01_990', # Occupation - Management
  'LABOUR_PART_RT'  = 'v_CA01_740', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT'  = 'v_CA01_741', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT'  = 'v_CA01_742', # Labour Force - Unemployment Rate
  'IN_LAB_FORCE_TOT'  = 'v_CA01_735', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES'  = 'v_CA01_736', # Population aged 15 years and over by Labour force status - in labour force
  'IN_LAB_FORCE_NO'  = 'v_CA01_739', # Population aged 15 years and over by Labour force status - not in labour force
  
  'HOME_OWN_TOT'    = 'v_CA01_96', # Home ownership - Total  the same as repair_tot
  'HOME_OWN_OWN'    = 'v_CA01_99', # Home ownership - Owner
  'HOME_OWN_RENT'   = 'v_CA01_100', # Home ownership - Renter
  'HOME_OWN_BAND'   = 'v_CA01_101', # Home ownership - Band Housing
  
  'DWELLING_VALUE_AVG'  = 'v_CA01_1674', # Average value of dwelling
  'DWELLING_COST_OWN'   = 'v_CA01_1671', # Average owner major payments
  'DWELLING_RENT_AVG'  = 'v_CA01_1667', # Average gross rent
  
  # 'REPAIRS_TOT' = 'v_CA01_96', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_ONLY_REGULAR' = 'v_CA01_102', # Occupied private dwellings by dwelling condition	- Only regular maintenance
  'REPAIRS_ONLY_MINOR' = 'v_CA01_103', # Occupied private dwellings by dwelling condition	- Only minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA01_104' # Occupied private dwellings by dwelling condition	- Major repairs needed
)

CA01_META_DATA <- create_census_meta_data( CENSUS_NUMBER = "CA01", VECTORS = CA01_VECTORS )

CA01_META_DATA %>% readr::write_csv(here::here("out", "StatsCAN_Census_01_BC_META_DATA_DIP.csv")   )

CA01_DATA <- get_census(dataset="CA01",
                        regions=list(PR="59"),
                        vectors=
                          CA01_VECTORS,
                        level='DA')

CA01_DATA <- CA01_DATA %>% 
  janitor::clean_names(case = 'screaming_snake')

CA01_DATA %>% 
  readr::write_csv(here::here("out", "StatsCAN_Census_01_BC_DA_DIP.csv")   )

CA01_DATA_labels <- create_census_data_label(CA01_DATA, CA01_VECTORS,CENSUS_COMMON_LABELS)

str(CA01_DATA)
# tibble [7,848 × 45]
str(CA01_DATA_labels)
# 45 ?

CA01_DATA = CA01_DATA %>%
  mutate(REGION_NAME = as.character(REGION_NAME) )

# A simple dictionary printed to console
create_dictionary(CA01_DATA)

# add label to dictionary, which is similar to description.
skimr::skim(CA01_DATA)  # this is for exploring purpose
# Create labels as a named vector.

CA01_DATA_dict = create_dictionary(CA01_DATA,
                                   # id_var = GEO_UID,
                                   var_labels = CA01_DATA_labels)

CA01_DATA_dict %>% readr::write_csv(here::here("out", "StatsCAN_Census_01_BC_DA_DICT_DIP.csv")   )
