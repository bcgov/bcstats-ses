# library("remotes")
# install_github("bcgov/safepaths")
pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb)

######################################################################################
# 
# Translation Master File: a table with different levels of geography to link the data
# https://www2.gov.bc.ca/assets/gov/health/forms/5512datadictionary.pdf
# 
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

TMF_file = use_network_path("data/GCS_202406.csv")

TMF = read_csv(TMF_file)

TMF %>% glimpse()
# TMF is a dataframe in a postal code level. 
# ACTIVE field shows which postal code region is still available.

# what we want is a DA level dataframe, but DA is not aligned with other regional variable very well, we may need to break the DA into two parts and join to regions. We could use population or postal code as weights. 
# The da 2021 is in short form: for example, 0225 as string
# DA id  in long form
# 59 09 0103
# PR-CD-DA code
# Province 59: British Columbia
# CD 09: Fraser Valley
# DA 0103
# ACTIVE == "Y" is critical, otherwise, DA will not match to its CSD since the DA is in short form. 

TMF = TMF %>% 
  mutate(DA_NUM = as.numeric(str_c("59", CD_2021, DA_2021, sep = "")))

# TMF_names = TMF %>% names() %>% paste(collapse = ",")
  


TMF_DA_LEVEL = TMF %>% 
  filter(ACTIVE == "Y") %>% 
  count(DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021)  


# 6967



# It seems not every DA has one to one matching to CHSA regions such as CHSA. So to join to other tables with other regions, we will need TO separate the DA into different regions using population as weights.  
# 


TMF_DA_CHSA_LEVEL = TMF %>% 
  filter(ACTIVE == "Y") %>% 
  count(DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021, CHSA)

# 7067

# In order to speed up performance, reduce API quota usage, and reduce unnecessary network calls, please set up a persistent cache directory via `set_cancensus_cache_path('<local cache path>', install = TRUE)`.
# This will add your cache directory as environment variable to your .Renviron to be used across sessions and projects.



# set up CENSUSMAPPER API 
# set_cancensus_api_key(config::get("CENSUSMAPPER_API_KEY"), install = TRUE)
set_cancensus_cache_path(use_network_path("data/census_cache"), install = TRUE)
# or
# options(cancensus.api_key = "your_api_key")
# options(cancensus.cache_path = "custom cache path")
# options(cancensus.cache_path = use_network_path("data/census_cache"))
getOption("cancensus.cache_path")
# Your cache path has been stored in your .Renviron and can be accessed by Sys.getenv("CM_CACHE_PATH")
library(sf)

list_census_datasets()

# retrieve sf dataframe
# van <- get_census(dataset='CA21', regions=list(CMA="59933"),
#                       vectors=c("median_hh_income"="v_CA21_906"), level='CSD', quiet = TRUE, 
#                       geo_format = 'sf', labels = 'short')
# 
# # DA	Dissemination Area	56589
# # DB	Dissemination Block (2001-2016)	489676
# 
# van_db <- get_census(dataset='CA21', regions=list(CMA="59933"),
#                   vectors=c("median_hh_income"="v_CA21_906"), level='DB', quiet = TRUE, 
#                   geo_format = NA, labels = 'short')


# van_da <- get_census(dataset='CA21', regions=list(CMA="59933"),
#                      vectors=c("median_hh_income"="v_CA21_906"), level='DA', quiet = TRUE, 
#                      geo_format = NA, labels = 'short')

# view all available Census variables for a given dataset.
# Vector: short variable code
# Type: variables are provided as aggregates of female responses, male responses, or total (male+female) responses
# Label: detailed variable name
# Units: provides information about whether the variable represents a count integer, a ratio, a percentage, or a currency figure
# Parent_vector: shows the immediate hierarchical parent category for that variable, where appropriate
# Aggregation: indicates how the variable should be aggregated with others, whether it is additive or if it is an average of another variable
# Description: a rough description of a variable based on its hierarchical structure. This is constructed by cancensus by recursively traversing the labels for every variable’s hierarchy, and facilitates searching for specific variables using key terms.
vector_list_21 = list_census_vectors("CA21")
# A tibble: 7,709 × 7
# Variable list from census 2021
# "1", # pop
# "6", # pop_dens
# '8', #
# '14','15', # age 15 24, 
# '35','37',  # dep_rat
# "41",'42', # det_homes
# "50" , # Total - Private households by household size - 100% data
# "57", # Average household size
# "78", "86", #sing_par
# '115', # med_at_inc
# "151", # gov_trans
# 
# "244", # Median after-tax income of household in 2020 ($)
# '345', # lim_at
# 
# "381", # Gini index on adjusted household after-tax income
# "382", # P90/P10 ratio on adjusted household after-tax income
# "1402", "1403", # pc_indig
# "1414", "1416", # pct_rent
# 
# "1470", # Total - Households 'spending 30% or more of income on shelter costs' or 'not suitable' or 'major repairs needed'
# "1472", # Not suitable only: too crowded
# "1473", # Major repairs needed only
# "1476", #  'Not suitable' and 'major repairs needed'
# "1477", # 'Spending 30% or more of income on shelter costs' and 'not suitable' and 'major repairs needed'
# "1537", # pct_npr "1"
# "1683",  "1684", # pct_vismin
# "1995", "1996",  #pct_nograd
# "2223", "2224", # labor_part_rate
# "2226", #labor_unemp_rate "2224"
# "2246", "2249" #pct_mgmt_occ

# v_CA21_1143
# Total
# P90/P10 ratio on adjusted household after-tax income
# Income; Inequality measures for the population in private households; P90/P10 ratio on adjusted household after-tax income


# get census data vectors for labor force involvement rates
# em_labels <- c("Number of employment income recipients aged 15 years and over in private households in 2020", "Median employment income in 2020 among recipients", "Number of employment insurance benefits recipients aged 15 years and over in private households in 2020")
# em_vectors <- 
#   find_census_vectors("employment", dataset= "CA21", type = "total", query_type = "semantic") %>% 
#   filter(str_detect(label, 
#                     pattern = paste(em_labels, collapse = "|")
#                      ))
#   
#   # union(search_census_vectors("labor", "CA21"))
# lf_labels <- c("Total - Population aged 15 years and over by labour force status", )
# lf_vectors <-  find_census_vectors("labor force", dataset= "CA21", type = "total", query_type = "semantic") 
# 
# income_vector = find_census_vectors('after tax income', dataset = 'CA21', type = 'total', query_type = 'semantic')

CA21_VECTORS = c(
  'POP'             = 'v_CA21_1',
  # Population, 2021
  
  'POP_PCT_CHANGE'  = 'v_CA21_3',
  # Population percentage change, 2016 to 2021
  
  'POP_DENS' = 'v_CA21_6',
  # POP_DENS
  
  ###########################################################################################################################################
  # Age group
  'AGE_GRP_TOT'     = 'v_CA21_8',
  # Total - Age
  'AGE_GRP_00_04'   = 'v_CA21_14',
  # 0 - 4 years
  'AGE_GRP_00_15'   = 'v_CA21_15',
  # 4 - 15 years
  
  ###########################################################################################################################################
  # Race
  # Indigenous identity, status, and ancestry
  # Total - Indigenous identity for the population in private households
  # Indigenous identity
  # Non-Indigenous identity
  
  'ABORIGINAL_TOT'  = 'v_CA21_4201', # Aboriginal Identity - Total Total - Aboriginal identity for the population in private households - 25% sample data
  'ABORIGINAL_YES'  = 'v_CA21_4204', # Aboriginal Identity - Yes Aboriginal identity
  'ABORIGINAL_NO'   = 'v_CA21_4225', # Aboriginal Identity - No Non-Aboriginal identity Done
  
  'REG_INDIAN_TOT'  = 'v_CA21_4228', # Population by Registered or Treaty Indian status - Total
  'REG_INDIAN_YES'  = 'v_CA21_4231', # Population by Registered or Treaty Indian status - Reigstered or Treaty Indian
  'REG_INDIAN_NO'   = 'v_CA21_4234', # Population by Registered or Treaty Indian status - Not a Reigstered or Treaty Indian Done
  
  # Visible minority and ethnic origin
  # Total - Visible minority for the population in private households
  # Total visible minority population
  # Not a visible minority
  
  'MINORITY_TOT'    = 'v_CA21_4872', # Visible Minority - Total
  'MINORITY_YES'    = 'v_CA21_4875', # Visible Minority - Yes
  'MINORITY_NO'     = 'v_CA21_4914', # Visible Minority - No Done
  
  # 25% Data
  # Citizenship and immigration
  # Total - Citizenship for the population in private households
  # Canadian citizens
  # Not Canadian citizens
  
  
  'CITIZEN_TOT'     = 'v_CA21_4389', # Citizenship - Total
  'CITIZEN_CAN'     = 'v_CA21_4392', # Citizenship - Canadian
  'CITIZEN_NOT_CAN' = 'v_CA21_4401', # Citizenship - Not Canadian
  
  # 25% Data
  # Citizenship and immigration
  # Total - Immigrant status and period of immigration for the population in private households
  # Non-immigrants
  # Immigrants
  
  'IMMIGRANT_TOT'   = 'v_CA21_4404', # Immigrant - Total
  'IMMIGRANT_YES'   = 'v_CA21_4410', # Immigrant - Yes
  'IMMIGRANT_NO'    = 'v_CA21_4407', # Immigrant - No
  
  # 100% data
  # Language
  # Knowledge of official languages for the total population excluding institutional residents
  # English only
  # French only
  # English and French
  # Neither English nor French
  
  'LANG_KNOW_TOT'  = 'v_CA21_1144', # Knowledge of official languages - total
  'LANG_KNOW_EN'   = 'v_CA21_1147', # Knowledge of official languages - english only
  'LANG_KNOW_FR'   = 'v_CA21_1150', # Knowledge of official languages - french only
  'LANG_KNOW_BOTH' = 'v_CA21_1153', # Knowledge of official languages - english and french
  'LANG_KNOW_NONE' = 'v_CA21_1156', # Knowledge of official languages - neither english nor french Done
  
  # 100% data
  # Language
  # First official language spoken for the total population excluding institutional residents
  # English
  # French
  # English and French
  # Neither English nor French
  
  'LANG_SPOKE_TOT'   = 'v_CA21_1159', # First official language spoken - total
  'LANG_SPOKE_EN'    = 'v_CA21_1162', # First official language spoken - english only
  'LANG_SPOKE_FR'    = 'v_CA21_1165', # First official language spoken - french only
  'LANG_SPOKE_BOTH'  = 'v_CA21_1168', # First official language spoken - english and french
  'LANG_SPOKE_NONE'  = 'v_CA21_1171', # First official language spoken - neither english nor french Done
  
  
  ###########################################################################################################################################
  # Family
  # 'SING_PAR1' = 'v_CA21_78',
  # # SING_PAR1
  # 'SING_PAR2' = 'v_CA21_86',
  # # SING_PAR2
  
  'MARITAL_TOT'     = 'v_CA21_453',  # Marital Status - Total
  'MARITAL_SEP'     = 'v_CA21_483',  # Marital Status - Separated
  'MARITAL_DIV'     = 'v_CA21_486',  # Marital Status - Divorced
  
  'SING_PARENT_TOT' = 'v_CA21_507',  # Lone Parent - Total
  'SING_PARENT_F'   = 'v_CA21_508',  # Lone Parent - Female
  'SING_PARENT_M'   = 'v_CA21_509',  # Lone Parent - Male Done
  
  
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
  
  
  'INC_BT_HHS_MED'  = 'v_CA21_906', # Total Household Income Before Tax - Median
  'INC_AT_HHS_MED'  = 'v_CA21_907', # Total Household Income After Tax - Average
  
  'MED_AT_INC'   = 'v_CA21_115',
  # MED_AT_INC
  
  'LIM_AT' = 'v_CA21_3455',
  # LIM_AT
  
  'INC_BT_IND_MED'  = 'v_CA21_560', # Total Individual Income Before Tax - Median -  Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2020 among recipients ($)
  'INC_BT_IND_MED_19'  = 'v_CA21_818', # Total Individual Income Before Tax - Median -Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2019 among recipients ($)
  'INC_BT_IND_AVG'  = 'v_CA21_605', # Total Individual Income Before Tax - Average ? Number of total income recipients aged 15 years and over in private households in 2020 Average total income in 2020 among recipients ($) - 35%
  
  
  # 25% Data
  # Income
  # Economic family income
  # Income statistics for economic families in private households
  # Average total income of economic family in 2020 ($)
  # Average after-tax income of economic family in 2020 ($)
  
  'INC_BT_FAM_MED'  = 'v_CA21_965', # Total Family Income Before Tax - Median
  # 'INC_BT_FAM_AVG'  = 'v_CA21_990', # Total Family Income Before Tax - Average  / / Average after-tax income of economic families in 2020 ($)
  'INC_AT_FAM_MED'  = 'v_CA21_966', # Total Family Income After Tax - Median
  # 'INC_AT_FAM_AVG'  = 'v_CA21_991', # Total Family Income After Tax - Average   Done
  
  # Inequality measures for the population in private households
  # 'GINI_INDEX_         # Gini index on adjusted household total income
  # 'GINI_INDEX_         # Gini index on adjusted household market income
  'GINI_INDEX_AT_INC' = 'v_CA21_1142',         # Gini index on adjusted household after-tax income
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
  'OCC_BUS_FIN_AD'  = 'v_CA21_6573', # Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA21_6576', # Occupation - Natural and Applied Sciences
  'OCC_HLTH'        = 'v_CA21_6579', # Occupation - Health
  'OCC_SOCI_SERV'   = 'v_CA21_6582', # Occupation - Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA21_6585', # Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV'   = 'v_CA21_6588', # Occupation - Sales and Service
  'OCC_TRADES'      = 'v_CA21_6591', # Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC'    = 'v_CA21_6594', # Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL'    = 'v_CA21_6597', # Occupation - Manufacturing and Utilities Done
  
  ###########################################################################################################################################
  # Education attainment
  # 25% Data
  # Education
  # Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households
  # No certificate, diploma or degree
  # Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households
  # No certificate, diploma or degree
  # High (secondary) school diploma or equivalency certificate
  # Postsecondary certificate, diploma or degree
  # Postsecondary certificate or diploma below bachelor level
  # Apprenticeship or trades certificate or diploma
  # College, CEGEP or other non-university certificate or diploma
  # University certificate or diploma below bachelor level
  # Bachelor's degree or higher
  # Bachelor's degree
  # University certificate or diploma above bachelor level
  # Degree in medicine, dentistry, veterinary medicine or optometry
  # Master's degree
  # Earned doctorate
  
  'EDUC_TOT'        = 'v_CA21_5817', # Education for population aged 25 - 64 - Total
  'EDUC_NONE'       = 'v_CA21_5820', # Education for population aged 25 - 64 - No certificate, diploma or degree
  'EDUC_HIGHSCH'    = 'v_CA21_5871', # Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate
  'EDUC_POSTSEC'    = 'v_CA21_5874', # Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree
  'EDUC_COLLEGE'    = 'v_CA21_5889', # Education for population aged 25 - 64 - College, CEGEP or other non-university certificate or diploma
  'EDUC_BACHELOR'   = 'v_CA21_5898', # Education for population aged 25 - 64 - Bachelor's degree
  'EDUC_TRADES'     = 'v_CA21_5880', # Education for population aged 25 - 64 - Apprenticeship or trades certificate or diploma
  'EDUC_CRT_ABV_BACH' = 'v_CA21_5901', # Education for population aged 25 - 64 - University certificate or diploma above bachelor level
  'EDUC_MEDICAL'   = 'v_CA21_5904', # Education for population aged 25 - 64 - Degree in medicine, dentistry, veterinary medicine or optometry
  'EDUC_MASTERS'   = 'v_CA21_5907', # Education for population aged 25 - 64 - Master's degree
  'EDUC_PHD'   = 'v_CA21_5910', # Education for population aged 25 - 64 - Earned doctorate Done
  
  
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
  # 'LABOUR_UNEM_RT'  = 'v_CA21_5618', # Labour Force - Unemployment Rate
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
  'HOME_SUITABLE_TOT' = 'v_CA21_4260', # Private households by housing suitability - Total
  'HOME_SUITABLE_YES' = 'v_CA21_4261', # Private households by housing suitability - Suitable
  'HOME_SUITABLE_NO'  = 'v_CA21_4262' # Private households by housing suitability - Not suitable Done
  
 )

bc_da <- get_census(dataset='CA21', 
                    regions=list(PR="59"),
                     vectors=CA21_VECTORS, 
                    level='DA', 
                    quiet = TRUE, 
                     geo_format = NA,
                    labels = 'short')

bc_da %>% readr::write_csv(use_network_path("data/bc_da_21.csv"))
bc_da <- readr::read_csv(use_network_path("data/bc_da_21.csv"))
bc_da %>% glimpse()
# 7848

# bc_da %>% 
#   count(GeoUID)
# 7848, so no duplicated GeoUID

# bc_da %>%
#   count(CSD_UID)
# 751 CSD

# The number of DA in Census2021 (7848) is larger than the number of DA in TMF (6967)

# DA id is in long form
# 59 09 0103
# PR-CD-DA code
# Province 59: British Columbia
# CD 09: Fraser Valley
# DA 0103

# `Region Name` actually is the DA number 59150004
# which does not include CSD information, CSD looks like 5915055, 5915 is the CD, so DA and CSD should be using together. 
# test if there are different DA

bc_da %>% 
  anti_join(TMF_DA_LEVEL %>% mutate(GeoUID = as.character(DA_NUM)), by = c("GeoUID" ))
# 881 DAs are only available in Census 2021, but not in TMF.

TMF_DA_LEVEL %>% mutate(GeoUID = as.character(DA_NUM)) %>% 
  anti_join(bc_da, by = c(  "GeoUID"))

# 6967 TMF DAs are all available in Census 2021, 

# for example,59010124

# bc_da %>%
#   filter(GeoUID == "59010124")
# 
# TMF_DA_LEVEL %>% mutate(GeoUID = as.character(DA_NUM)) %>% 
#   filter(GeoUID == "59010124")

TMF_DA_LEVEL %>% 
  count(CD_2021)
# 29 cd

TMF_DA_LEVEL %>% 
  count(CD_2021,CSD_2021)
# 420 csd in TMF are active.

TMF %>% 
  count(CD_2021,CSD_2021)
# 421 csd in TMF

bc_da %>% 
  count(CSD_UID)
# 751 csd in census





########################################################################################################
#  # what is SGC.Code
# Purpose: The SGC is Statistics Canada’s official classification for geographic areas, enabling the production of integrated statistics by geographic area.
# Structure: It consists of a four-level hierarchy of geographic units identified by a seven-digit numerical coding system3.
# Components: The classification includes geographical regions, provinces and territories, census divisions, and census subdivisions4.
# Updates: The SGC is updated every five years, with the 2021 version being the eleventh edition, incorporating changes from the 2016 version.
# https://www.statcan.gc.ca/en/subjects/standard/sgc/2021/index
# Census subdivision, census division 
########################################################################################################

SGC_structure_file = "https://www.statcan.gc.ca/en/statistical-programs/document/sgc-cgt-2021-structure-eng.csv"
SGC_structure = readr::read_csv(SGC_structure_file)
BC_SGC_structure = SGC_structure %>% 
  filter(str_starts(as.character(Code), "59"))

BC_SGC_structure %>% 
  count(`Hierarchical structure`)
# 29 CD and 751 CSDs in BC

SGC_structure %>% 
  count()


SGC_element_file = "https://www.statcan.gc.ca/en/statistical-programs/document/sgc-cgt-2021-element-eng.csv"
SGC_element = readr::read_csv(SGC_structure_file)
BC_SGC_element = SGC_element %>%
  filter(str_starts(as.character(Code), "59"))

BC_SGC_element %>% 
  count(`Hierarchical structure`)
# 29 CD and 751 CSDs in BC. the same as census


########################################################################################################

#  B.C. crime trends and statistics
# https://www2.gov.bc.ca/gov/content/justice/criminal-justice/policing-in-bc/publications-statistics-legislation/crime-police-resource-statistics

# Incident-based crime statistics, by detailed violations, police services in British Columbia 1, 2, 3, 4, 5
# Frequency: Annual

# Statistics Canada. Table 35-10-0184-01 Incident-based crime statistics, by detailed violations, police services in British Columbia, annual (number unless otherwise noted)


# Release date: 2023-07-27



# Geography: Province or territory, Policing district/zone

# https://www2.gov.bc.ca/assets/gov/law-crime-and-justice/criminal-justice/police/publications/statistics/bc-crime-statistics-2022.xlsx

# Policing district/zone is different from 

# https://catalogue.data.gov.bc.ca/dataset/policing-jurisdictions-and-regions-in-bc


# annual and policing district data. Luckily, BC stats team already aggregate the one data variable (total rate excluding traffic) to region level which is close to CD. 

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510018401

########################################################################################################
# print("file out of date: this could take a while")
cansim_id <- "35-10-0184-01"
options(cansim.cache_path = use_network_path("data/cansim_cache"))
getOption("cansim.cache_path")
connection <- cansim::get_cansim_sqlite(cansim_id)
# 
# connection %>% glimpse()
# 
# 
Violations_list = connection %>%
  count(Violations) %>%
  collect()
# # 314 types of crime
# we should choose: 1. 

# Total, all Criminal Code violations (excluding traffic) [50]
# Total violent Criminal Code violations [100]
# Homicide [110]

crime_GEO_list = connection %>%
  count(GEO ) %>%
  collect()
# # 237 regions: Policing district/zone. id Police Services Respondent Codes: RESP like 59774
#  [59774] need to parse out and join to TMF RESP



bc_crime_stats <- connection %>%
  filter(
    # GEO=="British Columbia",
    # str_starts( GeoUID, "59"),
    Violations %IN% c("Total, all Criminal Code violations (excluding traffic) [50]", "Total violent Criminal Code violations [100]", "Homicide [110]")  ,
    Statistics  == "Rate per 100,000 population"
  ) %>%
  # filter(REF_DATE  > lubridate::today() - lubridate::years(11))%>%
  cansim::collect_and_normalize() %>%
  janitor::clean_names()

bc_resp_lookup = bc_crime_stats %>% 
  count(geo, geo_uid)
# 237 resps

# policy zone is like: Colwood, British Columbia, Royal Canadian Mounted Police, municipal [59819]

# bc_crime_stats = bc_crime_stats %>%
#   mutate(
#     `Period Starting` = lubridate::ym(ref_date),
#     Source = paste("Statistics Canada. Table", cansim_id, "Crime Rate per 100,000 population")
#   ) %>%
#   filter(`Period Starting` > lubridate::today() - lubridate::years(11))# %>%
# select(`Period Starting`, Geo, Geouid, Violations, VECTOR, Value,Statistics, Source)

# From TMF get the look-up table for RESP


# TMF %>% 
#   filter(ACTIVE == "Y") %>% 
#   count(RESP)
# # 193
# 
# TMF %>%
#   filter(ACTIVE == "Y") %>%
#   count(CD_2021)
# 29

TMF %>%
  filter(ACTIVE == "Y") %>%
  count( CD_2021, CSD_2021,MUN_NAME_2021)
# 410
TMF %>%
  filter(ACTIVE == "Y") %>%
  count(RESP, CD_2021, CSD_2021,MUN_NAME_2021)
# 473 CSDs. 
# some CSDs share the same resp

##########################################################################
# the number of RESPs is between the number of CDs and the number of CSDs
# so we can aggregate RESP to CDs, potentially without overlapping the CDs or RESPs. 
# For CSD or DAs, many CSDs or DAs have to share RESP together, so it is better to calculate the ratios within each RESP and CSDs or DAs could share the ratios. 
# If one CD or CSD has two or more RESPs, we could average them weighting by the number of the postal code regions within the RESPs. 
###########################################################################

TMF %>%
  filter(ACTIVE == "Y") %>%
  count(RESP, CD_2021, CSD_2021,MUN_NAME_2021) %>% 
  group_by(CD_2021, CSD_2021,MUN_NAME_2021) %>% 
  mutate(n_resp = n_distinct(RESP)) %>% 
  filter(n_resp>1) %>% 
  arrange(CD_2021, CSD_2021,MUN_NAME_2021)
# 101 CSDs shared RESP


# TMF %>% 
#   filter(ACTIVE == "Y") %>% 
#   count(DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021)
# 6967
TMF %>%
  filter(ACTIVE == "Y") %>%
  count(RESP,DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021)
# # 7009, this is the best combination to get the DA information, and later, we can aggregate to CD or CSD level.
# some DAs share the RESP so 7009> 6997

TMF %>%
  filter(ACTIVE == "Y") %>%
  count(RESP,DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021) %>% 
  group_by(DA_NUM,) %>% 
  mutate(n_resp = n_distinct(RESP)) %>% 
  arrange(DA_NUM) %>% 
  filter(n_resp>1)
# 84 DAs have more than one RESP


# one option is to join to da table
bc_da_crime_stats_year = bc_crime_stats %>% 
  select(ref_date,geo,uom,scalar_factor,coordinate,value,geo_uid,val_norm,date,classification_code_for_violations,violations,statistics) %>% 
  mutate(RESP = as.numeric(geo_uid)) %>% 
  right_join(TMF %>% 
                filter(ACTIVE == "Y") %>%
                count(RESP,DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021),
            by = join_by("RESP" == "RESP"))
# to eliminate the dupliated RESP rate within DAs, we use n of postal code regions as weights

bc_da_crime_stats_year %>% names %>% paste(collapse = ",")
bc_da_crime_stats_year = bc_da_crime_stats_year %>% 
  group_by(DA_NUM,CD_2021,CSD_2021,DA_2021,MUN_NAME_2021,ref_date, classification_code_for_violations,violations,statistics) %>% 
  summarise(crimte_rate = weighted.mean(value, w = n))
  

# unexpected many-to-many relationship between `x` and `y`.
# each RESP is corresponding to many DAs. Within RESP, many DAs share the same rates.
# the left table has many years data and three types of rates. 

bc_crime_stats %>% 
  names() %>% 
  paste(collapse = ",") 

# second option is to join to CSD table
bc_csd_crime_stats_year = bc_crime_stats %>% 
  select(ref_date,geo,uom,scalar_factor,coordinate,value,geo_uid,val_norm,date,classification_code_for_violations,violations,statistics) %>% 
  mutate(RESP = as.numeric(geo_uid)) %>% 
  right_join(TMF %>% 
              filter(ACTIVE == "Y") %>%
              count(RESP, CD_2021, CSD_2021,  MUN_NAME_2021),
            by = join_by("RESP" == "RESP"))
# Detected an unexpected many-to-many relationship between `x` and `y`.
# and why there are so many missing values? Many RESPs don't have corresponding CSD?
# OK, those RESPs with missing values may not be a valid RESP which have a lot of missing values in crime rate as well. or have crime rates zero. 
# or those RESPs are not available anymore since the TMF only have most updated valid/active csd and RESPs.
# for example, 59893



bc_crime_stats %>% 
  filter(geo_uid == "59893") %>% 
  tail
# the values are not available for last 6 years


# to eliminate the dupliated RESP rate within DAs, we use n of postal code regions as weights

bc_csd_crime_stats_year %>% names %>% paste(collapse = ",")
bc_csd_crime_stats_year = bc_csd_crime_stats_year %>% 
  group_by(CD_2021,CSD_2021,MUN_NAME_2021,ref_date, classification_code_for_violations,violations,statistics) %>% 
  summarise(crimte_rate = weighted.mean(value, w = n))

# third option is to join to CD table


bc_cd_crime_stats_year = bc_crime_stats %>% 
  select(ref_date,geo,uom,scalar_factor,coordinate,value,geo_uid,val_norm,date,classification_code_for_violations,violations,statistics) %>% 
  mutate(RESP = as.numeric(geo_uid)) %>% 
  right_join(TMF %>% 
               filter(ACTIVE == "Y") %>%
               count(RESP, CD_2021),
             by = join_by("RESP" == "RESP"))
# Detected an unexpected many-to-many relationship between `x` and `y`.


# to eliminate the dupliated RESP rate within DAs, we use n of postal code regions as weights

bc_cd_crime_stats_year %>% names %>% paste(collapse = ",")
bc_cd_crime_stats_year = bc_cd_crime_stats_year %>% 
  group_by(CD_2021,ref_date, classification_code_for_violations,violations,statistics) %>% 
  summarise(crimte_rate = weighted.mean(value, w = n))


########################################################################################################
#  B.C. population projections need to be manually downloaded :( from https://bcstats.shinyapps.io/popApp/
# 
# Geography: Regional District

# Should be available CSD, instead of CD check out the app
# we use data in decimal database, but there are many problems in the data in decimal database
########################################################################################################

bc_pop_estimate_muni_file = use_network_path("data/BCStats/Population_Projections.csv")

bc_pop_estimate_muni_df = read_csv(bc_pop_estimate_muni_file, skip = 6)

bc_pop_estimate_muni_df %>% 
  count( Region, `Municipality`)
# 29 regions similar to CD
# 192 Municipality similar to CSDs

bc_pop_estimate_muni_year_df = TMF %>% 
  filter(ACTIVE == "Y") %>%
  count( CD_2021, CSD_2021,  MUN_NAME_2021) %>% 
  mutate(CSD_char = str_c(CD_2021, CSD_2021)) %>% 
  left_join(bc_pop_estimate_muni_df %>% 
              mutate(CSD_char = str_pad(Region, width = 5, side= "left", pad = "0")), 
            by = join_by("CSD_char")) %>% 
  pivot_wider(names_from = Gender,
              values_from = Total)

bc_pop_estimate_muni_year_df %>% 
  count(CSD_char)
# 420 csd
bc_pop_estimate_muni_year_df %>% 
  filter(CSD_2021 == "017")

 
# Central Kootenay a, Cowichan Valley a, Cowichan, Nanaimo a, Alberni-Clayoquot a, Strathcona, Sunshine Coast A   are treated differently in pop estimate. Such as " Unincorporated Areas - Alberni-Clayoquot "
# should split the value weighted by number of the postal code regions? Those are small CSDs which have less than 100 postal code regions
bc_pop_estimate_muni_df %>% 
  filter(str_detect(Municipality, "Sunshine Coast"))
# should we create a list for those small regions/CSDs??????
# we use data in decimal database
  

######################################################################  
# current-census-dissemination-areas
########################################################################################################
bcdc_search("current-census-dissemination-areas", n = 5)
bcdc_get_record("a091fd65-d682-4a24-8c0e-68de7c87e3a3")
bcdc_tidy_resources('c17be172-c264-48d8-82f2-0ce68d0901cb') 
# Available Resources (1):
  # 1. WMS getCapabilities request (wms)
# Access the full 'Resources' data frame using:
  bcdc_tidy_resources('a091fd65-d682-4a24-8c0e-68de7c87e3a3')
# Query and filter this data using:
bc_da_2021_economic_region = bcdc_query_geodata('a091fd65-d682-4a24-8c0e-68de7c87e3a3')  %>% 
  collect()
# • Using collect() on this object will return 7848 features and 13 fields  


bc_da_2021_economic_region %>% plot()

######################################################################  
# cCurrent Census Division Boundaries
########################################################################################################

bcdc_get_record("ef17918a-597a-4012-8534-f8e71d8735b3")

# Available Resources (1):
  # 1. WMS getCapabilities request (wms)
# Access the full 'Resources' data frame using: bcdc_tidy_resources('ef17918a-597a-4012-8534-f8e71d8735b3')
# Query and filter this data using: 
bc_cd_sf =  bcdc_query_geodata('ef17918a-597a-4012-8534-f8e71d8735b3') %>% collect()

bc_cd_sf %>% plot()
# 29 census districts
  # 3: Current Census Division Boundaries (multiple, wms, kml)
  # ID: ef17918a-597a-4012-8534-f8e71d8735b3
  # Name: current-census-division-boundaries
  # 4: BC Sub-Provincial Population Estimates and Projections (csv)
  # ID: 86839277-986a-4a29-9f70-fa9b1166f6cb
  # Name: bc-sub-provincial-population-estimates-and-projections







########################################################################################################

# a simple features dataframe for BCs economic regions (for mapping)
########################################################################################################

bc_reg_sf <-   bcmaps::census_economic() %>%
    sf::st_transform("+proj=longlat +datum=WGS84") %>%
    janitor::clean_names() %>%
    select(region=economic_region_name, geometry) %>%
    mutate(
      region = stringr::word(region, 1, sep = "/"),
      region = janitor::make_clean_names(region),
      region = case_when(
        region == "nechako" ~ "north_coast_&_nechako",
        region == "north_coast" ~ "north_coast_&_nechako",
        TRUE ~ region),
      region = stringr::str_replace_all(region, "vancouver_island_and_coast", "vancouver_island_coast"),
      region = stringr::str_replace_all(region, "lower_mainland_southwest", "mainland_south_west"),
      region = stringr::str_replace_all(region, "northeast", "north_east")
    )

########################################################################################################

# BC maps for BCs economic regions (for mapping)
########################################################################################################

library(tidyverse)
library(bcdata)
library(sf)
library(bcmaps)
library(rmapshaper)
library(janitor)
#library(viridis)

#economic regions spatial data from the B.C. Data Catalogue using the bcdata package
# https://catalogue.data.gov.bc.ca/dataset/1aebc451-a41c-496f-8b18-6f414cde93b7
economic_regions <-
  bcdc_get_data("1aebc451-a41c-496f-8b18-6f414cde93b7") %>%
  clean_names() %>%
  mutate(geo = case_when(economic_region_id == 5910 ~ "Vancouver Island and Coast",
                         economic_region_id == 5920 ~ "Lower Mainland-Southwest",
                         economic_region_id == 5930 ~ "Thompson-Okanagan",
                         economic_region_id == 5940 ~ "Kootenay",
                         economic_region_id == 5950 ~ "Cariboo",
                         economic_region_id == 5960 ~ "North Coast and Nechako",
                         economic_region_id == 5970 ~ "North Coast and Nechako",
                         economic_region_id == 5980 ~ "Northeast")) %>%
  group_by(geo) %>%
  summarise() %>%
  rmapshaper::ms_clip(bcmaps::bc_bound(class = "sf")) %>%
  ms_simplify(keep = 0.075, sys = TRUE)

## cmas 
# census metropolitan areas spatial data from the B.C. Data Catalogue using the bcdata package 
# https://catalogue.data.gov.bc.ca/dataset/a6fb34b7-0937-4718-8f1f-43dba2c0f407
cmas <- 
  bcdc_get_data("a6fb34b7-0937-4718-8f1f-43dba2c0f407") %>%
  clean_names() %>%
  filter(census_metro_area_name %in% c("Kelowna", "Abbotsford - Mission", "Vancouver", "Victoria")) %>%
  mutate(geo = str_remove_all(census_metro_area_name, " ")) 

bc <- bc_bound() %>%
  select(-island) %>%
  mutate(id = row_number()) %>%
  ms_simplify(keep = 0.25, sys = TRUE)

qs::qsave(economic_regions, here::here("app", "economic_regions.qs"))
qs::qsave(cmas, here::here("app", "cmas.qs"))
qs::qsave(bc, here::here("app", "bc.qs"))

## health authorities
# https://catalogue.data.gov.bc.ca/dataset/7bc6018f-bb4f-4e5d-845e-c529e3d1ac3b
has <-
  bcdc_get_data('7bc6018f-bb4f-4e5d-845e-c529e3d1ac3b', resource = 'dfd14c9b-45f8-4a7e-ad42-9a881778e417') %>%
  clean_names() 



