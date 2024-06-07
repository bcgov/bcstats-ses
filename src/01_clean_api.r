pacman::p_load(cancensus,geojsonsf, tidyverse)
# In order to speed up performance, reduce API quota usage, and reduce unnecessary network calls, please set up a persistent cache directory via `set_cancensus_cache_path('<local cache path>', install = TRUE)`.
# This will add your cache directory as environment varianble to your .Renviron to be used across sessions and projects.

# set up CENSUSMAPPER API 
# set_cancensus_api_key(config::get("CENSUSMAPPER_API_KEY"), install = TRUE)
# set_cancensus_cache_path('G:\\Operations\\Data Science and Analytics\\2024 SES Index\\data\\census_cache', install = TRUE)
# or
# options(cancensus.api_key = "your_api_key")
# options(cancensus.cache_path = "custom cache path")

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


van_da <- get_census(dataset='CA21', regions=list(CMA="59933"),
                     vectors=c("median_hh_income"="v_CA21_906"), level='DA', quiet = TRUE, 
                     geo_format = NA, labels = 'short')

# view all available Census variables for a given dataset.
# Vector: short variable code
# Type: variables are provided as aggregates of female responses, male responses, or total (male+female) responses
# Label: detailed variable name
# Units: provides information about whether the variable represents a count integer, a ratio, a percentage, or a currency figure
# Parent_vector: shows the immediate hierarchical parent category for that variable, where appropriate
# Aggregation: indicates how the variable should be aggregated with others, whether it is additive or if it is an average of another variable
# Description: a rough description of a variable based on its hierarchical structure. This is constructed by cancensus by recursively traversing the labels for every variable’s hierarchy, and facilitates searching for specific variables using key terms.
vector_list_21 = list_census_vectors("CA21")

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
                     geo_format = NA, labels = 'short')

bc_da %>% readr::write_csv("G:\\Operations\\Data Science and Analytics\\2024 SES Index\\data\\bc_da.csv")

bc_da %>% glimpse()
########################################################################################################
#  Community Well-Being Index
# install.packages("remotes")
# remotes::install_github("vlucet/rgovcan")
# https://open-canada.github.io/r4gc/canada-related-open-source-r-codes-and-packages.html
########################################################################################################

library("rgovcan")
# start with a package id corresponding to an actual record and retrieve a ckan_package.
# https://open.canada.ca/data/en/dataset/56578f58-a775-44ea-9cc5-9bf7c78410e6
id <- "56578f58-a775-44ea-9cc5-9bf7c78410e6" # Package ID
id_search <- govcan_get_record(record_id = id)
id_search # outputs a `ckan_package`

# Once the packages have been retrieved from the portal, you can use govcan_get_resources on those results to display the ckan_resources contained in the packages (a “resource” is any dataset attached to a given record). This outputs a ckan_resource_stack when called on a unique package.
id_resources <- govcan_get_resources(id_search)
id_resources # outputs a `resource_stack`
# download the resources with govcan_dl_resources(). These can either be stored to a certain directory or load into session (* this option might fail due to current issues with ckanr::ckan_fetch).

path <- "G:\\Operations\\Data Science and Analytics\\2024 SES Index\\data\\CWBI"
dir.create(path, recursive = TRUE)

govcan_dl_resources(id_resources, path = path, included = c("CSV"))


# Some files are not available, so use tryCatch to avoid the errors.
for (i in id_resources){
  print(i)
  str(i)
  tryCatch(
  {govcan_dl_resources(i, path = path, included = c("CSV"))},
  
  error = function(e){
    message("data is not available")
  }
  
  )
}






