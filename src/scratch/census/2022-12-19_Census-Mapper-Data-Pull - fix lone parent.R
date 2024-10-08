rm(list = ls())

# Load packages
pacman::p_load(
  cancensus,
  mapview,
  leaflet,
  leafpop,
  sf,
  maptools,
  GGally,
  tidyverse,
  dplyr,
  scales,
  dbplyr,
  readxl,
  ggthemes,
  ggpubr,
  RODBC,
  DBI,
  odbc,
  rgeos,
  visdat,
  naniar,
  simputation,
  styler,
  hash,
  # plyr,
  # reshape2,
  rgeos
)

# Set up API key and path to save data
options(cancensus.api_key = "CensusMapper_1a8514c30536d723ad1c450aafe1b26b")
options(cancensus.cache_path = "K:/Projects/Advanced Analytics/1 Projects/2019/2019-253 Census database all years/cancensus data")

# # To view available Census datasets
# list_census_datasets()
#
# # To view available named regions at different levels of Census hierarchy for the 2016 Census (for example)
# list_census_regions("CA16")
# list_census_regions("CA11")

# To view available Census variables for the 2016 Census
CA21_vectors_all <- list_census_vectors("CA21")
CA16_vectors_all <- list_census_vectors("CA16")
CA11_vectors_all <- list_census_vectors("CA11")
CA06_vectors_all <- list_census_vectors("CA06")
CA01_vectors_all <- list_census_vectors("CA01")

# # https://stackoverflow.com/questions/13774773/check-whether-value-exist-in-one-data-frame-or-not
# CA16_CA11_match <- as.data.frame(CA11_vectors$label %in% CA16_vectors$label)
# CA16_CA11_match_idx <- CA11_vectors$label[CA11_vectors$label %in% CA16_vectors$label]
# CA16_CA11_match_df <- CA11_vectors[CA11_vectors$label %in% CA16_vectors$label,  ]


# ********************************************************************************
# PULL 2021 DATA
# ********************************************************************************


# census_data_pop_change <- get_census(dataset='CA21', regions=list(DA="59151302"), vectors=c("v_CA21_3"), labels="detailed", geo_format=NA, level='Regions')
# POP_PCT_CHANGE is not available, we need to get data in CA16 and calculate it.
CA21_VECTORS = c(
  'POP'             = 'v_CA21_1',  # Population, 2021

  'POP_PCT_CHANGE'  = 'v_CA21_3',  # Population percentage change, 2016 to 2021
  'AGE_GRP_TOT'     = 'v_CA21_8',    # Total - Age
  'AGE_GRP_TOT_F'   = 'v_CA21_10',    # Total - Age - Female
  'AGE_GRP_TOT_M'   = 'v_CA21_9',    # Total - Age - Male
  'AGE_GRP_00_04'   = 'v_CA21_14',    # 0 - 4 years
  'AGE_GRP_05_09'   = 'v_CA21_32',   # 5 - 9 years
  'AGE_GRP_10_14'   = 'v_CA21_50',   # 10 - 14 years
  'AGE_GRP_15_19'   = 'v_CA21_71',   # 15 - 19 years
  'AGE_GRP_20_24'   = 'v_CA21_89',   # 20 - 24 years
  'AGE_GRP_25_29'   = 'v_CA21_107',  # 25 - 29 years
  'AGE_GRP_30_34'   = 'v_CA21_125',  # 30 - 34 years
  'AGE_GRP_35_39'   = 'v_CA21_143',  # 35 - 39 years
  'AGE_GRP_40_44'   = 'v_CA21_161',  # 40 - 44 years
  'AGE_GRP_45_49'   = 'v_CA21_179',  # 45 - 49 years
  'AGE_GRP_50_54'   = 'v_CA21_197',  # 50 - 54 years
  'AGE_GRP_55_59'   = 'v_CA21_215',  # 55 - 59 years
  'AGE_GRP_60_64'   = 'v_CA21_233',  # 60 - 64 years
  'AGE_GRP_65_PLUS' = 'v_CA21_251',  # 65 and over Done

  'DWELL_TOT'       = 'v_CA21_434',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA21_435',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA21_440',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA21_439',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA21_442',  # Dwellings - Movable Done

  'MARITAL_TOT'     = 'v_CA21_453',  # Marital Status - Total
  'MARITAL_MARR'    = 'v_CA21_459',  # Marital Status - Married
  'MARITAL_NEVER'   = 'v_CA21_480',  # Marital Status - Never Married
  'MARITAL_SEP'     = 'v_CA21_483',  # Marital Status - Separated
  'MARITAL_DIV'     = 'v_CA21_486',  # Marital Status - Divorced
  'MARITAL_WID'     = 'v_CA21_489',  # Marital Status - Widowed Done

  'FAM_SIZE_TOT'    = 'v_CA21_492',  # Family Size - Total Census families in private households by family size
  'FAM_SIZE_2'      = 'v_CA21_493',  # Family Size - 2 persons
  'FAM_SIZE_3'      = 'v_CA21_494',  # Family Size - 3 persons
  'FAM_SIZE_4'      = 'v_CA21_495',  # Family Size - 4 persons
  'FAM_SIZE_5'      = 'v_CA21_496',  # Family Size - 5+ persons Done

  'SING_PARENT_TOT' = 'v_CA21_507',  # Lone Parent - Total
  'SING_PARENT_F'   = 'v_CA21_508',  # Lone Parent - Female
  'SING_PARENT_M'   = 'v_CA21_509',  # Lone Parent - Male Done
  # in 2021 there are married couple and common-law couple. Not sure if previous census had these two groups.If not, may add these up to get a consistent measure.
  'COUPLES_TOT'        = 'v_CA21_501', # Total - Married couples census families in private households
  'COUPLES_CHILD_Y'    = 'v_CA21_502', # Married couples census families in private households - 100% data; Couples with children
  'COUPLES_CHILD_N'    = 'v_CA21_503', # Married couples census families in private households - 100% data; Couples without children
  'CL_COUPLES_TOT'     = 'v_CA21_504', # Total - Common-law couples census families in private households
  'CL_COUPLES_CHILD_Y' = 'v_CA21_505', # Common-law couples census families in private households - 100% data; Couples with children
  'CL_COUPLES_CHILD_N' = 'v_CA21_506', # Common-law couples census families in private households - 100% data; Couples without children Done

  'INC_BT_IND_MED'  = 'v_CA21_560', # Total Individual Income Before Tax - Median -  Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2020 among recipients ($)
  'INC_BT_IND_MED_19'  = 'v_CA21_818', # Total Individual Income Before Tax - Median -Number of total income recipients aged 15 years and over in private households in 2020 - Median total income in 2019 among recipients ($)
  'INC_BT_IND_AVG'  = 'v_CA21_605', # Total Individual Income Before Tax - Average ? Number of total income recipients aged 15 years and over in private households in 2020 Average total income in 2020 among recipients ($) - 35%

  #   Economic family income
  # Income statistics for economic families in private households
  # Median total income of economic family in 2020 ($)
  # Median after-tax income of economic family in 2020 ($)
  # Average family size of economic families

  # Median on 100% /  Avg on 25%


  # 25% Data
  # Income
  # Economic family income
  # Income statistics for economic families in private households
  # Average total income of economic family in 2020 ($)
  # Average after-tax income of economic family in 2020 ($)

  'INC_BT_FAM_MED'  = 'v_CA21_965', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG'  = 'v_CA21_990', # Total Family Income Before Tax - Average  / / Average after-tax income of economic families in 2020 ($)
  'INC_AT_FAM_MED'  = 'v_CA21_966', # Total Family Income After Tax - Median
  'INC_AT_FAM_AVG'  = 'v_CA21_991', # Total Family Income After Tax - Average   Done

  # 'v_CA21_1007' is income statistics for persons aged 15 years and over not in economic families in private households / Average after-tax income in 2020 ($)   Total

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

  # Household income 25%
  # Income statistics for private households
  # Average total income of household in 2020 ($)
  # Average after-tax income of household in 2020 ($)

  'INC_BT_HHS_AVG'  = 'v_CA21_915', # Total Household Income Before Tax - Median
  'INC_AT_HHS_AVG'  = 'v_CA21_916', # Total Household Income After Tax - Average


  # 100% data

  # Adjusted after-tax family income decile group for the population in private householdsTotalMaleFemale
  # In bottom half of the distributionTotalMaleFemale
  # In top half of the distributionTotalMaleFemale
  # In top decile
  'INC_FAM_TOTAL' = 'v_CA21_1100',
  'INC_FAM_TOP' = 'v_CA21_1121', # Total - Economic family income - In the top half of the distribution
  'INC_FAM_BOTTOM' = 'v_CA21_1103', # Total - Economic family income - In the bottom half of the distribution
  'INC_FAM_DEC_10' = 'v_CA21_1136', # Total - Economic family income - In the top decile


  # Household total income groups in 2020 for private households
  # Household after-tax income groups in 2020 for private households



  'INC_HHS_GRP_TOT' = 'v_CA21_944', # Total - Household after-tax income groups - Total - ? not sure if it is the one
  'INC_HHS_100k_125k' = 'v_CA21_961', # Total - Household after-tax income groups - $100,000 to $124,999
  'INC_HHS_125k_150k' = 'v_CA21_962', # Total - Household after-tax income groups - $125,000 to $149,999
  'INC_HHS_150k_PLUS' = 'v_CA21_963', # Total - Household after-tax income groups - $150,000 and over Done

  # Income statistics for couple-with-children economic families in private households
  # Median total income of couple-with-children economic families in 2020 ($)
  # Median after-tax income of couple-with-children economic families in 2020 ($)
  # Income statistics for one-parent economic families in private households
  # Median total income of one-parent economic families in 2020 ($)
  # Median after-tax income of one-parent economic families in 2020 ($)




  'INC_AT_LONE_PARENT_MED' = 'v_CA21_978', # Median after-tax income of lone-parent economic families : Income statistics for one-parent economic families in private households: Median after-tax income of one-parent economic families in 2020 ($)
  'INC_AT_CPL_W_CHILD_MED' = 'v_CA21_974', # Median after-tax income of couple economic families with children: in 21 couple-only and couple-with-children: Median after-tax income of couple-with-children economic families in 2020 ($) Done

  #  25% Data

  # Income statistics for couple-with-children economic families in private households
  # Average total income of couple-with-children economic families in 2020 ($)
  # Average after-tax income of couple-with-children economic families in 2020 ($)
  # Income statistics for one-parent economic families in private households
  # Average total income of one-parent economic families in 2020 ($)
  # Average after-tax income of one-parent economic families in 2020 ($)

  'INC_AT_LONE_PARENT_AVG' = 'v_CA21_1000', # Average after-tax income of lone-parent economic families: Average after-tax income of one-parent economic families in 2020 ($)
  'INC_AT_CPL_W_CHILD_AVG' = 'v_CA21_997', # Average after-tax income of couple economic families with children: Average total income of couple-with-children economic families in 2020 ($) Done


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

  #
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

  'HOME_VALUE_AVG'  = 'v_CA21_4312', # Average value of dwelling
  'HOME_VALUE_MED'  = 'v_CA21_4311', # Median value of dwelling
  'HOME_COST_OWN'   = 'v_CA21_4310', # Average monthly shelter costs for owned dwellings Done

  # 25% Data
  # Housing
  # Total - Tenant households in non-farm, non-reserve private dwellings
  # Median monthly shelter costs for rented dwellings ($)
  # Average monthly shelter costs for rented dwellings ($)

  'HOME_RENT_AVG'   = 'v_CA21_4318', # Average monthly shelter costs for rented dwellings
  'HOME_RENT_MED'   = 'v_CA21_4317', # Median monthly shelter costs for rented dwellings Done
  # 25% Data
  # Housing
  # Total - Tenant households in non-farm, non-reserve private dwellings
  # % of tenant households in subsidized housing
  'SUBSIDIZED_HOUS' = 'v_CA21_4314', # % of tenant households in subsidized housing Done
  # 25% Data
  # Housing
  # Total - Private households by housing suitability
  # Suitable
  # Not suitable
  'HOME_SUITABLE_TOT' = 'v_CA21_4260', # Private households by housing suitability - Total
  'HOME_SUITABLE_YES' = 'v_CA21_4261', # Private households by housing suitability - Suitable
  'HOME_SUITABLE_NO'  = 'v_CA21_4262', # Private households by housing suitability - Not suitable Done

  # 25% Data
  # Housing
  # Total - Occupied private dwellings by dwelling condition
  # Only regular maintenance and minor repairs needed
  # Major repairs needed

  'REPAIRS_TOT' = 'v_CA21_4272', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA21_4273', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA21_4274', # Occupied private dwellings by dwelling condition	- Major repairs needed Done

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


  # Education
  # Total - Major field of study - Classification of Instructional Programs (CIP) 2021 for the population aged 15 years and over in private households

  # No postsecondary certificate, diploma or degree

  # Education
  # Visual and performing arts, and communications technologies
  # Humanities
  # Social and behavioural sciences and law # Total
  # Business, management and public administration# Total
  # Physical and life sciences and technologies# Total
  # Mathematics, computer and information sciences# Total
  # Architecture, engineering, and related trades# Total
  # Agriculture, natural resources and conservation# Total
  # Health and related fields# Total
  # Personal, protective and transportation services
  'MAJOR_TOT'        = 'v_CA21_5913', # Major field of study, 15 years and over - Total
  'MAJOR_EDUC'       = 'v_CA21_5919', # Major field of study, 15 years and over - Education
  'MAJOR_ART_COM'    = 'v_CA21_5925', # Major field of study, 15 years and over - Visual and performing arts, and communications technologies
  'MAJOR_HUMANITIES' = 'v_CA21_5934', # Major field of study, 15 years and over - Humanities
  'MAJOR_SOC_SCI'    = 'v_CA21_5961', # Major field of study, 15 years and over - Social and behavioural sciences and law
  'MAJOR_BUS_MGT'    = 'v_CA21_5985', # Major field of study, 15 years and over - Business, management and public administration
  'MAJOR_PHY_SCI'    = 'v_CA21_5997', # Major field of study, 15 years and over - Physical and life sciences and technologies
  'MAJOR_MATH_COMP'  = 'v_CA21_6015', # Major field of study, 15 years and over - Mathematics, computer and information sciences
  'MAJOR_ENGR'       = 'v_CA21_6030', # Major field of study, 15 years and over - Architecture, engineering, and related technologies
  'MAJOR_NAT_RSRC'   = 'v_CA21_6054', # Major field of study, 15 years and over - Agriculture, natural resources and conservation
  'MAJOR_HLTH'       = 'v_CA21_6063', # Major field of study, 15 years and over - Health and related fields
  'MAJOR_SERVICES'   = 'v_CA21_6081', # Major field of study, 15 years and over - Personal, protective and transportation services Done


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


CA21_DATA <- get_census(dataset="CA21",
                        regions=list(PR="59"),
                        vectors=
                          CA21_VECTORS,
                        level='DA')

# add census year
CA21_DATA <- add_column(CA21_DATA, CENSUS_YEAR = 2021, .before = "GeoUID")

test = CA21_DATA %>%
  filter(GeoUID == "59410254")

CA21_DATA %>%
  summarise(
    across(
      .cols = c(INC_BT_FAM_AVG, INC_BT_FAM_MED, INC_AT_FAM_AVG, INC_AT_FAM_MED),
      .fns = ~mean(.x, na.rm = T)
    )
  )



# ca21 employment rate and participation rate--------------------------------------------------------------------

# fix employment rate and participation rate
# 'IN_LAB_FORCE_TOT'  = 'v_CA21_6492', # Population aged 15 years and over by Labour force status - Total
# 'IN_LAB_FORCE_YES'  = 'v_CA21_6495', # Population aged 15 years and over by Labour force status - in labour force
# 'IN_LAB_FORCE_NO'  = 'v_CA21_6504', # Population aged 15 years and over by Labour force status - not in labour force Done
# # 'LABOUR_PART_RT'  = 'v_CA21_5612', # Labour Force - Participation Rate  in yes tot/tot
# # 'LABOUR_EMPL_RT'  = 'v_CA21_5615', # Labour Force - Employment Rate  need to calculate emp cnt/in labor
# 'LABOUR_EMPL_CNT'  = 'v_CA21_6498',
# # 'LABOUR_UNEM_RT'  = 'v_CA21_5618', # Labour Force - Unemployment Rate
# 'LABOUR_UNEM_CNT'  = 'v_CA21_6501',  # Need to calculate the rate cnt/total Done


CA21_DATA = CA21_DATA %>%
  mutate(
    LABOUR_PART_RT = IN_LAB_FORCE_YES/IN_LAB_FORCE_TOT       ,
    LABOUR_EMPL_RT = LABOUR_EMPL_CNT/IN_LAB_FORCE_YES,
    LABOUR_UNEM_RT = LABOUR_UNEM_CNT/IN_LAB_FORCE_YES
  )

CA21_DATA %>% glimpse()
# ----------- fix the TIME LEAVING FOR WORK - add into 5-7, 7-9 and after 9 -----------

# CA_16_GO_TO_WORK_DATA_TMP <- PULL_GO_TO_WORK_DATA[, c('GeoUID',
#                                                       'GO_TO_WORK_TOT'
# )]
#
# CA_16_GO_TO_WORK_DATA_TMP <- add_column(CA_16_GO_TO_WORK_DATA_TMP,
#                                         GO_TO_WORK_5AM_7AM = rowSums(PULL_GO_TO_WORK_DATA[, c('GO_TO_WORK_5AM_6AM', 'GO_TO_WORK_6AM_7AM')])
# )
#
# CA_16_GO_TO_WORK_DATA_TMP <- add_column(CA_16_GO_TO_WORK_DATA_TMP,
#                                         GO_TO_WORK_7AM_9AM = rowSums(PULL_GO_TO_WORK_DATA[, c('GO_TO_WORK_7AM_8AM', 'GO_TO_WORK_8AM_9AM')])
# )
#
# CA_16_GO_TO_WORK_DATA_TMP <- add_column(CA_16_GO_TO_WORK_DATA_TMP,
#                                         GO_TO_WORK_AFTER_9AM = rowSums(PULL_GO_TO_WORK_DATA[, c('GO_TO_WORK_9AM_12PM', 'GO_TO_WORK_12PM_4AM')])
# )


CA21_DATA = CA21_DATA %>%
  mutate(
    GO_TO_WORK_5AM_7AM = rowSums(CA21_DATA[, c('GO_TO_WORK_5AM_6AM', 'GO_TO_WORK_6AM_7AM')])     ,
    GO_TO_WORK_7AM_9AM = rowSums(CA21_DATA[, c('GO_TO_WORK_7AM_8AM', 'GO_TO_WORK_8AM_9AM')]),
    GO_TO_WORK_AFTER_9AM = rowSums(CA21_DATA[, c('GO_TO_WORK_9AM_12PM', 'GO_TO_WORK_12PM_4AM')])
  )



CA21_DATA %>% write_csv("Data/CA21_DATA.csv")
CA21_DATA = read_csv("Data/CA21_DATA.csv")


CA21_DATA %>% glimpse()
# 7848!

# check if DA_ID, GeoID is duplicated
pacman::p_load(
  tidyverse,   # deduplication, grouping, and slicing functions
  janitor,     # function for reviewing duplicates
  stringr)      # for string searches, can be used in "rolling-up" values


CA21_DATA %>%
  tabyl(GeoUID ) %>%
  filter(n>1)

# no duplicated GeoUID: "59010124"

get_vector_fn = function(selected_vector, all_vector, file_name = "CA21_vectors_for_ses"){
  selected_ses_vector =   data.frame(
    name = names(selected_vector),
    vector = selected_vector
  ) %>% left_join(
    all_vector
  )

  selected_ses_vector %>% readr::write_csv(paste0("Data/",file_name,".csv"))

  return(selected_ses_vector)
}

missing_cols_2021_raw <- miss_var_summary(CA21_DATA, show_pct = 1)


# CA21_vectors_ses  =  data.frame(
#   name = names(CA21_VECTORS),
#   vector = CA21_VECTORS
# ) %>% left_join(
#   CA21_vectors_all
# )
#
#
# CA21_vectors_ses %>% readr::wirte_csv("Data/CA21_vectors_for_ses.csv")

# ********************************************************************************
# PULL 2016 DATA
# ********************************************************************************

CA16_VECTORS = c(
  'POP'             = 'v_CA16_401',  # Population, 2016
  'AGE_GRP_TOT'     = 'v_CA16_1',    # Total - Age
  'AGE_GRP_TOT_F'   = 'v_CA16_3',    # Total - Age - Female
  'AGE_GRP_TOT_M'   = 'v_CA16_2',    # Total - Age - Male
  'AGE_GRP_00_04'   = 'v_CA16_7',    # 0 - 4 years
  'AGE_GRP_05_09'   = 'v_CA16_25',   # 5 - 9 years
  'AGE_GRP_10_14'   = 'v_CA16_43',   # 10 - 14 years
  'AGE_GRP_15_19'   = 'v_CA16_64',   # 15 - 19 years
  'AGE_GRP_20_24'   = 'v_CA16_82',   # 20 - 24 years
  'AGE_GRP_25_29'   = 'v_CA16_100',  # 25 - 29 years
  'AGE_GRP_30_34'   = 'v_CA16_118',  # 30 - 34 years
  'AGE_GRP_35_39'   = 'v_CA16_136',  # 35 - 39 years
  'AGE_GRP_40_44'   = 'v_CA16_154',  # 40 - 44 years
  'AGE_GRP_45_49'   = 'v_CA16_172',  # 45 - 49 years
  'AGE_GRP_50_54'   = 'v_CA16_190',  # 50 - 54 years
  'AGE_GRP_55_59'   = 'v_CA16_208',  # 55 - 59 years
  'AGE_GRP_60_64'   = 'v_CA16_226',  # 60 - 64 years
  'AGE_GRP_65_PLUS' = 'v_CA16_244',  # 65 and over
  'DWELL_TOT'       = 'v_CA16_408',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA16_409',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA16_410',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA16_415',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA16_417',  # Dwellings - Movable
  'MARITAL_TOT'     = 'v_CA16_451',  # Marital Status - Total
  'MARITAL_MARR'    = 'v_CA16_457',  # Marital Status - Married
  'MARITAL_NEVER'   = 'v_CA16_466',  # Marital Status - Never Married
  'MARITAL_SEP'     = 'v_CA16_469',  # Marital Status - Separated
  'MARITAL_DIV'     = 'v_CA16_472',  # Marital Status - Divorced
  'MARITAL_WID'     = 'v_CA16_475',  # Marital Status - Widowed
  'FAM_SIZE_TOT'    = 'v_CA16_478',  # Family Size - Total
  'FAM_SIZE_2'      = 'v_CA16_479',  # Family Size - 2 persons
  'FAM_SIZE_3'      = 'v_CA16_480',  # Family Size - 3 persons
  'FAM_SIZE_4'      = 'v_CA16_481',  # Family Size - 4 persons
  'FAM_SIZE_5'      = 'v_CA16_482',  # Family Size - 5+ persons
  'SING_PARENT_TOT' = 'v_CA16_488',  # Lone Parent - Total
  'SING_PARENT_F'   = 'v_CA16_489',  # Lone Parent - Female
  'SING_PARENT_M'   = 'v_CA16_490',  # Lone Parent - Male
  'COUPLES_TOT' = 'v_CA16_491', # Total - Couple census families in private households
  'COUPLES_CHILD_Y' = 'v_CA16_493', # Couple census families in private households - 100% data; Couples with children
  'COUPLES_CHILD_N' = 'v_CA16_492', # Couple census families in private households - 100% data; Couples without children
  'INC_BT_IND_MED'  = 'v_CA16_2207', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG'  = 'v_CA16_4957', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED'  = 'v_CA16_2447', # Total Family Income Before Tax - Median Median total income of economic families in 2015 ($)
  'INC_BT_FAM_AVG'  = 'v_CA16_4994', # Total Family Income Before Tax - Average / Average total income of economic families in 2015 ($)
  'INC_AT_FAM_MED'  = 'v_CA16_2448', # Total Family Income After Tax - Median Median after-tax income of economic families in 2015 ($)
  'INC_AT_FAM_AVG'  = 'v_CA16_4995', # Total Family Income After Tax - Average / Average after-tax income of economic families in 2015 ($)
  'INC_BT_HHS_MED'  = 'v_CA16_2397', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG'  = 'v_CA16_4985', # Total Household Income Before Tax - Average
  'INC_FAM_TOP' = 'v_CA16_2492', # Total - Economic family income - In the top half of the distribution
  'INC_FAM_BOTTOM' = 'v_CA16_2474', # Total - Economic family income - In the bottom half of the distribution
  'INC_FAM_DEC_10' = 'v_CA16_2507', # Total - Economic family income - In the top decile
  'INC_HHS_GRP_TOT' = 'v_CA16_2426', # Total - Household after-tax income groups - Total
  'INC_HHS_100k_125k' = 'v_CA16_2443', # Total - Household after-tax income groups - $100,000 to $124,999
  'INC_HHS_125k_150k' = 'v_CA16_2444', # Total - Household after-tax income groups - $125,000 to $149,999
  'INC_HHS_150k_PLUS' = 'v_CA16_2445', # Total - Household after-tax income groups - $150,000 and over
  'INC_AT_LONE_PARENT_AVG' = 'v_CA16_5004', # Average after-tax income of lone-parent economic families
  'INC_AT_CPL_W_CHILD_AVG' = 'v_CA16_5001', # Average after-tax income of couple economic families with children
  'INC_AT_LONE_PARENT_MED' = 'v_CA16_2460', # Median after-tax income of lone-parent economic families
  'INC_AT_CPL_W_CHILD_MED' = 'v_CA16_2456', # Median after-tax income of couple economic families with children
  'LICO_AT_PREVALENCE' = 'v_CA16_2570', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
  'LIM_AT_PREVALENCE' = 'v_CA16_2540', # Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)
  'ABORIGINAL_TOT'  = 'v_CA16_3852', # Aboriginal Identity - Total
  'ABORIGINAL_YES'  = 'v_CA16_3855', # Aboriginal Identity - Yes
  'ABORIGINAL_NO'   = 'v_CA16_3876', # Aboriginal Identity - No
  'REG_INDIAN_TOT'  = 'v_CA16_3879', # Population by Registered or Treaty Indian status - Total
  'REG_INDIAN_YES'  = 'v_CA16_3882', # Population by Registered or Treaty Indian status - Reigstered or Treaty Indian
  'REG_INDIAN_NO'   = 'v_CA16_3885', # Population by Registered or Treaty Indian status - Not a Reigstered or Treaty Indian
  'MINORITY_TOT'    = 'v_CA16_3954', # Visible Minority - Total
  'MINORITY_YES'    = 'v_CA16_3957', # Visible Minority - Yes
  'MINORITY_NO'     = 'v_CA16_3996', # Visible Minority - No
  'CITIZEN_TOT'     = 'v_CA16_3390', # Citizenship - Total
  'CITIZEN_CAN'     = 'v_CA16_3393', # Citizenship - Canadian
  'CITIZEN_NOT_CAN' = 'v_CA16_3402', # Citizenship - Not Canadian
  'IMMIGRANT_TOT'   = 'v_CA16_3405', # Immigrant - Total
  'IMMIGRANT_YES'   = 'v_CA16_3411', # Immigrant - Yes
  'IMMIGRANT_NO'    = 'v_CA16_3408', # Immigrant - No
  'OCC_TOT'         = 'v_CA16_5654', # Occupation - Total
  'OCC_MGMT'        = 'v_CA16_5663', # Occupation - Management
  'OCC_BUS_FIN_AD'  = 'v_CA16_5666', # Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA16_5669', # Occupation - Natural and Applied Sciences
  'OCC_HLTH'        = 'v_CA16_5672', # Occupation - Health
  'OCC_SOCI_SERV'   = 'v_CA16_5675', # Occupation - Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA16_5678', # Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV'   = 'v_CA16_5681', # Occupation - Sales and Service
  'OCC_TRADES'      = 'v_CA16_5684', # Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC'    = 'v_CA16_5687', # Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL'    = 'v_CA16_5690', # Occupation - Manufacturing and Utilities
  'EDUC_TOT'        = 'v_CA16_5096', # Education for population aged 25 - 64 - Total
  'EDUC_NONE'       = 'v_CA16_5099', # Education for population aged 25 - 64 - No certificate, diploma or degree
  'EDUC_HIGHSCH'    = 'v_CA16_5102', # Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate
  'EDUC_POSTSEC'    = 'v_CA16_5105', # Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree
  'EDUC_COLLEGE'    = 'v_CA16_5117', # Education for population aged 25 - 64 - College, CEGEP or other non-university certificate or diploma
  'EDUC_BACHELOR'   = 'v_CA16_5126', # Education for population aged 25 - 64 - Bachelor's degree
  'EDUC_TRADES'     = 'v_CA16_5108', # Education for population aged 25 - 64 - Apprenticeship or trades certificate or diploma
  'EDUC_CRT_ABV_BACH' = 'v_CA16_5129', # Education for population aged 25 - 64 - University certificate or diploma above bachelor level
  'EDUC_MEDICAL'   = 'v_CA16_5132', # Education for population aged 25 - 64 - Degree in medicine, dentistry, veterinary medicine or optometry
  'EDUC_MASTERS'   = 'v_CA16_5135', # Education for population aged 25 - 64 - Master's degree
  'EDUC_PHD'   = 'v_CA16_5138', # Education for population aged 25 - 64 - Earned doctorate
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
  'HOME_VALUE_AVG'  = 'v_CA16_4896', # Average value of dwelling
  'HOME_VALUE_MED'  = 'v_CA16_4895', # Median value of dwelling
  'HOME_COST_OWN'   = 'v_CA16_4894', # Average monthly shelter costs for owned dwellings
  'HOME_RENT_AVG'  = 'v_CA16_4901', # Average monthly shelter costs for rented dwellings
  'HOME_RENT_MED'  = 'v_CA16_4900', # Median monthly shelter costs for rented dwellings
  'SUBSIDIZED_HOUS' = 'v_CA16_4898', # % of tenant households in subsidized housing
  'HOME_SUITABLE_TOT' = 'v_CA16_4859', # Private households by housing suitability - Total
  'HOME_SUITABLE_YES' = 'v_CA16_4860', # Private households by housing suitability - Suitable
  'HOME_SUITABLE_NO' = 'v_CA16_4861', # Private households by housing suitability - Not suitable
  'REPAIRS_TOT' = 'v_CA16_4870', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA16_4871', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA16_4872', # Occupied private dwellings by dwelling condition	- Major repairs needed
  'LANG_KNOW_TOT' = 'v_CA16_512', # Knowledge of official languages - total
  'LANG_KNOW_EN' = 'v_CA16_515', # Knowledge of official languages - english only
  'LANG_KNOW_FR' = 'v_CA16_518', # Knowledge of official languages - french only
  'LANG_KNOW_BOTH' = 'v_CA16_521', # Knowledge of official languages - english and french
  'LANG_KNOW_NONE' = 'v_CA16_524', # Knowledge of official languages - neither english nor french
  'LANG_SPOKE_TOT' = 'v_CA16_527', # First official language spoken - total
  'LANG_SPOKE_EN' = 'v_CA16_530', # First official language spoken - english only
  'LANG_SPOKE_FR' = 'v_CA16_533', # First official language spoken - french only
  'LANG_SPOKE_BOTH'  = 'v_CA16_536', # First official language spoken - english and french
  'LANG_SPOKE_NONE'  = 'v_CA16_539', # First official language spoken - neither english nor french
  'MAJOR_TOT'        = 'v_CA16_5141', # Major field of study, 15 years and over - Total
  'MAJOR_EDUC'       = 'v_CA16_5147', # Major field of study, 15 years and over - Education
  'MAJOR_ART_COM'    = 'v_CA16_5153', # Major field of study, 15 years and over - Visual and performing arts, and communications technologies
  'MAJOR_HUMANITIES' = 'v_CA16_5162', # Major field of study, 15 years and over - Humanities
  'MAJOR_SOC_SCI'    = 'v_CA16_5189', # Major field of study, 15 years and over - Social and behavioural sciences and law
  'MAJOR_BUS_MGT'    = 'v_CA16_5213', # Major field of study, 15 years and over - Business, management and public administration
  'MAJOR_PHY_SCI'    = 'v_CA16_5225', # Major field of study, 15 years and over - Physical and life sciences and technologies
  'MAJOR_MATH_COMP'  = 'v_CA16_5243', # Major field of study, 15 years and over - Mathematics, computer and information sciences
  'MAJOR_ENGR'       = 'v_CA16_5258', # Major field of study, 15 years and over - Architecture, engineering, and related technologies
  'MAJOR_NAT_RSRC'   = 'v_CA16_5282', # Major field of study, 15 years and over - Agriculture, natural resources and conservation
  'MAJOR_HLTH'       = 'v_CA16_5291', # Major field of study, 15 years and over - Health and related fields
  'MAJOR_SERVICES'   = 'v_CA16_5303' # Major field of study, 15 years and over - Personal, protective and transportation services

)

CA16_DATA <- get_census(dataset="CA16",
                           regions=list(PR="59"),
                           vectors=
                             CA16_VECTORS,
                           level='DA')



# ----------- fix the TIME LEAVING FOR WORK - add into 5-7, 7-9 and after 9 -----------

CA_16_GO_TO_WORK_VEC = c(
  'GO_TO_WORK_TOT' = 'v_CA16_5831', # Total - Time leaving for work for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address
  'GO_TO_WORK_5AM_6AM' = 'v_CA16_5834', # Between 5 a.m. and 5:59 a.m.
  'GO_TO_WORK_6AM_7AM' = 'v_CA16_5837', # Between 6 a.m. and 6:59 a.m.
  'GO_TO_WORK_7AM_8AM' = 'v_CA16_5840', # Between 7 a.m. and 7:59 a.m.
  'GO_TO_WORK_8AM_9AM' = 'v_CA16_5843', # Between 8 a.m. and 8:59 a.m.
  'GO_TO_WORK_9AM_12PM' = 'v_CA16_5846', # Between 9 a.m. and 11:59 a.m.
  'GO_TO_WORK_12PM_4AM' = 'v_CA16_5849' # Between 12 p.m. and 4:59 a.m.
  )

CA16_VECTORS = c(CA16_VECTORS, CA_16_GO_TO_WORK_VEC)


CA16_vectors_ses = get_vector_fn(CA16_VECTORS, CA16_vectors_all, file_name = "CA16_vectors_for_ses")


PULL_GO_TO_WORK_DATA <- get_census(dataset="CA16",
                                regions=list(PR="59"),
                                vectors=
                                  CA_16_GO_TO_WORK_VEC,
                                level='DA')

CA_16_GO_TO_WORK_DATA_TMP <- PULL_GO_TO_WORK_DATA[, c('GeoUID',
                                                      'GO_TO_WORK_TOT'
                                                      )]

CA_16_GO_TO_WORK_DATA_TMP <- add_column(CA_16_GO_TO_WORK_DATA_TMP,
                                        GO_TO_WORK_5AM_7AM = rowSums(PULL_GO_TO_WORK_DATA[, c('GO_TO_WORK_5AM_6AM', 'GO_TO_WORK_6AM_7AM')])
)

CA_16_GO_TO_WORK_DATA_TMP <- add_column(CA_16_GO_TO_WORK_DATA_TMP,
                                        GO_TO_WORK_7AM_9AM = rowSums(PULL_GO_TO_WORK_DATA[, c('GO_TO_WORK_7AM_8AM', 'GO_TO_WORK_8AM_9AM')])
)

CA_16_GO_TO_WORK_DATA_TMP <- add_column(CA_16_GO_TO_WORK_DATA_TMP,
                                        GO_TO_WORK_AFTER_9AM = rowSums(PULL_GO_TO_WORK_DATA[, c('GO_TO_WORK_9AM_12PM', 'GO_TO_WORK_12PM_4AM')])
)

# merge with CA16_DATA
CA16_DATA <- merge(CA16_DATA, CA_16_GO_TO_WORK_DATA_TMP, by="GeoUID")

# add census year
CA16_DATA <- add_column(CA16_DATA, CENSUS_YEAR = 2016, .before = "GeoUID")

# ********************************************************************************
# PULL 2011 DATA
# ********************************************************************************

CA11_VECTORS = c(
  'POP'             = 'v_CA11F_1',   # Population, 2011
  'AGE_GRP_TOT'     = 'v_CA11F_5',   # Total - Age
  'AGE_GRP_TOT_F'   = 'v_CA11F_7',   # Total - Age - Female
  'AGE_GRP_TOT_M'   = 'v_CA11F_6',   # Total - Age - Male
  'AGE_GRP_00_04'   = 'v_CA11F_8',   # 0 - 4 years
  'AGE_GRP_05_09'   = 'v_CA11F_11',  # 5 - 9 years
  'AGE_GRP_10_14'   = 'v_CA11F_14',  # 10 - 14 years
  'AGE_GRP_15_19'   = 'v_CA11F_17',  # 15 - 19 years
  'AGE_GRP_20_24'   = 'v_CA11F_35',  # 20 - 24 years
  'AGE_GRP_25_29'   = 'v_CA11F_38',  # 25 - 29 years
  'AGE_GRP_30_34'   = 'v_CA11F_41',  # 30 - 34 years
  'AGE_GRP_35_39'   = 'v_CA11F_44',  # 35 - 39 years
  'AGE_GRP_40_44'   = 'v_CA11F_47',  # 40 - 44 years
  'AGE_GRP_45_49'   = 'v_CA11F_50',  # 45 - 49 years
  'AGE_GRP_50_54'   = 'v_CA11F_53',  # 50 - 54 years
  'AGE_GRP_55_59'   = 'v_CA11F_56',  # 55 - 59 years
  'AGE_GRP_60_64'   = 'v_CA11F_59',  # 60 - 64 years
  'AGE_GRP_65_PLUS' = 'v_CA11F_62',  # 65 - 69 years // USE THIS FIELD TO ADD OTHER AGE GROUPS ABOVE 65
  'DWELL_TOT'       = 'v_CA11F_199', # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA11F_200', # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA11F_201', # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA11F_207', # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA11F_202', # Dwellings - Movable
  'MARITAL_TOT'     = 'v_CA11F_83',  # Marital Status - Total
  'MARITAL_MARR'    = 'v_CA11F_89',  # Marital Status - Married
  'MARITAL_NEVER'   = 'v_CA11F_98',  # Marital Status - Never Married
  'MARITAL_SEP'     = 'v_CA11F_101', # Marital Status - Separated
  'MARITAL_DIV'     = 'v_CA16_472',  # Marital Status - Divorced
  'MARITAL_WID'     = 'v_CA11F_104', # Marital Status - Widowed
  'FAM_SIZE_TOT'    = 'v_CA11F_110', # Family Size - Total
  'FAM_SIZE_2'      = 'v_CA11F_111', # Family Size - 2 persons
  'FAM_SIZE_3'      = 'v_CA11F_112', # Family Size - 3 persons
  'FAM_SIZE_4'      = 'v_CA11F_113', # Family Size - 4 persons
  'FAM_SIZE_5'      = 'v_CA11F_114', # Family Size - 5+ persons
  'SING_PARENT_TOT' = 'v_CA11F_129', # Lone Parent - Total
  'SING_PARENT_F'   = 'v_CA11F_130', # Lone Parent - Female
  'SING_PARENT_M'   = 'v_CA11F_134', # Lone Parent - Male
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
  'ABORIGINAL_TOT'  = 'v_CA11N_1351', # Aboriginal Identity - Total
  'ABORIGINAL_YES'  = 'v_CA11N_1354', # Aboriginal Identity - Yes
  'ABORIGINAL_NO'   = 'v_CA11N_1372', # Aboriginal Identity - No
  'REG_INDIAN_TOT'  = 'v_CA11N_1375', # Population by Registered or Treaty Indian status - Total
  'REG_INDIAN_YES'  = 'v_CA11N_1378', # Population by Registered or Treaty Indian status - Reigstered or Treaty Indian
  'REG_INDIAN_NO'   = 'v_CA11N_1381', # Population by Registered or Treaty Indian status - Not a Reigstered or Treaty Indian
  'MINORITY_TOT'    = 'v_CA11N_457', # Visible Minority - Total
  'MINORITY_YES'    = 'v_CA11N_460', # Visible Minority - Yes
  'MINORITY_NO'     = 'v_CA11N_499', # Visible Minority - No
  'CITIZEN_TOT'     = 'v_CA11N_1', # Citizenship - Total
  'CITIZEN_CAN'     = 'v_CA11N_4', # Citizenship - Canadian
  'CITIZEN_NOT_CAN' = 'v_CA11N_13',# Citizenship - Not Canadian
  'IMMIGRANT_TOT'   = 'v_CA11N_16', # Immigrant - Total
  'IMMIGRANT_YES'   = 'v_CA11N_22', # Immigrant - Yes
  'IMMIGRANT_NO'    = 'v_CA11N_19', # Immigrant - No
  'OCC_TOT'         = 'v_CA11N_2026', # Occupation - Total
  'OCC_MGMT'        = 'v_CA11N_2035', # Occupation - Management
  'OCC_BUS_FIN_AD'  = 'v_CA11N_2038', # Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA11N_2041', # Occupation - Natural and Applied Sciences
  'OCC_HLTH'        = 'v_CA11N_2044', # Occupation - Health
  'OCC_SOCI_SERV'   = 'v_CA11N_2047', # Occupation - Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA11N_2050', # Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV'   = 'v_CA11N_2053', # Occupation - Sales and Service
  'OCC_TRADES'      = 'v_CA11N_2056', # Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC'    = 'v_CA11N_2059', # Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL'    = 'v_CA11N_2062', # Occupation - Manufacturing and Utilities
  'EDUC_TOT'        = 'v_CA11N_1801', # Education for population aged 25 - 64 - Total
  'EDUC_NONE'       = 'v_CA11N_1804', # Education for population aged 25 - 64 - No certificate, diploma or degree
  'EDUC_HIGHSCH'    = 'v_CA11N_1807', # Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate
  'EDUC_POSTSEC'    = 'v_CA11N_1810', # Education for population aged 25 - 64 - Postsecondary certificate, diploma or degree
  'EDUC_COLLEGE'    = 'v_CA11N_1816', # Education for population aged 25 - 64 - College, CEGEP or other non-university certificate or diploma
  'EDUC_BACHELOR'   = 'v_CA11N_1825', # Education for population aged 25 - 64 - Bachelor's degree
  'EDUC_TRADES'     = 'v_CA11N_1813', # Education for population aged 25 - 64 - Apprenticeship or trades certificate or diploma
  'EDUC_CRT_ABV_BACH' = 'v_CA11N_1828', # Education for population aged 25 - 64 - University certificate or diploma above bachelor level
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
  'HOME_VALUE_AVG'  = 'v_CA11N_2287', # Average value of dwelling
  'HOME_VALUE_MED'  = 'v_CA11N_2286', # Median value of dwelling
  'HOME_COST_OWN'   = 'v_CA11N_2285', # Average monthly shelter costs for owned dwellings
  'HOME_RENT_AVG'  = 'v_CA11N_2292', # Average monthly shelter costs for rented dwellings
  'HOME_RENT_MED'  = 'v_CA11N_2291', # Median monthly shelter costs for rented dwellings
  'SUBSIDIZED_HOUS' = 'v_CA11N_2289', # % of tenant households in subsidized housing
  'HOME_SUITABLE_TOT' = 'v_CA11N_2274', # Private households by housing suitability - Total
  'HOME_SUITABLE_YES' = 'v_CA11N_2275', # Private households by housing suitability - Suitable
  'HOME_SUITABLE_NO' = 'v_CA11N_2276', # Private households by housing suitability - Not suitable
  'REPAIRS_TOT' = 'v_CA11N_2230', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_MINOR' = 'v_CA11N_2231', # Occupied private dwellings by dwelling condition	- Only regular maintenance or minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA11N_2232', # Occupied private dwellings by dwelling condition	- Major repairs needed
  'LANG_KNOW_TOT' = 'v_CA11F_551', # Knowledge of official languages - total
  'LANG_KNOW_EN' = 'v_CA11F_554', # Knowledge of official languages - english only
  'LANG_KNOW_FR' = 'v_CA11F_557', # Knowledge of official languages - french only
  'LANG_KNOW_BOTH' = 'v_CA11F_560', # Knowledge of official languages - english and french
  'LANG_KNOW_NONE' = 'v_CA11F_563', # Knowledge of official languages - neither english nor french
  'LANG_SPOKE_TOT' = 'v_CA11F_566', # First official language spoken - total
  'LANG_SPOKE_EN' = 'v_CA11F_569', # First official language spoken - english only
  'LANG_SPOKE_FR' = 'v_CA11F_572', # First official language spoken - french only
  'LANG_SPOKE_BOTH' = 'v_CA11F_575', # First official language spoken - english and french
  'LANG_SPOKE_NONE' = 'v_CA11F_578', # First official language spoken - neither english nor french
  'MAJOR_TOT'        = 'v_CA11N_1831', # Major field of study, 15 years and over - Total
  'MAJOR_EDUC'       = 'v_CA11N_1837', # Major field of study, 15 years and over - Education
  'MAJOR_ART_COM'    = 'v_CA11N_1840', # Major field of study, 15 years and over - Visual and performing arts, and communications technologies
  'MAJOR_HUMANITIES' = 'v_CA11N_1843', # Major field of study, 15 years and over - Humanities
  'MAJOR_SOC_SCI'    = 'v_CA11N_1846', # Major field of study, 15 years and over - Social and behavioural sciences and law
  'MAJOR_BUS_MGT'    = 'v_CA11N_1849', # Major field of study, 15 years and over - Business, management and public administration
  'MAJOR_PHY_SCI'    = 'v_CA11N_1852', # Major field of study, 15 years and over - Physical and life sciences and technologies
  'MAJOR_MATH_COMP'  = 'v_CA11N_1855', # Major field of study, 15 years and over - Mathematics, computer and information sciences
  'MAJOR_ENGR'       = 'v_CA11N_1858', # Major field of study, 15 years and over - Architecture, engineering, and related technologies
  'MAJOR_NAT_RSRC'   = 'v_CA11N_1861', # Major field of study, 15 years and over - Agriculture, natural resources and conservation
  'MAJOR_HLTH'       = 'v_CA11N_1864', # Major field of study, 15 years and over - Health and related fields
  'MAJOR_SERVICES'   = 'v_CA11N_1867', # Major field of study, 15 years and over - Personal, protective and transportation services
  'GO_TO_WORK_TOT' = 'v_CA11N_2218', # Total employed population aged 15 years and over by time leaving for work
  'GO_TO_WORK_5AM_7AM' = 'v_CA11N_2221', # Total employed population aged 15 years and over by time leaving for work - Between 5 and 6:59 a.m.
  'GO_TO_WORK_7AM_9AM' = 'v_CA11N_2224', # Total employed population aged 15 years and over by time leaving for work - Between 7 and 9:00 a.m.
  'GO_TO_WORK_AFTER_9AM' = 'v_CA11N_2227' # Total employed population aged 15 years and over by time leaving for work - Anytime after 9:00 a.m.
)

CA11_DATA <- get_census(dataset="CA11",
                            regions=list(PR="59"),
                            vectors=
                            CA11_VECTORS,
                            level='DA')

# ----------- fix the COUPLE CENSUS FAMILIES CAT - add married/common law -----------

CA_11_COUPLES_VEC = c(
  'COUPLES_TOT' = 'v_CA11F_116', # Total - Couple census families in private households
  'MARRIED_CHILD_Y' = 'v_CA11F_119', # Married couples - With children at home
  'MARRIED_CHILD_N' = 'v_CA11F_118', # Married couples - Without children at home
  'COMMON_LAW_CHILD_Y' = 'v_CA11F_125', # Common-law couples - With children at home
  'COMMON_LAW_CHILD_N' = 'v_CA11F_124' # Common-law couples - Without children at home
)




PULL_COUPLES_DATA <- get_census(dataset="CA11",
                                regions=list(PR="59"),
                                vectors=
                                  CA_11_COUPLES_VEC,
                                level='DA')

CA_11_COUPLES_DATA_TMP <- PULL_COUPLES_DATA[, c('GeoUID', 'COUPLES_TOT')]

CA_11_COUPLES_DATA_TMP <- add_column(CA_11_COUPLES_DATA_TMP,
                                     COUPLES_CHILD_Y = rowSums(PULL_COUPLES_DATA[, c('MARRIED_CHILD_Y', 'COMMON_LAW_CHILD_Y')])
)

CA_11_COUPLES_DATA_TMP <- add_column(CA_11_COUPLES_DATA_TMP,
                                     COUPLES_CHILD_N = rowSums(PULL_COUPLES_DATA[, c('MARRIED_CHILD_N', 'COMMON_LAW_CHILD_N')])
)

# merge with CA11_DATA
CA11_DATA <- merge(CA11_DATA, CA_11_COUPLES_DATA_TMP, by="GeoUID")

# ---------------- fix the 65+ age category for DA Level ----------------

CA_11_AGR_GRP_65_PLUS_VECTORS = c(
  'AGE_GRP_65_69'   = 'v_CA11F_62', # 65 - 69 years
  'AGE_GRP_70_74'   = 'v_CA11F_65', # 70 - 74 years
  'AGE_GRP_75_79'   = 'v_CA11F_68', # 75 - 79 years
  'AGE_GRP_80_84'   = 'v_CA11F_71', # 80 - 84 years
  'AGE_GRP_85_PLUS' = 'v_CA11F_74'  # 85 and over
)

CA11_AGR_GRP_65_PLUS_TEMP <- get_census(dataset="CA11",
                                   regions=list(PR="59"),
                                   vectors=
                                     CA_11_AGR_GRP_65_PLUS_VECTORS,
                                   level='DA')

AGR_GRP_65_PLUS_VARS = keys(hash(CA_11_AGR_GRP_65_PLUS_VECTORS))

CA11_AGR_GRP_65_PLUS_TEMP <- tibble(GeoUID = CA11_AGR_GRP_65_PLUS_TEMP$GeoUID,
                               AGE_GRP_65_PLUS = rowSums(CA11_AGR_GRP_65_PLUS_TEMP[,AGR_GRP_65_PLUS_VARS]))

# merge by DA ID
CA11_DATA <- merge(CA11_DATA, CA11_AGR_GRP_65_PLUS_TEMP[, c("GeoUID", "AGE_GRP_65_PLUS")], by="GeoUID")

# replace old column with new
CA11_DATA$AGE_GRP_65_PLUS.x <- CA11_DATA$AGE_GRP_65_PLUS.y

# drop column
CA11_DATA$AGE_GRP_65_PLUS.y <- NULL
CA11_DATA$`NHS Non Return Rate` <- NULL

# rename column
colnames(CA11_DATA)[colnames(CA11_DATA)=="AGE_GRP_65_PLUS.x"] <- "AGE_GRP_65_PLUS"

# -------------------------- ADD EMPTY COLUMNS -------------------------

CA11_DATA <- add_column(CA11_DATA,
                        EDUC_MEDICAL = NA,
                        EDUC_MASTERS = NA,
                        EDUC_PHD = NA,
                        INC_HHS_125k_150k = NA,
                        INC_HHS_150k_PLUS = NA,
                        LICO_AT_PREVALENCE = NA)

CA11_DATA <- add_column(CA11_DATA, CENSUS_YEAR = 2011, .before = "GeoUID")


CA11_VECTORS = c(CA11_VECTORS,CA_11_COUPLES_VEC, CA_11_AGR_GRP_65_PLUS_VECTORS )

CA11_vectors_ses = get_vector_fn(CA11_VECTORS, CA11_vectors_all, file_name = "CA11_vectors_for_ses")


# ********************************************************************************
# PULL 2006 DATA
# ********************************************************************************

CA06_VECTORS = c(
  'POP'             = 'v_CA06_1',    # Population, 2006
  'AGE_GRP_TOT'     = 'v_CA06_2',    # Total - Age
  'AGE_GRP_TOT_F'   = 'v_CA06_22',   # Total - Age - Female
  'AGE_GRP_TOT_M'   = 'v_CA06_3',    # Total - Age - Male
  'DWELL_TOT'       = 'v_CA06_119',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA06_120',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA06_124',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA06_125',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA06_127',  # Dwellings - Movable
  'MARITAL_TOT'     = 'v_CA06_41',   # Marital Status - Total
  'MARITAL_MARR'    = 'v_CA06_43',   # Marital Status - Married
  'MARITAL_NEVER'   = 'v_CA06_42',   # Marital Status - Never Married
  'MARITAL_SEP'     = 'v_CA06_44',   # Marital Status - Separated
  'MARITAL_DIV'     = 'v_CA06_45',   # Marital Status - Divorced
  'MARITAL_WID'     = 'v_CA06_46',   # Marital Status - Widowed
  'FAM_SIZE_TOT'    = 'v_CA06_50',   # Family Size - Total
  'FAM_SIZE_2'      = 'v_CA06_51',   # Family Size - 2 persons
  'FAM_SIZE_3'      = 'v_CA06_52',   # Family Size - 3 persons
  'FAM_SIZE_4'      = 'v_CA06_53',   # Family Size - 4 persons
  'FAM_SIZE_5'      = 'v_CA06_54',   # Family Size - 5+ persons
  'SING_PARENT_TOT' = 'v_CA06_69',   # Lone Parent - Total
  'SING_PARENT_F'   = 'v_CA06_70',   # Lone Parent - Female
  'SING_PARENT_M'   = 'v_CA06_74',   # Lone Parent - Male
  'INC_BT_IND_MED'  = 'v_CA06_1583', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG'  = 'v_CA06_1584', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED'  = 'v_CA06_1741', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG'  = 'v_CA06_1742', # Total Family Income Before Tax - Average
  'INC_AT_FAM_MED'  = 'v_CA06_1785', # Total Family Income After Tax - Median
  'INC_AT_FAM_AVG'  = 'v_CA06_1786', # Total Family Income After Tax - Average
  'INC_BT_HHS_MED'  = 'v_CA06_2000', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG'  = 'v_CA06_2001', # Total Household Income Before Tax - Average
  'LICO_AT_PREVALENCE' = 'v_CA06_1981', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
  'ABORIGINAL_TOT'  = 'v_CA06_564', # Aboriginal Identity - Total
  'ABORIGINAL_YES'  = 'v_CA06_565', # Aboriginal Identity - Yes
  'ABORIGINAL_NO'   = 'v_CA06_571', # Aboriginal Identity - No
  'REG_INDIAN_TOT'  = 'v_CA06_572', # Population by Registered or Treaty Indian status - Total
  'REG_INDIAN_YES'  = 'v_CA06_573', # Population by Registered or Treaty Indian status - Reigstered or Treaty Indian
  'REG_INDIAN_NO'   = 'v_CA06_574', # Population by Registered or Treaty Indian status - Not a Reigstered or Treaty Indian
  'MINORITY_TOT'    = 'v_CA06_1302', # Visible Minority - Total
  'MINORITY_YES'    = 'v_CA06_1303', # Visible Minority - Yes
  'MINORITY_NO'     = 'v_CA06_1316', # Visible Minority - No
  'CITIZEN_TOT'     = 'v_CA06_469',  # Citizenship - Total
  'CITIZEN_CAN'     = 'v_CA06_470',  # Citizenship - Canadian
  'CITIZEN_NOT_CAN' = 'v_CA06_473',  # Citizenship - Not Canadian
  'IMMIGRANT_TOT'   = 'v_CA06_474', # Immigrant - Total
  'IMMIGRANT_YES'   = 'v_CA06_478', # Immigrant - Yes
  'IMMIGRANT_NO'    = 'v_CA06_475', # Immigrant - No
  'OCC_TOT'         = 'v_CA06_827', # Occupation - Total
  'OCC_MGMT'        = 'v_CA06_830', # Occupation - Management
  'OCC_BUS_FIN_AD'  = 'v_CA06_835', # Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA06_842', # Occupation - Natural and Applied Sciences
  'OCC_HLTH'        = 'v_CA06_845', # Occupation - Health
  'OCC_SOCI_SERV'   = 'v_CA06_850', # Occupation - Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA06_854', # Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV'   = 'v_CA06_857', # Occupation - Sales and Service
  'OCC_TRADES'      = 'v_CA06_868', # Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC'    = 'v_CA06_878', # Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL'    = 'v_CA06_882', # Occupation - Manufacturing and Utilities
  'EDUC_TOT'        = 'v_CA06_1248', # Education for population aged 25 - 64 - Total
  'EDUC_NONE'       = 'v_CA06_1249', # Education for population aged 25 - 64 - No certificate, diploma or degree
  'EDUC_HIGHSCH'    = 'v_CA06_1251', # Education for population aged 25 - 64 - Secondary (high) school diploma or equivalency certificate
  'EDUC_COLLEGE'    = 'v_CA06_1253', # Education for population aged 25 - 64 - College, CEGEP or other non-university certificate or diploma
  'EDUC_BACHELOR'   = 'v_CA06_1257', # Education for population aged 25 - 64 - Bachelor's degree
  'EDUC_TRADES'     = 'v_CA06_1252', # Education for population aged 25 - 64 - Apprenticeship or trades certificate or diploma
  'EDUC_CRT_ABV_BACH' = 'v_CA06_1258', # Education for population aged 25 - 64 - University certificate or diploma above bachelor level
  'EDUC_MEDICAL' = 'v_CA06_1259', # Education for population aged 25 - 64 - Degree in medicine, dentistry, veterinary medicine or optometry
  'EDUC_MASTERS'   = 'v_CA06_1260', # Education for population aged 25 - 64 - Master's degree
  'EDUC_PHD'   = 'v_CA06_1261', # Education for population aged 25 - 64 - Earned doctorate
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
  'HOME_VALUE_AVG'  = 'v_CA06_2054', # Average value of dwelling
  'HOME_COST_OWN'   = 'v_CA06_2055', # Average owner major payments
  'HOME_RENT_AVG'  = 'v_CA06_2050', # Average gross rent
  'LANG_KNOW_TOT' = 'v_CA06_243', # Knowledge of official languages - total
  'LANG_KNOW_EN' = 'v_CA06_244', # Knowledge of official languages - english only
  'LANG_KNOW_FR' = 'v_CA06_245', # Knowledge of official languages - french only
  'LANG_KNOW_BOTH' = 'v_CA06_246', # Knowledge of official languages - english and french
  'LANG_KNOW_NONE' = 'v_CA06_247', # Knowledge of official languages - neither english nor french
  'LANG_SPOKE_TOT' = 'v_CA06_248', # First official language spoken - total
  'LANG_SPOKE_EN' = 'v_CA06_249', # First official language spoken - english only
  'LANG_SPOKE_FR' = 'v_CA06_250', # First official language spoken - french only
  'LANG_SPOKE_BOTH' = 'v_CA06_251', # First official language spoken - english and french
  'LANG_SPOKE_NONE' = 'v_CA06_252' # First official language spoken - neither english nor french
)

CA06_DATA <- get_census(dataset="CA06",
                        regions=list(PR="59"),
                        vectors=
                        CA06_VECTORS,
                        level='DA')

# ----------- fix the COUPLE CENSUS FAMILIES CAT - add married/common law -----------

CA_06_COUPLES_VEC = c(
  'COUPLES_TOT' = 'v_CA06_56', # Total - Couple census families in private households
  'MARRIED_CHILD_Y' = 'v_CA06_59', # Married couples - With children at home
  'MARRIED_CHILD_N' = 'v_CA06_58', # Married couples - Without children at home
  'COMMON_LAW_CHILD_Y' = 'v_CA06_65', # Common-law couples - With children at home
  'COMMON_LAW_CHILD_N' = 'v_CA06_64' # Common-law couples - Without children at home
)

CA06_VECTORS = c(CA06_VECTORS,CA_06_COUPLES_VEC )

PULL_COUPLES_DATA <- get_census(dataset="CA06",
                                        regions=list(PR="59"),
                                        vectors=
                                  CA_06_COUPLES_VEC,
                                        level='DA')

CA_06_COUPLES_DATA_TMP <- PULL_COUPLES_DATA[, c('GeoUID', 'COUPLES_TOT')]

CA_06_COUPLES_DATA_TMP <- add_column(CA_06_COUPLES_DATA_TMP,
                                             COUPLES_CHILD_Y = rowSums(PULL_COUPLES_DATA[, c('MARRIED_CHILD_Y', 'COMMON_LAW_CHILD_Y')])
)

CA_06_COUPLES_DATA_TMP <- add_column(CA_06_COUPLES_DATA_TMP,
                                     COUPLES_CHILD_N = rowSums(PULL_COUPLES_DATA[, c('MARRIED_CHILD_N', 'COMMON_LAW_CHILD_N')])
)

# merge with CA06_DATA
CA06_DATA <- merge(CA06_DATA, CA_06_COUPLES_DATA_TMP, by="GeoUID")

# ----------- fix the LONE PARENT INC category - avg M and F -----------

CA_06_LONE_PARENT_INC_VEC = c(
  'INC_AT_LONE_PARENT_AVG_M' = 'v_CA06_1820', # Average after-tax income of lone-parent economic families - Male
  'INC_AT_LONE_PARENT_AVG_F' = 'v_CA06_1827', # Average after-tax income of lone-parent economic families	 - Female
  'INC_AT_LONE_PARENT_MED_M' = 'v_CA06_1819', # Median after-tax income of lone-parent economic families - Male
  'INC_AT_LONE_PARENT_MED_F' = 'v_CA06_1826' # Median after-tax income of lone-parent economic families	 - Female
  )

CA06_VECTORS = c(CA06_VECTORS,CA_06_LONE_PARENT_INC_VEC )
PULL_LONE_PARENT_INC_DATA <- get_census(dataset="CA06",
                                regions=list(PR="59"),
                                vectors=
                                  CA_06_LONE_PARENT_INC_VEC,
                                level='DA')

CA_06_LONE_PARENT_INC_DATA_TMP <- PULL_LONE_PARENT_INC_DATA[, c('GeoUID', 'INC_AT_LONE_PARENT_AVG_M', 'INC_AT_LONE_PARENT_AVG_F',
                                                                'INC_AT_LONE_PARENT_MED_M', 'INC_AT_LONE_PARENT_MED_F')]

CA_06_LONE_PARENT_INC_DATA_TMP <- add_column(CA_06_LONE_PARENT_INC_DATA_TMP,
                                             INC_AT_LONE_PARENT_AVG = rowSums(PULL_LONE_PARENT_INC_DATA[, c('INC_AT_LONE_PARENT_AVG_M', 'INC_AT_LONE_PARENT_AVG_F')] / 2)
)

CA_06_LONE_PARENT_INC_DATA_TMP <- add_column(CA_06_LONE_PARENT_INC_DATA_TMP,
                                             INC_AT_LONE_PARENT_MED = rowSums(PULL_LONE_PARENT_INC_DATA[, c('INC_AT_LONE_PARENT_MED_M', 'INC_AT_LONE_PARENT_MED_F')] / 2)
)

CA_06_LONE_PARENT_INC_DATA <- CA_06_LONE_PARENT_INC_DATA_TMP %>% select(GeoUID, INC_AT_LONE_PARENT_AVG, INC_AT_LONE_PARENT_MED)

# merge with CA06_DATA
CA06_DATA <- merge(CA06_DATA, CA_06_LONE_PARENT_INC_DATA, by="GeoUID")

# ----------- fix the REPAIRS category - combine regular and minor -----------

CA_06_REPAIRS_VECTORS = c(
  'REPAIRS_TOT' = 'v_CA06_105', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_ONLY_REGULAR' = 'v_CA06_106', # Occupied private dwellings by dwelling condition	- Only regular maintenance
  'REPAIRS_ONLY_MINOR' = 'v_CA06_107', # Occupied private dwellings by dwelling condition	- Only minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA06_108' # Occupied private dwellings by dwelling condition	- Major repairs needed
)


CA06_VECTORS = c(CA06_VECTORS, CA_06_REPAIRS_VECTORS)

PULL_REPAIRS_DATA <- get_census(dataset="CA06",
                             regions=list(PR="59"),
                             vectors=
                               CA_06_REPAIRS_VECTORS,
                             level='DA')

CA_06_REPAIRS_DATA_TMP <- PULL_REPAIRS_DATA[, c('GeoUID', 'REPAIRS_TOT', 'REPAIRS_ONLY_REGULAR', 'REPAIRS_ONLY_MINOR', 'REPAIRS_MAJOR')]

CA_06_REPAIRS_DATA_TMP <- add_column(CA_06_REPAIRS_DATA_TMP,
                                      REPAIRS_MINOR = rowSums(PULL_REPAIRS_DATA[, c('REPAIRS_ONLY_REGULAR', 'REPAIRS_ONLY_MINOR')])
)

CA_06_REPAIRS_DATA <- CA_06_REPAIRS_DATA_TMP %>% select(GeoUID, REPAIRS_TOT, REPAIRS_MINOR, REPAIRS_MAJOR)

# merge with CA06_DATA
CA06_DATA <- merge(CA06_DATA, CA_06_REPAIRS_DATA, by="GeoUID")

# ----------- fix the MAJOR category - combine Male and Female -----------

CA_06_MAJOR_MALE_VEC = c(
  'MAJOR_TOT'        = 'v_CA06_1208', # Major field of study, 25-64 years - Total
  'MAJOR_EDUC'       = 'v_CA06_1209', # Major field of study, 25-64 years - Education
  'MAJOR_ART_COM'    = 'v_CA06_1210', # Major field of study, 25-64 years - Visual and performing arts, and communications technologies
  'MAJOR_HUMANITIES' = 'v_CA06_1211', # Major field of study, 25-64 years - Humanities
  'MAJOR_SOC_SCI'    = 'v_CA06_1212', # Major field of study, 25-64 years - Social and behavioural sciences and law
  'MAJOR_BUS_MGT'    = 'v_CA06_1213', # Major field of study, 25-64 years - Business, management and public administration
  'MAJOR_PHY_SCI'    = 'v_CA06_1214', # Major field of study, 25-64 years - Physical and life sciences and technologies
  'MAJOR_MATH_COMP'  = 'v_CA06_1215', # Major field of study, 25-64 years - Mathematics, computer and information sciences
  'MAJOR_ENGR'       = 'v_CA06_1216', # Major field of study, 25-64 years - Architecture, engineering, and related technologies
  'MAJOR_NAT_RSRC'   = 'v_CA06_1217', # Major field of study, 25-64 years - Agriculture, natural resources and conservation
  'MAJOR_HLTH'       = 'v_CA06_1218', # Major field of study, 25-64 years - Health and related fields
  'MAJOR_SERVICES'   = 'v_CA06_1219' # Major field of study, 25-64 years - Personal, protective and transportation services
)

CA_06_MAJOR_FEMALE_VEC = c(
  'MAJOR_TOT'        = 'v_CA06_1221', # Major field of study, 25-64 years - Total
  'MAJOR_EDUC'       = 'v_CA06_1222', # Major field of study, 25-64 years - Education
  'MAJOR_ART_COM'    = 'v_CA06_1223', # Major field of study, 25-64 years - Visual and performing arts, and communications technologies
  'MAJOR_HUMANITIES' = 'v_CA06_1224', # Major field of study, 25-64 years - Humanities
  'MAJOR_SOC_SCI'    = 'v_CA06_1225', # Major field of study, 25-64 years - Social and behavioural sciences and law
  'MAJOR_BUS_MGT'    = 'v_CA06_1226', # Major field of study, 25-64 years - Business, management and public administration
  'MAJOR_PHY_SCI'    = 'v_CA06_1227', # Major field of study, 25-64 years - Physical and life sciences and technologies
  'MAJOR_MATH_COMP'  = 'v_CA06_1228', # Major field of study, 25-64 years - Mathematics, computer and information sciences
  'MAJOR_ENGR'       = 'v_CA06_1229', # Major field of study, 25-64 years - Architecture, engineering, and related technologies
  'MAJOR_NAT_RSRC'   = 'v_CA06_1230', # Major field of study, 25-64 years - Agriculture, natural resources and conservation
  'MAJOR_HLTH'       = 'v_CA06_1231', # Major field of study, 25-64 years - Health and related fields
  'MAJOR_SERVICES'   = 'v_CA06_1232' # Major field of study, 25-64 years - Personal, protective and transportation services
)

hash(CA_06_MAJOR_MALE_VEC)[["MAJOR_TOT"]]

names(CA_06_MAJOR_MALE_VEC) = paste0((names(CA_06_MAJOR_MALE_VEC)), "_MALE")
names(CA_06_MAJOR_FEMALE_VEC) = paste0((names(CA_06_MAJOR_FEMALE_VEC)), "_FEMALE")


CA06_VECTORS = c(CA06_VECTORS, CA_06_MAJOR_MALE_VEC, CA_06_MAJOR_FEMALE_VEC)

CA06_MAJOR_DATA <- list()

CA_06_MAJOR_MALE <- get_census(dataset="CA06",
                                     regions=list(PR="59"),
                                     vectors=
                                     CA_06_MAJOR_MALE_VEC,
                                     level='DA')



CA06_MAJOR_DATA$MALE <- CA_06_MAJOR_MALE[, c('GeoUID',
                                                     keys(hash(CA_06_MAJOR_MALE_VEC))
)]

CA_06_MAJOR_FEMALE <- get_census(dataset="CA06",
                                       regions=list(PR="59"),
                                       vectors=
                                       CA_06_MAJOR_FEMALE_VEC,
                                       level='DA')

CA06_MAJOR_DATA$FEMALE <- CA_06_MAJOR_FEMALE[, c('GeoUID',
                                                         keys(hash(CA_06_MAJOR_FEMALE_VEC))
)]

# add M and F by major group
#CA06_AGE_GRP_DATA_TOTAL <- aggregate(. ~ GeoUID, do.call(rbind, CA06_AGE_GRP_DATA), sum)

CA06_MAJOR_DATA_TOTAL <- bind_rows(CA06_MAJOR_DATA) %>%
  group_by(GeoUID, .drop=FALSE) %>%
  summarize_each(funs(sum))

# merge all major cats to CA06_DATA
CA06_DATA <- merge(CA06_DATA, CA06_MAJOR_DATA_TOTAL[,], by="GeoUID")

# ----------- fix the EDUC_POSTSEC category -----------

CA_06_EDUC_POSTSEC_VECTORS = c(
  'EDUC_APPRENT' = 'v_CA06_1252', # Total	Apprenticeship or trades certificate or diploma
  'EDUC_COLLEGE' = 'v_CA06_1253', # Total College, CEGEP or other non-university certificate or diploma
  'EDUC_UNI'     = 'v_CA06_1254'  # Total	University certificate, diploma or degree
)

CA06_VECTORS = c(CA06_VECTORS,CA_06_EDUC_POSTSEC_VECTORS)

PULL_EDUC_DATA <- get_census(dataset="CA06",
                                     regions=list(PR="59"),
                                     vectors=
                                     CA_06_EDUC_POSTSEC_VECTORS,
                                     level='DA')

CA_06_EDUC_POSTSEC_DATA <- PULL_EDUC_DATA[, c('GeoUID', 'EDUC_APPRENT', 'EDUC_COLLEGE', 'EDUC_UNI')]

CA_06_EDUC_POSTSEC_DATA <- add_column(CA_06_EDUC_POSTSEC_DATA,
  EDUC_POSTSEC = rowSums(PULL_EDUC_DATA[, c('EDUC_APPRENT', 'EDUC_COLLEGE', 'EDUC_UNI')])
)

# merge with CA06_DATA
CA06_DATA <- merge(CA06_DATA, CA_06_EDUC_POSTSEC_DATA[,c('GeoUID', 'EDUC_POSTSEC')], by="GeoUID")

# ----------- fix the age categories to add M and F for DA level -----------

CA_06_AGR_GRP_MALE_VECTORS = c(
  'AGE_GRP_00_04'   = 'v_CA06_4',   # 0 - 4 years - Male
  'AGE_GRP_05_09'   = 'v_CA06_5',   # 5 - 9 years - Male
  'AGE_GRP_10_14'   = 'v_CA06_6',   # 10 - 14 years - Male
  'AGE_GRP_15_19'   = 'v_CA06_7',   # 15 - 19 years - Male
  'AGE_GRP_20_24'   = 'v_CA06_8',   # 20 - 24 years - Male
  'AGE_GRP_25_29'   = 'v_CA06_9',   # 25 - 29 years - Male
  'AGE_GRP_30_34'   = 'v_CA06_10',  # 30 - 34 years - Male
  'AGE_GRP_35_39'   = 'v_CA06_11',  # 35 - 39 years - Male
  'AGE_GRP_40_44'   = 'v_CA06_12',  # 40 - 44 years - Male
  'AGE_GRP_45_49'   = 'v_CA06_13',  # 45 - 49 years - Male
  'AGE_GRP_50_54'   = 'v_CA06_14',  # 50 - 54 years - Male
  'AGE_GRP_55_59'   = 'v_CA06_15',  # 55 - 59 years - Male
  'AGE_GRP_60_64'   = 'v_CA06_16',  # 60 - 64 years - Male
  'AGE_GRP_65_69' = 'v_CA06_17',    # 65 - 69 years
  'AGE_GRP_70_74' = 'v_CA06_18',    # 70 - 74 years
  'AGE_GRP_75_79' = 'v_CA06_19',    # 75 - 79 years
  'AGE_GRP_80_84' = 'v_CA06_20',    # 80 - 84 years
  'AGE_GRP_85_PLUS' = 'v_CA06_21'   # 85 and over
)

CA_06_AGR_GRP_FEMALE_VECTORS = c(
  'AGE_GRP_00_04'   = 'v_CA06_23',   # 0 - 4 years - Male
  'AGE_GRP_05_09'   = 'v_CA06_24',   # 5 - 9 years - Male
  'AGE_GRP_10_14'   = 'v_CA06_25',   # 10 - 14 years - Male
  'AGE_GRP_15_19'   = 'v_CA06_26',   # 15 - 19 years - Male
  'AGE_GRP_20_24'   = 'v_CA06_27',   # 20 - 24 years - Male
  'AGE_GRP_25_29'   = 'v_CA06_28',   # 25 - 29 years - Male
  'AGE_GRP_30_34'   = 'v_CA06_29',   # 30 - 34 years - Male
  'AGE_GRP_35_39'   = 'v_CA06_30',   # 35 - 39 years - Male
  'AGE_GRP_40_44'   = 'v_CA06_31',   # 40 - 44 years - Male
  'AGE_GRP_45_49'   = 'v_CA06_32',   # 45 - 49 years - Male
  'AGE_GRP_50_54'   = 'v_CA06_33',   # 50 - 54 years - Male
  'AGE_GRP_55_59'   = 'v_CA06_34',   # 55 - 59 years - Male
  'AGE_GRP_60_64'   = 'v_CA06_35',   # 60 - 64 years - Male
  'AGE_GRP_65_69' = 'v_CA06_36',     # 65 - 69 years
  'AGE_GRP_70_74' = 'v_CA06_37',     # 70 - 74 years
  'AGE_GRP_75_79' = 'v_CA06_38',     # 75 - 79 years
  'AGE_GRP_80_84' = 'v_CA06_39',     # 80 - 84 years
  'AGE_GRP_85_PLUS' = 'v_CA06_40'    # 85 and over
)


names(CA_06_AGR_GRP_MALE_VECTORS) = paste0(names(CA_06_AGR_GRP_MALE_VECTORS), "_MALE")

names(CA_06_AGR_GRP_FEMALE_VECTORS) = paste0(names(CA_06_AGR_GRP_FEMALE_VECTORS), "FE_MALE")

CA06_VECTORS = c(CA06_VECTORS,
                 CA_06_AGR_GRP_MALE_VECTORS,
                 CA_06_AGR_GRP_FEMALE_VECTORS)



CA06_AGE_GRP_DATA <- list()

CA06_AGE_GRP_DATA_MALE <- get_census(dataset="CA06",
                        regions=list(PR="59"),
                        vectors=
                        CA_06_AGR_GRP_MALE_VECTORS,
                        level='DA')

CA06_AGE_GRP_DATA$MALE <- CA06_AGE_GRP_DATA_MALE[, c('GeoUID',
  keys(hash(CA_06_AGR_GRP_MALE_VECTORS))
)]

CA06_AGE_GRP_DATA_FEMALE <- get_census(dataset="CA06",
                                     regions=list(PR="59"),
                                     vectors=
                                     CA_06_AGR_GRP_FEMALE_VECTORS,
                                     level='DA')

CA06_AGE_GRP_DATA$FEMALE <- CA06_AGE_GRP_DATA_FEMALE[, c('GeoUID',
  keys(hash(CA_06_AGR_GRP_FEMALE_VECTORS))
)]

# add M and F by age group
#CA06_AGE_GRP_DATA_TOTAL <- aggregate(. ~ GeoUID, do.call(rbind, CA06_AGE_GRP_DATA), sum)

CA06_AGE_GRP_DATA_TOTAL <- bind_rows(CA06_AGE_GRP_DATA) %>%
  group_by(GeoUID, .drop=FALSE) %>%
  summarize_each(funs(sum))

# ---------------- fix the 65+ age category for DA level -----------------------------

CA06_AGR_GRP_65_PLUS_TEMP <- tibble(GeoUID = CA06_AGE_GRP_DATA_TOTAL$GeoUID,
                                    AGE_GRP_65_PLUS = rowSums(CA06_AGE_GRP_DATA_TOTAL[,AGR_GRP_65_PLUS_VARS]))

CA06_AGE_GRP_DATA_TOTAL[,AGR_GRP_65_PLUS_VARS] <- NULL

# merge 65 plus cat into CA06_AGE_GRP_DATA_TOTAL
CA06_AGE_GRP_DATA_TOTAL <- merge(CA06_AGE_GRP_DATA_TOTAL, CA06_AGR_GRP_65_PLUS_TEMP[, c("GeoUID", "AGE_GRP_65_PLUS")], by="GeoUID")

# merge all age cats to CA06_DATA
CA06_DATA <- merge(CA06_DATA, CA06_AGE_GRP_DATA_TOTAL[,], by="GeoUID")

# -------------------------- ADD EMPTY COLUMNS -------------------------

CA06_DATA <- add_column(CA06_DATA,
                        HOME_VALUE_MED = NA,
                        HOME_RENT_MED = NA,
                        INC_FAM_TOP = NA,
                        INC_FAM_BOTTOM = NA,
                        INC_FAM_DEC_10 = NA,
                        INC_HHS_GRP_TOT = NA,
                        INC_HHS_100k_125k = NA,
                        INC_HHS_125k_150k = NA,
                        INC_HHS_150k_PLUS = NA,
                        INC_AT_CPL_W_CHILD_AVG = NA,
                        INC_AT_CPL_W_CHILD_MED = NA,
                        LIM_AT_PREVALENCE = NA,
                        SUBSIDIZED_HOUS = NA,
                        HOME_SUITABLE_TOT = NA,
                        HOME_SUITABLE_YES = NA,
                        HOME_SUITABLE_NO = NA,
                        GO_TO_WORK_TOT = NA,
                        GO_TO_WORK_5AM_7AM = NA,
                        GO_TO_WORK_7AM_9AM = NA,
                        GO_TO_WORK_AFTER_9AM = NA)

# add census year
CA06_DATA <- add_column(CA06_DATA, CENSUS_YEAR = 2006, .before = "GeoUID")




CA06_vectors_ses = get_vector_fn(CA06_VECTORS, CA06_vectors_all, file_name = "CA06_vectors_for_ses")


# ********************************************************************************
# PULL 2001 DATA
# ********************************************************************************

CA01_VECTORS = c(
  'POP'             = 'v_CA01_2',    # Population, 2001
  'AGE_GRP_TOT'     = 'v_CA01_5',    # Total - Age
  'AGE_GRP_TOT_F'   = 'v_CA01_25',   # Total - Age - Female
  'AGE_GRP_TOT_M'   = 'v_CA01_6',    # Total - Age - Male
  'DWELL_TOT'       = 'v_CA01_112',  # Dwellings - Total
  'DWELL_HOUS'      = 'v_CA01_113',  # Dwellings - Single detached house
  'DWELL_APT_LRG'   = 'v_CA01_117',  # Dwellings - Apartment 5+ stories
  'DWELL_APT_SML'   = 'v_CA01_118',  # Dwellings - Apartment < 5 stories
  'DWELL_MOVE'      = 'v_CA01_120',  # Dwellings - Movable
  'MARITAL_TOT'     = 'v_CA01_44',   # Marital Status - Total
  'MARITAL_MARR'    = 'v_CA01_46',   # Marital Status - Married
  'MARITAL_NEVER'   = 'v_CA01_45',   # Marital Status - Never Married
  'MARITAL_SEP'     = 'v_CA01_47',   # Marital Status - Separated
  'MARITAL_DIV'     = 'v_CA01_48',   # Marital Status - Divorced
  'MARITAL_WID'     = 'v_CA01_49',   # Marital Status - Widowed
  'SING_PARENT_TOT' = 'v_CA01_67',   # Lone Parent - Total
  'SING_PARENT_F'   = 'v_CA01_68',   # Lone Parent - Female
  'SING_PARENT_M'   = 'v_CA01_72',   # Lone Parent - Male
  'INC_BT_IND_MED'  = 'v_CA01_1449', # Total Individual Income Before Tax - Median
  'INC_BT_IND_AVG'  = 'v_CA01_1448', # Total Individual Income Before Tax - Average
  'INC_BT_FAM_MED'  = 'v_CA01_1508', # Total Family Income Before Tax - Median
  'INC_BT_FAM_AVG'  = 'v_CA01_1507', # Total Family Income Before Tax - Average
  # no income after tax family
  'INC_BT_HHS_MED'  = 'v_CA01_1634', # Total Household Income Before Tax - Median
  'INC_BT_HHS_AVG'  = 'v_CA01_1633', # Total Household Income Before Tax - Average
  'LICO_AT_PREVALENCE' = 'v_CA01_1620', # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
  'ABORIGINAL_TOT'  = 'v_CA01_717', # Aboriginal Identity - Total
  'ABORIGINAL_YES'  = 'v_CA01_718', # Aboriginal Identity - Yes
  'ABORIGINAL_NO'   = 'v_CA01_724', # Aboriginal Identity - No
  'MINORITY_TOT'    = 'v_CA01_702', # Visible Minority - Total
  'MINORITY_YES'    = 'v_CA01_703', # Visible Minority - Yes
  'MINORITY_NO'     = 'v_CA01_716', # Visible Minority - No
  'CITIZEN_TOT'     = 'v_CA01_399',  # Citizenship - Total
  'CITIZEN_CAN'     = 'v_CA01_400',  # Citizenship - Canadian
  'CITIZEN_NOT_CAN' = 'v_CA01_401',  # Citizenship - Not Canadian
  'IMMIGRANT_TOT'   = 'v_CA01_402', # Immigrant - Total
  'IMMIGRANT_YES'   = 'v_CA01_406', # Immigrant - Yes
  'IMMIGRANT_NO'    = 'v_CA01_403', # Immigrant - No
  'OCC_TOT'         = 'v_CA01_989', # Occupation - Total
  'OCC_MGMT'        = 'v_CA01_990', # Occupation - Management
  'OCC_BUS_FIN_AD'  = 'v_CA01_995', # Occupation - Business, Finance and Admin
  'OCC_NAT_APP_SCI' = 'v_CA01_1002', # Occupation - Natural and Applied Sciences
  'OCC_HLTH'        = 'v_CA01_1005', # Occupation - Health
  'OCC_SOCI_SERV'   = 'v_CA01_1010', # Occupation - Education, Law and Social, Community and Government
  'OCC_ART_CUL_REC' = 'v_CA01_1014', # Occupation - Arts, Culture, Recreation, Sport
  'OCC_SALE_SERV'   = 'v_CA01_1017', # Occupation - Sales and Service
  'OCC_TRADES'      = 'v_CA01_1028', # Occupation - Trades, Transport and Equipment Operators
  'OCC_NAT_RSRC'    = 'v_CA01_1038', # Occupation - Natural Resources, Agriculture / Production
  'OCC_MAN_UTIL'    = 'v_CA01_1042', # Occupation - Manufacturing and Utilities
  'LABOUR_PART_RT'  = 'v_CA01_740', # Labour Force - Participation Rate
  'LABOUR_EMPL_RT'  = 'v_CA01_741', # Labour Force - Employment Rate
  'LABOUR_UNEM_RT'  = 'v_CA01_742', # Labour Force - Unemployment Rate
  'IN_LAB_FORCE_TOT'  = 'v_CA01_735', # Population aged 15 years and over by Labour force status - Total
  'IN_LAB_FORCE_YES'  = 'v_CA01_736', # Population aged 15 years and over by Labour force status - in labour force
  'IN_LAB_FORCE_NO'  = 'v_CA01_739', # Population aged 15 years and over by Labour force status - not in labour force
  'HOME_OWN_TOT'    = 'v_CA01_96', # Home ownership - Total
  'HOME_OWN_OWN'    = 'v_CA01_99', # Home ownership - Owner
  'HOME_OWN_RENT'   = 'v_CA01_100', # Home ownership - Renter
  'HOME_OWN_BAND'   = 'v_CA01_101', # Home ownership - Band Housing
  'HOME_VALUE_AVG'  = 'v_CA01_1674', # Average value of dwelling
  'HOME_COST_OWN'   = 'v_CA01_1671', # Average owner major payments
  'HOME_RENT_AVG'  = 'v_CA01_1667', # Average gross rent
  'LANG_KNOW_TOT' = 'v_CA01_213', # Knowledge of official languages - total
  'LANG_KNOW_EN' = 'v_CA01_214', # Knowledge of official languages - english only
  'LANG_KNOW_FR' = 'v_CA01_215', # Knowledge of official languages - french only
  'LANG_KNOW_BOTH' = 'v_CA01_216', # Knowledge of official languages - english and french
  'LANG_KNOW_NONE' = 'v_CA01_217', # Knowledge of official languages - neither english nor french
  'LANG_SPOKE_TOT' = 'v_CA01_218', # First official language spoken - total
  'LANG_SPOKE_EN' = 'v_CA01_219', # First official language spoken - english only
  'LANG_SPOKE_FR' = 'v_CA01_220', # First official language spoken - french only
  'LANG_SPOKE_BOTH' = 'v_CA01_221', # First official language spoken - english and french
  'LANG_SPOKE_NONE' = 'v_CA01_222' # First official language spoken - neither english nor french
)

CA01_DATA <- get_census(dataset="CA01",
                        regions=list(PR="59"),
                        vectors=
                          CA01_VECTORS,
                        level='DA')

# ----------- fix the COUPLE CENSUS FAMILIES CAT - add married/common law -----------

CA_01_COUPLES_VEC = c(
  'COUPLES_TOT' = 'v_CA01_54', # Total - Couple census families in private households
  'MARRIED_CHILD_Y' = 'v_CA01_57', # Married couples - With children at home
  'MARRIED_CHILD_N' = 'v_CA01_56', # Married couples - Without children at home
  'COMMON_LAW_CHILD_Y' = 'v_CA01_63', # Common-law couples - With children at home
  'COMMON_LAW_CHILD_N' = 'v_CA01_62' # Common-law couples - Without children at home
)


CA01_VECTORS = c(CA01_VECTORS, CA_01_COUPLES_VEC)

PULL_COUPLES_DATA <- get_census(dataset="CA01",
                                regions=list(PR="59"),
                                vectors=
                                  CA_01_COUPLES_VEC,
                                level='DA')

CA_01_COUPLES_DATA_TMP <- PULL_COUPLES_DATA[, c('GeoUID', 'COUPLES_TOT')]

CA_01_COUPLES_DATA_TMP <- add_column(CA_01_COUPLES_DATA_TMP,
                                     COUPLES_CHILD_Y = rowSums(PULL_COUPLES_DATA[, c('MARRIED_CHILD_Y', 'COMMON_LAW_CHILD_Y')])
)

CA_01_COUPLES_DATA_TMP <- add_column(CA_01_COUPLES_DATA_TMP,
                                     COUPLES_CHILD_N = rowSums(PULL_COUPLES_DATA[, c('MARRIED_CHILD_N', 'COMMON_LAW_CHILD_N')])
)

# merge with CA01_DATA
CA01_DATA <- merge(CA01_DATA, CA_01_COUPLES_DATA_TMP, by="GeoUID")

# ----------- fix the REPAIRS category - combine regular and minor -----------

CA_01_REPAIRS_VECTORS = c(
  'REPAIRS_TOT' = 'v_CA01_96', # Occupied private dwellings by dwelling condition	- Total
  'REPAIRS_ONLY_REGULAR' = 'v_CA01_102', # Occupied private dwellings by dwelling condition	- Only regular maintenance
  'REPAIRS_ONLY_MINOR' = 'v_CA01_103', # Occupied private dwellings by dwelling condition	- Only minor repairs needed
  'REPAIRS_MAJOR' = 'v_CA01_104' # Occupied private dwellings by dwelling condition	- Major repairs needed
)


CA01_VECTORS = c(CA01_VECTORS, CA_01_REPAIRS_VECTORS)

PULL_REPAIRS_DATA <- get_census(dataset="CA01",
                                regions=list(PR="59"),
                                vectors=
                                  CA_01_REPAIRS_VECTORS,
                                level='DA')

CA_01_REPAIRS_DATA_TMP <- PULL_REPAIRS_DATA[, c('GeoUID', 'REPAIRS_TOT', 'REPAIRS_ONLY_REGULAR', 'REPAIRS_ONLY_MINOR', 'REPAIRS_MAJOR')]

CA_01_REPAIRS_DATA_TMP <- add_column(CA_01_REPAIRS_DATA_TMP,
                                     REPAIRS_MINOR = rowSums(PULL_REPAIRS_DATA[, c('REPAIRS_ONLY_REGULAR', 'REPAIRS_ONLY_MINOR')])
)

CA_01_REPAIRS_DATA <- CA_01_REPAIRS_DATA_TMP %>% select(GeoUID, REPAIRS_TOT, REPAIRS_MINOR, REPAIRS_MAJOR)

# merge with CA01_DATA
CA01_DATA <- merge(CA01_DATA, CA_01_REPAIRS_DATA, by="GeoUID")

# ----------- fix the MAJOR category - combine Male and Female -----------

CA_01_MAJOR_MALE_VEC = c(
  'MAJOR_TOT'        = 'v_CA01_1360', # Major field of study, total pop - Total
  'MAJOR_EDUC'       = 'v_CA01_1361', # Major field of study, total pop - Educational, recreational and counselling services
  'MAJOR_ART_COM'    = 'v_CA01_1362', # Major field of study, total pop - Fine and applied arts
  'MAJOR_HUMANITIES' = 'v_CA01_1363', # Major field of study, total pop - Humanities and related fields
  'MAJOR_SOC_SCI'    = 'v_CA01_1364', # Major field of study, total pop - Social sciences and related fields
  'MAJOR_BUS_MGT'    = 'v_CA01_1365', # Major field of study, total pop - Commerce, management and business administration
  'MAJOR_PHY_SCI'    = 'v_CA01_1366', # Major field of study, total pop - Agricultural, biological, nutritional, and food sciences
  'MAJOR_MATH_COMP'  = 'v_CA01_1370', # Major field of study, total pop - Mathematics, computer and physical sciences
  'MAJOR_ENGR'       = 'v_CA01_1367', # Major field of study, total pop - Engineering and applied sciences
  'MAJOR_HLTH'       = 'v_CA01_1369' # Major field of study, total pop - Health professions and related technologies
)

# missing 'MAJOR_NAT_RSRC'
# missin 'MAJOR_SERVICES'

CA_01_MAJOR_FEMALE_VEC = c(
  'MAJOR_TOT'        = 'v_CA01_1372', # Major field of study, total pop - Total
  'MAJOR_EDUC'       = 'v_CA01_1373', # Major field of study, total pop - Educational, recreational and counselling services
  'MAJOR_ART_COM'    = 'v_CA01_1374', # Major field of study, total pop - Fine and applied arts
  'MAJOR_HUMANITIES' = 'v_CA01_1375', # Major field of study, total pop - Humanities and related fields
  'MAJOR_SOC_SCI'    = 'v_CA01_1376', # Major field of study, total pop - Social sciences and related fields
  'MAJOR_BUS_MGT'    = 'v_CA01_1377', # Major field of study, total pop - Commerce, management and business administration
  'MAJOR_PHY_SCI'    = 'v_CA01_1378', # Major field of study, total pop - Agricultural, biological, nutritional, and food sciences
  'MAJOR_MATH_COMP'  = 'v_CA01_1382', # Major field of study, total pop - Mathematics, computer and physical sciences
  'MAJOR_ENGR'       = 'v_CA01_1379', # Major field of study, total pop - Engineering and applied sciences
  'MAJOR_HLTH'       = 'v_CA01_1381' # Major field of study, total pop - Health professions and related technologies
)

# missing 'MAJOR_NAT_RSRC'
# missin 'MAJOR_SERVICES'

names(CA_01_MAJOR_MALE_VEC) = paste0(names(CA_01_MAJOR_MALE_VEC), "_MALE")

names(CA_01_MAJOR_FEMALE_VEC) = paste0(names(CA_01_MAJOR_FEMALE_VEC), "FE_MALE")

CA01_VECTORS = c(CA01_VECTORS,CA_01_MAJOR_MALE_VEC ,CA_01_MAJOR_FEMALE_VEC)

CA01_MAJOR_DATA <- list()

CA_01_MAJOR_MALE <- get_census(dataset="CA01",
                               regions=list(PR="59"),
                               vectors=
                                 CA_01_MAJOR_MALE_VEC,
                               level='DA')

CA01_MAJOR_DATA$MALE <- CA_01_MAJOR_MALE[, c('GeoUID',
                                             keys(hash(CA_01_MAJOR_MALE_VEC))
)]

CA_01_MAJOR_FEMALE <- get_census(dataset="CA01",
                                 regions=list(PR="59"),
                                 vectors=
                                   CA_01_MAJOR_FEMALE_VEC,
                                 level='DA')

CA01_MAJOR_DATA$FEMALE <- CA_01_MAJOR_FEMALE[, c('GeoUID',
                                                 keys(hash(CA_01_MAJOR_FEMALE_VEC))
)]

# add M and F by major group
#CA06_AGE_GRP_DATA_TOTAL <- aggregate(. ~ GeoUID, do.call(rbind, CA06_AGE_GRP_DATA), sum)

CA01_MAJOR_DATA_TOTAL <- bind_rows(CA01_MAJOR_DATA) %>%
  group_by(GeoUID, .drop=FALSE) %>%
  summarize_each(funs(sum))

# merge all major cats to CA06_DATA
CA01_DATA <- merge(CA01_DATA, CA01_MAJOR_DATA_TOTAL[,], by="GeoUID")

# ---------------- fix the age categories to add M and F ----------------

CA_01_AGR_GRP_MALE_VECTORS = c(
  'AGE_GRP_00_04'   = 'v_CA01_7',   # 0 - 4 years - Male
  'AGE_GRP_05_09'   = 'v_CA01_8',   # 5 - 9 years - Male
  'AGE_GRP_10_14'   = 'v_CA01_9',   # 10 - 14 years - Male
  'AGE_GRP_15_19'   = 'v_CA01_10',   # 15 - 19 years - Male
  'AGE_GRP_20_24'   = 'v_CA01_11',   # 20 - 24 years - Male
  'AGE_GRP_25_29'   = 'v_CA01_12',   # 25 - 29 years - Male
  'AGE_GRP_30_34'   = 'v_CA01_13',  # 30 - 34 years - Male
  'AGE_GRP_35_39'   = 'v_CA01_14',  # 35 - 39 years - Male
  'AGE_GRP_40_44'   = 'v_CA01_15',  # 40 - 44 years - Male
  'AGE_GRP_45_49'   = 'v_CA01_16',  # 45 - 49 years - Male
  'AGE_GRP_50_54'   = 'v_CA01_17',  # 50 - 54 years - Male
  'AGE_GRP_55_59'   = 'v_CA01_18',  # 55 - 59 years - Male
  'AGE_GRP_60_64'   = 'v_CA01_19',  # 60 - 64 years - Male
  'AGE_GRP_65_69' = 'v_CA01_20',    # 65 - 69 years
  'AGE_GRP_70_74' = 'v_CA01_21',    # 70 - 74 years
  'AGE_GRP_75_79' = 'v_CA01_22',    # 75 - 79 years
  'AGE_GRP_80_84' = 'v_CA01_23',    # 80 - 84 years
  'AGE_GRP_85_PLUS' = 'v_CA01_24'   # 85 and over
)

CA_01_AGR_GRP_FEMALE_VECTORS = c(
  'AGE_GRP_00_04'   = 'v_CA01_26',   # 0 - 4 years - Male
  'AGE_GRP_05_09'   = 'v_CA01_27',   # 5 - 9 years - Male
  'AGE_GRP_10_14'   = 'v_CA01_28',   # 10 - 14 years - Male
  'AGE_GRP_15_19'   = 'v_CA01_29',   # 15 - 19 years - Male
  'AGE_GRP_20_24'   = 'v_CA01_30',   # 20 - 24 years - Male
  'AGE_GRP_25_29'   = 'v_CA01_31',   # 25 - 29 years - Male
  'AGE_GRP_30_34'   = 'v_CA01_32',   # 30 - 34 years - Male
  'AGE_GRP_35_39'   = 'v_CA01_33',   # 35 - 39 years - Male
  'AGE_GRP_40_44'   = 'v_CA01_34',   # 40 - 44 years - Male
  'AGE_GRP_45_49'   = 'v_CA01_35',   # 45 - 49 years - Male
  'AGE_GRP_50_54'   = 'v_CA01_36',   # 50 - 54 years - Male
  'AGE_GRP_55_59'   = 'v_CA01_37',   # 55 - 59 years - Male
  'AGE_GRP_60_64'   = 'v_CA01_38',   # 60 - 64 years - Male
  'AGE_GRP_65_69' = 'v_CA01_39',     # 65 - 69 years
  'AGE_GRP_70_74' = 'v_CA01_40',     # 70 - 74 years
  'AGE_GRP_75_79' = 'v_CA01_41',     # 75 - 79 years
  'AGE_GRP_80_84' = 'v_CA01_42',     # 80 - 84 years
  'AGE_GRP_85_PLUS' = 'v_CA01_43'    # 85 and over
)

names(CA_01_AGR_GRP_MALE_VECTORS) = paste0(names(CA_01_AGR_GRP_MALE_VECTORS), "_MALE")
names(CA_01_AGR_GRP_FEMALE_VECTORS) = paste0(names(CA_01_AGR_GRP_FEMALE_VECTORS), "FE_MALE")


CA01_VECTORS = c(CA01_VECTORS, CA_01_AGR_GRP_MALE_VECTORS,CA_01_AGR_GRP_FEMALE_VECTORS )

CA01_AGE_GRP_DATA <- list()

CA01_AGE_GRP_DATA_MALE <- get_census(dataset="CA01",
                                     regions=list(PR="59"),
                                     vectors=
                                       CA_01_AGR_GRP_MALE_VECTORS,
                                     level='DA')

CA01_AGE_GRP_DATA$MALE <- CA01_AGE_GRP_DATA_MALE[, c('GeoUID',
                                                     keys(hash(CA_01_AGR_GRP_MALE_VECTORS))
)]

CA01_AGE_GRP_DATA_FEMALE <- get_census(dataset="CA01",
                                       regions=list(PR="59"),
                                       vectors=
                                         CA_01_AGR_GRP_FEMALE_VECTORS,
                                       level='DA')

CA01_AGE_GRP_DATA$FEMALE <- CA01_AGE_GRP_DATA_FEMALE[, c('GeoUID',
                                                         keys(hash(CA_01_AGR_GRP_FEMALE_VECTORS))
)]

# add M and F by age group
#CA01_AGE_GRP_DATA_TOTAL <- aggregate(. ~ GeoUID, do.call(rbind, CA01_AGE_GRP_DATA), sum)

CA01_AGE_GRP_DATA_TOTAL <- bind_rows(CA01_AGE_GRP_DATA) %>%
  group_by(GeoUID, .drop=FALSE) %>%
  summarize_each(funs(sum))

# ---------------- fix the 65+ age category -----------------------------

AGR_GRP_65_PLUS_VARS = c(
  'AGE_GRP_65_69',
  'AGE_GRP_70_74',
  'AGE_GRP_75_79',
  'AGE_GRP_80_84',
  'AGE_GRP_85_PLUS'
)

CA01_AGR_GRP_65_PLUS_TEMP <- tibble(GeoUID = CA01_AGE_GRP_DATA_TOTAL$GeoUID,
                                    AGE_GRP_65_PLUS = rowSums(CA01_AGE_GRP_DATA_TOTAL[,AGR_GRP_65_PLUS_VARS]))

CA01_AGE_GRP_DATA_TOTAL[,AGR_GRP_65_PLUS_VARS] <- NULL

# merge 65 plus cat into CA01_AGE_GRP_DATA_TOTAL
CA01_AGE_GRP_DATA_TOTAL <- merge(CA01_AGE_GRP_DATA_TOTAL, CA01_AGR_GRP_65_PLUS_TEMP[, c("GeoUID", "AGE_GRP_65_PLUS")], by="GeoUID")

# merge all age cats to CA01_DATA
CA01_DATA <- merge(CA01_DATA, CA01_AGE_GRP_DATA_TOTAL[,], by="GeoUID")

# -------------------------- FIX THE EDUCATION LEVELS -------------------------

EDUC_VECTORS_TEMP = c(
  'EDUC_TOT'           = 'v_CA01_1384', # Total population 20 years and over by highest level of schooling
  'EDUC_WO_HS'         = 'v_CA01_1387', # High School - Without high school graduation certificate
  'EDUC_WO_COL_CERT'   = 'v_CA01_1391', # College - Without certificate or diploma
  'EDUC_WO_UNI_DEGREE' = 'v_CA01_1394', # University - Without degree
  'EDUC_WO_UNI_CERT'   = 'v_CA01_1395', # University - Without certificate or diploma
  'EDUC_HIGHSCH'       = 'v_CA01_1388', # High School -With high school graduation certificate
  'EDUC_COLLEGE'       = 'v_CA01_1392', # College - With certificate or diploma
  'EDUC_BACHELOR'      = 'v_CA01_1397', # University - With bachelor's degree or higher
  'EDUC_UNI_CERT'      = 'v_CA01_1396', # University - With certificate or diploma
  'EDUC_TRADES'        = 'v_CA01_1389' # Trades certificate or diploma
)

EDUC_DATA_TEMP <- get_census(dataset="CA01",
                             regions=list(PR="59"),
                             vectors=
                               EDUC_VECTORS_TEMP,
                             level='DA')

EDUC_DATA_TEMP <- add_column(EDUC_DATA_TEMP, EDUC_NONE = EDUC_DATA_TEMP$EDUC_WO_HS +
                               EDUC_DATA_TEMP$EDUC_WO_COL_CERT +
                               EDUC_DATA_TEMP$EDUC_WO_UNI_DEGREE +
                               EDUC_DATA_TEMP$EDUC_WO_UNI_CERT)

EDUC_DATA_TEMP <- add_column(EDUC_DATA_TEMP, EDUC_POSTSEC = EDUC_DATA_TEMP$EDUC_COLLEGE +
                               EDUC_DATA_TEMP$EDUC_TRADES +
                               EDUC_DATA_TEMP$EDUC_UNI_CERT +
                               EDUC_DATA_TEMP$EDUC_BACHELOR)

EDUC_DATA <- EDUC_DATA_TEMP %>% select(GeoUID, EDUC_TOT, EDUC_NONE, EDUC_HIGHSCH, EDUC_COLLEGE, EDUC_POSTSEC, EDUC_BACHELOR, EDUC_TRADES)

CA01_VECTORS = c(CA01_VECTORS, AGR_GRP_65_PLUS_VARS, EDUC_VECTORS_TEMP)

# -------------------------- ADD EMPTY COLUMNS -------------------------

CA01_DATA <- add_column(CA01_DATA,
                        FAM_SIZE_TOT = NA,
                        FAM_SIZE_2 = NA,
                        FAM_SIZE_3 = NA,
                        FAM_SIZE_4 = NA,
                        FAM_SIZE_5 = NA,
                        MAJOR_NAT_RSRC = NA,
                        MAJOR_SERVICES =  NA,
                        EDUC_CRT_ABV_BACH = NA,
                        EDUC_MEDICAL = NA,
                        EDUC_MASTERS = NA,
                        EDUC_PHD = NA,
                        HOME_VALUE_MED = NA,
                        HOME_RENT_MED = NA,
                        INC_AT_FAM_MED = NA,
                        INC_AT_FAM_AVG = NA,
                        INC_FAM_TOP = NA,
                        INC_FAM_BOTTOM = NA,
                        INC_FAM_DEC_10 = NA,
                        INC_HHS_GRP_TOT = NA,
                        INC_HHS_100k_125k = NA,
                        INC_HHS_125k_150k = NA,
                        INC_HHS_150k_PLUS = NA,
                        INC_AT_LONE_PARENT_AVG = NA,
                        INC_AT_CPL_W_CHILD_AVG = NA,
                        INC_AT_LONE_PARENT_MED = NA,
                        INC_AT_CPL_W_CHILD_MED = NA,
                        LIM_AT_PREVALENCE = NA,
                        SUBSIDIZED_HOUS = NA,
                        HOME_SUITABLE_TOT = NA,
                        HOME_SUITABLE_YES = NA,
                        HOME_SUITABLE_NO = NA,
                        REG_INDIAN_TOT = NA,
                        REG_INDIAN_YES = NA,
                        REG_INDIAN_NO = NA,
                        GO_TO_WORK_TOT = NA,
                        GO_TO_WORK_5AM_7AM = NA,
                        GO_TO_WORK_7AM_9AM = NA,
                        GO_TO_WORK_AFTER_9AM = NA)

# merge all educ cats to CA01_DATA
CA01_DATA <- merge(CA01_DATA, EDUC_DATA[,], by="GeoUID")

# add census year field
CA01_DATA <- add_column(CA01_DATA, CENSUS_YEAR = 2001, .before = "GeoUID")



# ********************************************************************************
# APPEND YEARS TOGETHER
# ********************************************************************************

# ALL_YEARS_DATA <- rbind(CA16_DATA, CA11_DATA, CA06_DATA, CA01_DATA)

# save(ALL_YEARS_DATA, file = "K:/Projects/Advanced Analytics/1 Projects/2019/2019-253 Census database all years/RData files/ALL_YEARS_DATA.RData")

 load("K:/Projects/Advanced Analytics/1 Projects/2019/2019-253 Census database all years/RData files/ALL_YEARS_DATA.RData")

#
#  ALL_YEARS_DATA %>% glimpse()
#
#  ALL_YEARS_DATA_21 %>% glimpse()
 missing_cols_all_raw <- miss_var_summary(ALL_YEARS_DATA, show_pct = 1)

ALL_YEARS_DATA_21= bind_rows(ALL_YEARS_DATA, CA21_DATA)

missing_cols_all_raw <- miss_var_summary(ALL_YEARS_DATA_21, show_pct = 1)


ALL_YEARS_DATA_21 %>% write_csv("Data/ALL_YEARS_DATA_21.csv")

ALL_YEARS_DATA_21 %>% dim()
# 37952
# COMPLETE <- ALL_YEARS_DATA[complete.cases(ALL_YEARS_DATA), ]



# ********************************************************************************
# CREATE PERCENTAGES FOR EACH VARIABLE
# ********************************************************************************

# https://stackoverflow.com/questions/54030049/divide-all-columns-by-a-chosen-column-using-mutate-all


get_rate_fn= function(df,vars_vector, var_tot){
# vars_vector: a vector of string
  # var_tot: a enquo?
  DF_PCT <- df %>%
    # select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(vars_vector), {{var_tot}}) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = all_of(vars_vector),
        # .fns = ~ifelse(is.na({{var_tot}}), NA_real_ ,.x / {{var_tot}} * 100)
        .fns =  ~.x / {{var_tot}} * 100
      ))
  return(DF_PCT)
}


# 1) age group
AGE_GRP_VARS = c(
  'AGE_GRP_00_04',
  'AGE_GRP_05_09',
  'AGE_GRP_10_14',
  'AGE_GRP_15_19',
  'AGE_GRP_20_24',
  'AGE_GRP_25_29',
  'AGE_GRP_30_34',
  'AGE_GRP_35_39',
  'AGE_GRP_40_44',
  'AGE_GRP_45_49',
  'AGE_GRP_50_54',
  'AGE_GRP_55_59',
  'AGE_GRP_60_64',
  'AGE_GRP_65_PLUS'
)

# ALL_YEARS_DATA_21 %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(AGE_GRP_VARS), AGE_GRP_TOT) %>%
#   glimpse()
#
# AGE_GRP_PCT <- ALL_YEARS_DATA_21 %>%
#   dplyr::select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(AGE_GRP_VARS), AGE_GRP_TOT) %>%
#   dplyr::mutate(
#     dplyr::across(
#       .cols = AGE_GRP_65_PLUS ,
#       .fns = ~((.x/AGE_GRP_TOT) * 100)
#     ))
#
# AGE_GRP_PCT %>% glimpse()

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_21,
                                     vars_vector = AGE_GRP_VARS,
                                     var_tot = AGE_GRP_TOT)

ALL_YEARS_DATA_PCT_21 %>% glimpse()
# 37,952 5 runs of census

missing_cols_2021_raw <- miss_var_summary(ALL_YEARS_DATA_PCT_21, show_pct = 1)



# 2) dwellings
DWELL_VARS = c(
  'DWELL_HOUS',
  'DWELL_APT_LRG',
  'DWELL_APT_SML',
  'DWELL_MOVE'
)

# DWELL_PCT <- ALL_YEARS_DATA_21 %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(DWELL_VARS), DWELL_TOT) %>% glimpse()
#   mutate(
#     across(
#       .cols = all_of(DWELL_VARS),
#       .fns = ~(.x / DWELL_TOT * 100)
#     )
#
#     ) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(DWELL_VARS))
#
# DWELL_PCT %>% glimpse()

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_21,
                                     vars_vector = DWELL_VARS,
                                     var_tot = DWELL_TOT)

# 3) marital status
MARITAL_VARS = c(
  'MARITAL_MARR',
  'MARITAL_NEVER',
  'MARITAL_SEP',
  'MARITAL_DIV',
  'MARITAL_WID'
)

# MARITAL_PCT <- ALL_YEARS_DATA_21 %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(MARITAL_VARS), MARITAL_TOT) %>%
#   mutate(
#     across(
#       .cols = all_of(MARITAL_VARS),
#       .fns = ~(.x / MARITAL_TOT * 100)
#     )
#
#     ) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(MARITAL_VARS))
# # done


ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = MARITAL_VARS,
                                     var_tot = MARITAL_TOT)



# 4) family size
FAM_SIZE_VARS = c(
  'FAM_SIZE_2',
  'FAM_SIZE_3',
  'FAM_SIZE_4',
  'FAM_SIZE_5'
)

# FAM_SIZE_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(FAM_SIZE_VARS), FAM_SIZE_TOT) %>%
#   mutate(
#     FAM_SIZE_VARS, funs(. / FAM_SIZE_TOT * 100)
#     ) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(FAM_SIZE_VARS))
ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = FAM_SIZE_VARS,
                                     var_tot = FAM_SIZE_TOT)
# 5) single parents
SING_PARENT_VARS = c(
  'SING_PARENT_F',
  'SING_PARENT_M',
  'SING_PARENT_TOT'
)

# SING_PARENT_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(SING_PARENT_VARS), POP) %>%
#   mutate_at(SING_PARENT_VARS, funs(. / POP * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, all_of(SING_PARENT_VARS))
#

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = SING_PARENT_VARS,
                                     var_tot = POP)

# 6) couples with children
COUPLES_VARS = c(
  'COUPLES_CHILD_Y',
  'COUPLES_CHILD_N'
)

# COUPLES_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, COUPLES_VARS, COUPLES_TOT) %>%
#   mutate_at(COUPLES_VARS, funs(. / COUPLES_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, COUPLES_VARS)




ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = COUPLES_VARS,
                                     var_tot = COUPLES_TOT)
# 7) income distribution
# INC_FAM_TOTAL column has many NA. Could not be divided by.


INC_DIST_VARS = c(
  'INC_FAM_TOP',
  'INC_FAM_BOTTOM',
  'INC_FAM_DEC_10'
)

# ALL_YEARS_DATA_PCT_21 %>%
#   filter(CENSUS_YEAR ==2021) %>%
#   select(all_of(INC_DIST_VARS),  INC_FAM_TOTAL) %>%
#   glimpse()


# ALL_YEARS_DATA_PCT_21 %>% glimpse()
# INC_DIST_PCT <- ALL_YEARS_DATA_PCT_21 %>%
#   # select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, INC_DIST_VARS, INC_DIST_TOT) %>%
#   mutate_at(INC_DIST_VARS, funs(. / INC_FAM_TOTAL * 100))
# %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, INC_DIST_VARS)



ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = INC_DIST_VARS,
                                     var_tot = INC_FAM_TOTAL)



# 8) income groups
INC_GROUP_VARS = c(
  'INC_HHS_100k_125k',
  'INC_HHS_125k_150k',
  'INC_HHS_150k_PLUS'
)

# INC_GROUP_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, INC_GROUP_VARS, INC_HHS_GRP_TOT) %>%
#   mutate_at(INC_GROUP_VARS, funs(. / INC_HHS_GRP_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, INC_GROUP_VARS)


ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = INC_GROUP_VARS,
                                     var_tot = INC_HHS_GRP_TOT)


# 9) aboriginal identity
ABORIGINAL_VARS = c(
  'ABORIGINAL_YES',
  'ABORIGINAL_NO'
)

# ABORIGINAL_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, ABORIGINAL_VARS, ABORIGINAL_TOT) %>%
#   mutate_at(ABORIGINAL_VARS, funs(. / ABORIGINAL_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, ABORIGINAL_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = ABORIGINAL_VARS,
                                     var_tot = ABORIGINAL_TOT)

# 10) registered indian status
REG_INDIAN_VARS = c(
  'REG_INDIAN_YES',
  'REG_INDIAN_NO'
)

# REG_INDIAN_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, REG_INDIAN_VARS, REG_INDIAN_TOT) %>%
#   mutate_at(REG_INDIAN_VARS, funs(. / REG_INDIAN_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, REG_INDIAN_VARS)


ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = REG_INDIAN_VARS,
                                     var_tot = REG_INDIAN_TOT)

# 11) visbile minority
MINORITY_VARS = c(
  'MINORITY_YES',
  'MINORITY_NO'
)

# MINORITY_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, MINORITY_VARS, MINORITY_TOT) %>%
#   mutate_at(MINORITY_VARS, funs(. / MINORITY_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, MINORITY_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = MINORITY_VARS,
                                     var_tot = MINORITY_TOT)


# 12) citizenship
CITIZEN_VARS = c(
  'CITIZEN_CAN',
  'CITIZEN_NOT_CAN'
)

# CITIZEN_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, CITIZEN_VARS, CITIZEN_TOT) %>%
#   mutate_at(CITIZEN_VARS, funs(. / CITIZEN_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, CITIZEN_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = CITIZEN_VARS,
                                     var_tot = CITIZEN_TOT)



# 13) immigrant status
IMMIGRANT_VARS = c(
  'IMMIGRANT_YES',
  'IMMIGRANT_NO'
)
#
# IMMIGRANT_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, IMMIGRANT_VARS, IMMIGRANT_TOT) %>%
#   mutate_at(IMMIGRANT_VARS, funs(. / IMMIGRANT_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, IMMIGRANT_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = IMMIGRANT_VARS,
                                     var_tot = IMMIGRANT_TOT)



# 14) occupation
OCC_VARS = c(
  'OCC_MGMT',
  'OCC_BUS_FIN_AD',
  'OCC_NAT_APP_SCI',
  'OCC_HLTH',
  'OCC_SOCI_SERV',
  'OCC_ART_CUL_REC',
  'OCC_SALE_SERV',
  'OCC_TRADES',
  'OCC_NAT_RSRC',
  'OCC_MAN_UTIL'
)

# OCC_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, OCC_VARS, OCC_TOT) %>%
#   mutate_at(OCC_VARS, funs(. / OCC_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, OCC_VARS)


ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = OCC_VARS,
                                     var_tot = OCC_TOT)



# 15) education
EDUC_VARS = c(
  'EDUC_NONE',
  'EDUC_HIGHSCH',
  'EDUC_POSTSEC',
  'EDUC_COLLEGE',
  'EDUC_BACHELOR',
  'EDUC_TRADES',
  'EDUC_CRT_ABV_BACH',
  'EDUC_MEDICAL',
  'EDUC_MASTERS',
  'EDUC_PHD'
)
#
# EDUC_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, EDUC_VARS, EDUC_TOT) %>%
#   mutate_at(EDUC_VARS, funs(. / EDUC_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, EDUC_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = EDUC_VARS,
                                     var_tot = EDUC_TOT)





# 16) school major
MAJOR_VARS = c(
  'MAJOR_EDUC',
  'MAJOR_ART_COM',
  'MAJOR_HUMANITIES',
  'MAJOR_SOC_SCI',
  'MAJOR_BUS_MGT',
  'MAJOR_PHY_SCI',
  'MAJOR_MATH_COMP',
  'MAJOR_ENGR',
  'MAJOR_NAT_RSRC',
  'MAJOR_HLTH',
  'MAJOR_SERVICES'
)

# MAJOR_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, MAJOR_VARS, MAJOR_TOT) %>%
#   mutate_at(MAJOR_VARS, funs(. / MAJOR_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, MAJOR_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = MAJOR_VARS,
                                     var_tot = MAJOR_TOT)


# 17) labour force status
IN_LAB_FORCE_VARS = c(
  'IN_LAB_FORCE_YES',
  'IN_LAB_FORCE_NO'
)

# IN_LAB_FORCE_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, IN_LAB_FORCE_VARS, IN_LAB_FORCE_TOT) %>%
#   mutate_at(IN_LAB_FORCE_VARS, funs(. / IN_LAB_FORCE_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, IN_LAB_FORCE_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = IN_LAB_FORCE_VARS,
                                     var_tot = IN_LAB_FORCE_TOT)


# 18) housing suitability
HOME_SUITABLE_VARS = c(
  'HOME_SUITABLE_YES',
  'HOME_SUITABLE_NO'
)

# HOME_SUITABLE_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, HOME_SUITABLE_VARS, HOME_SUITABLE_TOT) %>%
#   mutate_at(HOME_SUITABLE_VARS, funs(. / HOME_SUITABLE_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, HOME_SUITABLE_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = HOME_SUITABLE_VARS,
                                     var_tot = HOME_SUITABLE_TOT)


# 19) home repairs
REPAIRS_VARS = c(
  'REPAIRS_MINOR',
  'REPAIRS_MAJOR'
)

# REPAIRS_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, REPAIRS_VARS, REPAIRS_TOT) %>%
#   mutate_at(REPAIRS_VARS, funs(. / REPAIRS_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, REPAIRS_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = REPAIRS_VARS,
                                     var_tot = REPAIRS_TOT
                                     )

# 20) home ownership
HOME_OWN_VARS = c(
  'HOME_OWN_OWN',
  'HOME_OWN_RENT',
  'HOME_OWN_BAND'
)

# HOME_OWN_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, HOME_OWN_VARS, HOME_OWN_TOT) %>%
#   mutate_at(HOME_OWN_VARS, funs(. / HOME_OWN_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, HOME_OWN_VARS)


ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = HOME_OWN_VARS,
                                     var_tot = HOME_OWN_TOT
)


# 21) knowledge of language
LANG_KNOW_VARS = c(
  'LANG_KNOW_EN',
  'LANG_KNOW_FR',
  'LANG_KNOW_BOTH',
  'LANG_KNOW_NONE'
)

# LANG_KNOW_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, LANG_KNOW_VARS, LANG_KNOW_TOT) %>%
#   mutate_at(LANG_KNOW_VARS, funs(. / LANG_KNOW_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, LANG_KNOW_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = LANG_KNOW_VARS,
                                     var_tot = LANG_KNOW_TOT
)


# 22) spoken language
LANG_SPOKE_VARS = c(
  'LANG_SPOKE_EN',
  'LANG_SPOKE_FR',
  'LANG_SPOKE_BOTH',
  'LANG_SPOKE_NONE'
)

# LANG_SPOKE_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, LANG_SPOKE_VARS, LANG_SPOKE_TOT) %>%
#   mutate_at(LANG_SPOKE_VARS, funs(. / LANG_SPOKE_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, LANG_SPOKE_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = LANG_SPOKE_VARS,
                                     var_tot = LANG_SPOKE_TOT
)


# 23) time leave for work
GO_TO_WORK_VARS = c(
  'GO_TO_WORK_5AM_7AM',
  'GO_TO_WORK_7AM_9AM',
  'GO_TO_WORK_AFTER_9AM'
)

# GO_TO_WORK_PCT <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, GO_TO_WORK_VARS, GO_TO_WORK_TOT) %>%
#   mutate_at(GO_TO_WORK_VARS, funs(. / GO_TO_WORK_TOT * 100)) %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, GO_TO_WORK_VARS)

ALL_YEARS_DATA_PCT_21  = get_rate_fn(ALL_YEARS_DATA_PCT_21,
                                     vars_vector = GO_TO_WORK_VARS,
                                     var_tot = GO_TO_WORK_TOT
)



# ********************************************************************************
# 24) add in non-pct variables
# ********************************************************************************
#
# NON_PCT_VARS = c(
#   'INC_BT_IND_MED',
#   'INC_BT_IND_AVG',
#   'INC_BT_FAM_MED',
#   'INC_BT_FAM_AVG',
#   'INC_AT_FAM_MED',
#   'INC_AT_FAM_AVG',
#   'INC_BT_HHS_MED',
#   'INC_BT_HHS_AVG',
#   'INC_AT_LONE_PARENT_AVG',
#   'INC_AT_CPL_W_CHILD_AVG',
#   'INC_AT_LONE_PARENT_MED',
#   'INC_AT_CPL_W_CHILD_MED',
#   'LICO_AT_PREVALENCE',
#   'LIM_AT_PREVALENCE',
#   'LABOUR_PART_RT',
#   'LABOUR_EMPL_RT',
#   'LABOUR_UNEM_RT',
#   'HOME_VALUE_AVG',
#   'HOME_VALUE_MED',
#   'HOME_COST_OWN',
#   'HOME_RENT_AVG',
#   'HOME_RENT_MED',
#   'SUBSIDIZED_HOUS'
# )
#
# NON_PCT_DATA <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, NON_PCT_VARS)
#




# # ********************************************************************************
# # 25) add POP DENSITY
# # ********************************************************************************
#
# POP_DENSITY <- ALL_YEARS_DATA %>%
#   select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, POP, `Area (sq km)`) %>%
#   mutate(POP_DENSITY = POP / `Area (sq km)`) %>% select(GeoUID, CSD_UID, CD_UID, CENSUS_YEAR, POP_DENSITY)
#
ALL_YEARS_DATA_PCT_21 = ALL_YEARS_DATA_PCT_21 %>%
  mutate(POP_DENSITY = POP / `Area (sq km)`)


# # ********************************************************************************
# # CREATE PCT DATA FRAME FOR WRITING TO EDW
# # ********************************************************************************
#
# # # merge old school way
# # ALL_YEARS_DATA_PCT <- AGE_GRP_PCT
# # ALL_YEARS_DATA_PCT <- merge(ALL_YEARS_DATA_PCT, DWELL_PCT, by=c("CENSUS_YEAR", "GeoUID"))
#
# # merge fancy way https://stackoverflow.com/questions/14096814/merging-a-lot-of-data-frames
# ALL_YEARS_DATA_PCT <- Reduce(function(x, y) merge(x, y, all=TRUE), list(POP_DENSITY,
#                                                                         AGE_GRP_PCT,
#                                                                         DWELL_PCT,
#                                                                         MARITAL_PCT,
#                                                                         FAM_SIZE_PCT,
#                                                                         SING_PARENT_PCT,
#                                                                         COUPLES_PCT,
#                                                                         INC_DIST_PCT,
#                                                                         INC_GROUP_PCT,
#                                                                         ABORIGINAL_PCT,
#                                                                         REG_INDIAN_PCT,
#                                                                         MINORITY_PCT,
#                                                                         CITIZEN_PCT,
#                                                                         IMMIGRANT_PCT,
#                                                                         OCC_PCT,
#                                                                         EDUC_PCT,
#                                                                         MAJOR_PCT,
#                                                                         IN_LAB_FORCE_PCT,
#                                                                         HOME_SUITABLE_PCT,
#                                                                         REPAIRS_PCT,
#                                                                         HOME_OWN_PCT,
#                                                                         LANG_KNOW_PCT,
#                                                                         LANG_SPOKE_PCT,
#                                                                         GO_TO_WORK_PCT,
#                                                                         NON_PCT_DATA
#                                                                         ))

# ALL_YEARS_DATA_PCT_21= ALL_YEARS_DATA_PCT_21 %>%
#   mutate(
#     CENSUS_YEAR = case_when(
#       is.na(CENSUS_YEAR) ~ 2021,
#       T ~ CENSUS_YEAR
#     )
#   )

# colnames(ALL_YEARS_DATA_PCT)[colnames(ALL_YEARS_DATA_PCT)=="GeoUID"] <- "DA_ID"
ALL_YEARS_DATA_PCT_21 = ALL_YEARS_DATA_PCT_21 %>%
  dplyr::rename(DA_ID = GeoUID)

# save(ALL_YEARS_DATA_PCT,
#      file = "K:/Projects/Advanced Analytics/1 Projects/2019/2019-253 Census database all years/RData files/ALL_YEARS_DATA_PCT.RData")



# --------------------------------------------------------------------
# GET RID OF INF AND NAN
# --------------------------------------------------------------------

# is.na(ALL_YEARS_DATA_PCT) <- do.call(cbind,lapply(ALL_YEARS_DATA_PCT, is.infinite))
# is.na(ALL_YEARS_DATA_PCT) <- do.call(cbind,lapply(ALL_YEARS_DATA_PCT, is.nan))

# COMPLETE_PCT <- ALL_YEARS_DATA_PCT[complete.cases(ALL_YEARS_DATA_PCT), ]
# save(COMPLETE_PCT, file = "COMPLETE_PCT.RData")


# just 2021
CA21_DATA_PCT <- ALL_YEARS_DATA_PCT_21 %>% filter(CENSUS_YEAR == 2021)

# just 2016
CA16_DATA_PCT <- ALL_YEARS_DATA_PCT_21 %>% filter(CENSUS_YEAR == 2016)

# just 2011
CA11_DATA_PCT <- ALL_YEARS_DATA_PCT_21 %>% filter(CENSUS_YEAR == 2011)

# just 2006
CA06_DATA_PCT <- ALL_YEARS_DATA_PCT_21 %>% filter(CENSUS_YEAR == 2006)

# just 2001
CA01_DATA_PCT <- ALL_YEARS_DATA_PCT_21 %>% filter(CENSUS_YEAR == 2001)

CA01_DATA_PCT %>%
  group_by(CENSUS_YEAR) %>%
  summarise(
    across(
      .cols = c(INC_BT_FAM_AVG, INC_BT_FAM_MED, INC_AT_FAM_AVG, INC_AT_FAM_MED),
      .fns = ~mean(.x, na.rm = T)
    )
  )



# Fix POP CHANGE RATE in 2021 and other year. They are all missing.

create_pop_change_rate_fn = function(df1, df2){
   df1 %>%
    # select(DA_ID, POP ) %>%
    left_join(
      df2 %>%
        select(DA_ID, POP_PRE  = POP )
    ) %>%
    mutate(
      POP_PCT_CHANGE =(POP-POP_PRE)/POP_PRE
    ) %>% select(-POP_PRE)
}

CA21_DATA_PCT = CA21_DATA_PCT %>%
  # select(DA_ID, POP ) %>%
  left_join(
    CA16_DATA_PCT %>%
      select(DA_ID, POP2016  = POP )
  ) %>%
  mutate(
    POP_PCT_CHANGE =(POP-POP2016)/POP2016
  ) %>% select(-POP2016)


CA16_DATA_PCT = CA16_DATA_PCT %>%
  # select(DA_ID, POP ) %>%
  left_join(
    CA11_DATA_PCT %>%
      select(DA_ID, POP2011  = POP )
  ) %>%
  mutate(
    POP_PCT_CHANGE =(POP-POP2011)/POP2011
  ) %>% select(-POP2011)

CA11_DATA_PCT = CA11_DATA_PCT %>%
  # select(DA_ID, POP ) %>%
  left_join(
    CA06_DATA_PCT %>%
      select(DA_ID, POP2006  = POP )
  ) %>%
  mutate(
    POP_PCT_CHANGE =(POP-POP2006)/POP2006
  ) %>% select(-POP2006)

CA06_DATA_PCT = create_pop_change_rate_fn(CA06_DATA_PCT, CA01_DATA_PCT)


# # ********************************************************************************
# # DROP IF ROW IS ALL NA (can keep rows if some missing info)
# # ********************************************************************************
#
# # https://stackoverflow.com/questions/6471689/remove-rows-in-r-matrix-where-all-data-is-na
# DROP_ANY_NA <- ALL_YEARS_DATA_PCT[rowSums(is.na(ALL_YEARS_DATA_PCT[,5:ncol(ALL_YEARS_DATA_PCT)])) != ncol(ALL_YEARS_DATA_PCT[,5:ncol(ALL_YEARS_DATA_PCT)]), ]
ALL_YEARS_DATA_PCT_POPCHANGE_21= bind_rows(CA21_DATA_PCT,
                                 CA16_DATA_PCT,
                                 CA11_DATA_PCT,
                                 CA06_DATA_PCT,
                                 CA01_DATA_PCT
                                 )
ALL_YEARS_DATA_PCT_POPCHANGE_21 %>% glimpse()
ALL_YEARS_DATA_PCT_POPCHANGE_21 %>% dplyr::count(CENSUS_YEAR)

ALL_YEARS_DATA_PCT_POPCHANGE_21 =ALL_YEARS_DATA_PCT_POPCHANGE_21 %>%
  janitor::clean_names("all_caps")


ALL_YEARS_DATA_PCT_POPCHANGE_21 %>% write_csv("Data/ALL_YEARS_DATA_PCT_POPCHANGE_21.csv")



ALL_YEARS_DATA_PCT_POPCHANGE_21 %>%
  arrow::write_parquet(
    sink = "Data/ALL_YEARS_DATA_PCT_POPCHANGE_21.parquet",
    compression = "uncompressed")
# readr::write_csv(x, file = csv)
ALL_YEARS_DATA_PCT_POPCHANGE_21 =
  arrow::read_parquet(
    file = "Data/ALL_YEARS_DATA_PCT_POPCHANGE_21.parquet")


ALL_YEARS_DATA_PCT_POPCHANGE_21 %>%
  group_by(CENSUS_YEAR) %>%
  summarise(
    across(
      .cols = c(INC_BT_FAM_AVG, INC_BT_FAM_MED, INC_AT_FAM_AVG, INC_AT_FAM_MED),
      .fns = ~mean(.x, na.rm = T)
    )
  )

# ********************************************************************************
# CHECK DA COVERAGE BY YEAR
# ********************************************************************************

# load('K:/Projects/Advanced Analytics/1 Projects/2019/2019-253 Census database all years/RData files/ALL_YEARS_DATA.RData')
# DA_TIME_SERIES <- ALL_YEARS_DATA_PCT_21 %>% select(CENSUS_YEAR, DA_ID)
# DA_TIME_SERIES$count <- 1
#
# DA_TIME_SERIES <- DA_TIME_SERIES %>% spread(CENSUS_YEAR, count)
# # head(separate_DF, 10)
#
# DA_TIME_SERIES <- DA_TIME_SERIES %>% mutate_all(~replace(., is.na(.), 0))
#
# DA_TIME_SERIES <- DA_TIME_SERIES %>%
#   mutate(coverage_count = `2001` + `2006` + `2011` + `2016` + `2021`)
#
# coverage_count <- DA_TIME_SERIES %>% group_by(coverage_count) %>% tally()

# DA_TIME_SERIES <- DA_TIME_SERIES %>%
#   mutate(coverage = case_when(`2001` == 1 & `2006` == 1 & `2011` == 1 & `2016` == 1 ~ 'all',
#                               `2001` == 1 & is.na(`2006`) & is.na(`2011`) & is.na(`2016`) ~ 'just 2001',
#                               is.na(`2001`) & `2006` == 1 & is.na(`2011`) & is.na(`2016`) ~ 'just 2006',
#                               is.na(`2001`) & is.na(`2006`) & `2011` == 1 & is.na(`2016`) ~ 'just 2011')
#   )

# ********************************************************************************
# WRITE TO EDW
# ********************************************************************************
source("c:/app/r/r_utility.r")
con = connect_edw("edw")
dbRemoveTable(con, name = "ECON_CENSUS_PCT_2001_TO_2021")
dbCreateTable(con, name = "ECON_CENSUS_PCT_2001_TO_2021",  fields = head(ALL_YEARS_DATA_PCT_POPCHANGE_21, 0))
dbReadTable(con, name = "ECON_CENSUS_PCT_2001_TO_2021")
dbAppendTable(con, name = "ECON_CENSUS_PCT_2001_TO_2021", value = ALL_YEARS_DATA_PCT_POPCHANGE_21)
# test = dbReadTable(con, name = "ECON_CENSUS_PCT_2001_TO_2021")


# ********************************************************************************
# Get the postal code for 2021. Monica did the postal code for 2011, 2006, and 2001 manually using information from UT since TMF does not have postal code for them.
# ********************************************************************************

#  the TRANSLATION_MASTER_DEC_2022 only has the CENSUS 2016 DA and postal code. wait?
# We are going to do it in another file.



# rcon <- RODBC::odbcConnect(
#   dsn = "EDW64",
#   uid = "edw_research",
#   pwd = "&Appl3C1d3r")
#
# # write to table
# RODBC::sqlSave(rcon, CA16_DATA_PCT,
#                tablename = 'TMP_MONICA_CENSUS_2016',
#                rownames = FALSE)

# # drop table
# sqlDrop(rcon, "TMP_MONICA_CENSUS_2006", errors = FALSE)
#
# sqlDrop(rcon, "ECON_CENSUS_2006_2011_2016", errors = FALSE)

