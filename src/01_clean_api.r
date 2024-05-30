pacman::p_load(cancensus,geojsonsf)
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
van <- get_census(dataset='CA21', regions=list(CMA="59933"),
                      vectors=c("median_hh_income"="v_CA21_906"), level='CSD', quiet = TRUE, 
                      geo_format = 'sf', labels = 'short')

# DA	Dissemination Area	56589
# DB	Dissemination Block (2001-2016)	489676

van_db <- get_census(dataset='CA21', regions=list(CMA="59933"),
                  vectors=c("median_hh_income"="v_CA21_906"), level='DB', quiet = TRUE, 
                  geo_format = NA, labels = 'short')


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

# (sum(case when CHARACTERISTIC_ID in ('14','15') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('8') and c1_count_total>0 then c1_count_total end)) as PCT_15_24,
# 
# sum(case when CHARACTERISTIC_ID = '115' then c1_count_total end) as MED_AT_INC,
# 
# sum(case when CHARACTERISTIC_ID = '345' then c1_count_total end)/100 as LIM_AT,
# 
# sum(case when CHARACTERISTIC_ID = '6' then c1_count_total end) as POP_DENS,
# 
# sum(case when CHARACTERISTIC_ID in ('35','37') then c1_count_total end)/100 as DEP_RAT,
# 
# (sum(case when CHARACTERISTIC_ID in ('42') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('41') and c1_count_total>0 then c1_count_total end)) as DET_HOMES,
# 
# (sum(case when CHARACTERISTIC_ID in ('86') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('78') and c1_count_total>0 then c1_count_total end)) as SING_PAR,
# 
# sum(case when CHARACTERISTIC_ID = '151' then c1_count_total end)/100 as GOV_TRANS,
# 
# (sum(case when CHARACTERISTIC_ID in ('1403') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1402')  and c1_count_total>0 then c1_count_total end)) as PCT_INDIG,
# 
# (sum(case when CHARACTERISTIC_ID in ('1416') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1414')  and c1_count_total>0 then c1_count_total end)) as PCT_RENT,
# 
# (sum(case when CHARACTERISTIC_ID in ('1537') and c1_count_total is not null then c1_count_total else 0 end) / sum(case when CHARACTERISTIC_ID in ('1')  and c1_count_total>0  then c1_count_total end)) as PCT_NPR,
# 
# (sum(case when CHARACTERISTIC_ID in ('1684') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1683')  and c1_count_total>0 then c1_count_total end)) as PCT_VISMIN,
# 
# (sum(case when CHARACTERISTIC_ID in ('1996') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('1995')  and c1_count_total>0 then c1_count_total end)) as PCT_NOGRAD,
# 
# (sum(case when CHARACTERISTIC_ID in ('2224') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2223')  and c1_count_total>0 then c1_count_total end)) as LAB_PART_RATE,
# 
# (sum(case when CHARACTERISTIC_ID in ('2226') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2224')  and c1_count_total>0 then c1_count_total end)) as LAB_UNEMP_RATE,
# 
# (sum(case when CHARACTERISTIC_ID in ('2249') then c1_count_total end) / sum(case when CHARACTERISTIC_ID in ('2246')  and c1_count_total>0 then c1_count_total end)) as PCT_MGMT_OCC
# 


CA21_VECTORS = c(
  'POP'             = 'v_CA21_1',  # Population, 2021
  
  'POP_PCT_CHANGE'  = 'v_CA21_3',  # Population percentage change, 2016 to 2021
  
  'POP_DENS' = 'v_CA21_6',  # POP_DENS
  
  'AGE_GRP_TOT'     = 'v_CA21_8',    # Total - Age
  'AGE_GRP_00_04'   = 'v_CA21_14',    # 0 - 4 years
  'AGE_GRP_00_15'   = 'v_CA21_15',    # 4 - 15 years
  
  'MED_AT_INC'   = 'v_CA21_115',    # MED_AT_INC
  
  'LIM_AT' = 'v_CA21_3455',    # LIM_AT
  
)




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






