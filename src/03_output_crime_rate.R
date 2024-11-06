# this file is used for cleaning the crime rate Excel file and exporting to DIP.
# library("remotes")
# install_github("bcgov/safepaths")
pacman::p_load( tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb)

######################################################################################
# Crime rate data
# 
######################################################################################

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
print("file out of date: this could take a while")

# Use Case
# get_cansim_sqlite is particularly beneficial when working with large tables or when you need to perform complex filtering operations before bringing data into memory. For example, table 35-10-0184-01 is several gigabytes in size, making it impractical to load entirely into memory.
# However, for smaller tables that can be easily processed in memory, get_cansim may still be a simpler and more straightforward option. The choice between the two functions depends on the specific requirements of your data analysis task and the size of the dataset you're working with.
# it was supposed to save sqlite to a cache folder. 
cansim_id <- "35-10-0184-01"
# it is too slow to index in sqlite on a network drive, so switch to a local folder, and create a copy from LAN use_network_path("data/cansim_cache") to C drive repo_folder/data. 
options(cansim.cache_path = "./data")
# options(cansim.cache_path = use_network_path("data/cansim_cache"))
getOption("cansim.cache_path")
connection <- cansim::get_cansim_sqlite(cansim_id,
                                        # auto_refresh=TRUE,
                                        cache_path = getOption("cansim.cache_path")
                                        # refresh=TRUE # only occasionally refresh
)
# ignore the warning, the cache does reflect the date right. It is retrieved in July 30th, so it is updated. 
# 
connection %>% glimpse()
# 
# 
Violations_list = connection %>%
  count(Violations) %>%
  collect()
# # 314 types of crime
# we should choose the most important ones. 

# BC stats previous SES project: 
# The BC stats "Definition and Data Sources, BC Socio-Economic Indices and Profiles" provide a list of potential variables for our project.   


# 1. The Crime Rate is the number of offences per 1,000 population (per 1,000 population 12-17 for juvenile crime rates). Data are 3-year averages. 
# 2. Serious violent crime rate is based on reporting within the crime categories of homicide, attempted murder, sexual and non-sexual assault (level 2 and 3: resulting in bodily harm, wounding, disfiguring, maiming or endangering the life of someone) as well as robbery and abduction. Data are 3-year averages. 
# 3. Breaking & Entering is the only property crime included in the "Serious Property" crime rate. Serious Property Crimes exclude Motor Vehicle Theft (shown separately) and minor crimes such as bicycle theft and pick pocketing. Data are 3-year averages. 
# 4. Total Serious Crime rate is the total of the serious violent crime rate and the serious property crime rates. Data are 3-year averages. 
# 5. The Number of Serious Crime Offences per Police Officer is the total number of serious offences defined above divided by the police strength. Data are 3-year averages. 
# 6. Change in Crime rate is the % change in the serious crime rate average for the latest 3 years over the serious crime rate for the previous 3 years. Data represent changes in 3-year averages. 
# 7. Since a high percent of Motor Vehicle Thefts remain unsolved, generalizing about the characteristics of offenders is difficult. Data are 3-year averages. 
# 8. Drug offence rates include possession and trafficking/importing/cultivation of illegal drugs excluding cannabis. Variations in the rates between regions may reflect differing police enforcement of drug possession between jurisdictions as well as differing levels on non-cannabis drug use. Total rates are based on number of offences (charges for juveniles) and are reported per 100,000 population. Data are 3-year averages. 
# 9. Illicit Drug Deaths are unintentional (accidental) deaths due to illicit drugs based on the permanent residence of the victim. Data are 3-year averages. The deaths are based on the following International Classification of Disease( ICD-10 )codes for illicit drug deaths: 
#   
#   D521, D590, D592, D611, D642, E032, E064, E231, E242, E273, F55, F551, G210, G211, G240, 
# G251, G254, G256, G444, G620, G720, H263, I427, I952, J702, J703, J704, L105, L233, L244, L251, L270, L271, L432, L560, L561, L640, M022, M102, M320, M804, M814, M835, M871, N140, N141, N142, O355, P040, P041, P044, P584, P961, P962, R781, R782, R783, R784, R785, R786, R825. 
# 
# http://apps.who.int/classifications/icd10/browse/2010/en 
# 
# 10.Juvenile crime rates. See crime rate definitions above. Note that juvenile rates are based on charges as it is only when a charge is laid that the age of the suspect is determined. 
# 


# Brett's idea
# Total, all Criminal Code violations (excluding traffic) [50]
# Total violent Criminal Code violations [100]
# Homicide [110]

crime_GEO_list = connection %>%
  count(GEO ) %>%
  collect()
# # 237 regions: Policing district/zone. id Police Services Respondent Codes: RESP like 59774
#  [59774] need to parse out and join to TMF RESP


# Only look at data after 2000
bc_crime_stats <- connection %>%
  filter(
    # GEO=="British Columbia",
    # str_starts( GeoUID, "59"),
    REF_DATE >="2000",
    Violations %IN% c("Total, all Criminal Code violations (excluding traffic) [50]",
                      "Total violent Criminal Code violations [100]",
                      "Homicide [110]")  ,
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


TMF %>%
  filter(ACTIVE == "Y") %>%
  count(RESP)
# # 193
TMF %>%
  filter(ACTIVE == "Y") %>%
  count(DA_2021)
# 3,714 DA
TMF %>%
  filter(ACTIVE == "Y") %>%
  count(CD_2021)
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
  count(RESP,DA_2021,  CD_2021, CSD_2021,MUN_NAME_2021) %>% 
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
