
######################################################################################
# 
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
# it is important to note that not all of the 837 CSD’s in the province will be represented on the TMF(Translation Master File which is the BC Stats official classification for geographic areas), as the appearance of
# a CSD on the TMF is dependent upon whether or not there is a postal code geocoded to that area. A large number of
# the RDEA’s and the vast majority of IRs are not represented on the TMF

# Census Subdivision (CSD) CSDs are municipalities (as determined by provincial legislation) or areas treated as municipal equivalents 
# 
######################################################################################


# the location is saved in config.yaml file. 


# set up CENSUSMAPPER API 

# set_cancensus_api_key(config::get("CENSUSMAPPER_API_KEY"), install = TRUE)

# In order to speed up performance, reduce API quota usage, and reduce unnecessary network calls, please set up a persistent cache directory via `set_cancensus_cache_path('<local cache path>', install = TRUE)`.
# This will add your cache directory as environment variable to your .Renviron to be used across sessions and projects.
# set_cancensus_cache_path(use_network_path("data/census_cache"), install = TRUE)
# or
# options(cancensus.api_key = "your_api_key")
# options(cancensus.cache_path = "custom cache path")

# options(cancensus.api_key = config::get("CENSUSMAPPER_API_KEY"))
# options(cancensus.cache_path = use_network_path("data/census_cache"))

# getOption("cancensus.cache_path")
# Your cache path has been stored in your .Renviron and can be accessed by 
Sys.getenv("CM_CACHE_PATH")

list_census_datasets()


# This test task has been done, so skip it. We can delete this part after the review
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
vector_list_21  <-  list_census_vectors("CA21")
# A tibble: 7,709 × 7


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

# The better way may be to get the vecor list first, and then retrieve the meta data using cancensus function, at the end, create variable name in a csv file and use that file to create this named vector list. 
# We want to keep the vector which is the key to link cancensus table. 
# maybe we only get a long form table, and pivot to a wide table later. 