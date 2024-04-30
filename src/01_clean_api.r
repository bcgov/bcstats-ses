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
