library(cancensus)
# In order to speed up performance, reduce API quota usage, and reduce unnecessary network calls, please set up a persistent cache directory via `set_cancensus_cache_path('<local cache path>', install = TRUE)`.
# This will add your cache directory as environment varianble to your .Renviron to be used across sessions and projects.
set_cancensus_cache_path('<local cache path>', install = TRUE)
library(sf)
# retrieve sf dataframe
toronto <- get_census(dataset='CA21', regions=list(CMA="35535"),
                      vectors=c("median_hh_income"="v_CA21_906"), level='CSD', quiet = TRUE, 
                      geo_format = 'sf', labels = 'short')