# this file is used for outputting data for importing to DIP

pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb)



# 1. BC Translation_Master_File
TMF %>% readr::write_csv(here::here("out", "Translation_Master_File_DIP.csv"))

# 2. BC Census 2021
bc_da %>% readr::write_csv(here::here("out", "StatsCAN_Census_21_BC_DA_DIP.csv")   )

# 3. BC SGC structure: Statistics Canada’s official classification for geographic areas
BC_SGC_structure %>% readr::write_csv(here::here("out", "BC_SGC_Structure_21_DIP.csv")   )
