# this file is used for clean the data for joining those tables

pacman::p_load(cancensus,geojsonsf, tidyverse,config,bcmaps, bcdata, janitor,cansim,safepaths, arrow, duckdb)



# 1. BC Translation_Master_File
# TMF
# TMF %>% glimpse()
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

TMF_DA_LEVEL  <-  TMF %>% 
  filter(ACTIVE == "Y") %>% 
  count(DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021)  


# 6967



# It seems not every DA has one to one matching to CHSA regions such as CHSA. So to join to other tables with other regions, we will need TO separate the DA into different regions using population as weights.  
# 


TMF_DA_CHSA_LEVEL  <-  TMF %>% 
  filter(ACTIVE == "Y") %>% 
  count(DA_NUM, CD_2021, CSD_2021, DA_2021, MUN_NAME_2021, CHSA)

# 7067

# 




# 2. BC Census 2021
# Census 2021


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





# 3. BC SGC structure: Statistics Canada’s official classification for geographic areas


BC_SGC_structure %>% 
  count(HIERARCHICAL_STRUCTURE)
# 29 CD and 751 CSDs in BC


BC_SGC_element %>% 
  count(`HIERARCHICAL_STRUCTURE`)
# 29 CD and 751 CSDs in BC. the same as census



