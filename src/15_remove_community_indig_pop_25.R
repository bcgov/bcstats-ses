# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(DBI)
library(odbc)
library(bcdata)
## opinionated options
options(scipen = 999) #prevent scientific notation in numbers
source("./src/utils.R") # get the functions for plotting maps

# Load configuration using config package
# This will automatically look for a file named config.yml in the current and parent directory
config <- config::get()


#Setting paths

indig_population_file_path = file.path(
  config$lan_path,
  config$file_path$indigenous_pop_file_path,
  config$file_name$indigenous_pop_file_name
)

#1 load indigenous data
indig_population <- read_csv(indig_population_file_path)

#for CHSA level geography
indig_population_total_CHSA <- indig_population %>%
  filter(Gender == "Total", Region.Type == 'Community Health Service Area') %>%
  select(Region, Region.Name, Year, `Indigenous Identity`, TOTAL) %>%
  mutate(Region = as.character(Region))

#for CSD level geography
con <- dbConnect(
  odbc::odbc(),
  Driver = config$data_server$driver,
  Server = config$data_server$server,
  Database = config$data_server$database,
  Trusted_Connection = "Yes"
)

indig_pop_total_CSD <- dbGetQuery(
  con,
  "select ALT_GEO_CODE, GEO_NAME, 
case when GEO_NAME like '%(IRI)%' or GEO_NAME like '%(NL)%' or GEO_NAME like '%(IGD)%' or GEO_NAME like '%(TAL)%' then 'Y' else 'N' end as Indigenous_community,
sum(case when CHARACTERISTIC_ID=1 and c1_count_total is not null then c1_count_total else 0 end) as Total_Pop,
sum(case when CHARACTERISTIC_ID=1403 and c1_count_total is not null then c1_count_total else 0 end) as Indigenous_Pop,
sum(case when CHARACTERISTIC_ID=1403 and c1_count_total is not null then c1_count_total else 0 end)/sum(case when CHARACTERISTIC_ID=1 and c1_count_total is not null then c1_count_total else 0 end) as pcnt_indigenous
from [Population_Labour_Social].[Prod].[FCT_CENSUS_2021_BC_CSD_UD]
where 
CHARACTERISTIC_ID in (1,1403) and
c1_count_total>0
group by ALT_GEO_CODE, GEO_NAME, 
case when GEO_NAME like '%(IRI)%' or GEO_NAME like '%(NL)%' or GEO_NAME like '%(IGD)%' or GEO_NAME like '%(TAL)%' then 'Y' else 'N' end
order by pcnt_indigenous
"
)

indig_pop_total_CSD |> glimpse()

#close connection
dbDisconnect(con)
indig_pop_total_CSD <- indig_pop_total_CSD %>%
  mutate(ALT_GEO_CODE = as.character(ALT_GEO_CODE)) |>
  mutate(pcnt_indigenous = Indigenous_Pop / Total_Pop) |>
  mutate(
    pcnt_indigenous_25 = ifelse(pcnt_indigenous > .25, 1, 0)
  ) %>%
  arrange(desc(pcnt_indigenous))

#2a load pop projections
# URL for Community Health Service Areas population estimates
# url2 <- "https://catalogue.data.gov.bc.ca/dataset/86839277-986a-4a29-9f70-fa9b1166f6cb/resource/8a4866ba-cf90-4688-aacc-378227eaac2e/download/community-health-service-area-population.csv"
# Read the CSV directly into R
# chsa_population <- read_csv(url2, col_types = cols(Gender = col_character()))
# Alternatively, use the bcdata package to access BC Data Catalogue
# Set the record ID for the dataset
record_id <- "86839277-986a-4a29-9f70-fa9b1166f6cb"

#####################################################################
# Pull the record’s full metadata
rec <- bcdc_get_record(record_id)

# Tidy list of resources (IDs, names, formats, download URLs)
bcdc_tidy_resources(rec)
resource_id <- "8a4866ba-cf90-4688-aacc-378227eaac2e"

chsa_population <- bcdc_get_data(
  record = record_id,
  resource = resource_id,
  col_types = cols(Gender = col_character())
)


#2b normalize pop projections
chsa_population_total <- chsa_population %>%
  filter(Gender == "T") %>%
  select(Region, Region.Name, Year, Total) %>%
  mutate(Region = as.character(Region))


#3 join the 2 population sets to get proportion indigenous by CHSA
chsa_Indig_pop <- chsa_population_total %>%
  left_join(indig_population_total_CHSA, by = c("Year", "Region")) %>%
  mutate(pcnt_indigenous = TOTAL / Total) %>%
  filter(Year <= "2025", `Indigenous Identity` == "Total - Indigenous") %>% #change this to filter on different identities: First Nations , Total - Indigenous, why 2023
  select(
    Year,
    Region,
    Region.Name = Region.Name.x,
    Total_population = Total,
    `Indigenous Identity`,
    Indigenous_population = TOTAL,
    pcnt_indigenous
  ) %>%
  mutate(
    pcnt_indigenous_25 = ifelse(pcnt_indigenous > .25, 1, 0)
  ) %>%
  arrange(desc(pcnt_indigenous))

chsa_Indig_pop %>%
  filter(pcnt_indigenous > .25) %>%
  distinct(Region) %>%
  summarise(count = n())
#232 CHSAs,  26 CHSAs >.25

#------------------------------------------------------------------------------------------------------
#4 load SEI data
SEI_DET_CHSA <- read_csv(
  file = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_DET_CHSA
  ),
  col_types = cols(CHSA_UID = col_character())
)

SEI_LONG_CHSA <- read_csv(
  file = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_LONG_CHSA
  ),
  col_types = cols(CHSA_UID = col_character())
)


#5 by CHSA join to SEI
SEI_DET_CHSA_Indig_pop_25 <- SEI_DET_CHSA %>%
  left_join(
    chsa_Indig_pop |> select(Region, Year, pcnt_indigenous_25),
    by = c("CHSA_UID" = "Region", "CALENDAR_YEAR" = "Year")
  ) |>
  filter(pcnt_indigenous_25 == 0 | is.na(pcnt_indigenous_25)) |>
  select(-pcnt_indigenous_25)

#export SEI_DET_CHSA without communities with >25% indigenous population
SEI_DET_CHSA_Indig_pop_25 |>
  write_csv(file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    paste0(config$file_name$SEI_DET_CHSA, "_no_indig25.csv")
  ))

# by CHSA join to SEI LONG
# keep consistent with DET, we filter out CHSAs that are filterd out in DET.
SEI_LONG_CHSA_Indig_pop_25 <- SEI_LONG_CHSA %>%
  filter(CHSA_UID %in% SEI_DET_CHSA_Indig_pop_25$CHSA_UID)

#export SEI_LONG_CHSA without communities with >25% indigenous population
SEI_LONG_CHSA_Indig_pop_25 |>
  write_csv(file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    paste0(config$file_name$SEI_LONG_CHSA, "_no_indig25.csv")
  ))


#------------------------------------------------------------------------------------------------------

SEI_DET_CSD <- read_csv(
  file = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_DET_CSD
  ),
  col_types = cols(CSD_UID = col_character())
)

SEI_LONG_CSD <- read_csv(
  file = file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    config$file_name$SEI_LONG_CSD
  ),
  col_types = cols(CSD_UID = col_character())
)


#6 join the population sets to get proportion indigenous by CSD
SEI_DET_CSD |> glimpse()

CSD_Indig_pop_SEI <- SEI_DET_CSD |>
  left_join(
    indig_pop_total_CSD |> mutate(ALT_GEO_CODE = as.character(ALT_GEO_CODE)),
    by = c("CSD_UID" = "ALT_GEO_CODE")
  )

# validate join
CSD_Indig_pop_SEI %>%
  filter(TOTAL_INDEX_0_100 != 'masked') %>%
  # filter(Indigenous_community == "Y") %>%
  # filter(Indigenous_community == "Y", pcnt_indigenous > .25) %>%
  filter(Indigenous_community == "Y" | pcnt_indigenous > .25) %>%
  distinct(CSD_UID) %>%
  summarise(count = n())
#751 CSDs, 609 non masked CSDs, 289 indigenous communities (320 other), 304 indigenous communities or >.25, 204 indigenous communities and >.25, 15 non indigenous and >.25

SEI_DET_CSD_Indig_pop_25 <- SEI_DET_CSD %>%
  left_join(
    indig_pop_total_CSD |>
      select(ALT_GEO_CODE, pcnt_indigenous_25, Indigenous_community),
    by = c("CSD_UID" = "ALT_GEO_CODE")
  ) %>%
  filter(pcnt_indigenous_25 == 0 | is.na(pcnt_indigenous_25)) %>%
  filter(Indigenous_community != "Y" | is.na(Indigenous_community)) |>
  select(-pcnt_indigenous_25, -Indigenous_community)


#export CSD SEI without communities with >25% indigenous population
SEI_DET_CSD_Indig_pop_25 |>
  write_csv(file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    paste0(config$file_name$SEI_DET_CSD, "_no_indig25.csv")
  ))

SEI_LONG_CSD_Indig_pop_25 <- SEI_LONG_CSD %>%
  filter(CSD_UID %in% SEI_DET_CSD_Indig_pop_25$CSD_UID)


#export CSD SEI without communities with >25% indigenous population
SEI_LONG_CSD_Indig_pop_25 |>
  write_csv(file.path(
    config$lan_path,
    config$file_path$sei_file_path,
    paste0(config$file_name$SEI_LONG_CSD, "_no_indig25.csv")
  ))

##########################################################################################
# 7 explore data, visualizations, verify removal of indig >.25
#by CHSA
chsa_Indig_pop_SEI_DET <- chsa_Indig_pop %>%
  left_join(
    SEI_DET_CHSA %>%
      select(CALENDAR_YEAR, CHSA_UID, TOTAL_INDEX_0_100, SEI_INDEX_0_100),
    by = c("Region" = "CHSA_UID", "Year" = "CALENDAR_YEAR")
  )

chsa_Indig_pop_SEI_DET <- chsa_Indig_pop_SEI_DET %>%
  mutate(SEI_INDEX_0_100 = as.numeric(SEI_INDEX_0_100))

#--------------------------------------------------
#charts
#CHSA TOTAL_INDEX_0_100 and pct indig
ggplot(
  chsa_Indig_pop_SEI_DET,
  aes(x = pcnt_indigenous, y = TOTAL_INDEX_0_100)
) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Total Index", x = "pcnt_indigenous", y = "Total_Index") +
  theme_minimal()

#CHSA SEI and pct indig
ggplot(chsa_Indig_pop_SEI_DET, aes(x = pcnt_indigenous, y = SEI_INDEX_0_100)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "SEI Index", x = "pcnt_indigenous", y = "SEI") +
  theme_minimal()


#CSD TOTAL_INDEX_0_100 and pct indig
ggplot(CSD_Indig_pop_SEI, aes(x = pcnt_indigenous, y = TOTAL_INDEX_0_100)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "SEI Index", x = "pcnt_indigenous", y = "SEI") +
  theme_minimal()
#CSD SEI and pct indig
ggplot(CSD_Indig_pop_SEI, aes(x = pcnt_indigenous, y = SEI_INDEX_0_100)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "SEI Index", x = "pcnt_indigenous", y = "SEI") +
  theme_minimal()
