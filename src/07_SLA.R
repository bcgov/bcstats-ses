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

# this file is used for outputting data for importing to DIP



# Quick comment: Brett is on vacation right now (2024-12-02) and I'm hoping he has some background information on this dataset (for example, what columns do we care about, etc.). I'm forging ahead in the meantime using my intuition.

pacman::p_load(tidyverse, config, bcdata, janitor, safepaths, datadictionary)

SLA_file = use_network_path("data/StatsCanLFS/SLA2016_FinalClassification.xlsx")
SLA = readxl::read_excel(SLA_file)

# only BC - this is weird but I think it's correct - since when was BC the 59th province?
SLA = SLA |>
  filter(PR == "59") |>
  select(-PR)

# Clean the names as per previous work
SLA = SLA |> 
  rename(CSDName = CSDname, CSDType = CSDtype) |>
  janitor::clean_names(case = "screaming_snake")

# The most granular column is CSD. Note that they all begin with '59'.
SLA |>
  pull(CSD) |>
  substr(1,2) |>
  unique()

# So let's dump that 59 nonsense
SLA$CSD = substr(SLA$CSD, 3, nchar(SLA$CSD))

# convert everything to characters. This is because it appears that other files, such as the TMF, don't do integers (which would be my preference, for the record).
SLA = SLA |>
 mutate(across(everything(), as.character))

# if the SLA NAME is null the code should be too, no?
SLA = SLA |>
  mutate(SLA_CODE = case_when(is.na(SLA_NAME) ~ NA_character_, T ~ SLA_CODE))

# there is some wackiness involving SLA_CODE and CMACASLA_CODE. For example: there are zero rows here, indicating that these columns are redundant.
SLA |> na.omit() |> filter(SLA_CODE != CMACASLA_CODE)

# however, this indicates that CMACASLA_CODE is more granular than SLA_CODE. Why?????????????? In any case, we'll just keep all the columns and let the benighted data scientist have to sort this out
SLA |> filter(is.na(SLA_CODE)) |> select(1:3)

# Another Issue: using SLA makes for a bad join key because there are duplicate rows. For example:
SLA |> count(SLA_CODE)

# Joining to other tables in this project is not perfectly straightforward. As discussed, you want to join on CSD. Here's an example:

# SLA |> inner_join(TMF, by=join_by(CSD == CDCSD_2021))
# A tibble: 133,958 × 76


# lookin' good
print(SLA)

readr::write_csv(SLA, here::here("out", "SLA_DIP.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

# my attempt at a data dictionary. Sources are: G:/Operations/Data%20Science%20and%20Analytics/2024%20SES%20Index/data/StatsCanLFS/dq240917d-eng.pdf and https://www.statcan.gc.ca/en/subjects/standard/sgc/2016/definitions
SLA_dict_labels = c(
  "SLA_CODE" = "Code for the SLA",
  "SLA_NAME" = "Self-contained Labour Areas (SLA). SLA are functional areas composed of Census Subdivisions (CSD) grouped according to commuting patterns (OECD, 2020).",
  "CMACASLA_CODE" = "Code for CMA, CA, and/or SLA",
  "CSD" = "Census subdivision (CSD) is the general term for municipalities (as determined by provincial/territorial legislation) or areas treated as municipal equivalents for statistical purposes (e.g., Indian reserves, Indian settlements and unorganized territories).",
  "CMA" = "A census metropolitan area (CMA) or a census agglomeration (CA) is formed by one or more adjacent municipalities centred on a population centre (known as the core). A CMA must have a total population of at least 100,000 of which 50,000 or more must live in the core, based on adjusted data from the previous census. A CA must have a core population of at least 10,000, also based on data from the previous census. To be included in the CMA or CA, other adjacent municipalities must have a high degree of integration with the core, as measured by commuting flows derived from data on place of work from the previous census.",
  "CSD_NAME" = "Name of the CSD",
  "CSD_TYPE" = "Type of CSD"
)

SLA_dict = create_dictionary(SLA, var_labels = SLA_dict_labels)

write.csv2(SLA_dict, here::here("out/SLA_Dict_DIP.csv"))
