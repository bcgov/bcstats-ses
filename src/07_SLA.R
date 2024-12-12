# Copyright 2024 Province of British Columbia
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

# waiting for a dictionary from Brett - see above
SLA_dict_labels = c(
  "SLA_NAME" = "Self-contained Labour Areas (SLA). SLA are functional areas composed of Census Subdivisions (CSD) grouped according to commuting patterns (OECD, 2020)."
)

SLA_dict = create_dictionary(SLA, var_labels = SLA_dict_labels)

write.csv2(SLA_dict, here::here("out/SLA_Dict_DIP.csv"))