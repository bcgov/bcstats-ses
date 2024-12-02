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

pacman::p_load(cancensus, geojsonsf, tidyverse, config, bcmaps, bcdata, janitor, cansim, safepaths, arrow, duckdb, datadictionary)

SLA_file = use_network_path("data/StatsCanLFS/SLA2016_FinalClassification.xlsx")
SLA = readxl::read_excel(SLA_file)

# only BC - but note this is weird - since when was BC the 59th province?
SLA = SLA |>
  filter(PR == "59")

# clean the names. We prefer all uppercase
SLA = SLA |> 
  rename(CSDName = CSDname, CSDType = CSDtype) |>
  janitor::clean_names(case = "screaming_snake")

# convert to integer where appropriate
SLA = SLA |>
  mutate(across(c(SLA_CODE, CMACASLA_CODE, PR, CSD, CMA), as.integer))

# if the SLA NAME is null the code should be too, no?
SLA = SLA |>
  mutate(SLA_CODE = case_when(is.na(SLA_NAME) ~ NA_integer_, T ~ SLA_CODE))

SLA = SLA |>
  arrange(SLA_CODE)

# lookin' good
print(SLA)

readr::write_csv(LFS, here::here("out", "SLA_DIP.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

# waiting for a dictionary from Brett
SLA_dict_labels = c(
  "SLA_NAME" = "Self-contained Labour Areas (SLA). SLA are functional areas composed of Census Subdivisions (CSD) grouped according to commuting patterns (OECD, 2020)."
)

SLA_dict = create_dictionary(SLA, var_labels = SLA_dict_labels)

write.csv2(SLA_dict, here::here("out/SLA_Dict_DIP.csv"))