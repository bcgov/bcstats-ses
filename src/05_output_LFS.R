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

# load LFS data from StatsCan. It's a small file so this doesn't take very long
cansim_id = "14-10-0457-01"
LFS_raw = cansim::get_cansim_sqlite(cansim_id) |>
  collect()  

# only BC
LFS = LFS_raw |>
  filter(str_detect(GEO, "British Columbia")) |>
  mutate(GEO = str_replace(GEO, ", British Columbia", ""))

# clean the names. We prefer all uppercase
LFS = LFS |> 
  janitor::clean_names(case = "screaming_snake" )

# VALUE has some NAs (corresponding to ".." for STATUS). I'm not sure what to do here -- remove? I'll keep them for now but recode STATUS so that it's "E" for "use with caution" and NA otherwise.

LFS |> filter(is.na(VALUE)) |> select(VALUE, STATUS) |> unique()
LFS = LFS |> mutate(STATUS = case_when(is.na(VALUE) ~ NA, T ~ STATUS))
LFS |> select(STATUS) |> unique()

# REF_DATE is monthly so we can use zoo's yearmon()
LFS = LFS |>
  mutate(REF_DATE = zoo::as.yearmon(LFS$REF_DATE))

# DGUID, SYMBOL, and TERMINATED are only NAs; dump 'em
LFS = LFS |> select(where(~ !all(is.na(.))))

# SCALAR_FACTOR and SCALAR_ID have only one unique value, "units" and 0, respectively. Dump these columns.
LFS = LFS |> 
  select(
    LFS |> 
      summarise(across(everything(), n_distinct)) |>
      pivot_longer(cols = everything()) |>
      filter(value > 1) |>
      pull(name)
  )

# I assume we only want the estimates (and not confidence interval boundaries, etc.).
LFS = LFS |>
  filter(STATISTICS == "Estimate") |>
  select(-STATISTICS)

# Do we want to pivot on LABOUR_FORCE_CHARACTERISTIC? I guess we can always do that later.

# the geography type (CA = "census agglomeration", CMA = "census metropolitan area" and SLA = "self-contained labour area") is printed in square brackets at the end of GEO. This is not good. Let's split this type out in a new column called GEO_TYPE and remove it from GEO.
LFS = LFS |>
  mutate(GEO_TYPE = str_extract(GEO, "\\[(.+)\\]", group=1), .after='GEO') |>
  mutate(REGION = str_remove(GEO, " \\[.+\\]")) |>
  select(-GEO)

# verify it looks nice
LFS$GEO_TYPE |> unique()
LFS$REGION |> unique()

# We can see that UOM, UOM_ID and DECIMALS are redundant
select(LFS, LABOUR_FORCE_CHARACTERISTICS, UOM, UOM_ID, DECIMALS) |> unique()
LFS = select(LFS, -UOM, -UOM_ID, -DECIMALS)

# We also don't need VECTOR and COORDINATE so let's select the columns we want
LFS = select(LFS, REF_DATE, GEO_TYPE, REGION, LABOUR_FORCE_CHARACTERISTICS, VALUE, STATUS)

# Convert cols GEO_TYPE, LABOUR_FORCE_CHARACTERISTICS, STATISTICS, UOM to factors
LFS = LFS |> mutate(across(c(GEO_TYPE, LABOUR_FORCE_CHARACTERISTICS, STATUS), as.factor))

# lookin' good
print(LFS)





# load SLA geography linkage file
SLA_file = use_network_path("data/StatsCanLFS/SLA2016_FinalClassification.xlsx")
SLA_raw = readxl::read_excel(SLA_file)

# only BC - this is weird but I think it's correct - since when was BC the 59th province?
SLA = SLA_raw |>
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


# this tibble will be used to fill in blanks in the SLA. It has been verified by Brett.
SLA_lookup = tribble(~CMA, ~SLA_NAME,
  905, "Cranbrook",
  907, "Nelson",
  910, "Trail",
  913, "Penticton",
  915, "Kelowna",
  918, "Vernon",
  920, "Salmon Arm",
  925, "Kamloops",
  930, "Chilliwack",
  932, "Abbotsford - Mission",
  933, "Vancouver",
  934, "Squamish",
  935, "Victoria",
  936, "Ladysmith",
  937, "Duncan",
  938, "Nanaimo",
  939, "Parksville",
  940, "Port Alberni",
  943, "Courtenay",
  944, "Campbell River",
  945, "Powell River",
  950, "Williams Lake",
  952, "Quesnel",
  955, "Prince Rupert",
  965, "Terrace",
  970, "Prince George",
  975, "Dawson Creek",
  977, "Fort St. John"
)

# fill in the blanks with SLA_lookup. I feel like there's a better way to do this but whatever
SLA = SLA |>
  left_join(SLA_lookup, by="CMA") |>
  mutate(SLA_NAME = case_match(SLA_NAME.x, NA ~ SLA_NAME.y, .default = SLA_NAME.x)) |>
  select(-SLA_NAME.x, -SLA_NAME.y)

# verify there are no NAs anymore
stopifnot(SLA |> is.na() |> sum() == 0)

# convert everything to characters. This is because it appears that other files, such as the TMF, don't do integers (which would be my preference, for the record).
SLA = SLA |>
  mutate(across(everything(), as.character))

# if the SLA NAME is null the code should be too, no?
SLA = SLA |>
  mutate(SLA_CODE = case_when(is.na(SLA_NAME) ~ NA_character_, T ~ SLA_CODE))

# all we need are SLA_NAME, CSD, CSD_NAME, CMA
SLA = SLA |> 
  select(SLA_NAME, CSD, CSD_NAME, CMA)


# This is how to join these tables. Note the many-to-many relationship!!!
LFS = LFS |>
  inner_join(SLA, by=join_by("REGION" == "SLA_NAME"), relationship = 'many-to-many')


readr::write_csv(LFS, here::here("out", "Labour_Fource_Survey_DIP.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

LFS_dict_labels = c(
  "REF_DATE" = "The month and year of the observation (in '%b %Y' format)",
  
  # "GEO" = "Geographical Area of BC.\nExcluded from the coverage of the estimates are persons living on reserves and other Indigenous settlements in the provinces, full-time members of the Canadian Armed Forces, the institutionalized population, and households in extremely remote areas with very low population density. These groups together represent an exclusion of approximately 2% of the population aged 15 and older.",
  
  "GEO_TYPE" = "A census metropolitan area (CMA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CMA must have a total population of at least 100,000 of which 50,000 or more must live in the core. 
  
  A census agglomeration (CA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CA must have a core population of at least 10,000 based on data from the previous Census of Population Program. 
  
  A self-contained labour area (SLA) is a functional area composed of census subdivisions which are not already included in a CMA or CA. All three types of regions are determined using commuting flows derived from census program place of work data.",
  
  "REGION" = "Self-contained Labour Areas (SLA) name. SLA are functional areas composed of Census Subdivisions (CSD) grouped according to commuting patterns (OECD, 2020).",
  
  "LABOUR_FORCE_CHARACTERISTICS" = "The employment rate is the small area estimate of the number of employed persons expressed as a percentage of the population 15 years of age and older. Estimates are percentages, rounded to the nearest tenth.
  
  The unemployment rate is the number of unemployed people as a percentage of the labour force (employed and unemployed). The unemployment rate is the number of unemployed persons expressed as a percentage of the labour force. Unemployed persons are those who were without work, had looked for work in the past four weeks, and were available for work. Those persons on layoff or who had a new job to start in four weeks or less are also considered unemployed. The labour force is all civilian, non-institutionalized persons 15 years or age and older who were employed or unemployed. Estimates are percentages, rounded to the nearest tenth.
  
  Employment is the small area estimate of the number of persons who worked for pay or profit, or had a job but were not at work due to own illness or disability, personal or family responsibilities, labour dispute, vacation, or other reason. Estimates are rounded to the nearest ten.",
  
  "VALUE" = "The value of the observation", 
  
  "STATUS" = "One of 'E' (use with caution) or 'F' (too unreliable to be published).",
  
  "CSD" = "Census subdivision (CSD) is the general term for municipalities (as determined by provincial/territorial legislation) or areas treated as municipal equivalents for statistical purposes (e.g., Indian reserves, Indian settlements and unorganized territories).",
  
  "CSD_NAME" = "Name of the CSD",
  
  "CMA" = "A census metropolitan area (CMA) or a census agglomeration (CA) is formed by one or more adjacent municipalities centred on a population centre (known as the core). A CMA must have a total population of at least 100,000 of which 50,000 or more must live in the core, based on adjusted data from the previous census. A CA must have a core population of at least 10,000, also based on data from the previous census. To be included in the CMA or CA, other adjacent municipalities must have a high degree of integration with the core, as measured by commuting flows derived from data on place of work from the previous census."

)

LFS_dict = create_dictionary(LFS, var_labels = LFS_dict_labels)

# since there are comma "," in the labels so use write.csv2 with semicolon ";" as delimiter. 
write.csv2(LFS_dict, here::here("out/Labour_Force_Survey_Dict_DIP.csv"))
