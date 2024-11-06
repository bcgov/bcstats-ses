# this file is used for outputting data for importing to DIP

pacman::p_load(cancensus, geojsonsf, tidyverse, config, bcmaps, bcdata, janitor, cansim, safepaths, arrow, duckdb, datadictionary)

LFS_file = use_network_path("data/StatsCanLFS/1410045701_databaseLoadingData.csv")

LFS = readr::read_csv(LFS_file)

# only BC
LFS = LFS |>
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

# the geography type (CA = "census agglomeration", CMA = "census metropolitan area" and SLA = "self-contained labour area") is printed in square brackets at the end of GEO. This is not good. Let's split this type out in a new column called GEO_TYPE and remove it from GEO.
LFS = LFS |>
  mutate(GEO_TYPE = str_extract(GEO, "\\[(.+)\\]", group=1), .after='GEO') |>
  mutate(GEO = str_remove(GEO, " \\[.+\\]"))

# We can see that UOM and UOM_ID are redundant so let's get rid of UOM_ID
select(LFS, UOM, UOM_ID) |> unique()
LFS = select(LFS, -UOM_ID)

# DECIMALS is also redundant with UOM so let's drop it
LFS |> select(UOM, DECIMALS) |> unique()
LFS = select(LFS, -DECIMALS)

# Convert cols GEO_TYPE, LABOUR_FORCE_CHARACTERISTICS, STATISTICS, UOM and STATUS to factors
LFS = LFS |> mutate(across(c(GEO_TYPE, LABOUR_FORCE_CHARACTERISTICS, STATISTICS, UOM, STATUS), as.factor))

# lookin' good
print(LFS)

readr::write_csv(LFS, here::here("out", "Labour_Fource_Survey_DIP.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

LFS_dict_labels = c(
  "REF_DATE" = "The month and year of the observation (in '%b %Y' format)",
  
  "GEO" = "Geographical Area of BC.\nExcluded from the coverage of the estimates are persons living on reserves and other Indigenous settlements in the provinces, full-time members of the Canadian Armed Forces, the institutionalized population, and households in extremely remote areas with very low population density. These groups together represent an exclusion of approximately 2% of the population aged 15 and older.",
  
  "GEO_TYPE" = "A census metropolitan area (CMA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CMA must have a total population of at least 100,000 of which 50,000 or more must live in the core. 
  
  A census agglomeration (CA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CA must have a core population of at least 10,000 based on data from the previous Census of Population Program. 
  
  A self-contained labour area (SLA) is a functional area composed of census subdivisions which are not already included in a CMA or CA. All three types of regions are determined using commuting flows derived from census program place of work data.",
  
  "LABOUR_FORCE_CHARACTERISTICS" = "The employment rate is the small area estimate of the number of employed persons expressed as a percentage of the population 15 years of age and older. Estimates are percentages, rounded to the nearest tenth.
  
  The unemployment rate is the number of unemployed people as a percentage of the labour force (employed and unemployed). The unemployment rate is the number of unemployed persons expressed as a percentage of the labour force. Unemployed persons are those who were without work, had looked for work in the past four weeks, and were available for work. Those persons on layoff or who had a new job to start in four weeks or less are also considered unemployed. The labour force is all civilian, non-institutionalized persons 15 years or age and older who were employed or unemployed. Estimates are percentages, rounded to the nearest tenth.
  
  Employment is the small area estimate of the number of persons who worked for pay or profit, or had a job but were not at work due to own illness or disability, personal or family responsibilities, labour dispute, vacation, or other reason. Estimates are rounded to the nearest ten.",
  
  "STATISTICS" = "The statistic being measured",
  
  "UOM" = "Count of people or percentage",
  
  "VECTOR" = "A unique identifier assigned to a specific geographic unit or spatial feature", 
  
  "COORDINATE" = "The geographical position of the geographical point on the Earth's surface,  expressed in terms of latitude and longitude",
  
  "VALUE" = "The value of the observation", 
  
  "STATUS" = "One of 'E' (use with caution) or 'F' (too unreliable to be published)."
)

LFS_dict = create_dictionary(LFS, var_labels = LFS_dict_labels)

write.csv2(LFS_dict, here::here("out/Labour_Force_Survey_Dict_DIP.csv"))
