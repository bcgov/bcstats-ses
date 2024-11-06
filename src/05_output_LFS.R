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

LFS |> readr::write_csv(here::here("out", "LFS_DIP.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

TMF = read_csv("out/Translation_Master_File_DIP.csv")
TMF_dict = create_dictionary(TMF,
  id_var = "POSTALCODE",
  # file = "GCS202406_DICT.xlsx",
  var_labels = NULL)

LFS_dict = create_dictionary(LFS)

LFS_dict |> as_tibble()

tribble(~'Field Name', ~Description,
  "REF_DATE", "The month and year of the observation (in '%b %Y' format)",
  
  "GEO", "Geographical Area of BC.\nExcluded from the coverage of the estimates are persons living on reserves and other Indigenous settlements in the provinces, full-time members of the Canadian Armed Forces, the institutionalized population, and households in extremely remote areas with very low population density. These groups together represent an exclusion of approximately 2% of the population aged 15 and older.",
  
  "GEO_TYPE", "One of 'CMA', 'CA', or 'SLA'.
  
  A census metropolitan area (CMA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CMA must have a total population of at least 100,000 of which 50,000 or more must live in the core. 
  
  A census agglomeration (CA) is formed by one or more adjacent municipalities centered on a population centre known as the core. A CA must have a core population of at least 10,000 based on data from the previous Census of Population Program. 
  
  A self-contained labour area (SLA) is a functional area composed of census subdivisions which are not already included in a CMA or CA. All three types of regions are determined using commuting flows derived from census program place of work data.",
  
  "LABOUR_FORCE_CHARACTERISTICS", "One of 'Employment', 'Unemployment rate', or 'Employment rate'.
  
  The employment rate is the small area estimate of the number of employed persons expressed as a percentage of the population 15 years of age and older. Estimates are percentages, rounded to the nearest tenth.
  
  The unemployment rate is the number of unemployed people as a percentage of the labour force (employed and unemployed). The unemployment rate is the number of unemployed persons expressed as a percentage of the labour force. Unemployed persons are those who were without work, had looked for work in the past four weeks, and were available for work. Those persons on layoff or who had a new job to start in four weeks or less are also considered unemployed. The labour force is all civilian, non-institutionalized persons 15 years or age and older who were employed or unemployed. Estimates are percentages, rounded to the nearest tenth.
  
  Employment is the small area estimate of the number of persons who worked for pay or profit, or had a job but were not at work due to own illness or disability, personal or family responsibilities, labour dispute, vacation, or other reason. Estimates are rounded to the nearest ten.",
  
  "STATISTICS", "One of 'Estimate', 'Standard error', 'Lower bound of the 95% confidence interval', or 'Upper bound of the 95% confidence interval'",
  
  "UOM", "Unit of Measurement; One of 'Persons', or 'Percentage'",
  
  "VECTOR", "A unique identifier assigned to a specific geographic unit or spatial feature", 
  
  "COORDINATE", "The geographical position of the geographical point on the Earth's surface,  expressed in terms of latitude and longitude",
  
  "VALUE", "The value of the observation", 
  
  "STATUS", "One of 'E' (use with caution) or 'F' (too unreliable to be published)."
)

View(LFS_dict)

# f2 <- "https://www2.gov.bc.ca/assets/gov/health/forms/5512datadictionary.pdf"
# a better data dictionary in Geocoding Self-Service (GCS) User Guide Prepared by BC Stats March 2020, not online, provided by Brett.
# f3 <-  use_network_path("docs/GCS_User_Guide.pdf")

# it does not work well using pdf tools in R. so we had to manually make a csv file for it.

# manually create a item field in this detail dataframe to join the TMF_dict dataframe
# it is too hard to do it mannually. Chatgpt did it.

library(readr)
TMF_dict_detail <- read_csv( use_network_path("docs/TMF_data_dict.csv"))
View(TMF_dict_detail)


# TMF_dict_detail %>% pull(`Field Name`)
# 
# # Your vector of strings
# strings <- TMF_dict_detail %>% pull(`Field Name`)
# 
# # Create the mapping
# mapping <- setNames(strings, strings)
# 
# # Custom print function
# formatted_output <- paste0('"', names(mapping), '" = "', mapping, '"', collapse = ", ")
# 
# # Print the formatted string
# cat(formatted_output)

# then I can copy the results to our new switch function
# maybe better just do it in CSV file.
# or I should use those as labels in create_dictionary function. liek c(POSTAL_CODE = "Postal code")
# finally use chatgpt to translate the switch function to a case_when
create_item = function(x) {
  # Using case_when to replace the switch statement
  y <- case_when(
    x == "Postal code" ~ "POSTALCODE",
    x == "Birth Date" ~ "BIRTH_DATE",
    x == "Retired Date" ~ "RET_DATE",
    x == "Latitude" ~ "LATITUDE",
    x == "Longitude" ~ "LONGITUDE",
    x == "Census Division" ~ "CD",
    x == "Census Subdivision" ~ "CSD",
    x == "Census Subdivision Name" ~ "MUN_NAME",
    x == "Census Metropolitan Area or Census Agglomeration Area" ~ "CMACA",
    x == "Dissemination Area (DA)" ~ "DA",
    x == "Census Tract (CT)" ~ "CT",
    x == "Dissemination Block (DB)" ~ "DB",
    x == "Designated Place Name (DPL)" ~ "DPL",
    x == "Population Centre (POPCTR)" ~ "POPCTR",
    x == "Development Region (DR)" ~ "DR",
    x == "Health Authority (HA)" ~ "HA",
    x == "Health Service Delivery Area (HSDA)" ~ "HSDA",
    x == "Local Health Area (LHA)" ~ "LHA",
    x == "Community Health Service Area (CHSA)" ~ "CHSA",
    x == "Pre-2018 Local Health Area" ~ "LHA_PRE_2018",
    x == "Micro Health Area" ~ "MHA",
    x == "Ministry of Children & Families Region (MCFD)" ~ "MCFD",
    x == "Ministry of Children & Families Service Delivery Area (MCFD_SDA)" ~ "MCFD_SDA",
    x == "Ministry of Children & Families Local Service Area (MCFD_LSA)" ~ "MCFD_LSA",
    x == "College Region" ~ "CR",
    x == "School District (SD)" ~ "SD",
    x == "School District Trustee Electoral Area (TEA)" ~ "TEA",
    x == "Provincial Electoral District (PED)" ~ "PED",
    x == "Federal Electoral District (FED)" ~ "FED",
    x == "Police Services Respondent Area (RESP)" ~ "RESP",
    x == "Tourism Region" ~ "TOURISM",
    x == "Games Zone" ~ "GZ",
    x == "Community Name" ~ "COMM_NAME",
    x == "Modified Census Subdivision Name" ~ "Modified_MUN_NAME",
    x == "Modified Full Census Subdivision" ~ "CDCSD",
    TRUE ~ NA_character_  # Default case if no match
  )
  
  return(y)
}

TMF_dict_detail <- TMF_dict_detail %>%
  mutate(item_short = create_item(`Field Name`))

# for the datadictionary created from datadictionary function, we also need to create a shorten item name since some items have year as sufix such as CD_2021. 
TMF_dict <- TMF_dict %>% 
  mutate( item_short = str_remove(item, "_\\d{4}")) 


# TMF_dict's summary for numerica number does not mean a lot since it is a fact table, and all dimension tables are in another lookup.xlsx excel file. 
# so we could simplify the TMF_dict
# TMF_dict_simple <- TMF_dict %>% 
#   group_by(item) %>% 
# slice_head(n = 1)
# filter(row_number() == 1)

# or it does not matter, we can add value label later using our dimension table
TMF_dict <- TMF_dict %>% 
  left_join(TMF_dict_detail, join_by( item_short))


TMF_dict %>% readr::write_csv(here::here("out", "Translation_Master_File_Dict_DIP.csv"))



x=read_csv("out/StatsCAN_Census_21_BC_DA_DICT_DIP.csv")
