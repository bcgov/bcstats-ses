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

library(tidyverse)
library(DBI)
library(odbc)
library(lubridate)
library(glue)
library(dbplyr)
library(datadictionary)

province = "British Columbia"


# Function to download, unzip, and read dataset
download_and_process_dataset <- function(
  url,
  csv_folder,
  file_name,
  filter_pattern = "scores"
) {
  csv_file <- file.path(csv_folder, file_name)

  if (!file.exists(csv_file)) {
    message(paste0(
      file_name,
      " does not exist. Proceeding to download and process."
    ))

    # Download the ZIP file
    temp_zip <- tempfile(fileext = ".zip")
    download.file(url = url, destfile = temp_zip)

    # Create directory if it doesn't exist
    if (!dir.exists(csv_folder)) {
      dir.create(csv_folder, recursive = TRUE)
    }

    # Unzip the file
    unzip(temp_zip, exdir = csv_folder)

    # List files in the extracted directory to find the CSV file
    csv_files <- list.files(
      path = csv_folder,
      pattern = "\\.csv$",
      full.names = TRUE
    )

    # Filter files that contain the specified pattern
    csv_files <- csv_files[str_detect(csv_files, filter_pattern)]

    message(paste0("Data file found: ", paste(csv_files, collapse = ", ")))

    # Read the CSV file(s)
    if (length(csv_files) > 0) {
      # Read the first CSV file (or adapt if there are multiple files)
      data <- read_csv(csv_files[1]) %>%
        janitor::clean_names(case = "screaming_snake")

      # # Save the processed data
      # write_csv(data, csv_file)

      # Display first few rows
      message("Successfully processed data. First few rows:")
      print(head(data))

      # Return the data
      return(data)
    } else {
      warning("No CSV files found in the extracted directory")
      return(NULL)
    }

    # Clean up the temporary zip file
    unlink(temp_zip)
  } else {
    message(paste0(file_name, " already exists. Reading from file."))
    # If the CSV file already exists, read it directly
    data <- read_csv(csv_file) %>%
      janitor::clean_names(case = "screaming_snake")
    return(data)
  }
}


###################################################
# The CISR and CISV were designed to assess an area’s social resilience and social vulnerability to natural hazards and disasters.
# The Canadian Index of Social Resilience (CISR) and the Canadian Index of Social Vulnerability (CISV).

# https://www150.statcan.gc.ca/n1/pub/45-20-0001/452000012025001-eng.htm

###################################################

# check if csv file already exists
CISR_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CISR_CISV/"
)
CISR_csv_file = file.path(CISR_csv_folder, "cisr_scores_quintiles-eng.csv")


# Download CISR data
cisr_data <- download_and_process_dataset(
  url = "https://www150.statcan.gc.ca/pub/45-20-0001/2025001/csv/cisr-eng.zip",
  csv_folder = CISR_csv_folder,
  file_name = "cisr_scores_quintiles-eng.csv"
) %>%
  filter(PROVINCE_OR_TERRITORY == province)

cisr_data %>% glimpse()
readr::write_csv(cisr_data, here::here("out", "cisr_data.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

cisr_data_dict_labels = c(
  "DISSEMINATION_AREA_DA" = "DAs are small, relatively stable geographic units composed of one or more adjacent dissemination blocks where populations generally range from 400 to 700 people. DAs cover all the territory of Canada and are the smallest standard geographic area for which all census data are disseminated; disasters disproportionately affect certain people and communities across Canada. Disproportionate impacts depend on the demographic, socioeconomic, ethnocultural and structural characteristics of individuals and communities. Dimension scores were constructed from the principal component analysis process. Lower scores for each dimension correspond to areas that are less socially resilient, whereas higher scores for each dimension relate to areas that are more socially resilient.",
  "PROVINCE_OR_TERRITORY" = "British Columbia",
  "DIMENSION_1_SCORES" = "The first dimension is driven by seven meaningful indicators pertaining to education, employment and dwelling characteristics. Specifically, this dimension is characterized by a greater proportion of the population with a bachelor's degree or higher, a greater proportion of the working population in a creative class occupation, a greater proportion of the population with a high school diploma (or equivalent) or higher, a geographic location that is relatively less remote, a greater proportion of the working population that is not employed in a single sector, a relatively higher median value of dwellings and a greater proportion of dwellings that are permanent.",
  "DIMENSION_2_SCORES" = "The second dimension is driven by seven meaningful indicators pertaining to homeownership, income, employment and age. Specifically, this dimension is characterized by a greater proportion of households that own their home, a greater proportion of the population that has not moved in the last five years, a greater proportion of the population not in low income, a greater proportion of the labour force that is employed, a relatively higher median household income, a greater proportion of the population with a high school diploma (or equivalent) or higher and a smaller proportion of the population under 65 years of age.",
  "DIMENSION_3_SCORES" = "The third dimension is driven by three meaningful indicators pertaining to age, activities of daily living and dwelling characteristics. Specifically, this dimension is characterized by a greater proportion of the population under 65 years of age, a greater proportion of the population that did not report 'always' having difficulties with activities of daily living and a greater proportion of dwellings built after 1980.",
  "CISR_SCORES" = "Canadian Index of Social Resilience (CISR) scores correspond to an area’s social resilience based on the three dimensions. Scores are calculated by taking the average across the three dimensions of social resilience. Higher CISR scores correspond to DAs that are more resilient, and lower CISR scores correspond to DAs that are less resilient.",
  "CISR_QUINTILES" = "CISR quintiles were also derived by ordering CISR scores from smallest to largest and then dividing them into five equally sized groups, or quintiles. Quintiles are categorized from 1 to 5. A value of 5 corresponds to the DAs that were the most resilient, and a value of 1 corresponds to the DAs that were the least resilient.",
  "CISR_MOST_RESILIENT_DIMENSION" = "The CISR most resilient dimension indicates the dimension that had the highest score across the three dimensions of social resilience. This highlights the dimension that demonstrated the most resilience in a given DA."
)

cisr_data_dict = create_dictionary(
  cisr_data,
  var_labels = cisr_data_dict_labels
)

write_csv(cisr_data_dict, here::here("out/cisr_data_dict.csv"))

#################################################################################################

# check if csv file already exists
CISV_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CISR_CISV/"
)
CISV_csv_file = file.path(CISR_csv_folder, "cisv_scores_quintiles-eng.csv")


# Download CISR data
cisv_data <- download_and_process_dataset(
  url = "https://www150.statcan.gc.ca/pub/45-20-0001/2025001/csv/cisv-eng.zip",
  csv_folder = CISV_csv_folder,
  file_name = "cisv_scores_quintiles-eng.csv",
  filter_pattern = "cisv_scores_quintiles"
) %>%
  filter(PROVINCE_OR_TERRITORY == province)

cisv_data %>% glimpse()
readr::write_csv(cisv_data, here::here("out", "cisv_data.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

cisv_data_dict_labels = c(
  "DISSEMINATION_AREA_DA" = "DAs are small, relatively stable geographic units composed of one or more adjacent dissemination blocks where populations generally range from 400 to 700 people. DAs cover all the territory of Canada and are the smallest standard geographic area for which all census data are disseminated; disasters disproportionately affect certain people and communities across Canada. Dimension scores were constructed from the principal component analysis process. Lower scores for each dimension correspond to areas that are less socially vulnerable, whereas higher scores for each dimension relate to areas that are more socially vulnerable.",
  "PROVINCE_OR_TERRITORY" = "British Columbia",
  "DIMENSION_1_SCORES" = "The first dimension is driven by eight meaningful indicators pertaining to racialized identity and immigration, dwelling characteristics, remoteness, and employment. Specifically, this dimension is characterized by a greater proportion of the population that identified as racialized, a greater proportion of dwellings deemed not acceptable, a greater proportion of the population that identified as recent immigrants (within the last five years), a greater proportion of the population with no working knowledge of either official language, a geographic location that is relatively less remote, a greater proportion of dwellings with five or more storeys, a greater proportion of the labour force that is unemployed and a relatively higher median value of dwellings.",
  "DIMENSION_2_SCORES" = "The second dimension is driven by six meaningful indicators pertaining to income, labour force participation, age, activity limitations and gender. Specifically, this dimension is characterized by a greater proportion of the population receiving government pension benefits as their main source of income, a greater proportion of the population not in the labour force, a greater proportion of the population aged 65 or over living alone, a greater proportion of the population reporting 'always' having difficulties with activities of daily living because of physical problems, a greater proportion of the population that are women and a greater proportion of the population in low income.",
  "DIMENSION_3_SCORES" = "The third dimension is driven by eight meaningful indicators pertaining to education, Indigenous identity, family composition, income, remoteness and employment. Specifically, this dimension is characterized by a greater proportion of the population without a high school diploma (or equivalent), a greater proportion of the population that identified as Indigenous, a greater proportion of one-parent families with more than three children, a greater proportion of the population receiving employment insurance benefits, a geographic location that is relatively more remote, a greater proportion of the labour force that is unemployed, a greater proportion of the population in low income and a greater proportion of the population receiving social assistance as their main source of income.",
  "DIMENSION_4_SCORES" = "The fourth dimension is driven by 10 meaningful indicators pertaining to activity limitations, moving, income and dwelling characteristics. Specifically, this dimension is characterized by a greater proportion of the population reporting 'always' having difficulties with activities of daily living because of psychological problems, a greater proportion of recent movers (within the last five years), a greater proportion of the population receiving social assistance as their main source of income, a relatively lower median value of dwellings, a relatively lower median household income, a greater proportion of the population reporting 'always' having difficulties with activities of daily living because of physical problems, a greater proportion of dwellings with five or more storeys, a greater proportion of the population in low income, a greater proportion of dwellings deemed not acceptable and a smaller proportion of the population with no knowledge of either official language.",
  "CISV_SCORES" = "Canadian Index of Social Vulnerability (CISV) scores correspond to an area’s social vulnerability based on the four dimensions. Scores are calculated by taking the average across the four dimensions of social vulnerability. Higher CISV scores correspond to DAs that are more vulnerable, and lower CISV scores correspond to DAs that are less vulnerable.",
  "CISV_QUINTILES" = "CISV quintiles were also derived by ordering CISV scores from smallest to largest and then dividing them into five equally sized groups, or quintiles. Quintiles are categorized from 1 to 5. A value of 5 corresponds to the DAs that were the most vulnerable, and a value of 1 corresponds to the DAs that were the least vulnerable.",
  "CISV_MOST_VULNERABLE_DIMENSION" = "The CISV most vulnerable dimension indicates the dimension that had the highest score across the four dimensions of social vulnerability. This highlights the dimension that demonstrated the most vulnerability in a given DA."
)

cisv_data_dict = create_dictionary(
  cisv_data,
  var_labels = cisv_data_dict_labels
)

write_csv(cisv_data_dict, here::here("out/cisv_data_dict.csv"))


###################################################
# The Canadian Index of Multiple Deprivation https://www150.statcan.gc.ca/n1/pub/45-20-0001/452000012023002-eng.htm
# The Canadian Index of Multiple Deprivation (CIMD) is a composite index that measures multiple dimensions of deprivation at the neighbourhood level in Canada.
#         users should consult The Canadian Index of Multiple Deprivation: User Guide, 2021 related to this product.
#         Data tables: Reference period: 2021
# https://www150.statcan.gc.ca/n1/pub/45-20-0001/452000012023001-eng.htm
# Canada CSV | XLSX
#####################################################
# Function to download, unzip, and read dataset
# check if csv file already exists
CIMD_csv_folder = file.path(
  config::get("lan_path"),
  "2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CIMD/"
)
CIMD_csv_file = file.path(CIMD_csv_folder, "bc_scores_quintiles_EN.csv")


# Download CISR data
cidm_data <- download_and_process_dataset(
  url = "https://www150.statcan.gc.ca/pub/45-20-0001/2023001/csv/bc_scores_quintiles_csv-eng.zip.zip",
  csv_folder = CIMD_csv_folder,
  file_name = "bc_scores_quintiles_EN.csv",
  filter_pattern = "sbc_scores_quintiles_EN"
) %>%
  filter(PROVINCE == province)

cidm_data %>% glimpse()
readr::write_csv(cidm_data, here::here("out", "cidm_data.csv"))


#################################################################################################
# Data dictionary
#################################################################################################

cidm_dict_labels = c(
  "DISSEMINATION_AREA_DA" = "Dessemination area id in 2021 Canadian Census; Component scores were constructed from the component analysis process. Lower scores for each dimension correspond to areas that are the least marginalized, while higher scores for each dimension relate to areas that are the most marginalized",
  "PROVINCE" = "British Columbia",
  "DA_POPULATION" = "DA population count",
  "RESIDENTIAL_INSTABILITY_QUINTILES" = "residential instability, speaks to the tendency of neighbourhood inhabitants to fluctuate over time, taking into consideration both housing and familial characteristics. For ease of use, CIMD also provides users with quintile rankings. Within each dimension, the component scores were ordered from smallest to largest and then divided into five equally sized groups, or quintiles, and categorized from 1 through 5. A value of 1 corresponds to DAs that were the least deprived for that dimension, and a value of 5 corresponds to DAs that were the most deprived. Note that depending on a DA’s characteristics, it could be the most deprived for one dimension and the least deprived for another",
  "RESIDENTIAL_INSTABILITY_SCORES" = "residential instability, speaks to the tendency of neighbourhood inhabitants to fluctuate over time, taking into consideration both housing and familial characteristics",
  "ETHNO_CULTURAL_COMPOSITION_QUINTILES" = "ethno-cultural composition. This dimension refers to the community make-up of immigrant populations. For ease of use, CIMD also provides users with quintile rankings. Within each dimension, the component scores were ordered from smallest to largest and then divided into five equally sized groups, or quintiles, and categorized from 1 through 5. A value of 1 corresponds to DAs that were the least deprived for that dimension, and a value of 5 corresponds to DAs that were the most deprived. Note that depending on a DA’s characteristics, it could be the most deprived for one dimension and the least deprived for another",
  "ETHNO_CULTURAL_COMPOSITION_SCORES" = "ethno-cultural composition. This dimension refers to the community make-up of immigrant populations",
  "ECONOMIC_DEPENDENCY_QUINTILES" = "Economic dependency relates to reliance on the workforce, or a dependence on sources of income other than employment income. For ease of use, CIMD also provides users with quintile rankings. Within each dimension, the component scores were ordered from smallest to largest and then divided into five equally sized groups, or quintiles, and categorized from 1 through 5. A value of 1 corresponds to DAs that were the least deprived for that dimension, and a value of 5 corresponds to DAs that were the most deprived. Note that depending on a DA’s characteristics, it could be the most deprived for one dimension and the least deprived for another",
  "ECONOMIC_DEPENDENCY_SCORES" = "Economic dependency relates to reliance on the workforce, or a dependence on sources of income other than employment income.",
  "SITUATIONAL_VULNERABILITY_QUINTILES" = "Situational vulnerability, the fourth dimension, speaks to variations in socio-demographic conditions in the areas of housing and education, while taking into account other demographic characteristics. For ease of use, CIMD also provides users with quintile rankings. Within each dimension, the component scores were ordered from smallest to largest and then divided into five equally sized groups, or quintiles, and categorized from 1 through 5. A value of 1 corresponds to DAs that were the least deprived for that dimension, and a value of 5 corresponds to DAs that were the most deprived. Note that depending on a DA’s characteristics, it could be the most deprived for one dimension and the least deprived for another",
  "SITUATIONAL_VULNERABILITY_SCORES" = "Situational vulnerability, the fourth dimension, speaks to variations in socio-demographic conditions in the areas of housing and education, while taking into account other demographic characteristics."
)

cidm_data_dict = create_dictionary(cidm_data, var_labels = cidm_dict_labels)

write_csv(cidm_data_dict, here::here("out/cidm_data_dict.csv"))
