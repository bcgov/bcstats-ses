<!--
Copyright 2024 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->


Collect data from StatsCan and other statistical agencies and convert to clean CSV files for use in a secure research area. Note that this code **does not** produce the actual SES index, but only produces a subset of the required cleaned datasets.



### Data

1. [StatsCan census 2021](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/index-eng.cfm). It includes income, education, vocation, and family information. 
  * A `background.txt` in `census` folder stores the information of the detail of StatsCan Census data.
2. Translation Master file(TMF). TMF is a table with different levels of geography to link the data.It is a fact table and comes with a set of dimension tables (lookup tables) for the categorical variables.  
3. [B.C. crime trends and statistics](https://www2.gov.bc.ca/gov/content/justice/criminal-justice/policing-in-bc/publications-statistics-legislation/crime-police-resource-statistics). 
 * It requires a lookup table to link policing jurisdictions and regions in BC. 
4. [BC population projection](https://bcstats.shinyapps.io/popApp/)
5. [BC Wild fire data](https://www2.gov.bc.ca/gov/content/safety/wildfire-status/about-bcws/wildfire-statistics/wildfire-averages)
6. [BC Remoteness Index](https://www150.statcan.gc.ca/n1/pub/17-26-0001/172600012020001-eng.htm)
7. SLA. 
8. BC population estimate.
9. remoteness metric.
10. BC housing value. 

All data are stored on the LAN. BC Stats data may be slightly different from the public-available data.  

To access those data, you need to have a secure connection to the LAN. The `safepaths` package is used to access the data.The safe paths network paths are stored in the `.Renviron` file or a config file.

A data documentation is created for how, when, and where we collect and clean those data. The data documentation is stored in the project folder on the LAN.

### Usage

Each data is cleaned and saved to a CSV file in a single R script. 

Some data sets may have a standalone look up table for their categorical variables. 

* `01_output_statscan_census.R` downloads, cleans, and saves the census data in the `output` folder. 


