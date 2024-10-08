Collect data from StatsCan and other statistical agencies and convert to clean CSV files for use in a secure research area. Note that this code **does not** produce the actual SES index, but only produces a subset of the required cleaned datasets.


### Data

1. [StatsCan census 2021](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/index-eng.cfm). It includes income, education, vocation, and family information. 
2. Translation Master file(TMF). TMF is a table with different levels of geography to link the data.It is a fact table and comes with a set of dimension tables (lookup tables) for the categorical variables.  
3. [B.C. crime trends and statistics](https://www2.gov.bc.ca/gov/content/justice/criminal-justice/policing-in-bc/publications-statistics-legislation/crime-police-resource-statistics). 
 * It requires a lookup table to link policing jurisdictions and regions in BC. 
4. [BC population projection](https://bcstats.shinyapps.io/popApp/)
5. [BC Wild fire data](https://www2.gov.bc.ca/gov/content/safety/wildfire-status/about-bcws/wildfire-statistics/wildfire-averages)

All data are stored on the LAN. BC Stats data may be slightly different from the public-available data.  

### Usage

Each data is cleaned and outputed to a CSV file in a single R script. Some data sets may have a standalone look up table for their categorical variables. 