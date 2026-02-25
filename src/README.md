<!--
Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->

# BC Stats SES Index - Data Processing Pipeline

This document describes the complete data processing pipeline for generating the BC community Socio-Economic Status (SES) index.

## Overview

The pipeline cleans and processes external data sources from Statistics Canada and other agencies to create analysis-ready datasets for the SES index. The final outputs are published to the BC Data Catalogue.

## Data Sources

1. **StatsCan Census 2021** - Income, education, vocation, family information
2. **Translation Master File (TMF)** - Geography linkage tables
3. **BC Crime Statistics** - Crime rates by region
4. **BC Population Projections** - From BC Stats
5. **BC Wildfire Data** - Historical wildfire statistics
6. **Remoteness Index** - StatsCan remoteness classification
7. **SLA (Service Learning Award)** - Education data
8. **BC Population Estimates** - Annual population counts
9. **Housing Values** - BC Assessment data
10. **CISV/CISR/CIMD** - StatsCan vulnerability indices
11. **CHSA-DA Crosswalk** - Geographic linkage
12. **Internet Connectivity Data** - PHH and CITZ broadband availability

## Processing Scripts

### Core Data Processing (in order)

| Script | Description |
|--------|-------------|
| `01_output_statscan_census.R` | Downloads and cleans StatsCan census data |
| `03_output_crime_rate.R` | Processes BC crime statistics |
| `04_output_TMF.R` | Creates Translation Master File |
| `05_output_LFS.R` | Processes Labour Force Survey data |
| `06_output_wildfire.R` | Processes wildfire data |
| `07_SLA.R` | Processes SLA education data |
| `08_BC_population_estimates.R` | Creates population estimates |
| `09_output_remoteness.R` | Calculates remoteness indices |
| `10_output_housing_value.R` | Processes housing value data |
| `11_output_CISV_CISR_CIMD.R` | Processes vulnerability indices |
| `12_output_CHSA_DA_lookup.R` | Creates CHSA-DA geographic crosswalk |
| `14_bc_connectivity_data.R` | Processes internet connectivity data |

### Geographic Suppression (Final Step)

| Script | Description |
|--------|-------------|
| `15_remove_geo_suppression_ids.R` | Removes indigenous geographies from final outputs |

---

## Geographic Suppression Process

### Purpose

Removes indigenous geographies from SEI Data Catalogue files to comply with data suppression requirements:

- **CHSAs**: Removes "Nisga's" regions (identified by name pattern)
- **CSDs**: Removes Indian reserves, Nisga'a lands, and Indian government districts

### CSD Type Codes Used

The script uses official Statistics Canada CSD type codes:

| Code | Description |
|------|-------------|
| `IRI` | Indian reserve |
| `NL` | Nisga'a land |
| `IGD` | Indian government district |
| `TAL` | Tla'amin Lands |

### How It Works

1. **Downloads StatsCan Geographic Attribute File** directly from Statistics Canada
   - URL: `https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/files-fichiers/2021_92-151_X.zip`
   
2. **Falls back to database query** if download fails

3. **Queries for CHSAs** with "Nisga's" in the name

4. **Removes identified geographies** from SEI DET and LONG files

5. **Outputs 4 filtered CSV files** for BC Data Catalogue:
   - `SEI_DET_CHSA_2023.csv`
   - `SEI_LONG_CHSA_2023.csv`
   - `SEI_DET_CSD_2023.csv`
   - `SEI_LONG_CSD_2023.csv`

---

## How to Re-Run the Pipeline (Year over Year)

### Prerequisites

1. **R Environment**
   ```r
   # Install required packages
   renv::restore()
   ```

2. **Database Access**
   - Requires connection to the BC Stats SQL Server
   - Network access to LAN data paths

3. **Configuration**
   - Update `config.yml` with current year settings
   - Verify data paths in `.Renviron`

### Step-by-Step Instructions

#### Step 1: Update Configuration

Edit `config.yml` to set the current year:

```yaml
# Example configuration
data_server:
  driver: "SQL Server"
  server: "your-server"
  database: "Population_Labour_Social"

lan_path: "G:/Operations/Data Science and Analytics"

file_path:
  sei_file_path: "2024 SES Index/exports"
  # ... other paths
```

#### Step 2: Update Script 15 for New Census Year

When running for a new census year (e.g., 2026):

1. **Update the download URL** in `15_remove_geo_suppression_ids.R`:
   ```r
   # Change from 2021 to 2026
   geo_attr_url <- 'https://www12.statcan.gc.ca/census-recensement/2026/geo/aip-pia/attribute-attribs/files-fichiers/2026_92-151_X.zip'
   ```

2. **Update CSD type filters** if new types are introduced by StatsCan

3. **Update year in output filenames**:
   ```r
   # Change from 2023 to new year
   "SEI_DET_CHSA_2026.csv"
   ```

#### Step 3: Run Data Processing Scripts

Run scripts in numerical order:

```r
# Run each script sequentially
source("src/01_output_statscan_census.R")
source("src/03_output_crime_rate.R")
# ... continue through pipeline
source("src/14_bc_connectivity_data.R")
```

#### Step 4: Run Geographic Suppression

```r
# Final step - removes indigenous geographies
source("src/15_remove_geo_suppression_ids.R")
```

### Annual Maintenance Tasks

1. **Check for StatsCan Updates**
   - New census years may have different geographic codes
   - CSD type definitions may change

2. **Verify Data Source URLs**
   - StatsCan download links may change format
   - BC Data Catalogue resources may be updated

3. **Update Indigenous Type List**
   - Review StatsCan documentation for new indigenous CSD types
   - Add/remove codes from `indig_csd_types` vector as needed

4. **Validate Output**
   - Compare record counts with previous year
   - Verify suppressed geographies are correctly identified
   - Check for new indigenous communities that need suppression

### Troubleshooting

| Issue | Solution |
|-------|----------|
| Download fails | Script automatically falls back to database query |
| No CSDs found | Check `indig_csd_types` vector matches StatsCan codes |
| Database connection fails | Verify VPN/LAN connection and credentials |
| Column name mismatch | Check StatsCan geographic attribute file structure |

---

## Output Files

Final outputs are saved to:
```
G:/Operations/Data Science and Analytics/2024 SES Index/bc data catalogue/Data Catalogue final products/2023/
```

Files include:
- SEI detailed files (DET) - One row per geography
- SEI longitudinal files (LONG) - Time series data

---

## Data Documentation

Detailed documentation about data sources, collection methods, and processing steps is stored on the LAN:
```
G:/Operations/Data Science and Analytics/2024 SES Index/data/
```

---

## License

```
Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
```
