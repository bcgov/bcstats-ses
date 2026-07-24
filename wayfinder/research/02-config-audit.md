# 02 - Config Audit

> Unblocks: #4 (config architecture & annual-refresh contract)
> Parent ticket: [02-config-audit](../tickets/02-config-audit.md)

## Current state of `config_year.yml`

```yaml
default:
  gcs:
    schema: "prod"
    table: "FCT_GCS_202606"
  cansim_cache_path: "C:/Temp/cansim_cache"
  cancensus_cache_path: "C:/Temp/census_cache"
```

**Scripts that already read `config_year.yml`:**

| Script | What it reads | Lines
|---|---|---
| `04_output_TMF.R` | `gcs.schema`, `gcs.table`, `cansim_cache_path` | 94, 98, 100, 102
| `03_output_crime_rate.R` | `gcs.schema`, `gcs.table`, `cansim_cache_path` | 38, 93, 108

**Scripts that read `config.yml` (gitignored) via `config::get()`:**

| Script | Keys used | Lines
|---|---|---
| `03_output_crime_rate.R` | `lan_path`, `data_server` | 34, 206
| `04_output_TMF.R` | `data_server`, `lan_path` | 77, 90
| `05_output_LFS.R` | `lan_path` (implicit via `config::get`) | 123
| `06a_output_wildfire.R` | `lan_path` (implicit) | 274, 282, 287, 290
| `06b_output_wildfire.R` | `lan_path`, `wildfire_data_path`, `database.*` | 69-70, 100-105, 117-121
| `07_SLA.R` | (loads config but uses `use_network_path` only) | 21
| `08_BC_population_estimates.R` | (none -- uses `use_network_path` / `SAFEPATHS_NETWORK_PATH`) | 54
| `09_output_remoteness.R` | (loads config but uses `use_network_path`) | --
| `10_output_housing_value.R` | `database.*`, `tables.*`, `output.*` | 55-63, 89, 118, 162, 175, 230
| `11_output_CISV_CISR_CIMD.R` | `lan_path` | 23, 43, 98, 163
| `12_output_CHSA_DA_lookup.R` | `database.*`, `tables.*`, `file_path.*`, `lan_path` | 80-86, 111, 146, 233, 270, 584
| `13_BC_DA_population_estimates.r` | `file_path.*` | 25, 28-30
| `14_connectivity.R` | (loads config) | 78
| `15_remove_geo_suppression_ids.R` | `lan_path`, `data_server.*`, `file_path.cancensus_cache_path` | 41, 44, 58, 197-200
| `17_data_preparation_for_powerbi.R` | `lan_path` | 22, 32
| `18_trend_ses.R` | `lan_path` | 22, 25

---

## Inventory of hardcoded values

### A. Years used as data/snapshot parameters (non-comment)

| Value | File:Line | Purpose | Proposed home |
|---|---|---|---|
| `2021` (CENSUS_YEAR) | `01_output_statscan_census.R:342` | Census year label for CA21 dataset | keep inline (census-dataset intrinsic, each block is a separate historical census pull) |
| `2016` (CENSUS_YEAR) | `01_output_statscan_census.R:546` | Census year label for CA16 dataset | keep inline |
| `2011` (CENSUS_YEAR) | `01_output_statscan_census.R:707` | Census year label for CA11/NHS dataset | keep inline |
| `2006` (CENSUS_YEAR) | `01_output_statscan_census.R:902` | Census year label for CA06 dataset | keep inline |
| `2001` (CENSUS_YEAR) | `01_output_statscan_census.R:1100` | Census year label for CA01 dataset | keep inline |
| `2000` (crime_start_year) | `03_output_crime_rate.R:47` | Start year of crime data window | `config_year.yml` -- already partially parameterised; just needs to be added to the yml |
| `2024` (filter threshold) | `06a_output_wildfire.R:191` | Threshold to split historic vs current wildfire datasets | `config_year.yml` (script already has TODO noting this) |
| `2000` (MIN_FIRE_YEAR) | `06a_output_wildfire.R:60` | Start year for wildfire historic data | `config_year.yml` |
| `2016:2025` (YEAR_RANGE) | `06b_output_wildfire.R:210` | Year range for wildfire DA summary (test) | `config_year.yml` or keep inline (exploratory) |
| `2011:2025` (YEAR_RANGE) | `06b_output_wildfire.R:219` | Year range for wildfire final summary | `config_year.yml` or keep inline (exploratory) |
| `2000:2024` (years) | `08_BC_population_estimates.R:90` | Year range for population estimates | `config_year.yml` |
| `2019:2023` | `12_output_CHSA_DA_lookup.R:198` | Year range for 2021-census-era CHSA crosswalk | `config_year.yml` |
| `2016:2018` | `12_output_CHSA_DA_lookup.R:210` | Year range for 2016-census-era CHSA crosswalk | `config_year.yml` |
| `2016:2023` | `12_output_CHSA_DA_lookup.R:270,332,440` | Year range for DB population, CHSA lookup | `config_year.yml` |
| `2023` (output_year) | `15_remove_geo_suppression_ids.R:53` | Year for data catalogue output products | `config_year.yml` |

### B. SQL table / schema names

| Value | File:Line | Purpose | Proposed home |
|---|---|---|---|
| `FCT_GCS_202509` | `06b_output_wildfire.R:130` | GCS snapshot table in raw SQL | `config_year.yml` -- **conflicts with** `config_year.yml` which says `FCT_GCS_202606`; 06b bypasses config and hardcodes a *different* snapshot |
| `FCT_CENSUS_2021_BC_DA` | `06b_output_wildfire.R:138` | Census attribute table in raw SQL | `config_year.yml` (new key, e.g. `census_da_table`) |
| `FCT_CENSUS_2021_BC_CSD_UD` | `15_remove_geo_suppression_ids.R:215` | Census CSD attribute table in raw SQL (fallback query) | `config_year.yml` (new key) |
| `Population_Labour_Social` | `06b_output_wildfire.R:130,138`; `15_remove_geo_suppression_ids.R:215` | SQL catalog/database name | `config.yml` (secret-adjacent: server detail) |
| `Prod` (schema) | `10_output_housing_value.R:89,92,118,162,175,193,225,230`; `12_output_CHSA_DA_lookup.R:111,146,153` | SQL schema for BC Assessment / GCS / CHSA tables | `config.yml` (server detail) -- most scripts already use `config$database` but these hardcode `'Prod'` |

### C. LAN / local file paths (not in config)

| Value | File:Line | Purpose | Proposed home |
|---|---|---|---|
| `"2024 SES Index/data/raw_data/crime_rate/Pop by DA and RESP.xlsx"` | `03_output_crime_rate.R:187` | DA-to-RESP lookup from Econ team | `config_year.yml` (new key `crime_da_resp_lookup`) |
| `"2024 SES Index/data/output/Crime_Rate_Dict_DIP.csv"` | `03_output_crime_rate.R:340` | LAN output path for crime dict | `config_year.yml` or derive from a base LAN output path |
| `"2024 SES Index/docs/TMF_data_dict.csv"` | `04_output_TMF.R:164` | TMF data dictionary from Econ team | `config_year.yml` (new key `tmf_data_dict`) |
| `"2024 SES Index/data/raw_data/TMF/GCS_Lookup_Table.xlsx"` | `04_output_TMF.R:249` | TMF dimension/lookup tables | `config_year.yml` (new key `tmf_lookup_table`) |
| `"2024 SES Index/data/StatsCanLFS/SLA2016_FinalClassification.xlsx"` | `05_output_LFS.R:123` | SLA classification file | `config_year.yml` (new key `sla_file`) |
| `"2024 SES Index/data/other/StatsCAN_sgc/lda_000a21a_e/lda_000b21a_e.shp"` | `06a_output_wildfire.R:274`; `06b_output_wildfire.R:86`; `09_output_remoteness.R:150` | DA boundary shapefile | `config_year.yml` (new key `da_boundary_shp`) |
| `"2024 SES Index/data/raw_data/remoteness/..."` (multiple) | `09_output_remoteness.R:201,213,222,242,285,312,405` | Remoteness input/output paths (servicebc, hospital, school geocoded data) | `config_year.yml` (new keys under `remoteness:`) |
| `"2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CISR_CISV/"` | `11_output_CISV_CISR_CIMD.R:44,99` | CISR/CISV data folder | `config_year.yml` (new key `cisr_cisv_folder`) |
| `"2024 SES Index/data/raw_data/StatsCAN_CISR_CISV_CIMD/CIMD/"` | `11_output_CISV_CISR_CIMD.R:164` | CIMD data folder | `config_year.yml` (new key `cimd_folder`) |
| `"2024 SES Index/data/raw_data/chsa_da_crosswalk/"` | `12_output_CHSA_DA_lookup.R:585` | CHSA-DA crosswalk output folder | `config_year.yml` (new key `chsa_da_crosswalk_folder`) |
| `"2024 SES Index/exports/2025-07-31-initial-index-results"` | `17_data_preparation_for_powerbi.R:267,464,470`; `18_trend_ses.R:29` | Index model export folder | `config_year.yml` (new key `index_exports_path`) |
| `"2021 Census Boundary Files - 2023 Release/csd_2021.shp"` | `18_trend_ses.R:73` | CSD shapefile for trend maps | `config_year.yml` (new key `csd_boundary_shp`) |
| `"2024 SES Index/data/raw_data/remoteness/lda_000a21a_e/..."` | `09_output_remoteness.R:149,170` | DA/CSD shapefiles for remoteness | `config_year.yml` (same as `da_boundary_shp`) |
| `"data/raw_data/TMF/GCS_202406.csv"` | `08_BC_population_estimates.R:54` | Legacy TMF file (pre-database) | `config_year.yml` or remove (script 08 is superseded by 04 which reads from DB) |
| `"2024 SES Index"` (project folder base) | `15_remove_geo_suppression_ids.R:58`; `utils.R:140,353` | Project folder name used in LAN paths | `config_year.yml` (new key `project_folder`, default `"2024 SES Index"`) |
| `"2024 SES Index/data/other/StatsCAN_sgc/chsa_2022_wgs.json"` | `17_data_preparation_for_powerbi.R:124` | CHSA topojson for PowerBI | `config_year.yml` (new key `chsa_geojson`) |
| `"2024 SES Index/data/other/StatsCAN_sgc/lower_mainland_chsa.json"` | `17_data_preparation_for_powerbi.R:233` | Lower mainland CHSA subset output | `config_year.yml` or keep inline (ad-hoc) |
| `"2024 SES Index/data/other/StatsCAN_sgc/sgc_structure_csd_BC.csv"` | `17_data_preparation_for_powerbi.R:707` | SGC structure file | `config_year.yml` or keep inline (ad-hoc) |
| `"2024 SES Index/data/other/StatsCAN_sgc/bc_csd.geojson"` | `17_data_preparation_for_powerbi.R:110` | CSD geojson for PowerBI (commented out) | keep inline (commented out) |

### D. Download URLs (active, not commented)

| Value | File:Line | Purpose | Proposed home |
|---|---|---|---|
| `https://www12.statcan.gc.ca/.../2021_98260004.zip` | `09_output_remoteness.R:76` | Dissemination Geographies Relationship File | `config_year.yml` (new key `dguid_url`) |
| `https://www.statcan.gc.ca/.../sgc-cgt-2021-structure-eng.csv` | `09_output_remoteness.R:104`; `17_data_preparation_for_powerbi.R:664` | SGC 2021 structure classification CSV | `config_year.yml` (new key `sgc_structure_url`) |
| `https://www12.statcan.gc.ca/.../lda_000b21a_e.zip` | `06a_output_wildfire.R:281` | DA boundary file download fallback | `config_year.yml` (new key `da_boundary_url`) |
| `https://www150.statcan.gc.ca/pub/45-20-0001/2025001/csv/cisr-eng.zip` | `11_output_CISV_CISR_CIMD.R:51` | CISR scores download | `config_year.yml` (new key `cisr_url`) |
| `https://www150.statcan.gc.ca/pub/45-20-0001/2025001/csv/cisv-eng.zip` | `11_output_CISV_CISR_CIMD.R:106` | CISV scores download | `config_year.yml` (new key `cisv_url`) |
| `https://www150.statcan.gc.ca/pub/45-20-0001/2023001/csv/bc_scores_quintiles_csv-eng.zip.zip` | `11_output_CISV_CISR_CIMD.R:171` | CIMD scores download | `config_year.yml` (new key `cimd_url`) |
| `https://www.statcan.gc.ca/.../sgc-cgt-2021-element-eng.csv` | `17_data_preparation_for_powerbi.R:661` | SGC 2021 element CSV | `config_year.yml` or keep inline (ad-hoc) |

### E. BC Data Catalogue record IDs

| Value | File:Line | Purpose | Proposed home |
|---|---|---|---|
| `"68f2f577-28a7-46b4-bca9-7e9770f2f357"` | `12_output_CHSA_DA_lookup.R:123,127`; `15_remove_geo_suppression_ids.R:250,255`; `17_data_preparation_for_powerbi.R:252,254,256` | CHSA boundaries dataset | `config_year.yml` (new key `bcdc_chsa_record_id`) |
| `"86839277-986a-4a29-9f70-fa9b1166f6cb"` | `08_BC_population_estimates.R:29` | BC sub-provincial population estimates | `config_year.yml` (new key `bcdc_population_record_id`) |
| `"22c7cb44-1463-48f7-8e47-88857f207702"` | `06a_output_wildfire.R:56,62` | BC Wildfire historical perimeters | `config_year.yml` (new key `bcdc_wildfire_historic_record_id`) |
| `"cdfc2d7b-c046-4bf0-90ac-4897232619e1"` | `06a_output_wildfire.R:181,188` | BC Wildfire current perimeters | `config_year.yml` (new key `bcdc_wildfire_current_record_id`) |
| `"0e15d04d-127c-457a-b999-20800c929927"` | `08_BC_population_estimates.R:37` | Population estimates resource ID | `config_year.yml` (new key `bcdc_population_resource_id`) |
| `"874aa151-afe6-400c-876c-aef1ce55102e"` | `17_data_preparation_for_powerbi.R:256` | CHSA master table resource ID | `config_year.yml` or keep inline (ad-hoc) |

### F. Magic values / thresholds / fixed codes

| Value | File:Line | Purpose | Proposed home |
|---|---|---|---|
| `c('IRI', 'IGD', 'NL', 'S-E', 'TAL', 'TWL')` | `15_remove_geo_suppression_ids.R:161` | Indigenous CSD type codes for geo suppression | `config_year.yml` (new key `indig_csd_types`) |
| `50000` (property value filter) | `10_output_housing_value.R:242,243` | Minimum improvement/land value threshold | `config_year.yml` or keep inline (domain constant, unlikely to change) |
| `10000` (SQUARE_METERS_PER_HECTARE) | `06a_output_wildfire.R:113` | Unit conversion constant | keep inline (physical constant) |
| `1000000` (HECTARES_TO_SQM) | `06b_output_wildfire.R:159` | Unit conversion constant (note: **this is wrong** -- should be 10000) | keep inline (but fix the value) |
| `100` (MAX_PERCENT) | `06a_output_wildfire.R:411` | Cap for fire percentage | keep inline (mathematical constant) |
| `2000` (MIN_FIRE_YEAR) | `06a_output_wildfire.R:60` | Start year for wildfire data | `config_year.yml` (duplicated entry, see A above) |
| `"59"` (BC province code / PRUID) | `06a_output_wildfire.R:299`; `09_output_remoteness.R:156,178`; `14_connectivity.R:439`; `15_remove_geo_suppression_ids.R:116`; `01_output_statscan_census.R:324,333` | BC province SGC code | keep inline (immutable geographic constant, used in 5+ scripts) |
| `"British Columbia"` (province filter) | `11_output_CISV_CISR_CIMD.R:30`; `12_output_CHSA_DA_lookup.R:77` | Province name filter for StatsCan data | keep inline (immutable) |
| `"2021A000259"` (BC DGUID prefix) | `09_output_remoteness.R:88` | BC province-level DGUID for filtering | keep inline (derived from census year + province code) |
| `'2021'` (census geo attributes arg) | `15_remove_geo_suppression_ids.R:83`; `14_connectivity.R:437` | Census vintage for geographic attributes | `config_year.yml` (new key `census_vintage`) or keep inline if 2021 is always the reference census |
| `"2023"` (bca_property_values_table key) | `10_output_housing_value.R:193` | Year key into property values table map | keep inline (driven by config.yml `tables.bca_property_values` keys) |
| `c(50, 100, 110)` (violation IDs) | `03_output_crime_rate.R:143` | StatsCan violation type IDs for crime stats | `config_year.yml` or keep inline (domain constant) |

### G. Census vector IDs

| Value | File:Line | Purpose | Proposed home |
|---|---|---|---|
| All `v_CA21_*`, `v_CA16_*`, `v_CA11*`, `v_CA06_*`, `v_CA01_*` vectors | `01_output_statscan_census.R` (lines 44-210, 416-500, 577-700, 757-900, 937-1100) | Census variable identifiers for each census year | keep inline (census-dataset intrinsic; each census has fixed vectors that never change) |

---

## Secrets / credential exposure check

**No secrets or credentials are committed in the tracked code.**

- `config.yml` (which holds `data_server`, `database`, `lan_path`, `CENSUSMAPPER_API_KEY`, etc.) is correctly listed in `.gitignore`.
- No passwords, tokens, or API keys appear as string literals in any `.R` file.
- The `CENSUSMAPPER_API_KEY` is referenced only in `src/background_info/01_statscan_census_background.md` as documentation (`config::get("CENSUSMAPPER_API_KEY")`), which reads from the gitignored `config.yml`.
- Database connections use `Trusted_Connection = "Yes"` (Windows integrated auth), so no password strings appear.
- **One concern:** The `Population_Labour_Social` catalog name and `Prod` schema name are hardcoded in raw SQL strings in `06b_output_wildfire.R` and `15_remove_geo_suppression_ids.R`. While not secrets per se, they are server-internal identifiers that arguably belong in `config.yml` alongside the other database parameters.

---

## Summary counts

| Category | Total findings | Proposed `config_year.yml` | Proposed `config.yml` | Keep inline |
|---|---|---|---|---|
| A. Years | 14 | 8 | 0 | 6 |
| B. SQL table/schema names | 6 | 3 | 3 | 0 |
| C. LAN / file paths | 20 | 20 | 0 | 0 |
| D. Download URLs | 7 | 7 | 0 | 0 |
| E. BC Data Catalogue IDs | 6 | 6 | 0 | 0 |
| F. Magic values / codes | 11 | 2 | 0 | 9 |
| **Total** | **64** | **46** | **3** | **15** |

### Biggest offenders by file

1. **`09_output_remoteness.R`** -- 8+ hardcoded LAN paths, 2 active download URLs, no use of config_year.yml
2. **`06b_output_wildfire.R`** -- hardcoded `FCT_GCS_202509` (conflicts with config_year.yml's `FCT_GCS_202606`), raw SQL with server catalog name, hardcoded shapefile path
3. **`17_data_preparation_for_powerbi.R`** -- 6+ hardcoded LAN paths including export folder with date stamp
4. **`11_output_CISV_CISR_CIMD.R`** -- 3 hardcoded download URLs + 3 LAN folder paths
5. **`15_remove_geo_suppression_ids.R`** -- hardcoded `output_year = 2023`, raw SQL with catalog/schema, indigenous CSD type codes

### Critical finding

**`06b_output_wildfire.R:130` hardcodes `FCT_GCS_202509`** while `config_year.yml` declares `FCT_GCS_202606` and scripts 03/04 correctly read from config. This means 06b is reading a **stale snapshot** and will silently diverge from the other scripts at next refresh. This is the single highest-priority fix.

### No secrets exposed

All credentials, API keys, and connection details reside in `config.yml` (gitignored). No passwords or tokens were found in tracked code.
