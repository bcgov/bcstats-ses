# Testability Audit

**Ticket:** `wayfinder/tickets/01-testability-audit.md`
**Date:** 2026-07-24
**Branch:** `wayfinder/refactor-best-practices`

---

## Existing test: `test/test_crime_rate_regression.R`

- **Framework:** Custom snapshot test (no testthat).
- **Pattern:** Record/Replay. If `test/snapshots/BC_DA_Crime_Rate_golden.csv` does not exist, it sources the entire `src/03_output_crime_rate.R` pipeline, reads the output CSV from `out/`, and saves it as the golden baseline. On subsequent runs it re-runs the pipeline and compares: (a) identical dimensions, (b) identical column names/order, (c) value-level equality via `all.equal()`.
- **CI suitability:** **None.** The test explicitly requires live access to the cansim SQLite table (35-10-0184-01) and the SQL Server database. It is documented as a local-only fidelity check.

---

## Per-Script Classification

### Legend

- **Pure / CI-testable** -- data transforms, calculations, joins, formatting, helpers with no DB query, LAN file I/O, or network download.
- **DB/LAN-coupled** -- `DBI::dbConnect`/`dbGetQuery`/`odbc`, `bcdata::bcdc_*` downloads, `safepaths::use_network_path` LAN reads/writes, `cancensus::get_census` API calls, `cansim::get_cansim_connection`.

---

### `src/01_output_statscan_census.R` (1140 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | `create_census_meta_data()` (L297-313) | Joins named vector of census vectors to `list_census_vectors()` metadata. Pure join, but calls `list_census_vectors()` which is an API call -- NOT testable without mock. |
| Pure | `create_census_data_label()` (L368-386) | Builds a named label vector from data attributes and vector list. Pure data-frame/tibble wrangling IF the input `DATA` attribute is pre-populated. |
| Pure | CA01 inline mutations (L1069-1091) | `EDUC_NONE = EDUC_WO_HS + EDUC_LESS_THAN_GRADE9`, `EDUC_POSTSEC = EDUC_TRADES + EDUC_COLLEGE + EDUC_UNI`, `LICO_AT_PREVALENCE = LICO_AT_INCIDENCE / LICO_AT_TOTAL`. Testable arithmetic. |
| DB/LAN | `get_census()` calls (L325, L528, L689, L872, L1057) | Downloads census data for 2021, 2016, 2011, 2006, 2001 via cancensus API (LAN cache). |
| DB/LAN | `write_csv` to `out/` (multiple) | File output. |

**CI-testable fraction: ~5%** (inline arithmetic for CA01; label-building logic could be tested if factored out with mock data)

---

### `src/03_output_crime_rate.R` (344 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Violation ID regex extraction (L121) | `gsub(".*\\[(\\d+)\\].*", "\\1", Violations)` -- pure string transform. |
| Pure | Population-weighted mean (L275-283) | `weighted.mean(VALUE, w = POP_CNT)` grouped by DA/violation/statistic. Pure math on a data frame. |
| DB/LAN | `cansim::get_cansim_connection()` (L105-110) | Downloads cansim table via sqlite cache. |
| DB/LAN | `readxl::read_excel(lan_path/...)` (L185-190) | Reads DA-RESP lookup from LAN. |
| DB/LAN | `DBI::dbConnect()` + `tbl(con, ...)` (L209-224) | Connects to SQL Server for TMF/GCS table. |
| DB/LAN | `use_network_path()` (L341) | Writes dictionary to LAN. |

**CI-testable fraction: ~5%** (the weighted-mean aggregation and regex extraction are testable if factored into functions)

---

### `src/04_output_TMF.R` (276 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | `create_item()` (L168-211) | Maps TMF field names to short column codes via `case_match()`. Fully pure. |
| Pure | `str_remove(item, "_\\d{4}$")` (L224) | Strips year suffix from item names. Inline but pure. |
| DB/LAN | `DBI::dbConnect()` + `tbl(con, ...)` (L80-104) | Connects to SQL Server for TMF/GCS table. |
| DB/LAN | `read_csv(lan_path/.../TMF_data_dict.csv)` (L163-165) | Reads TMF dictionary from LAN. |
| DB/LAN | `read_excel(lan_path/.../GCS_Lookup_Table.xlsx)` (L248) | Reads lookup tables from LAN. |

**CI-testable fraction: ~10%** (the `create_item()` function is ~45 lines of pure mapping logic)

---

### `src/05_output_LFS.R` (276 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Data cleaning pipeline (L51-111) | Filter BC, clean names, recode STATUS on NA, convert to yearmon, drop all-NA columns, drop single-distinct-value columns, filter to "Estimate", extract GEO_TYPE from brackets, select final columns. All pure dplyr/stringr transforms. |
| Pure | SLA lookup tribble + merge (L143-210) | Hard-coded CMA-to-SLA_NAME lookup table, left_join, coalesce SLA_NAME. Pure. |
| DB/LAN | `cansim::get_cansim_connection()` (L46-47) | Downloads LFS data from StatsCan. |
| DB/LAN | `readxl::read_excel(lan_path/...SLA2016_...)` (L122-127) | Reads SLA classification from LAN. |

**CI-testable fraction: ~30%** (the data cleaning and SLA merge logic is substantial and purely relational)

---

### `src/06a_output_wildfire.R` (617 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Fire consolidation logic (L98-155) | `group_by(FIRE_LABEL) %>% filter(n() > 1)` to detect and consolidate duplicate fires. Pure dplyr. |
| Pure | DA-level fire area grouping (L373-420) | `group_by(FIRE_YEAR, DAUID) %>% summarise(...)` for fire statistics per DA. Pure if sf data is available. |
| DB/LAN | `bcdata::bcdc_query_geodata()` (L62, L188) | Downloads wildfire perimeter data from BC Data Catalogue. |
| DB/LAN | `use_network_path()` (multiple: L73, L201, L282, L287, L290, etc.) | Reads/writes shapefiles and CSVs on LAN. |
| DB/LAN | `st_write(use_network_path(...))` (L73, L201) | Writes geodata to LAN. |

**CI-testable fraction: ~5%** (consolidation/grouping logic is embedded in long I/O-bound script)

---

### `src/06b_output_wildfire.R` (348 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Intersection area ratio calculations (L196-198) | `pcnt_fire = intersect_area_m2 / land_area_sqm`, `pcnt_fire_2 = intersect_area_m2 / da_area_m2`. Pure arithmetic. |
| Pure | CSD-level fire aggregation (L327-335) | `group_by(FIRE_YEAR, CSDdguid, CSDname, CSDtype) %>% summarise(...)`. Pure dplyr. |
| DB/LAN | `DBI::dbConnect()` + `dbGetQuery()` (L101, L119) | Connects to SQL Server for CSD names and fire data. |
| DB/LAN | `use_network_path()` (multiple) | Reads/writes data on LAN. |

**CI-testable fraction: ~5%**

---

### `src/07_SLA.R` (89 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | SLA data cleaning (L26-55) | Filter PR==59, rename columns, clean names, convert to character, set SLA_CODE to NA when SLA_NAME is NA. Pure dplyr. |
| DB/LAN | `readxl::read_excel(use_network_path(...))` (L21-22) | Reads SLA classification from LAN. |
| DB/LAN | `write_csv(...)` (L69) | Writes output. |

**CI-testable fraction: ~20%** (small script, but the cleaning logic is straightforwardly pure)

---

### `src/08_BC_population_estimates.R` (166 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | TMF reshape + fill (L74-105) | `pivot_longer` to melt CDCSD/CSD/MUNNAME across years, `expand_grid` for all year combos, `fill()` forward/backward. Pure tidyr. |
| Pure | Population join + rename (L111-129) | Filter estimates, pad CD codes, join with TMF, rename age columns. Pure dplyr. |
| DB/LAN | `bcdc_get_data()` (L33-34) | Downloads population estimates from BC Data Catalogue. |
| DB/LAN | `use_network_path("data/raw_data/TMF/...")` (L54) | Reads TMF CSV from LAN. |
| DB/LAN | `write_csv(use_network_path(...))` (L138, L163) | Writes to LAN. |

**CI-testable fraction: ~15%** (TMF reshape logic is reusable; population join is straightforward)

---

### `src/09_output_remoteness.R` (603 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | DA-level average calculation (L295-302) | `group_by(DAID, TAG_2) %>% summarise(AVG_DRV_TIME_SEC = mean(...), AVG_DRV_DIST = mean(...), N_ADDRESS = n_distinct(...))`. Pure aggregation. |
| Pure | CSD-level summary (L351-359) | Same pattern at CSD level. Pure. |
| DB/LAN | `bcdata::bcdc_query_geodata()` (multiple) | Downloads DA/CSD boundary files from BC Data Catalogue. |
| DB/LAN | `use_network_path()` (extensive: L149, L170, L201, L212, L221, L242, L260, L284, L311, L404) | Reads/writes address data, shapefiles, CSVs on LAN. |
| DB/LAN | `sf` spatial operations (st_read, st_join, st_write) | Read/write spatial data from LAN paths. |

**CI-testable fraction: ~10%** (the group_by/summarise logic is pure but small relative to the sf/LAN operations)

---

### `src/10_output_housing_value.R` (378 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Postal code cleaning (L163-169) | `case_when` to standardize postal codes. Pure string logic. |
| Pure | Median property value calculation (L273-280) | `group_by %>% mutate %>% filter %>% distinct` pattern for median. Pure dplyr. |
| DB/LAN | `DBI::dbConnect()` (L73-82) | Connects to SQL Server for BC Assessment data. |
| DB/LAN | `DBI::dbGetQuery()` (L187+) | Queries BC Assessment property data. |
| DB/LAN | `tbl_long_cols_mssql()` from utils.R | Uses DBI connection for column metadata. |

**CI-testable fraction: ~5%**

---

### `src/11_output_CISV_CISR_CIMD.R` (219 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Province filtering (L55) | `filter(PROVINCE_OR_TERRITORY == province)`. Trivial but pure. |
| Pure | Dictionary label construction (multiple) | Building named label vectors for CISR/CISV/CIMD data dictionaries. Pure. |
| DB/LAN | `download_and_process_dataset()` from utils.R | Downloads ZIP files from web, unzips, reads CSV. Network I/O. |
| DB/LAN | `read_note()` from utils.R | Reads note CSVs from disk. File I/O. |

**CI-testable fraction: ~10%** (dictionary label building; data is downloaded by utils.R)

---

### `src/12_output_CHSA_DA_lookup.R` (630 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | CHSADA population weighting (L501-543) | Computes `chsada_pop`, `da_pop`, `chsa_pop`, `chsada_to_da_pop_ratio`, `chsada_to_chsa_pop_ratio` via grouped mutates. This is a non-trivial pure algorithm for splitting DA values across CHSAs by population proportion. |
| Pure | Multi-CHSA/multi-DA detection (L343-368, L416-450, L448-450, L466-468) | `group_by %>% summarise(DA_CNT = n_distinct(...)) %>% filter(DA_CNT > 1)` pattern for detecting boundary crossings. Pure. |
| DB/LAN | `DBI::dbConnect()` (L90-99) | Connects to SQL Server for CHSA list. |
| DB/LAN | `bcdata::bcdc_query_geodata()` / `bcdc_get_data()` (L127, L131) | Downloads CHSA boundaries from BC Data Catalogue. |
| DB/LAN | `use_network_path()` (multiple: L583-586, L629) | Writes to LAN. |

**CI-testable fraction: ~15%** (the CHSADA weighting algorithm is the most complex pure logic in the pipeline)

---

### `src/13_BC_DA_population_estimates.r` (70 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Filter + rename (L60-70) | Filter Sex/Age/Type, select/rename columns. Pure dplyr. |
| DB/LAN | `list.files()` + `purrr::map()` + `read_csv()` (L30-45) | Reads population estimate CSVs from disk (path not LAN-coupled but still file I/O). |

**CI-testable fraction: ~10%**

---

### `src/14_connectivity.R` (1224 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | `parse_speed()` (L129-146) | Parses speed threshold strings (e.g. `"50_10"`, `"<5_1"`) into down/up/is_lt5_1 columns. Fully pure, well-documented. |
| Pure | `flag_all_tiers()` (L153-168) | Creates boolean flags for each speed tier from parsed speed data. Fully pure. |
| Pure | `compute_combined_max()` (L176-202) | Returns the higher of wired/wireless speed tier using a tier hierarchy. Fully pure. |
| Pure | `threshold_to_numeric()` (L208-228) | Maps speed threshold strings to numeric scores (5=50_10, ..., 1=<5_1, 0=NA). Fully pure. |
| Pure | `aggregate_comparison()` (L273-316) | Aggregates DB-level comparison deltas/outliers to CSD or DA level with dwelling-weighted means. Pure dplyr. |
| DB/LAN | `read_csv(lan_path/...)` (multiple: L324-336, L339-343) | Reads NBD speeds, PHH points, TMF from LAN. |
| DB/LAN | `read_excel(lan_path/...)` | Reads CITZ data from LAN. |
| DB/LAN | `write_csv()` (L510, L643, L674, L725, L826, L1008) | Writes outputs to LAN. |

**CI-testable fraction: ~12%** (5 well-factored pure functions totaling ~100 lines; the rest is LAN I/O)

---

### `src/15_remove_geo_suppression_ids.R` (496 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | `remove_geographies()` (L305-331) | Filters rows by excluding specified geography codes, logs removed count. Fully pure dplyr. |
| Pure | Nisga'a CHSA detection (L265-270) | `str_detect(CMNTY_HLTH_SERV_AREA_NAME, "Nisga")` pattern. Pure string logic. |
| Pure | Masked row filtering (L424-426, L459-461) | `filter(!TOTAL_INDEX_0_100 == "masked")`. Pure. |
| DB/LAN | `DBI::dbConnect()` + `dbGetQuery()` (L195-225) | Connects to SQL Server for Indigenous CSD lookup. |
| DB/LAN | `bcdata::bcdc_query_geodata()` (L255) | Downloads CHSA boundaries from BC Data Catalogue. |
| DB/LAN | `read_csv(lan_path/...)` (L280-298) | Reads SEI files from LAN. |

**CI-testable fraction: ~15%** (the `remove_geographies()` function is the best candidate here)

---

### `src/17_data_preparation_for_powerbi.R` (711 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | INDEX_LABEL mapping (L342-353) | `case_when` mapping from factor codes to human-readable index labels. Pure. |
| Pure | FACTOR_LABEL mapping (L482-490, L547-560) | `case_when` mapping variable names to short factor labels. Pure. |
| Pure | Data dictionary construction (L537, L633-641) | Joins and renames dictionary columns. Pure. |
| DB/LAN | `bcdata::bcdc_get_data()` / `bcdc_query_geodata()` (L254, L127) | Downloads CHSA boundaries. |
| DB/LAN | `read_csv()` (multiple) | Reads SEI output files (likely from LAN or out/). |
| DB/LAN | `use_network_path()` (L25-29) | Reads from LAN. |

**CI-testable fraction: ~10%**

---

### `src/18_trend_ses.R` (74 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| Pure | Trend calculation (L37-55) | `group_by(year) %>% summarise(mean, sd, se, bounds)`. Pure dplyr/stats. |
| Pure | ggplot trend chart (L56-66) | Plotting logic (side-effect only, no I/O beyond display). |
| DB/LAN | `read_csv(lan_path/...)` (L32-35) | Reads longitudinal index CSV from LAN. |
| DB/LAN | `st_read(lan_path/...)` (L71-74) | Reads CSD shapefile from LAN. |

**CI-testable fraction: ~25%** (small script; the trend summarise is most of the logic)

---

### `src/utils.R` (523 lines)

| Category | Section / Function | Description |
|----------|-------------------|-------------|
| DB/LAN | `tbl_long_cols_mssql()` (L365-415) | Connects to SQL Server via DBI, queries INFORMATION_SCHEMA.COLUMNS, sorts columns. DB-coupled. |
| LAN | `download_and_process_dataset()` (L419-487) | Downloads ZIP from URL, unzips, reads CSV. Network + file I/O. |
| LAN | `read_note()` (L489-523) | Lists and reads CSV files from a directory. File I/O. |
| LAN | `plot_bc_address_map()` (L39-88) | ggplot + `ggsave()` to local and LAN path. |
| LAN | `plot_csd_avg_map_fn()` (L103-149) | ggplot + `ggsave()` to local and LAN path. |
| LAN | `plot_csd_facility_map_fn()` (L169-246) | ggplot (no file save, but returns plot). Partially testable if ggsave is mocked. |
| LAN | `compare_two_csd_in_map()` (L267-362) | Combines two plots, `ggsave()` to local and LAN path. |

**CI-testable fraction: ~0%** (every function has a file I/O or DB side effect; none are factored into pure + I/O layers)

---

## Summary Table

| Script | Lines | CI-testable % | Notes |
|--------|------:|---------------:|-------|
| `01_output_statscan_census.R` | 1140 | 5% | Dominated by `get_census()` API calls for 5 census years. |
| `03_output_crime_rate.R` | 344 | 5% | cansim download + SQL Server + LAN Excel read. Weighted mean is inline. |
| `04_output_TMF.R` | 276 | 10% | `create_item()` is the only extracted function. |
| `05_output_LFS.R` | 276 | 30% | Best ratio: substantial data cleaning pipeline is pure dplyr. |
| `06a_output_wildfire.R` | 617 | 5% | bcdata + sf + LAN reads/writes throughout. |
| `06b_output_wildfire.R` | 348 | 5% | SQL Server + sf + LAN. |
| `07_SLA.R` | 89 | 20% | Small; cleaning logic is simple but pure. |
| `08_BC_population_estimates.R` | 166 | 15% | TMF reshape is reusable pure logic. |
| `09_output_remoteness.R` | 603 | 10% | Massive sf/LAN operations; small pure aggregation. |
| `10_output_housing_value.R` | 378 | 5% | SQL Server queries dominate. |
| `11_output_CISV_CISR_CIMD.R` | 219 | 10% | Download via utils.R; label building is pure. |
| `12_output_CHSA_DA_lookup.R` | 630 | 15% | CHSADA population weighting algorithm is non-trivial and pure. |
| `13_BC_DA_population_estimates.r` | 70 | 10% | Tiny script; filter/rename is pure. |
| `14_connectivity.R` | 1224 | 12% | 5 well-factored pure functions (~100 lines). |
| `15_remove_geo_suppression_ids.R` | 496 | 15% | `remove_geographies()` is clean and pure. |
| `17_data_preparation_for_powerbi.R` | 711 | 10% | case_when label mappings are pure. |
| `18_trend_ses.R` | 74 | 25% | Trend summarise is pure; small script. |
| `utils.R` | 523 | 0% | Every function has I/O or DB side effects. |
| **TOTAL** | **~8114** | **~10%** | Weighted average across all scripts. |

---

## Top Unit-Test Candidates (ranked)

### 1. `parse_speed()` -- `src/14_connectivity.R` L129-146
Pure string-parsing function. Takes a character vector of speed strings, returns a data frame with `down`, `up`, `is_lt5_1`. No dependencies on external state. Ideal for testthat with a few representative inputs.

### 2. `compute_combined_max()` -- `src/14_connectivity.R` L176-202
Pure logic function. Takes two speed threshold strings, returns the higher one based on a tier hierarchy. Clear edge cases (both NA, one NA, equal tiers). No dependencies.

### 3. `remove_geographies()` -- `src/15_remove_geo_suppression_ids.R` L305-331
Pure dplyr function. Filters rows by geography code, returns filtered data frame and logs removal count. Easy to test with a small tibble and a known set of codes to remove.

### 4. `create_item()` -- `src/04_output_TMF.R` L168-211
Pure `case_match` mapping. Maps 30+ TMF field names to short column codes. Testable by checking all known inputs produce expected outputs.

### 5. `threshold_to_numeric()` -- `src/14_connectivity.R` L208-228
Pure mapping function. Converts speed threshold strings to numeric scores. Simple but has clear input/output contract.

### 6. `flag_all_tiers()` -- `src/14_connectivity.R` L153-168
Pure boolean logic. Depends on `SPEED_TIERS` constant and output of `parse_speed()`. Testable in combination with `parse_speed()`.

### 7. CHSADA population weighting -- `src/12_output_CHSA_DA_lookup.R` L501-543
The most algorithmically complex pure logic in the pipeline (population-proportional DA-to-CHSA weight calculation). Currently inline; would need to be extracted into a function first.

### 8. `aggregate_comparison()` -- `src/14_connectivity.R` L273-316
Pure dplyr aggregation. Takes DB-level comparison data and rolls it up to CSD/DA level with dwelling-weighted means. Would need small test data.

---

## Key Observations

1. **The pipeline is overwhelmingly I/O-bound.** ~90% of the codebase is data acquisition (DBI, bcdata, cansim, cancensus, safepaths/LAN reads). Only ~10% is pure data transformation.

2. **Pure logic is rarely factored into functions.** Most transforms are inline pipe chains at the top level of each script. The exception is `14_connectivity.R`, which has 5 well-factored pure helper functions.

3. **`utils.R` has zero CI-testable functions.** Every function in utils.R performs file I/O or DB queries. The plotting functions all call `ggsave()` to local and/or LAN paths.

4. **The CHSADA weighting algorithm (script 12) is the highest-value extraction target.** It is the most complex pure logic in the pipeline but is currently a 40-line inline pipe chain.

5. **To meaningfully increase CI testability, the primary lever is extracting pure transform functions from inline pipe chains** in scripts 05, 08, 09, 12, and 17 -- not adding tests to the already-factored functions in script 14.
