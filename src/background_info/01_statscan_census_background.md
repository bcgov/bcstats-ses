# StatsCan Census 2021 Data Retrieval Guide

This document outlines how to retrieve Census 2021 data from Statistics Canada (StatsCan) using the `cancensus` package, including details on geographic levels, API setup, and examples of querying census data. This guide also covers setting up caching for efficiency and working with census vectors.

## Geographic Levels in Census Data

Census data in Canada is organized by different geographic levels:

- **Census Divisions (CD)**: These are the primary geographic areas used for the country-wide census. In British Columbia, there are 28 CDs (as of the 2011 Census), including 27 Regional Districts and 1 unorganized area (Stikine). Census Division boundaries align with regional districts and other administrative boundaries.
  
- **Census Subdivisions (CSD)**: These are municipal or municipal-equivalent areas and aggregate into Census Divisions (CDs). British Columbia contains 837 CSDs, but not all may be represented in the Translation Master File (TMF), as some areas lack postal codes.

Census data for British Columbia is organized using Standard Geographical Classification (SGC) codes, which follow a hierarchical structure:
- **Census Division (CD)**: A two-digit code.
- **Census Subdivision (CSD)**: A three-digit code.

The TMF used by BC Stats slightly differs from Statistics Canada's SGC due to the exclusion of the province code.

## Data File Location

The location of data files is stored in the `config.yaml` file for easy access.

## Setting Up the CENSUSMAPPER API

You will need to set your `CENSUSMAPPER` API key and configure caching for optimal performance.

### Set API Key

```r
set_cancensus_api_key(config::get("CENSUSMAPPER_API_KEY"), install = TRUE)
```

### Set Up Persistent Cache

To reduce API usage and speed up queries, configure a persistent cache directory. This will store retrieved data locally and be reused across sessions.

```r
set_cancensus_cache_path(use_network_path("data/census_cache"), install = TRUE)
```

Alternatively, you can set options manually:

```r
options(cancensus.api_key = "your_api_key")
options(cancensus.cache_path = "custom_cache_path")
```

The cache path can be accessed as follows:

```r
Sys.getenv("CM_CACHE_PATH")
```

## Retrieving Census Metadata

To explore available census datasets and vectors, use the following commands:

- **List available datasets**:
  ```r
  list_census_datasets()
  ```

- **List available vectors for a dataset**:
  ```r
  vector_list_21 <- list_census_vectors("CA21")
  ```

The `CA21` dataset contains 7,709 rows and 7 columns of census variables, providing information such as variable labels, units, and descriptions.

## Example: Retrieving Census Data

Here’s an example of retrieving census data for the Vancouver Census Metropolitan Area (CMA):

```r
van <- get_census(dataset='CA21', regions=list(CMA="59933"),
                  vectors=c("median_hh_income"="v_CA21_906"), level='CSD', 
                  quiet = TRUE, geo_format = 'sf', labels = 'short')
```

You can also retrieve data at more granular geographic levels like Dissemination Blocks (DB) or Dissemination Areas (DA):

```r
van_db <- get_census(dataset='CA21', regions=list(CMA="59933"),
                     vectors=c("median_hh_income"="v_CA21_906"), level='DB', 
                     quiet = TRUE, geo_format = NA, labels = 'short')

van_da <- get_census(dataset='CA21', regions=list(CMA="59933"),
                     vectors=c("median_hh_income"="v_CA21_906"), level='DA', 
                     quiet = TRUE, geo_format = NA, labels = 'short')
```

## Census Variables

Census data files typically contain the following variables:

- **Vector**: Short code for the variable.
- **Type**: Aggregates of male, female, or total responses.
- **Label**: Descriptive name of the variable.
- **Units**: Indicates counts, ratios, percentages, or currency figures.
- **Parent_vector**: The parent category of the variable, if applicable.
- **Aggregation**: Indicates whether the variable is additive or an average.
- **Description**: Detailed hierarchical description for ease of searching.

## Example: Employment Data Vectors

Here’s an example of retrieving vectors related to labor force and employment income:

```r
em_labels <- c("Number of employment income recipients aged 15 years and over in private households in 2020",
               "Median employment income in 2020 among recipients",
               "Number of employment insurance benefits recipients aged 15 years and over in private households in 2020")

em_vectors <- find_census_vectors("employment", dataset= "CA21", type = "total", query_type = "semantic") %>% 
              filter(str_detect(label, pattern = paste(em_labels, collapse = "|")))

lf_labels <- c("Total - Population aged 15 years and over by labour force status")
lf_vectors <- find_census_vectors("labor force", dataset= "CA21", type = "total", query_type = "semantic")

income_vector <- find_census_vectors("after tax income", dataset = "CA21", type = "total", query_type = "semantic")
```

## Best Practice: Named Vector List

To simplify retrieval of census data, we recommend generating a list of census vectors and metadata in a CSV file. You can then create a "named vector list" from this file for more organized data retrieval.

By structuring the data this way, you ensure the `vector` remains the key linking various datasets.

## Conclusion

This guide covers the basics of retrieving and working with Census 2021 data using `cancensus`. By setting up proper API access and caching, you can work efficiently with large datasets. The examples provided demonstrate how to query specific vectors and retrieve metadata for your analysis.
