-- Connect to your database or create a new one
.open db/bcstats_ses_1.duckdb

-- Load extensions if needed
.load httpfs
.load parquet

-- Read your CSV files
CREATE TABLE your_table AS SELECT * FROM read_csv_auto('your_file.csv');

-- Or read Parquet files
CREATE TABLE your_parquet_table AS SELECT * FROM parquet_scan('your_file.parquet');

-- Perform your data processing operations
CREATE TABLE result_table AS
SELECT ...
FROM your_table
WHERE ...
GROUP BY ...;

-- Export results if needed
COPY result_table TO 'output.csv' (HEADER, DELIMITER ',');

-- Or save as Parquet
COPY result_table TO 'output.parquet' (FORMAT PARQUET);