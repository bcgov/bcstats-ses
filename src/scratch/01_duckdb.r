library("duckdb")
# to start an in-memory database
# con <- dbConnect(duckdb())
# or
# con <- dbConnect(duckdb(), dbdir = ":memory:")
# to use a database file (not shared between processes)
con <- dbConnect(duckdb(), dbdir = "./src/scratch/my-db.duckdb", read_only = FALSE)
# to use a database file (shared between processes)
# con <- dbConnect(duckdb(), dbdir = "my-db.duckdb", read_only = TRUE)

# create a table
dbExecute(con, "CREATE TABLE items (item VARCHAR, value DECIMAL(10, 2), count INTEGER)")
# insert two items into the table
dbExecute(con, "INSERT INTO items VALUES ('jeans', 20.0, 1), ('hammer', 42.2, 2)")

flight = dbGetQuery(con, "SELECT * FROM './src/scratch/flights.csv'")

dbExecute(con, "CREATE TABLE ontime (
  FlightDate DATE,
  UniqueCarrier VARCHAR,
  OriginCityName VARCHAR,
  DestCityName VARCHAR
);
")


dbExecute(con, "COPY ontime FROM './src/scratch/flights.csv';")



dbExecute(con, "COPY (SELECT * FROM ontime) TO './src/scratch/' WITH (HEADER, DELIMITER ',');")

dbExecute(con, "COPY (SELECT * FROM ontime) TO './src/scratch/'  (
          FORMAT PARQUET,
          PARTITION_BY(FlightDate),
          OVERWRITE_OR_IGNORE 1
);")

dbExecute(con, "CREATE TABLE ontime2 AS
SELECT * FROM read_csv('./src/scratch/flights.csv',
                       delim = '|',
                       header = true,
                       columns = {
                         'FlightDate': 'DATE',
                         'UniqueCarrier': 'VARCHAR'
                       });
")

# retrieve the items again
res <- dbGetQuery(con, "SELECT * FROM ontime2")

path = './src/scratch/flights.csv'
duckdb_read_csv(con, "ontime22", 
                path,
                col.types = c(
                  FlightDate = "DATE",
                  UniqueCarrier = "VARCHAR"
                )
)


# Providing data types for columns
path <- tempfile(fileext = ".csv")
write.csv(iris, path, row.names = FALSE)

con <- dbConnect(duckdb())
duckdb_read_csv(con, "iris", path,
                # columns = c("FlightDate", "UniqueCarrier", "OriginCityName", "DestCityName"),
                # col.types = c(
                #   Sepal.Length = "DOUBLE",
                #   Sepal.Width = "DOUBLE",
                #   Petal.Length = "DOUBLE",
                #   Petal.Width = "DOUBLE",
                #   Species = "VARCHAR"
                # )
)



dbExecute(con, "CREATE TABLE ontime3 AS 
SELECT * FROM read_csv('./src/scratch/flights.csv',
                       delim = '|',
                       header = true,
                       column_types = {
                         'FlightDate': 'DATE',
                         'UniqueCarrier': 'VARCHAR'
                       });
")

# retrieve the items again
res <- dbGetQuery(con, "SELECT * FROM ontime3")

dbExecute(con, "CREATE TABLE ontime4 AS 
SELECT * FROM read_csv('./src/scratch/flights.csv',
                       delim = '|',
                     
                      names = ['DateOfFlight', 'CarrierName']);
")

# retrieve the items again  
res <- dbGetQuery(con, "SELECT * FROM ontime4")



# retrieve the items again
res <- dbGetQuery(con, "SELECT * FROM items")



# retrieve the items again
res <- dbGetQuery(con, "SELECT * FROM items")
print(res)
#     item value count
# 1  jeans  20.0     1
# 2 hammer  42.2     2

dbDisconnect(con, shutdown = T)
# duckdb_shutdown(drv = con)
