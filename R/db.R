# Normalized database connection helper (wayfinder ADR-0009).
#
## ----------------------------------------------------------
## Reasons for change, other notes
## ----------------------------------------------------------
## Six scripts repeated dbConnect() boilerplate with two DIFFERENT config
## keys for the same production server: config$database (06b/10/12, has
## trusted_connection) and config$data_server (15, no trusted_connection
## key, hardcoded "Yes"). That key drift is the same failure class as the
## 06b stale-snapshot bug (wayfinder #9). connect_db() normalizes on
## config$database with a backward-compatible fallback to data_server.

#' Connect to the BC Stats SQL Server using config.yml settings
#'
#' Reads connection details from the gitignored config.yml, preferring the
#' `database:` block (canonical) and falling back to `data_server:` for
#' configs that only define that key. trusted_connection defaults to "Yes"
#' (Windows integrated auth) when not specified.
#'
#' @param config Parsed config list from config::get(). If NULL, fetched.
#' @return An odbc connection object; errors with context on failure.
connect_db <- function(config = NULL) {
  if (is.null(config)) {
    config <- config::get()
  }

  db <- config$database
  if (is.null(db)) {
    db <- config$data_server
  }
  if (is.null(db)) {
    stop(
      "connect_db(): config.yml defines neither 'database:' nor 'data_server:'",
      call. = FALSE
    )
  }

  trusted <- db$trusted_connection
  if (is.null(trusted)) {
    trusted <- "Yes"
  }

  tryCatch(
    {
      con <- DBI::dbConnect(
        odbc::odbc(),
        Driver = db$driver,
        Server = db$server,
        Database = db$database,
        Trusted_Connection = trusted
      )
      cat("Successfully connected to the database\n")
      con
    },
    error = function(e) {
      stop(glue::glue("Failed to connect to database: {e$message}"))
    }
  )
}
