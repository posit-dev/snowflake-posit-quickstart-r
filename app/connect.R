library(DBI)
library(odbc)
library(dbplyr)
library(dplyr)

# DATABASE CONNECTION ----
# Connect to the database
conn <-
  DBI::dbConnect(
    odbc::snowflake(),
    warehouse = "DEFAULT_WH",
    database = "HEART_FAILURE",
    schema = "PUBLIC"
  )

# TABLE ----
heart_failure <- tbl(conn, "HEART_FAILURE") |> rename_with(str_to_lower)
