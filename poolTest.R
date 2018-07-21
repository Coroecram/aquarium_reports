library(dplyr)
library(DT)
library(pool)

require("RPostgreSQL")

aws_pg <- dbPool(
  DBI::dbDriver("PostgreSQL"),
  dbname = "aquarium",
  host = Sys.getenv("AWS_PG_HOST"), port = 5432,
  user = Sys.getenv("AWS_PG_USER"), password = Sys.getenv("AWS_PG_PW")
)

  df_aqua <- aws_pg %>% tbl("aquarium_data")

  selected_readings <- df_aqua %>% filter (observed_at > Sys.Date() - 7 & observed_at < Sys.Date()) %>% collect()

  str_observed_at <- format(df_aqua %>% select(observed_at), "%B %d, %Y %H:%M:%S %p")

  selected_readings
