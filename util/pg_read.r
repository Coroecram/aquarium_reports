# install.packages("RPostgreSQL")
# install.packages("ggplot2")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "test"
}

# loads the PostgreSQL driver
drv <- DBI::dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "test_python",
                 host = "localhost", port = 5432,
                 user = "test_user", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "aquarium_data")
# TRUE

# query the data from postgreSQL
df_aqua <- dbGetQuery(con, "SELECT * from aquarium_data;")


# colnames(df_aqua)
head(df_aqua)
df_aqua$observed_at
# 2018-07-03 06:41:46
# df_aqua$ph_read
# df_aqua$temp_read
# df_aqua$lux_read

plot(df_aqua$observed_at, df_aqua$ph_read)
scatter.smooth(x=df_aqua$observed_at, y=df_aqua$ph_read, main="Aquarium pH")

# Basic Graph of the Data
# require(ggplot2)
# ggplot(df_aqua, aes(x = observed_at, y = ph_read))
