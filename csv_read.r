# install.packages(anytime)
# install.packages(ggplot2)
# install.packages(hexbin)

library(hexbin)
library(anytime)
# query the data from postgreSQL
df_aqua <- read.csv(file="./20180714_data.csv", header=TRUE, sep=",")

# colnames(df_aqua)
head(df_aqua)
# df_aqua$observed_at
df_aqua$observed_at <- anytime(df_aqua$observed_at)
# df_aqua$observed_at
# 2018-07-03 06:41:46
# df_aqua$ph_read
# df_aqua$temp_read
# df_aqua$lux_read
par(mfrow=c(3,2))

plot(df_aqua$observed_at, df_aqua$ph_read)
lines(df_aqua$observed_at, df_aqua$ph_read)
scatter.smooth(x=df_aqua$observed_at, y=df_aqua$ph_read, main="Aquarium pH")

plot(df_aqua$observed_at, df_aqua$temp_read)
lines(df_aqua$observed_at, df_aqua$temp_read)
scatter.smooth(x=df_aqua$observed_at, y=df_aqua$temp_read, main="Aquarium Temperature")

plot(df_aqua$observed_at, df_aqua$lux_read)
lines(df_aqua$observed_at, df_aqua$lux_read)
scatter.smooth(x=df_aqua$observed_at, y=df_aqua$lux_read, main="Aquarium Lux")

bin <- hexbin(df_aqua$observed_at, df_aqua$ph_read)
plot(bin, main = "pH Hexagonal Binning")

bin <- hexbin(df_aqua$observed_at, df_aqua$temp_read)
plot(bin, main = "Temp Hexagonal Binning")

bin <- hexbin(df_aqua$observed_at, df_aqua$lux_read)
plot(bin, main = "Lux Hexagonal Binning")


# Basic Graph of the Data
# require(ggplot2)
# ggplot(df_aqua, aes(x = observed_at, y = ph_read))
