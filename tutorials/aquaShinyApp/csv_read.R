# install.packages("anytime")
# install.packages("ggplot2")
# install.packages("hexbin")
# install.packages("reshape2")

library(hexbin)
library(anytime)
library(ggplot2)
library(reshape2)

# query the data from postgreSQL
df_aqua <- read.csv(file="./20180714_data.csv", header=TRUE, sep=",")

# colnames(df_aqua)
head(df_aqua)
# df_aqua$observed_at
df_aqua$observed_at <- anytime(df_aqua$observed_at)
df_aqua$id <- seq.int(nrow(df_aqua))
observed_at <- df_aqua$observed_at
id <- df_aqua$id
ph_read <- df_aqua$ph_read
temp_read <- df_aqua$temp_read
lux_read <- df_aqua$lux_read
# df_aqua$observed_at
# 2018-07-03 06:41:46
# df_aqua$ph_read
# df_aqua$temp_read
# df_aqua$lux_read
# par(mfrow=c(3,2))

plot(df_aqua$observed_at, df_aqua$ph_read, pch=".", col="orange", main="Aquarium pH", xlab="Observed At", ylab="pH Reading")
lines(df_aqua$observed_at, df_aqua$ph_read, col="orange")
scatter.smooth(x=df_aqua$observed_at, xlab="Observed At", y=df_aqua$ph_read, ylab="pH Reading", span=2, pch="+", col="orange", main="Aquarium pH")

plot(df_aqua$observed_at, df_aqua$temp_read, pch=".", col="blue", main="Aquarium Temperature", xlab="Observed At", ylab="Temp Reading")
lines(df_aqua$observed_at, df_aqua$temp_read, col="blue")
scatter.smooth(x=df_aqua$observed_at, xlab="Observed At", y=df_aqua$temp_read, ylab="Temp Reading", pch="+", col="blue", main="Aquarium Temperature")

plot(df_aqua$observed_at, df_aqua$lux_read, pch=".", col="green", main="Aquarium Lux", xlab="Observed At", ylab="Lux Reading")
lines(df_aqua$observed_at, df_aqua$lux_read, col="green")
scatter.smooth(x=df_aqua$observed_at, xlab="Observed At", y=df_aqua$lux_read, ylab="Lux Reading", pch="+", col="green", main="Aquarium Lux")

bin <- hexbin(df_aqua$observed_at, df_aqua$ph_read)
plot(bin, main = "pH Hexagonal Binning", ylab="pH Reading", xlab="Observed At")

bin <- hexbin(df_aqua$observed_at, df_aqua$temp_read)
plot(bin, main = "Temp Hexagonal Binning", ylab="Temp Reading", xlab="Observed At")

bin <- hexbin(df_aqua$observed_at, df_aqua$lux_read)
plot(bin, main = "Lux Hexagonal Binning", ylab="Lux Reading", xlab="Observed At")

warnings()

dat <- data.frame(observed_at, ph_read, temp_read, lux_read)
dat.m <- melt(dat, "observed_at")
ggplot(dat.m, aes(observed_at, value, colour = variable)) + geom_line() +
facet_wrap(~ variable, ncol = 1, scales = "free_y")
