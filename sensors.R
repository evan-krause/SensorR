library("reshape2")
library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")

sensors <- data.table(read.csv("sensors_wd.csv"))

sensors$date.1 <- NULL
sensors$date.2 <- NULL
sensors <- sensors %>% select(X, date, time, site, temp, rh, wet)

##Values separated by parameter
rh_vals <-
  subset(sensors,
         sensors$rh >= 0)
temp_vals <-
  subset(sensors,
         sensors$temp >= 0)
wet_vals <-
  subset(sensors,
         sensors$wet >= 0)

# Graphical explorations
ggplot(sensors[date >= "2021-04-01" & date <= "2021-04-28"],
       aes(site, rh)) +
  geom_boxplot()

ggplot(temp_vals[temp_vals$date == "2021-04-04",],
       aes(time, temp, color = site)) +
  geom_point(alpha = 0.5)
