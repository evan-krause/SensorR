library("reshape2")
library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")

sensors <- data.frame(read.csv("sensors_wd.csv"))

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
ggplot(sensors[sensors$date == "2021-04-01"], aes(x = time)) +
  geom_point(aes(rh, wet, color = site), alpha = 0.4) +
  geom_smooth(aes(rh , wet))

ggplot(temp_vals[temp_vals$date == "2021-04-04",],
       aes(time, color = site)) +
  geom_point(y = temp, alpha = 0.5) +
  geom_point(y = rh)

ggplot(sensors, sensors[sensors$date >= "2021-04-01" &
                          sensors$date <= "2021-04-30"], aes(temp, rh, color = site)) +
  geom_jitter()

ggplot(sensors[date >= "2021-04-28" & date <= "2021-04-30", ],
       aes(date, rh))
