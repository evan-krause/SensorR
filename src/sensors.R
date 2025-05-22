library(reshape2)
library(tidyverse)
library(zoo)

sensors <- read_csv("data/sensors_wd.csv")

sensors <- sensors %>%
  select(-7, -9) %>% 
  rename(index = 1,
         date = 5) %>%
  select(date, time, site, temp, rh, wet) %>%
  na.omit()

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

sensors %>%
  filter(date == "2021-04-01") %>%
  ggplot(aes(x = time)) +
  geom_point(aes(rh, wet, color = site), alpha = 0.4) +
  geom_smooth(aes(rh , wet), method = loess) 

#temp vals over time by site  
temp_vals %>%
  filter(date == "2021-04-04") %>%
  ggplot(aes(time, temp, color = site)) +
  geom_point(alpha = 0.5)



# sensors %>%
#   filter(date >= "2021-04-01" & date <= "2021-04-30") %>%
#   ggplot(aes(temp, rh, color = site)) +
#   geom_jitter()

