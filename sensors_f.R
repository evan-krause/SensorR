library("reshape2")
library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")

sensors <- data.table(read.csv("sensors_wd.csv"))
sensors$date.1 <- NULL
sensors$date.2 <- NULL
sensors <- sensors %>% select(X, date, time, site, temp, rh, wet)

##Values separated by parameter and year
rh_vals <-
  subset(sensors,
         sensors$para == "rh")
temp_vals <-
  subset(sensors,
         sensors$para == "temp")
wet_vals <-
  subset(sensors,
         sensors$para >= "wet")

## Temporal values 

rh_vals20 <- subset(rh_vals, rh_vals$Date < "2021-03-26")
rh_vals20_may <- rh_vals20[rh_vals20$Date >= "2020-05-01" & rh_vals20$Date < "2020-05-25", ]
rh_vals20_april <- rh_vals20[rh_vals20$Date >= "2020-04-01" & rh_vals20$Date < "2020-04-30", ]

rh_vals21 <- subset(rh_vals, rh_vals$Date >= "2021-03-26")
rh_vals21_march <- rh_vals21[rh_vals21$Date >= "2021-03-01" & rh_vals21$Date < "2021-03-31", ]
rh_vals21_april <- rh_vals21[rh_vals21$Date >= "2021-04-01" & rh_vals21$Date < "2021-04-30", ]
rh_vals21_may <- rh_vals21[rh_vals21$Date >= "2021-05-01" & rh_vals21$Date < "2021-05-30", ]
rh_vals21_june <- rh_vals21[rh_vals21$Date >= "2021-06-01" & rh_vals21$Date < "2021-06-30", ]

temp_vals20 <- subset(temp_vals, temp_vals$Date < "2021-03-26")
temp_vals20_may <- temp_vals20[temp_vals20$Date >= "2020-05-01" & temp_vals20$Date < "2020-05-25", ]
temp_vals20_april <- temp_vals20[temp_vals20$Date >= "2020-04-01" & temp_vals20$Date < "2020-04-30", ]

temp_vals21 <- subset(temp_vals, temp_vals$Date >= "2021-03-26")
temp_vals21_march <- temp_vals21[temp_vals21$Date >= "2021-03-01" & temp_vals21$Date < "2021-03-31", ]
temp_vals21_april <- temp_vals21[temp_vals21$Date >= "2021-04-01" & temp_vals21$Date < "2021-04-30",] 
temp_vals21_may <- temp_vals21[temp_vals21$Date >= "2021-05-01" & temp_vals21$Date < "2021-05-30", ]
temp_vals21_june <- temp_vals21[temp_vals21$Date >= "2021-06-01" & temp_vals21$Date < "2021-06-30", ]

wet_vals20 <- subset(wet_vals, wet_vals$Date < "2021-03-26")
wet_vals20_may <- wet_vals20[wet_vals20$Date >= "2020-05-01" & wet_vals20$Date < "2020-05-25", ]
wet_vals20_april <- wet_vals20[wet_vals20$Date >= "2020-04-01" & wet_vals20$Date < "2020-04-30", ]


wet_vals21 <- subset(wet_vals, wet_vals$Date >= "2021-03-26")
wet_vals21_march <-
  wet_vals21[wet_vals21$Date >= "2021-03-01" &
               wet_vals21$Date < "2021-03-31",]
wet_vals21_april <-
  wet_vals21[wet_vals21$Date >= "2021-04-01" &
               wet_vals21$Date < "2021-04-30",]
wet_vals21_may <-
  wet_vals21[wet_vals21$Date >= "2021-05-01" &
               wet_vals21$Date < "2021-05-30",]
wet_vals21_june <-
  wet_vals21[wet_vals21$Date >= "2021-06-01" &
               wet_vals21$Date < "2021-06-30",]

## North sites
n_main <- sensors %>% filter(site == "n.main")
n_m <- sensors %>% filter(site == "n.m")
n_e <- sensors %>% filter(site == "n.e")
n_x <- sensors %>% filter(site == "n.x")

## South sites
s_main <- sensors %>% filter(site == "s.main")
s_a11 <- sensors %>% filter(site == "s.a11")
s_a16 <- sensors %>% filter(site == "s.a16")
s_g <- sensors %>% filter(site == "s.g")
s_y <- sensors %>% filter(site == "s.y")


# N-main
nmain_temp <- subset(n_main, para == "temp")
nmain_rh <- subset(n_main, para == "rh")
nmain_wet <- subset(n_main, para == "wet")

# N-M
nm_temp = subset(n_m, para == "temp")
nm_rh = subset(n_m, para == "rh")
nm_wet = subset(n_m, para == "wet")

# N-E
ne_temp = subset(n_e, para == "temp")
ne_rh <- subset(n_e, para == "rh")
ne_wet <- subset(n_e, para == "wet")

# N-X
nx_temp <- subset(n_x, para == "temp")
nx_rh <- subset(n_x, para == "rh")
nx_wet <- subset(n_x, para == "wet")

# S-main
smain_temp <- subset(s_main, para == "temp")
smain_rh <- subset(s_main, para == "rh")
smain_wet <- subset(s_main, para == "wet")

# S-A11
sa11_temp <- subset(s_a11, para == "temp")
sa11_rh <- subset(s_a11, para == "rh")
sa11_wet <- subset(s_a11, para == "wet")

# S-A16
sa16_temp <- subset(s_a16, para == "temp")
sa16_rh <- subset(s_a16, para == "rh")
sa16_wet <- subset(s_a16, para == "wet")

# S-Y
sy_temp <- subset(s_y, para == "temp")
sy_rh <- subset(s_y, para == "rh")
sy_wet <- subset(s_y, para == "wet")

# S-G
sg_temp <- subset(s_g, para == "temp")
sg_rh <- subset(s_g, para == "rh")
sg_wet <- subset(s_g, para == "wet")

# Graphical explorations

## 2020 by site per month boxplots
ggplot(rh_vals20_april, aes(site, value)) + geom_boxplot()
ggplot(rh_vals20_may, aes(site, value)) + geom_boxplot()

ggplot(temp_vals20_april, aes(site, value)) + geom_boxplot()
ggplot(temp_vals20_may, aes(site, value)) + geom_boxplot()

ggplot(wet_vals20_april, aes(site, value)) + geom_boxplot()
ggplot(wet_vals20_may, aes(site, value)) + geom_boxplot()

## 2021 by site per month boxplots
ggplot(rh_vals21_march, aes(site, value)) + geom_boxplot()
ggplot(rh_vals21_april, aes(site, value)) + geom_boxplot()
ggplot(rh_vals21_may, aes(site, value)) + geom_boxplot()
ggplot(rh_vals21_june, aes(site, value)) + geom_boxplot()

ggplot(temp_vals21_march, aes(site, value)) + geom_boxplot()
ggplot(temp_vals21_april, aes(site, value)) + geom_boxplot()
ggplot(temp_vals21_may, aes(site, value)) + geom_boxplot()
ggplot(temp_vals21_june, aes(site, value)) + geom_boxplot()

ggplot(wet_vals21_march, aes(site, value)) + geom_boxplot()
ggplot(wet_vals21_april, aes(site, value)) + geom_boxplot()
ggplot(wet_vals21_may, aes(site, value)) + geom_boxplot()
ggplot(wet_vals21_june, aes(site, value)) + geom_boxplot()


ggplot(temp_vals21[temp_vals21$Date == "2021-04-04", ], aes(time, value, color = site)) + geom_point(alpha = 0.5) + geom_smooth()

t.test(
  nmain_temp$value,
  nx_temp$value,
  data = subset(sensorFinal, sensorFinal$Date <= "2021-03-26")
)

t.test(nm_rh$value,
       sg_rh$value,
       data = subset(sensorFinal, sensorFinal$Date <= "2021-03-26")) ## Woo!

t.test(nm_wet$value,
       sg_wet$value,
       data = subset(sensorFinal, sensorFinal$Date <= "2021-03-26")) ## Woo!

