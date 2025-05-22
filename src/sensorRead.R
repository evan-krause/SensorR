
#install.packages("tidyverse")

# req lib
library("reshape2")
library(tidyverse)


# import sensor file
sensors <- read_csv('data/ow_ns.csv')
names(sensors)[1] <- "datetime"

#Convert to long form
sensor_melt <- melt(sensors, na.rm = FALSE)

test_var = rep("rh_n-main", 5)
test_out = strsplit(test_var, split = "_") ## split para_reg_site form
unlist(test_out) ## simplify list structure to vector
matrix(unlist(test_out), ncol = 3, byrow = T) ## convert list to matrix
test_var <-
  as.character(sensor_melt$variable) ## define variable col as character

head((test_var))

test_out = strsplit(test_var, split = "_") ## split para_reg_site form

para_table <-
  data.table(matrix(unlist(test_out), ncol = 2, byrow = T)) ## convert to data frame
#para_frame <- para_frame %>% separate(X1, c("temp", "rh", "wet"))
names(para_table) = c("para", "site")

sensor2 <-
  cbind(sensor_melt, para_table) ## Combine melted data and site/para cols


#Convert datetime
sensor2$date <-
  format(as.POSIXct(sensor2$datetime, format = "%m/%d/%Y %H:%M"),
         "%Y/%m/%d")
sensor2$time <-
  format(as.POSIXct(sensor2$datetime, format = "%m/%d/%Y %H:%M"),
         "%H:%M")

#Convert date from char to date class
date_table <- data.table(as.Date.character(sensor2$date))
sensorFinal <- cbind(sensor2, date_table)
names(sensorFinal)[8] <- "date"
sensorFinal$date <- NULL

temp_table <- sensorFinal %>% filter(para == "temp")
names(temp_table)[3] <- "temp"

rh_table <- sensorFinal %>% filter(para == "rh")
names(rh_table)[3] <- "rh"

wet_table <- sensorFinal %>% filter(para == "wet")
names(wet_table)[3] <- "wet"


rh_table <- arrange(rh_table, desc(site))
temp_table <- arrange(temp_table, desc(site))
wet_table <- arrange(wet_table, desc(site))

temp_table$para <- NULL
temp_table$datetime <- NULL
temp_table$variable <- NULL
rh_table$para <- NULL
rh_table$datetime <- NULL
rh_table$variable <- NULL
rh_table$time <- NULL
rh_table$Date <- NULL
rh_table$site <- NULL
wet_table$para <- NULL
wet_table$datetime <- NULL
wet_table$variable <- NULL
wet_table$time <- NULL
wet_table$Date <- NULL
wet_table$site <- NULL

working <- cbind(temp_table, rh_table, wet_table)



write.csv(working, file = "sensors_wd.csv")

#Replace NA vals with 0 then subset
#sensorFinal <- replace(sensorFinal, is.na(sensorFinal$value), 1)
# sensorFinal <- subset(sensorFinal, value != 0.1)
# sensorFinal[is.na(sensorFinal$value)] <- 0.1
# sensorFinal <- subset(sensorFinal, value != 0.1)


##Values separated by parameter and year
rh_vals <-
  subset(sensorFinal,
         sensorFinal$para == "rh")
temp_vals <-
  subset(sensorFinal,
         sensorFinal$para == "temp")
wet_vals <-
  subset(sensorFinal,
         sensorFinal$para >= "wet")

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
n_main <- sensorFinal %>% filter(site == "n.main")
n_m <- sensorFinal %>% filter(site == "n.m")
n_e <- sensorFinal %>% filter(site == "n.e")
n_x <- sensorFinal %>% filter(site == "n.x")

## South sites
s_main <- sensorFinal %>% filter(site == "s.main")
s_a11 <- sensorFinal %>% filter(site == "s.a11")
s_a16 <- sensorFinal %>% filter(site == "s.a16")
s_g <- sensorFinal %>% filter(site == "s.g")
s_y <- sensorFinal %>% filter(site == "s.y")


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



ggplot(sensorFinal[sensorFinal$Date == "2021-05-02",], 
       aes(time, value, color = site, shape = para)) + 
  geom_jitter(alpha = 0.9) + labs(title = "CSO parameters May 5th, 2021 (0:00-23:55)")


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

        