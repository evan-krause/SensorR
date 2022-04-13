
#install.packages("tidyverse")

# req lib
library("reshape2")
library("dplyr")
library("ggplot2")
library("tidyr")

# import sensor file
sensors <- data.frame(read.csv('ow_ns.csv'))
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

para_frame <-
  data.frame(matrix(unlist(test_out), ncol = 2, byrow = T)) ## convert to data frame
#para_frame <- para_frame %>% separate(X1, c("temp", "rh", "wet"))
names(para_frame) = c("para", "site")

sensor2 <-
  cbind(sensor_melt, para_frame) ## Combine melted data and site/para cols


#Convert datetime
sensor2$date <-
  format(as.POSIXct(sensor2$datetime, format = "%m/%d/%Y %H:%M"),
         "%Y/%m/%d")
sensor2$time <-
  format(as.POSIXct(sensor2$datetime, format = "%m/%d/%Y %H:%M"),
         "%H:%M")

#Convert date from char to date class
date_frame <- data.frame(as.Date.character(sensor2$date))
sensorFinal <- cbind(sensor2, date_frame)
names(sensorFinal)[8] <- "Date"
sensorFinal$date <- NULL

#para_frame %>% separate(X1, c("temp", "rh", "wet"))
rh_frame <- sensorFinal %>% select(para == "rh", value)


#Replace NA vals with 0 then subset
##sensorFinal <- replace(sensorFinal, is.na(sensorFinal$value), 1)
sensorFinal <- subset(sensorFinal, value != 0.1)
sensorFinal[is.na(sensorFinal$value)] <- 0.1
sensorFinal <- subset(sensorFinal, value != 0.1)


##Values separated by parameter
rh_vals <-
  subset(sensorFinal,
         sensorFinal$para == "rh")
temp_vals <-
  subset(sensorFinal,
         sensorFinal$para == "temp")

wet_vals <-
  subset(sensorFinal,
         sensorFinal$para >= "wet")

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


#ggplot(vals_2020, aes(site, value, color = site)) + geom_point()


ggplot(sensors, aes(rh_n.main, wet_n.main)) + geom_point() + geom_smooth()
lm(sensors$rh_n.main ~ sensors$wet_n.main)



sensors$rh_n.main ~ sensors$wet_n.main

lm_nmain = lm(sensors$rh_n.main ~ sensors$wet_n.main)
lmnmain = lm(nmain_rh$value ~ nmain_wet$value)
summary(lm_nmain)
summary(lmnmain)
anova(lmnmain)
plot(lm_nmain)
anova(lm_nmain)
nmain_resid <- residuals(lm_nmain)
hist(nmain_resid)

lm_smain = lm(sensors$rh_s.main ~ sensors$wet_s.main)
summary.lm(lm_smain)
plot(lm_smain)
anova(lm_smain)
smain_resid <- residuals(lm_smain)
hist(smain_resid)

hist(
  sg_temp$value,
  data = subset(
    sg_temp,
    sg_temp$as.Date.character.sensor2.date. >= "2021-03-26"
  ),
  main = "2021 South-G temperature dist"
)
# hist(sensors$rh_n_m)
# hist(sensors$rh_n_x)
# 
# hist(sensors$wet_n_e)
# hist(sensors$wet_n_m)
#hist(sensorFinal$value)
hist(temp_vals$value) 
hist(rh_vals$value) 
hist(wet_vals$value)

lm(temp_vals$value ~ rh_vals$value)

# Additive linear model of RH as func of wetness + temperature
rh_wet_templm <- lm(rh_vals$value ~ wet_vals$value + temp_vals$value)
anova(lm(rh_vals$value ~ wet_vals$value + temp_vals$value))
summary(rh_wet_templm)
rh_wetTempresidual <- resid(lm(temp_vals$value ~ rh_vals$value)) 
hist(rh_wetTempresidual)
shapiro.test(rh_wetTempresidual) 
AIC(rh_wet_templm)

shapiro.test(sample(rh_wetTempresidual, size = 5000))


plot(rh_wet_templm)


plot(
   ne_temp$value ~ ne_rh$value,
   data = subset(
     sensorFinal,
     sensorFinal$as.Date.character.sensor2.date. >= "2021-03-26"
   ),
   main = "2021 North-E RH by Temperature",
   pch = 16,
   col = adjustcolor("black", 0.1)
 )
 #
 # plot(n_main$value,
 #     data = subset(sensor2, n_main$value == "rh"),
 #     pch = 16,
#     col = adjustcolor("black", 0.1))
# 
 boxplot(nmain_temp$value,
         ne_temp$value,
         nx_temp$value,
         smain_temp$value)
# hist(sensors$temp_n_main)
# 
t.test(nmain_temp$value, nx_temp$value,
       data = subset(sensorFinal,
       sensorFinal$Date <= "2021-03-26")
)

t.test(
  nm_rh$value, sg_rh$value,
       data = subset(sensorFinal, sensorFinal$Date <= "2021-03-26")) ## Woo!

t.test(
  nm_wet$value, sg_wet$value,
  data = subset(sensorFinal, sensorFinal$Date <= "2021-03-26")) ## Woo!


boxplot(nmain_rh$value, smain_rh$value,
        data = subset(sensorFinal,
                      sensorFinal$as.Date.character.sensor2.date. >= "2021-03-26"),
        main = "2020-2021 RH values at North and South main sites",
        names = c("North main", "South main"),
        ylab = "Relative humidity (%)")


        