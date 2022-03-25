
# req lib
library("reshape2")
# import sensor file
sensors <- data.frame(read.csv('ow_ns.csv'))
names(sensors)[1] <- "datetime"

# import sensor file
sensors <- data.frame(read.csv("ow_ns.csv"))


class(sensors)
head(sensors)

#Long form
sensor_melt <- melt(sensors,
                    na.rm = FALSE,
                    )
n_main_temp <-subset(sensor_melt,
       select = "variable"
)

# Define subset values
temp_vals <- subset(sensors, select = c("ï..datetime", "temp_n_main", "temp_n_e", "temp_n_m", "temp_n_x", "temp_s_main", "temp_s_a11", "temp_s_a16", "temp_s_y", "temp_s_g")) # nolint
rh_vals <- subset(sensors, select = c("ï..datetime", "rh_n_main", "rh_n_e", "rh_n_m", "rh_n_x", "rh_s_main", "rh_s_a11", "rh_s_a16", "rh_s_y", "rh_s_g")) # nolint
wet_vals <- subset(sensors, select = c("ï..datetime", "wet_n_main", "wet_n_e", "wet_n_m", "wet_n_x", "wet_s_main", "wet_s_a11", "wet_s_a16", "wet_s_y", "wet_s_g")) # nolint
events <- subset(sensors, select = c("ï..datetime", "event_n_main", "event_n_e", "event_n_m", "event_n_x", "event_s_main", "event_s_a11", "event_s_a16", "event_s_y", "event_s_g")) # nolint
intervals <- subset(sensors, select = c("ï..datetime", "wet_int_n_main", "wet_int_n_e", "wet_int_n_m", "wet_int_n_x", "wet_int_s_main", "wet_int_s_a11", "wet_int_s_a16", "wet_int_s_y", "wet_int_s_g")) # nolint
wet_vals <- subset(sensors, select = c("ï..datetime", "wet_n_main", "wet_n_e", "wet_n_m", "wet_n_x", "wet_s_main", "wet_s_a11", "wet_s_a16", "wet_s_y", "wet_s_g")) # nolint

hist(sensors$rh_n_e)
hist(sensors$rh_n_m)
hist(sensors$rh_n_x)

hist(sensors$wet_n_e)
hist(sensors$wet_n_m)
hist(sensors$wet_n_x)

plot(rh_n_e ~ temp_n_e,
     data = subset(sensors, rh_n_e < 99),
     pch = 16,
     col = adjustcolor("black", 0.1))

plot(rh_n_x ~ temp_n_x,
     data = subset(sensors, rh_n_e < 99),
     pch = 16,
     col = adjustcolor("black", 0.1))

boxplot(sensors$temp_n_main, sensors$temp_n_x, sensors$temp_n_e)
hist(sensors$temp_n_main)

t.test(sensors$temp_n_main, sensors$temp_s_main)

