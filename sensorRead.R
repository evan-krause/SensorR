
# import sensor file
sensors <- data.frame(read.csv('ow_ns.csv'))
class(sensors)

n_main_temp <- subset(sensors, select = c("temp_n_main"))