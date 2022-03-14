
# import sensor file
sensors <- data.frame(read.csv("ow_ns.csv"))
class(sensors)

temp_vals <- subset(sensors, select = c("ï..datetime", "temp_n_main", "temp_n_e", "temp_n_m", "temp_n_x", "temp_s_main", "temp_s_a11", "temp_s_a16", "temp_s_y", "temp_s_g"))

rh_vals <- subset(sensors, select = c("ï..datetime", "rh_n_main", "rh_n_e", "rh_n_m", "rh_n_x", "rh_s_main", "rh_s_a11", "rh_s_a16", "rh_s_y", "rh_s_g"))

wet_vals <- subset(sensors, select = c("ï..datetime", "wet_n_main", "wet_n_e", "wet_n_m", "wet_n_x", "wet_s_main", "wet_s_a11", "wet_s_a16", "wet_s_y", "wet_s_g"))