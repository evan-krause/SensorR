
# req lib
library("reshape2")
library("dplyr")
# import sensor file
sensors <- data.frame(read.csv('ow_ns.csv'))
names(sensors)[1] <- "datetime"

#Long form
sensor_melt <- melt(sensors, na.rm = FALSE)

test_var = rep("rh_n-main", 5) 
test_out = strsplit(test_var, split = "_") ## split para_reg_site form
unlist(test_out) ## simplify list structure to vector
matrix(unlist(test_out), ncol = 3, byrow = T) ## convert list to matrix
test_var <- as.character(sensor_melt$variable) ## define variable col as character

head((test_var))

test_out = strsplit(test_var, split = "_") ## split para_reg_site form

para_frame <- data.frame(matrix(unlist(test_out), ncol = 2, byrow = T)) ## convert to data frame
names(para_frame) = c("para", "site")

sensor2 <- cbind(sensor_melt, para_frame) ## Combine melted data and site/para cols


## North sites
n_main <- sensor2 %>% filter(site == "n.main")
n_m <- sensor2 %>% filter(site == "n.m")
n_e <- sensor2 %>% filter(site == "n.e")
n_x <- sensor2 %>% filter(site == "n.x")

## South sites
s_main <- sensor2 %>% filter(site == "s.main")
s_a11 <- sensor2 %>% filter(site == "s.a11")
s_a16 <- sensor2 %>% filter(site == "s.a16")
s_g <- sensor2 %>% filter(site == "s.g")
s_y <- sensor2 %>% filter(site == "s.y")

# N-main
nmain_temp <- subset(n_main, para == "temp")
nmain_rh <- subset(n_main, para == "rh")
nmain_wet <- subset(n_main, para == "wet")

#N-M
nm_temp = subset(n_m, para == "temp")
nm_rh = subset(n_m, para == "rh")
nm_wet = subset(n_m, para == "wet")

#N-E
ne_temp = subset(n_e, para == "temp")
ne_rh <- subset(n_e, para == "rh")
ne_wet <- subset(n_e, para == "wet")

#N-X
nx_temp <- subset(n_x, para == "temp")
nx_rh <- subset(n_x, para == "rh")
nx_wet <- subset(n_x, para == "wet")

#S-main
smain_temp <- subset(s_main, para == "temp")
smain_rh <- subset(s_main, para == "rh")
smain_wet <- subset(s_main, para == "wet")

#S-A11
sa11_temp <- subset(s_a11, para == "temp")
sa11_rh <- subset(s_a11, para == "rh")
sa11_wet <- subset(s_a11, para == "wet")

#S-A16
sa16_temp <- subset(s_a16, para == "temp")
sa16_rh <- subset(s_a16, para == "rh")
sa16_wet <- subset(s_a16, para == "wet")

#S-Y
sy_temp <- subset(s_y, para == "temp")
sy_rh <- subset(s_y, para == "rh")
sy_wet <- subset(s_y, para == "wet")

#S-G
sg_temp <- subset(s_g, para == "temp")
sg_rh <- subset(s_g, para == "rh")
sg_wet <- subset(s_g, para == "wet")




# hist(sensors$rh_n_e)
# hist(sensors$rh_n_m)
# hist(sensors$rh_n_x)
# 
# hist(sensors$wet_n_e)
# hist(sensors$wet_n_m)
# hist(sensors$wet_n_x)
# 
# plot(rh_n_e ~ temp_n_e,
#      data = subset(sensors, rh_n_e < 99),
#      pch = 16,
#      col = adjustcolor("black", 0.1))
# 
# plot(n_main$value,
#     data = subset(sensor2, n_main$value == "rh"),
#     pch = 16,
#     col = adjustcolor("black", 0.1))
# 
# boxplot(sensors$temp_n_main, sensors$temp_n_x, sensors$temp_n_e)
# hist(sensors$temp_n_main)
# 
# t.test(sensors$temp_n_main, sensors$temp_s_main)

