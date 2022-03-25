# req lib
library("reshape2")
# import sensor file
sensors <- data.frame(read.csv('ow_ns.csv'))
names(sensors)[1] <- "datetime"
class(sensors)

#Long form
sensor_melt <- melt(sensors,
                    na.rm = FALSE,
                    )
n_main_temp <-subset(sensor_melt,
       select = "variable"
)
