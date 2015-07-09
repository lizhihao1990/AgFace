# SYS3 TDR 3 temperarure dependency

# grab hourly df.cast from other script as df.cast.5min

library(plyr)
df.cast.5min$Date <- as.Date(df.cast.5min$TIMESTAMP, tz = "Australia/Melbourne")
df.cast.5min$Hour <- format(df.cast.5min$TIMESTAMP, "%H")
df.cast.hourly <- ddply(df.cast,
                       .(Date, Hour, SYSTEM),
                       summarise,
                       Temp = mean(IR_Narrow_Avg))

my.df <- merge(df.cast, df.cast.hourly)

names(my.df) <- gsub("\\.\\.1", "Temp", names(my.df))
my.df$Soil_Avg[my.df$Soil_Avg > 1] <- NA
my.melt <- melt(my.df,
                id.vars = c("SYSTEM", "Date", "Hour", "TIMESTAMP", "SensorID"))

library(ggplot2)
Sys3 <- my.melt[(my.melt$variable == "Soil_Avg" |
                    my.melt$variable == "Temp") &
                    my.melt$SYSTEM == "SYS3" &
                    my.melt$SensorID == "3", ]

Sys4  <- my.melt[(my.melt$variable == "Soil_Avg" |
                    my.melt$variable == "Temp") &
                    my.melt$SYSTEM == "SYS4" &
                    my.melt$SensorID == "3", ]                   

p <- ggplot(Sys3[Sys3$variable == "Temp" &
                 Sys3$value > 8, ],
            aes(x = TIMESTAMP, y = value))
  p <- p + geom_point()
  p <- p + facet_grid(variable ~ ., scale = "free_y")
p

Sys3.cast <- dcast(Sys3, SYSTEM + Date + Hour + TIMESTAMP + SensorID ~ variable)
Sys4.cast <- dcast(Sys4, SYSTEM + Date + Hour + TIMESTAMP + SensorID ~ variable)

p <- ggplot(Sys3.cast[Sys3.cast$Temp > 10, ], aes(x = TIMESTAMP))
  p <- p + geom_point(aes(y = Soil_Avg))
  p <- p + geom_point(aes(y = Temp/100), colour = "red")
  #p <- p + facet_grid(variable ~ .)
p

p <- ggplot(Sys3.cast, aes(x = Temp, y = Soil_Avg))
  p <- p + geom_point()
p

p <- ggplot(Sys4.cast, aes(x = Temp, y = Soil_Avg))
  p <- p + geom_point()
p
