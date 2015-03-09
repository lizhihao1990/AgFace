# ZIM leaf water turgor sensor data import
# data from Creswick campus testrun
# Markus Löw, Nov 2014

library(reshape2)
library(ggplot2)

# source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

setwd("~/AgFace/2014/ZIM_sensors/2014-11-13_Creswick_campus")

# import sensor IDs, and experimental layout
IDs <- read.csv("2014-11-13_Creswick_campus_configuration.csv")

# import raw ZIM sensor data
the.data <- "17-11-2014-13_30_28_zim_probe_datas_raw_LT_2014-11-12_21_30_00_to_2014-11-17_02_30_00.csv"

df <- read.csv(the.data, sep = "\t")

# redundant Timestamp, using local time later
df$Timestamp <- NULL

# special treatment for humidity sensor 1808
# names(df)[grep("Hum", names(df))]
names(df) <- gsub("1808\\.\\.", "1808.Per", names(df))

names(df) <- gsub("\\.{1,}", "_", names(df))
names(df) <- gsub("_MS_Creswick_Campus", "", names(df))
#names(df) <- gsub("_MS_Sensor_is_not_configured", "", names(df))
#names(df) <- gsub("X_LSN_Scout_outside_", "", names(df))
names(df) <- gsub("_in_Local_time", "", names(df))



# duplicate sensors
#pattern <- "[[:digit:]]_[[:digit:]]_[[:digit:]]"
#my.duplicates <- grep(pattern, names(df))

#df.orig <- df
#df <- df[, -my.duplicates]

# empty sensors, duplicates again
names.with.point <- grep("\\.", names(df))
# names(df)[names.with.point]

# df <- df[, -names.with.point]

# convert timestamp to POSIXct
# timestamp is already in POSIX format
df$Timestamp <- as.POSIXct(df$Timestamp, 
                           tz = "Australia/Melbourne")

df.melt <- melt(df,
                id = "Timestamp")
df.melt$Sensor.type <- sapply(
                       strsplit(
                       as.character(df.melt$variable), "_"), "[", 1)
df.melt$Sensor.type <- as.factor(df.melt$Sensor.type)
df.melt$SensorID <- sapply(
                    strsplit(
                    as.character(df.melt$variable), "_"), "[", 3)
df.melt$SensorID <- as.numeric(df.melt$SensorID)
df.melt$Unit <- sapply(
                strsplit(
                as.character(df.melt$variable), "_"), "[", 4)
df.melt$Unit <- as.factor(df.melt$Unit)
df.melt$Transmitter <- sapply(
                    strsplit(
                    as.character(df.melt$variable), "_"), "[", 5)
df.melt$Transmitter <- as.numeric(df.melt$Transmitter)
df.melt$Channel <- sapply(
                   strsplit(
                   as.character(df.melt$variable), "_"), "[", 6)
df.melt$Channel <- as.numeric(df.melt$Channel)

## assign treatments
ZIM <- merge(IDs, df.melt,
             by.x = c("Sensor_ID", "Transmitter", "Channel"),
             by.y = c("SensorID",  "Transmitter", "Channel"))
ZIM$Date   <- NULL
ZIM$Remark <- NULL
ZIM$Channel <- as.factor(ZIM$Channel)

p <- ggplot(ZIM[!is.na(ZIM$value) &
                ZIM$Object != "Leaf", ], 
            aes(x = Timestamp, y = value))
  p <- p + geom_line(aes(colour = Channel))
  p <- p + facet_grid(Object ~ ., scales = "free_y")
  p <- p + labs(y = "Turgor [kPa]")
  p <- p + theme_bw()
p

ggsave(file = "Carboard_plastic_turgor.pdf",
       width = 9, height = 7)
fig.cardboard.plastic <- p

p <- ggplot(ZIM[!is.na(ZIM$value) &
                ZIM$Sensor.type == "Turgor" &
                ZIM$Object == "Leaf", ], 
            aes(x = Timestamp, y = value))
  p <- p + geom_line()
  p <- p + facet_grid(Transmitter * Channel ~ ., scales = "free_y")
  p <- p + labs(y = "Turgor [kPa]")
  p <- p + theme_bw()
p

p <- ggplot(ZIM[!is.na(ZIM$value) &
                (ZIM$Sensor.type == "Temperature" |
                ZIM$Sensor.type == "Humidity") &
                ZIM$Object == "Leaf", ], 
            aes(x = Timestamp, y = value))
  p <- p + geom_line(aes(colour = Sensor.type))
  #p <- p + facet_grid(Sensor.type ~ ., scales = "free_y")
  p <- p + labs(y = "Humidity [%] or Temperature [°C]")
  p <- p + theme_bw()
p
ggsave(file = "Temperature_humidity.pdf",
       width = 9, height = 7)
fig.temp.hum <- p

library(lubridate)

# generate a 5 minute timecourse
min.time <- min(ZIM$Timestamp)
min.time <- floor_date(min.time, "day")

max.time <- max(ZIM$Timestamp)
max.time <- ceiling_date(max.time, "day")

my.5min <- seq(from = min.time, to = max.time, by = "5 min")

# extract minutes
ZIM$my.five <- as.numeric(format(ZIM$Timestamp, "%M"))
# round to closest five
ZIM$my.five <- floor(ZIM$my.five / 5) * 5

# construct new timestamp
ZIM$five_min <- paste(format(ZIM$Timestamp, "%Y-%m-%d %H"), ZIM$my.five, sep = ":")

ZIM$Timestamp_5min <- as.POSIXct(ZIM$five_min, format = "%Y-%m-%d %H:%M")
ZIM$five_min <- NULL
ZIM$my.five  <- NULL
ZIM$Timestamp <- NULL

# average per 5 min blocks
library(plyr)
ZIM.5min <- ddply(ZIM,
                  .(Timestamp_5min, Sensor_ID, Transmitter, Channel, Object, Location, Sensor.type),
                  .progress = "text",
                  summarise,
                  mean = mean(value, na.rm = TRUE)
)
ZIM.5min.temp <- ZIM.5min[ZIM.5min$Sensor.type == "Temperature",]
ZIM.5min.hum  <- ZIM.5min[ZIM.5min$Sensor.type == "Humidity",]

to.del <- c("Sensor_ID", "Transmitter", "Channel", "Object", "Location", "Sensor.type")

ZIM.5min.temp <- ZIM.5min.temp[!names(ZIM.5min.temp) %in% to.del]
ZIM.5min.hum  <- ZIM.5min.hum[!names(ZIM.5min.hum) %in% to.del]

names(ZIM.5min.temp) <- gsub("mean", "Temperature", names(ZIM.5min.temp))
names(ZIM.5min.hum)  <- gsub("mean", "Humidity",  names(ZIM.5min.hum))

ZIM.temp.hum <- merge(ZIM.5min.temp, ZIM.5min.hum)

ZIM.5min <- ZIM.5min[!(ZIM.5min$Sensor.type == "Temperature" | 
                       ZIM.5min$Sensor.type == "Humidity"), ]
ZIM.5min <- merge(ZIM.5min, ZIM.temp.hum)
names(ZIM.5min) <- gsub("mean", "Turgor", names(ZIM.5min))

# add a day identifier
ZIM.5min$Day  <- format(ZIM.5min$Timestamp_5min, "%Y-%m-%d")
ZIM.5min$ampm <- format(ZIM.5min$Timestamp_5min, "%p")


p <- ggplot(ZIM.5min, #[ZIM.5min$Object != "Leaf", ], 
            aes(x = Temperature, y = Turgor))
  p <- p + geom_point(aes(colour = as.factor(Sensor_ID), shape = Object), alpha = 0.5)
  p <- p + geom_smooth(se = FALSE, method = "lm", aes(colour = as.factor(Sensor_ID)))
  p <- p + facet_grid(ampm ~ Day)
  p <- p + theme_bw()
p

fig.temp.response <- p

pdf(file = "Cardboard_plastic_turgor_Nov2014.pdf",
    width = 9, height = 7)
print(fig.cardboard.plastic)
print(fig.temp.hum)
print(fig.temp.response)
dev.off()
