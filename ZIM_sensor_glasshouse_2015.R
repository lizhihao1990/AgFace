# ZIM leaf water turgor sensor data import

# Markus Löw, Oct 2014

library(reshape2)
library(ggplot2)
library(scales)
library(plyr)

# source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

setwd("~/AgFace/2015/ZIM/Creswick_glasshouse")

# import sensor IDs, and experimental layout
IDs <- read.csv("2015-07-27_Glasshouse_configuration.csv")

# import raw ZIM sensor data

the.data <- "07-09-2015-14_09_19_zim_probe_datas_5min_LT_2015-07-24_01_05_00_to_2015-08-31_13_59_00.csv"


df <- read.csv(the.data, sep = "\t")

names(df) <- gsub("\\.{1,}", "_", names(df))
names(df) <- gsub("_MS_Agface", "", names(df))
names(df) <- gsub("X_LSN_", "", names(df))
names(df) <- gsub("_in_Local_time", "", names(df))
#names(df) <- gsub("Green_leaf_([[:digit:]])", "Green.leaf.\\1", names(df))
names(df) <- gsub("Green_leaf_([[:digit:]])", "", names(df))
names(df) <- gsub("Dead_leaf_", "", names(df))
names(df) <- gsub("^_Turgor", "Turgor", names(df))
names(df) <- gsub("ID_1808", "ID_1808_rH", names(df))


# convert timestamp to POSIXct
# timestamp is already in POSIX format
df$Timestamp <- as.POSIXct(df$Timestamp, 
                           tz = "Australia/Melbourne")
# get rid of one sensor
df$Turgor_ID_5058_kPa_1416_2_MS_Sensor_is_not_configured <- NULL

# melt
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

df.melt$variable <- gsub("_MS_Creswick_Campus", "", df.melt$variable)

# assign treatments
ZIM <- merge(IDs, df.melt)#,
             #by.x = c("Sensor_ID", "Transmitter", "Channel"),
             #by.y = c("SensorID", "Transmitter", "Channel"))
ZIM$Date   <- NULL
ZIM$Remark <- NULL
ZIM$Sensor.type <- as.factor(ZIM$Sensor.type)

# visualisation
p <- ggplot(ZIM[!is.na(ZIM$value), ], aes(x = Timestamp, y = value))
     p <- p + geom_line(aes(colour = Subject))
     p <- p + facet_grid(Sensor_Type ~ .,
                         scales = "free_y")
     p <- p + theme_bw()
     p <- p + labs(y = "Turgor [kPa], or C or rh%",
                   x = "Date")
p

x <- ZIM
x$Unit <- NULL
x$Sensor.type <- NULL
#x$Sensor_Type <- NULL
x$Sensor_ID <- NULL
x$Transmitter <- NULL
x$Channel <- NULL
x$CO2_treatment <- NULL
x$Plot <- NULL
x$SensorID <- NULL
x$Ring <- NULL
#x$Object <- NA
#x$Object[grepl("leaf", x$Subject) == TRUE ] <- "Leaf"
#x$Object[grepl("Weather", x$Subject) == TRUE ] <- "Weather"
#x$Object <- as.factor(x$Object)

x.weather <- x[x$Sensor_Type == "Temperature", ]
x.weather$variable <- NULL
x.weather$Cultivar <- NULL
x.weather$Subject <- NULL
x.weather$Object <- NULL
x.weather$Sensor_Type <- NULL
names(x.weather) <- gsub("value", "Temperature", names(x.weather))

x.turgor <- x[grepl("leaf", x$Subject) == TRUE, ]

ZIM.cast <- merge(x.turgor, x.weather)
ZIM.cast$Date <- as.Date(ZIM.cast$Timestamp, tz = "Australia/Melbourne")
ZIM.cast$Day <- format(ZIM.cast$Timestamp, "%j")
ZIM.cast$Hour <- format(ZIM.cast$Timestamp, "%H")
#ZIM.cast$Day <- as.factor(ZIM.cast$Day)
ZIM.hour$Descript <- "Day"

# rescale turgor to be between 0 and 1
ZIM.cast <- ddply(ZIM.cast,
             .(Subject, Day, Date),
             mutate,
             turgor.rescale = rescale(value, to = c(0, 1))
             )

ZIM.hour <- ddply(ZIM.cast,
             .(Subject, Day, Date, Hour),
             summarise,
             timestamp.hour = mean(Timestamp),
             turgor.hour = mean(turgor.rescale, na.rm = TRUE),
             turgor.hour.sd = sd(turgor.rescale, na.rm = TRUE),
             temp.hour = mean(Temperature, na.rm = TRUE)
             )


theme_my <- theme_bw() + theme(
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "white"),
            legend.key       = element_blank())

p <- ggplot(ZIM.hour, aes(x = timestamp.hour, y = turgor.hour))
  p <- p + geom_line(aes(colour = Subject))
  p <- p + theme_my
p

p <- ggplot(ZIM.cast, #[ZIM.cast$Day > 205 & ZIM.cast$Day < 210, ], 
            aes(x = Temperature, y = turgor.rescale))
  p <- p + geom_path()
  p <- p + facet_grid(Subject ~ Day)
  p <- p + theme_my
  p <- p + labs(y = "Turgor, rescaled to daily min and max value",
                x = expression("Temperature ["~phantom()*degree~C~"]"))
p


p <- ggplot(ZIM.hour, #[ZIM.hour$Day > 205, ], 
            aes(x = temp.hour, y = turgor.hour))
  p <- p + geom_path(aes(colour = as.numeric(Hour)))
  p <- p + scale_colour_gradient2(midpoint = 12, low = "red", mid = "grey", high = "blue")
  p <- p + facet_grid(Subject ~ Date)
  p <- p + theme_my
  p <- p + labs(y = "Mean hourly turgor, rescaled to daily min and max value [dimensionless]",
                x = expression("Temperature ["~phantom()*degree~C~"]"),
                colour = "Hour of day")
p

