# ZIM leaf water turgor sensor data import

# Markus Löw, Oct 2014

library(reshape2)
library(ggplot2)

# source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

setwd("~/AgFace/2014/ZIM_sensors/2014-09-30_Pre-anthesis_heat_shock")

# import sensor IDs, and experimental layout
IDs <- read.csv("2014-09-30_Pre-anthesis_heat_shock_configuration.csv")

# import raw ZIM sensor data

the.data <- "09-10-2014-11_36_31_zim_probe_datas_raw_LT_2014-09-28_23_48_00_to_2014-10-09_00_36_00.csv"


df <- read.csv(the.data, sep = "\t")

names(df) <- gsub("\\.{1,}", "_", names(df))
names(df) <- gsub("_MS_Agface", "", names(df))
names(df) <- gsub("X_LSN_Scout_outside_", "", names(df))
names(df) <- gsub("_in_Local_time", "", names(df))

# convert timestamp to POSIXct
# timestamp is already in POSIX format
df$Timestamp <- as.POSIXct(df$Timestamp, 
                           tz = "Australia/Melbourne")

df.melt <- melt(df,
                id = "Timestamp")
df.melt$Sensor.type <- sapply(
                       strsplit(
                       as.character(df.melt$variable), "_"), "[", 1)
df.melt$SensorID <- sapply(
                    strsplit(
                    as.character(df.melt$variable), "_"), "[", 3)
df.melt$SensorID <- as.numeric(df.melt$SensorID)
df.melt$Unit <- sapply(
                strsplit(
                as.character(df.melt$variable), "_"), "[", 4)
df.melt$Transmitter <- sapply(
                    strsplit(
                    as.character(df.melt$variable), "_"), "[", 5)
df.melt$Transmitter <- as.numeric(df.melt$Transmitter)
df.melt$Channel <- sapply(
                   strsplit(
                   as.character(df.melt$variable), "_"), "[", 6)
df.melt$Channel <- as.numeric(df.melt$Channel)

# assign treatments
ZIM <- merge(IDs, df.melt,
             by.x = c("Sensor_ID", "Transmitter", "Channel"),
             by.y = c("SensorID", "Transmitter", "Channel"))
ZIM$Date   <- NULL
ZIM$Remark <- NULL


heat.start <- as.POSIXct("2014-10-01 10:00:00", tz = "Australia/Melbourne")
heat.stop <-  as.POSIXct("2014-10-03 16:00:00", tz = "Australia/Melbourne")

heat.dates <- as.numeric(c(heat.start, heat.stop))

# visualisation
p <- ggplot(ZIM[!is.na(ZIM$value), ], aes(x = Timestamp, y = value))
     p <- p + geom_line(aes(colour = Cultivar))
     p <- p + geom_vline(xintercept = heat.dates, colour = "grey")
     p <- p + facet_grid(Ring * CO2_treatment ~ Location_during_heat_stress,
                         scales = "free_y")
     p <- p + theme_bw()
     p <- p + labs(y = "Turgor [kPa], raw data",
                   x = "Date")
p

ggsave(file = "Leaf_water_turgor_during_pre-anthesis_heat_shock.pdf",
       width = 9, height = 7)

