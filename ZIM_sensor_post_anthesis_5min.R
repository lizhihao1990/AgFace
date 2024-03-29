# ZIM leaf water turgor sensor data import

# Markus Löw, Nov 2014

library(reshape2)
library(ggplot2)

# source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

setwd("~/AgFace/2014/ZIM_sensors/2014-10-15_Post_anthesis_heat_shock")

# import sensor IDs, and experimental layout
IDs <- read.csv("2014-10-15_Post-anthesis_heat_shock_configuration.csv")

# import raw ZIM sensor data
the.data <- "12-11-2014-15_37_51_zim_probe_datas_5min_LT_2014-10-11_22_14_00_to_2014-11-12_01_15_00.csv"

df <- read.csv(the.data, sep = "\t")

# special treatment for humidity sensor 1808
# names(df)[grep("Hum", names(df))]
names(df) <- gsub("1808\\.\\.", "1808.Per", names(df))

names(df) <- gsub("\\.{1,}", "_", names(df))
names(df) <- gsub("_MS_Agface_second_heat_shock", "", names(df))
names(df) <- gsub("_MS_Sensor_is_not_configured", "", names(df))
#names(df) <- gsub("X_LSN_Scout_outside_", "", names(df))
names(df) <- gsub("_in_Local_time", "", names(df))

# duplicate sensors
pattern <- "[[:digit:]]_[[:digit:]]_[[:digit:]]"
my.duplicates <- grep(pattern, names(df))

df.orig <- df
df <- df[, -my.duplicates]

# empty sensors, duplicates again
names.with.point <- grep("\\.", names(df))
# names(df)[names.with.point]

df <- df[, -names.with.point]

# convert timestamp to POSIXct
# timestamp is already in POSIX format
df$Timestamp <- as.POSIXct(df$Timestamp, 
                           tz = "Australia/Melbourne")

# Temperature dependance
df.melt <- melt(df,
                id = c("Timestamp",
                       "Temperature_ID_1417_C_1424_3", 
                        "Humidity_ID_1808_Per_1427_3"))
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

names(df.melt) <- gsub("Temperature_ID_1417_C_1424_3", "Temperature_C", names(df.melt))
names(df.melt) <- gsub("Humidity_ID_1808_Per_1427_3 ", "Humidity_Per", names(df.melt))

## assign treatments
ZIM5 <- merge(IDs, df.melt,
             by.x = c("Sensor_ID", "Transmitter", "Channel"),
             by.y = c("SensorID",  "Transmitter", "Channel"))
ZIM5$Date   <- NULL
ZIM5$Remark <- NULL
ZIM5$Channel <- as.factor(ZIM5$Channel)

heat.start <- as.POSIXct("2014-10-21 10:00:00", tz = "Australia/Melbourne")
heat.stop <-  as.POSIXct("2014-10-23 16:00:00", tz = "Australia/Melbourne")
heat.dates <- as.numeric(c(heat.start, heat.stop))

ZIM5$heat_shock <- "No heat"
ZIM5$heat_shock[ZIM5$Timestamp > heat.start &
                ZIM5$Timestamp < heat.stop] <- "Heat"
ZIM5$heat_shock <- as.factor(ZIM5$heat_shock)

# visualisation
p <- ggplot(ZIM5[!is.na(ZIM5$value) & 
                ZIM5$Sensor.type == "Turgor", ],
                aes(x = Temperature_C, y = value))
     p <- p + geom_point(aes(colour = Cultivar), alpha = 0.01)
     p <- p + geom_smooth(method = "lm", aes(colour = Cultivar), se = TRUE)
     p <- p + facet_grid(CO2_treatment ~ heat_shock * Location_during_heat_stress,
                         scales = "free_y")
     p <- p + theme_bw()
     p <- p + labs(y = "Turgor [kPa], raw data",
                   x = "Outside air temperature [C]")
p

ggsave(file = "Leaf_water_turgor_temperature_relationship_during_post-anthesis_heat_shock_5min.pdf",
       width = 9, height = 7)
ggsave(file = "Leaf_water_turgor_temperature_relationship_during_post-anthesis_heat_shock_5min.png",
       width = 9, height = 7)
