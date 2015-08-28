library(ggplot2)
library(reshape2)

setwd("~/AgFace/2015/Arduino/logger_test")

df <- read.csv("~/AgFace/2015/Arduino/logger_test/DATALOG_diode_only.CSV", header = FALSE)

#names(df) <- c("Epoch", "Date", "Temp",	"rH", "PARmVA",	"PARmVB", "Batt_mV", "TDRmV")
names(df) <- c("Epoch", "Date", "LightmV", "NA", "Batt")
df$Date <- as.POSIXct(df$Date,
             format = "%Y-%m-%d %H:%M:%S")
# get rid of data when there was no Batt_mV
start.time <- as.POSIXct("2015-08-21 00:00:00")
df <- df[df$Date > start.time, ]

# get rid of measurement outliers
#df <- df[df$Batt_mV < 5040, ]
#df <- df[df$Temp > -10 &
#         df$Temp < 30, ]
#df$rH[df$rH < -1] <- NA
#df$PARmVB[df$PARmVB > 1000] <- NA
#df <- df[df$Batt_mV > 4900, ]
#df <- df[df$TDRmV < 4000, ]

my.max <- max(df$LightmV)
df.melt <- melt(df,
                id.vars = c("Epoch", "Date"))

p <- ggplot(df.melt[df.melt$variable == "LightmV", ], aes(x = Date, y = value))
  p <- p + geom_line(aes(colour = variable))
  p <- p + geom_hline(yintercept = max(my.max))
  #p <- p + facet_grid(variable ~ ., scale = "free_y")
  p <- p + theme_bw()
p

