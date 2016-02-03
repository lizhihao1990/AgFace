# Import and analysis of ZIM turgor sensor data
# Yao Dai Sept 9 to Sept 15, TraitFace

# set the working directory
setwd("~/AgFace/2015/ZIM/Yao_TraitFace_round1/2015-11-23")
# setwd("c:\mydata\hjgkhjgk\")

# import the meta-data
# meta-data describe the experiment
meta <- read.csv("Sep09_Configuration.csv")
zim  <- read.csv("Sep09_Transformed.csv")

# check the data file
dim(meta)
dim(zim)

names(meta)
names(zim)

# prepare the zim data for merging with the meta-data
# install.packages("reshape2")
library(reshape2)


zim.melt <- melt(zim)

# rename the variable-column to "SensorID"
names(zim.melt) <- gsub("variable", "SensorID", names(zim.melt))

# the SensorID have an unnecessary "X" in front of them
zim.melt$SensorID <-  gsub("X", "", zim.melt$SensorID)
zim.melt$SensorID <- as.numeric(as.character(zim.melt$SensorID))

# merge the meta-data with the turgor data
ZIM <- merge(meta, zim.melt)

# convert the Time-factor into an actual time
ZIM$Time <- as.POSIXct(ZIM$Time, format = "%Y/%m/%d %H:%M")

write.csv(ZIM, file = "ZIMdata.csv", row.names = FALSE)

# create graphs
# Grammar of Graphics
# install.packages("ggplot2")
library(ggplot2)

p <- ggplot(ZIM, aes(x = Time, y = value))
  p <- p + geom_point(aes(colour = CO2_treatment))
  p <- p + facet_grid(Sensor_Type ~ Cultivar)
p


ZIMturgor <- ZIM[ZIM$Sensor_Type  == "Turgor", ]

ZIMtemp   <- ZIM[ZIM$Sensor_Type  == "Temperature", ]
names(ZIMtemp) <- gsub("value", "Temperature", names(ZIMtemp))
to.keep <- c("Time", "Temperature")
ZIMtemp2 <- ZIMtemp[, names(ZIMtemp) %in% to.keep]

# merge the temperature data with the turgor data
ZIMturgor2 <- merge(ZIMtemp2, ZIMturgor)

write.csv(ZIMturgor2, file = "ZIMturgor2.csv", row.names = FALSE)

# calculate means as there might be a problem with "stat_summary" to do it for us
library(plyr)
ZIMturgor2.mean <- ddply(ZIMturgor2,
                        .(Time, Cultivar, Ring, CO2_treatment),
                        summarise,
                        Temp.mean = mean(Temperature, na.rm = TRUE),
                        Turgor.mean = mean(value, na.rm = TRUE))

ZIMturgor.CO2mean <- ddply(ZIMturgor2.mean,
                        .(Time, Cultivar, CO2_treatment),
                        summarise,
                        Temp.mean = mean(Temp.mean, na.rm = TRUE),
                        Turgor.mean = mean(Turgor.mean, na.rm = TRUE),
                        Turgor.sd = sd(Turgor.mean, na.rm = TRUE))
                        
ZIMturgor.CO2mean$Day <- format(ZIMturgor.CO2mean$Time, "%Y-%m-%d")
                     
p <- ggplot(ZIMturgor, aes(x = Time, y = value))
  p <- p + geom_line(aes(colour = CO2_treatment))
  p <- p + facet_grid(Sensor_Type ~ Cultivar)
p

p <- ggplot(ZIMturgor, aes(x = Time, y = value))
  p <- p + stat_summary(aes(colour = CO2_treatment), 
                        fun.data  = "mean_sdl", fun.args = list(mult = 1), 
                        geom = "point")
  p <- p + facet_grid(Sensor_Type ~ Cultivar)
  p <- p + theme_bw()
p

p <- ggplot(ZIMturgor2, aes(x = Temperature, y = value))
  p <- p + stat_summary(aes(colour = CO2_treatment), 
                        fun.data  = "mean_sdl", fun.args = list(mult = 1), 
                        geom = "point")
  p <- p + facet_grid(Sensor_Type ~ Cultivar)
  p <- p + theme_bw()
p

# extract the day-information from time
ZIMturgor2$Day <- format(ZIMturgor2$Time, "%Y-%m-%d")
p <- ggplot(ZIMturgor2, aes(x = Temperature, y = value))
  p <- p + stat_summary(aes(colour = CO2_treatment), 
                        fun.data  = "mean_sdl", fun.args = list(mult = 1), 
                        geom = "point")
  p <- p + facet_grid(Cultivar ~ Day)
  p <- p + theme_bw()
p

limits <- aes(ymax = Turgor.mean + Turgor.sd, ymin = Turgor.mean - Turgor.sd )

p <- ggplot(ZIMturgor.CO2mean, aes(x = Temp.mean, y = Turgor.mean))
  p <- p + geom_errorbar(aes(ymax = Turgor.mean + Turgor.sd, 
                             ymin = Turgor.mean - Turgor.sd ), alpha = 0.03)
  #p <- p + geom_errorbar(limits, alpha = 0.03)
  p <- p + geom_path(aes(colour = CO2_treatment))
  p <- p + facet_grid(Cultivar ~ Day)
  p <- p + labs(y = "relative Turgor [dimensionsell]",
                x = "Mean Temperature [°C]")
  p <- p + theme_bw()
p
