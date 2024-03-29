# HH2 workflow

# 1) call import script
# 2) call processing script
# 3) call summarise script

# call helper scripts
source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")
source("~/AgFace/R_scripts/HH2_file_processing.R")
source("~/AgFace/R_scripts/HH2_visualisation_and_summary.R")

# set working directory
setwd("~/AgFace/2015/HH2_Soil_moisture/Pre-season_calibration/PR2")

#1) HH2_soil_moisture_import.R
#mydata <- HH2Import("2015-01-12-PR2-Sumps.csv")
mydata <- HH2Import("PR2-2015-03-04-Sumps.csv", sensor.type = "PR2")

#2) HH2_file_processing.R
mydata <- HH2SoilMoistureProcess(mydata, is.first = FALSE, pre.season = TRUE)

library(ggplot2)
p <- ggplot(mydata, aes(x = Time, y = Percent_Vol))
  p <- p + geom_line(aes(colour = as.factor(Sample)))
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
p

mydata$Day <- format(mydata$Time, "%Y-%m-%d")
mydata$Day <- as.Date(mydata$Day)
mydata$Week <- as.numeric(format(mydata$Time, "%W"))
mydata$Weekname <- paste("Week", mydata$Week, sep = " ")

p <- ggplot(mydata, aes(x = Day, y = Percent_Vol))
  p <- p + stat_summary(fun.data = mean_sdl, mult = 1)
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
  p <- p + labs(y = "Mean volumetric soil moisture [%] ± sd (n = 12 to 16)")
p
ggsave(file = "Soil_moisture_Jan_to_March_2015.pdf",
       width = 9, height = 7)
PR2.precal <- mydata
