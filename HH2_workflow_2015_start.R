# HH2 workflow

# 1) call import script
# 2) call processing script
# 3) call summarise script

# call helper scripts
source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")
source("~/AgFace/R_scripts/HH2_PR2_treatment_info_2015.R")
source("~/AgFace/R_scripts/HH2_file_processing_2015.R")
source("~/AgFace/R_scripts/HH2_visualisation_and_summary_2015.R")

# set working directory
setwd("~/AgFace/2015/PR2_Soil_moisture")

#1) HH2_soil_moisture_import.R
my.data <- HH2Import("PR2_20150611.csv", sensor.type = "PR2")

#2) HH2_file_processing.R
sm <- HH2SoilMoistureProcess(my.data, is.first = TRUE)

# next file
my.data1 <- HH2Import("PR2_20150622.csv", sensor.type = "PR2")

sm <- HH2SoilMoistureProcess(my.data1, is.first = FALSE)

# next file
my.data2 <- HH2Import("PR2-20150630.csv", sensor.type = "PR2")

sm <- HH2SoilMoistureProcess(my.data2, is.first = FALSE)

write.table(sm, file = "out.csv", row.names = F, sep = ",")

#3) HH2_visualisation_and_summary.R
# HH2Visual(mydata)

# implement calibration
load("~/AgFace/2014/HH2_Soil_moisture/PR2_soil_moisture_calibration_coefficients.RData")

calib.coef.cast <- calib.coef
sm$V <- sm$mV/1000

library(plyr)
sm.recal <- ddply(sm,
   .(Depth),
   function(x) {
   my.depth <- unique(x$Depth)
   my.inter <- calib.coef.cast$Intercept[calib.coef.cast == my.depth]
   my.slope <- calib.coef.cast$Slope[calib.coef.cast == my.depth]
   x$volSWC.recal <- with(x, ((1.125 - 5.53 * V + 67.17 * V^2 - 234.42 * 
                V^3 + 413.56 * V^4 - 356.68 * V^5 + 121.53 * V^6) - my.inter) / my.slope)
   return(x)
   
})

sm.recal$Date <- as.Date(sm.recal$Time, tz = "Australia/Melbourne")

library(ggplot2)
source("~/AgFace/R_scripts/MyThemes.R")

p <- ggplot(sm.recal, aes(x = Time, y = volSWC.recal))
  p <- p + geom_hline(yintercept = 1, colour = "grey", linetype = "dotted")
  p <- p + geom_line(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + facet_grid(Depth ~ Trial * Ring)
  p <- p + labs(y = expression("Soil moisture, calibrated Dec 2014"(m^3*m^-3)))
  p <- p + theme_my
  p <- p + theme(legend.position = "none",
                 axis.text.x = element_text(size = rel(0.75), angle = 90))
p

# Wet sumps only
p <- ggplot(sm.recal[sm.recal$Trial == "WetSump", ],
            aes(x = Time, y = volSWC.recal))
  p <- p + geom_hline(yintercept = 1, colour = "grey", linetype = "dotted")
  p <- p + geom_line(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + facet_grid(Depth ~ PlotID)
  p <- p + labs(y = expression("Soil moisture, calibrated Dec 2014"(m^3*m^-3)),
                colour = "Tube#")
  p <- p + theme_my
#  p <- p + theme(legend.position = "none",
#                 axis.text.x = element_text(size = rel(0.75), angle = 90))
p


p <- ggplot(sm.recal, aes(x = Date, y = volSWC.recal))
  p <- p + stat_summary(aes(colour = CO2_treatment), fun.data = "mean_sdl")
  p <- p + facet_grid(Depth ~ Trial)
  p <- p + labs(y = expression("Soil moisture, calibrated Dec 2014"(m^3*m^-3)))
  p <- p + theme_my
  #p <- p + theme(legend.position = "none",
  #               axis.text.x = element_text(size = rel(0.75), angle = 90))
p

p <- ggplot(sm.recal,
            aes(x = volSWC.recal, y = Depth))
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  p <- p + facet_grid(Trial * Ring ~ Date)
  p <- p + scale_y_reverse()
  p <- p + labs(x = expression("Soil moisture, calibrated Dec 2014"(m^3*m^-3)))
  p <- p + theme_my
  p <- p + theme(legend.position = "none")
p

