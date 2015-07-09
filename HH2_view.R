# HH2 data view

library(plyr)

# call helper scripts
source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")
source("~/AgFace/R_scripts/HH2_PR2_treatment_info_2015.R")
source("~/AgFace/R_scripts/HH2_file_processing_2015.R")
source("~/AgFace/R_scripts/HH2_visualisation_and_summary_2015.R")

# set working directory
setwd("~/AgFace/2015/PR2_Soil_moisture")

# load current soil moisture data
load("PR2_soil_moisture.RData")

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

library(plyr)
out <- ddply(sm.recal,
             .(Trial, Ring, Depth),
             summarise,
             mean = mean(volSWC.recal, na.rm = TRUE))

p <- ggplot(sm.recal, aes(x = Date, y = Percent_Vol))
  p <- p + geom_hline(yintercept = 50, colour = "grey", linetype = "dotted")
  p <- p + geom_line(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  #p <- p + geom_hline(data = out, aes(yintercept = mean))
  p <- p + facet_grid(Depth ~ Trial * Ring)
  p <- p + labs(y = expression("Soil moisture, uncalibrated (%)"))
  p <- p + theme_my
  p <- p + theme(legend.position = "none",
                 axis.text.x = element_text(size = rel(0.75), angle = 90))
p
fig.tubes.time.percent <- p

p <- ggplot(sm.recal, aes(x = Date, y = volSWC.recal))
  p <- p + geom_hline(yintercept = 1, colour = "grey", linetype = "dotted")
  p <- p + geom_line(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  #p <- p + geom_hline(data = out, aes(yintercept = mean))
  p <- p + facet_grid(Depth ~ Trial * Ring)
  p <- p + labs(y = expression("Soil moisture, calibrated Dec 2014 "(m^3*m^-3)))
  p <- p + theme_my
  p <- p + theme(legend.position = "none",
                 axis.text.x = element_text(size = rel(0.75), angle = 90))
p
fig.tubes.time.volSWC <- p

# Wet sumps only
p <- ggplot(sm.recal[sm.recal$Trial == "WetSump", ],
            aes(x = Time, y = volSWC.recal))
  p <- p + geom_hline(yintercept = 1, colour = "grey", linetype = "dotted")
  p <- p + geom_line(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + facet_grid(Depth ~ PlotID)
  p <- p + labs(y = expression("Soil moisture, calibrated Dec 2014 "(m^3*m^-3)),
                colour = "Tube#")
  p <- p + theme_my
#  p <- p + theme(legend.position = "none",
#                 axis.text.x = element_text(size = rel(0.75), angle = 90))
p
fig.wet.sump.time <- p

out <- ddply(sm.recal,
             .(Trial, Ring),
             summarise,
             mean = mean(volSWC.recal, na.rm = TRUE))

p <- ggplot(sm.recal,
            aes(x = volSWC.recal, y = Depth))  
  p <- p + geom_vline(xintercept = 1, colour = "grey", linetype = "dotted")
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  #p <- p + geom_vline(data = out, aes(xintercept = mean))
  p <- p + facet_grid(Trial * Ring ~ Date)
  p <- p + scale_y_reverse()
  p <- p + labs(x = expression("Soil moisture, calibrated Dec 2014 "(m^3*m^-3)),
                y = "Depth (mm)")
  p <- p + theme_my
  p <- p + theme(legend.position = "none")
p
fig.tubes.xprofiles <- p

p <- ggplot(sm.recal[sm.recal$Ring == "3" & sm.recal$Trial == "TraitFace", ],
            aes(x = volSWC.recal, y = Depth))  
  p <- p + geom_vline(xintercept = 1, colour = "grey", linetype = "dotted")
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  #p <- p + geom_vline(data = out, aes(xintercept = mean))
  p <- p + facet_grid(Trial * Ring ~ Date)
  p <- p + scale_y_reverse()
  p <- p + labs(x = expression("Soil moisture, calibrated Dec 2014 "(m^3*m^-3)),
                y = "Depth (mm)")
  p <- p + theme_my
  #p <- p + theme(legend.position = "none")
p
fig.tubes.xprofiles.select03 <- p

p <- ggplot(sm.recal[sm.recal$Ring == "15" & sm.recal$Trial == "TraitFace", ],
            aes(x = volSWC.recal, y = Depth))  
  p <- p + geom_vline(xintercept = 1, colour = "grey", linetype = "dotted")
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  #p <- p + geom_vline(data = out, aes(xintercept = mean))
  p <- p + facet_grid(Trial * Ring ~ Date)
  p <- p + scale_y_reverse()
  p <- p + labs(x = expression("Soil moisture, calibrated Dec 2014 "(m^3*m^-3)),
                y = "Depth (mm)")
  p <- p + theme_my
  #p <- p + theme(legend.position = "none")
p
fig.tubes.xprofiles.select15 <- p


my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))
names(my.figures.list) <- my.figures

my.width = 27
my.height = 21

pdf(file = "Soil_moisture_figures.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(my.figures.list)
dev.off()

data(iris)
p <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width))
  p <- p + geom_point()
  p <- p + stat_summary(fun.data = "mean_sdl")
p

