# recalculate HH2 profile probe data based on self-derived, soil- and depth-specific calibration

setwd("~/AgFace/2014/HH2_Soil_moisture")

load("HH2_calibration_workspace.RData")
setwd("~/AgFace/2015/HH2_Soil_moisture/Pre-season_calibration/")
sm <- pre.cal
library(plyr)
library(reshape2)
library(ggplot2)

# calculate SWC theta [m3 m-3] using individual slopes and intercepts per depth
# slope and intercept from the data frame "calib.coef"
# calculation is based on Volt instead of millivolt
sm$V <- sm$mV/1000

#calib.coef.strip <- calib.coef[, c(1:3, 7)]

#calib.coef.cast <- dcast(calib.coef.strip,
#                         Depth ~ term,
#                         value.var = "estimate")


calib.coef.cast <- calib.coef

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

sm.recal.out <- sm.recal
sm.recal.out$Percent_Vol <- NULL
sm.recal.out$Vol_Eror <- NULL
sm.recal.out$mV <- NULL
sm.recal.out$mV_Error <- NULL
sm.recal.out$V <- NULL
names(sm.recal.out) <- gsub("volSWC.recal", "volumetric_soil_moisture_m3m-3", names(sm.recal.out))

write.table(sm.recal.out, file = "Soil_moisture_2014_calibration_date_Dec_2014.csv",
            sep = ",", na = "", row.names = FALSE)
mydata <- sm.recal

# averaging plot
mydata$Day <- format(mydata$Time, "%Y-%m-%d")
mydata$Day <- as.Date(mydata$Day)
# the data set for October 13 is split between Oct 13 and 14
# moving Oct 13 to Oct 14
mydata$Day[mydata$Day == as.Date("2014-10-13")] <- as.Date("2014-10-14")
mydata$Week <- as.numeric(format(mydata$Time, "%W"))
mydata$Weekname <- paste("Week", mydata$Week, sep = " ")



#figures
p <- ggplot(mydata, 
            aes(x = Day, y = volSWC.recal))
  p <- p + stat_summary( 
                        fun.data = "mean_sdl", mult = 1, geom = "linerange", 
                        position = position_jitter(width = 0.25), alpha = 0.5)
 
  p <- p + geom_smooth(se = FALSE)
  p <- p + facet_grid(Depth ~ .)#, scale = "free_y")
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = expression("Soil moisture based on soil- and depth-specific calibration ["~m^3*m^-3*"]"))
p

fig.swc.calibrated.timecourse <- p


p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ], 
            aes(x = Day, y = volSWC.recal))
  p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_sdl", mult = 1, geom = "linerange", 
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = FALSE)
  p <- p + facet_grid(Depth ~ Cultivar, scale = "free_y")
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = expression("Soil moisture based on soil- and depth-specific calibration ["~m^3*m^-3*"]"))
p

fig.swc.calibrated.timecourse.free_y <- p

# playground
p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop) &
                   mydata$Tube_treatment == "ambient" &
                   mydata$Depth == 300, ], 
            aes(x = Day, y = volSWC.recal))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "mean_sdl", mult = 1, geom = "line", 
                        alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "mean_sdl", mult = 1, geom = "linerange", 
                        alpha = 0.5)
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "mean_sdl", mult = 1, geom = "point", 
                        alpha = 0.5)
  #p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = FALSE)

  p <- p + facet_grid(Cultivar ~ Depth)
  p <- p + theme_bw()
  p <- p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.position = c(0.05, 0.90))
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = expression("Soil moisture based on soil- and depth-specific calibration ["~m^3*m^-3*"]"))
p
ggsave(file = "Depth_300_rainfed_only.pdf",
       width = 9, height = 6)
ggsave(file = "Depth_300_rainfed_only.png",
       width = 11, height = 5)


p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ], 
            aes(x = Day, y = volSWC.recal))
  p <- p + stat_summary(aes(colour = CO2, linetype = Tube_treatment), 
                        fun.data = "mean_sdl", mult = 1, geom = "line", 
                        alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + stat_summary(aes(colour = CO2, linetype = Tube_treatment), 
                        fun.data = "mean_sdl", mult = 1, geom = "linerange", 
                        alpha = 0.5)
  p <- p + stat_summary(aes(colour = CO2, linetype = Tube_treatment), 
                        fun.data = "mean_sdl", mult = 1, geom = "point", 
                        alpha = 0.5)
  #p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = FALSE)

  p <- p + facet_grid(Depth ~ Cultivar, scales = "free_y")
  p <- p + theme_bw()
  p <- p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = expression("Soil moisture based on soil- and depth-specific calibration ["~m^3*m^-3*"]"))
p
fig.swc.wheat.timecourse.no_smooth.free_y <- p

p <- ggplot(mydata[mydata$Crop == "Lentil" &
                   !is.na(mydata$Crop), ], 
            aes(x = Day, y = volSWC.recal))
  p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = TRUE)
  p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_cl_normal", geom = "linerange", mult = 1,
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_cl_normal", geom = "point", mult = 1,
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + facet_grid(Depth ~ Cultivar, scale = "free_y")
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = "Soil moisture, soil-specific calibration [m3 m-3]")
p
fig.lentil.timecourse.free_y <- p

# average profile for each week
p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ], 
            aes(y = volSWC.recal, x = Depth))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(colour = CO2, linetype = Tube_treatment, shape = Cultivar),
                        geom = "line")
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(colour = CO2, linetype = Tube_treatment, shape = Cultivar))
  p <- p + coord_flip()
  p <- p + scale_x_reverse()
  p <- p + facet_grid(. ~ Weekname)
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Depth [mm]",
                y = expression("Soil moisture based on soil- and depth-specific calibration ["~m^3*m^-3*"]"))
p
fig.average.profile.per.week <- p

# average profile for last few weeks
p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop) &
                   mydata$Week > 42, ], 
            aes(y = volSWC.recal, x = Depth))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(colour = CO2, linetype = Tube_treatment, shape = Cultivar),
                        geom = "line")
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(colour = CO2, linetype = Tube_treatment, shape = Cultivar))
  p <- p + coord_flip()
  p <- p + scale_x_reverse()
  p <- p + facet_grid(. ~ Weekname)
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Depth [mm]",
                y = expression("Soil moisture based on soil- and depth-specific calibration ["~m^3*m^-3*"]"))
p
fig.average.profile.per.last.weeks <- p

library(plot3D)

m3 <- mydata[, c("CO2", "Cultivar", "Crop", "Tube_treatment", "Week", "Depth", "volSWC.recal")]

m3.cast <- dcast(m3, value.var = "volSWC.recal",
                Crop + Cultivar + CO2 + Tube_treatment + Depth ~ Week,
                fun.aggregate = mean)

MySubsets <- function(data) {
  require(plyr)
  my.list <- dlply(data,
                   .(Crop, Cultivar, CO2, Tube_treatment),
                   function(x) {
                   my.df <- x[, c("Depth", "Week", "volSWC.recal")]
                   my.df.cast <- dcast(my.df, value.var = "volSWC.recal",
                         Depth ~ Week,
                         fun.aggregate = mean)
                   my.df.cast$Depth <- NULL   
                   my.mat <- as.matrix(my.df.cast)
                   }
                   )
  return(my.list)
}

my.list <- MySubsets(mydata)

my.col <- ramp.col (col = c("indianred", "lightblue"), n = 10000, alpha = 0.33)
my.dif <- ramp.col (col = c("lightgrey", "yellow"), n = 10000, alpha = 0.33)

persp3D(z = as.matrix(my.list[[1]]))

My3D <- function(data) {
  my.names <- names(data)
  # print(my.names)
  persp3D(z = as.matrix(data[[1]]),
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        col = my.col,
        theta = 66, clab = "SWC",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = FALSE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [m3 m-3]",
        zlim = c(0, 1), #ticktype="detailed",
        main = my.names)
}

pdf(file = "Soil_moisture_m3_m-3_over_time_and_depth.pdf")
	for (i in 1:15) {
		print(My3D(my.list[i]))}
dev.off()

# differences between specific traits

Scout.aCO2.wet.minus.Yitpi.aCO2.wet <- my.list["Wheat.Scout.aCO2.wet"][[1]] - my.list["Wheat.Yitpi.aCO2.wet"][[1]]

Scout.eCO2.wet.minus.Yitpi.eCO2.wet <- my.list["Wheat.Scout.eCO2.wet"][[1]] - my.list["Wheat.Yitpi.eCO2.wet"][[1]]

Scout.aCO2.dry.minus.Yitpi.aCO2.dry <- my.list["Wheat.Scout.aCO2.ambient"][[1]] - my.list["Wheat.Yitpi.aCO2.ambient"][[1]]

Scout.eCO2.dry.minus.Yitpi.eCO2.dry <- my.list["Wheat.Scout.eCO2.ambient"][[1]] - my.list["Wheat.Yitpi.eCO2.ambient"][[1]]

pdf(file = "Scout_Yitpi_differences_m3_m-3.pdf",
    width = 18, height = 18)
par(mfrow=c(2,2))
print(persp3D(z = my.list["Wheat.Scout.aCO2.wet"][[1]],
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        colvar = Scout.aCO2.wet.minus.Yitpi.aCO2.wet,
        col = my.dif,
        theta = 66, #clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = TRUE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [m3 m-3]",
        zlim = c(0, 1), #ticktype="detailed",
        main = "Wheat.Scout.aCO2.wet",
        clab = "Difference Scout - Yitpi [m3 m-3]"))

print(persp3D(z = my.list["Wheat.Scout.eCO2.wet"][[1]],
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        colvar = Scout.eCO2.wet.minus.Yitpi.eCO2.wet,
        col = my.dif,
        theta = 66, #clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = TRUE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [m3 m-3]",
        zlim = c(0, 1), #ticktype="detailed",
        main = "Wheat.Scout.eCO2.wet",
        clab = "Difference Scout - Yitpi [m3 m-3]"))

print(persp3D(z = my.list["Wheat.Scout.aCO2.ambient"][[1]],
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        colvar = Scout.aCO2.dry.minus.Yitpi.aCO2.dry,
        col = my.dif,
        theta = 66, #clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = TRUE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [m3 m-3]",
        zlim = c(0, 1), #ticktype="detailed",
        main = "Wheat.Scout.aCO2.rainfed",
        clab = "Difference Scout - Yitpi [m3 m-3]"))

print(persp3D(z = my.list["Wheat.Scout.eCO2.ambient"][[1]],
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        colvar = Scout.eCO2.dry.minus.Yitpi.eCO2.dry,
        col = my.dif,
        theta = 66, #clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = TRUE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [m3 m-3]",
        zlim = c(0, 1), #ticktype="detailed",
        main = "Wheat.Scout.eCO2.rainfed",
        clab = "Difference Scout - Yitpi [m3 m-3]"))
dev.off()
        

library(lattice)
wireframe(volSWC.recal ~ Depth * Week,
          data = m3,
          zlim = range(seq(0, 1, by = 0.1)),
          scales = list(arrows=TRUE, cex= .85, col = "black", font = 1, tck = 1),
          drape = TRUE, colorkey = FALSE, shade = TRUE,
          aspect= c(1.6, 1))

# stats on fig.swc.calibrated.timecourse.free_y

# assemble some figures
pdf(file = "SWC_figures.pdf",
    width = 11, height = 8)
print(fig.BulkDensityHistogram)
print(fig.calib.mean)
print(fig.calib.mean.depth)
print(fig.SWCcores_vs_SWC_self_calibrated)
print(fig.individual.tube.correlation)
print(fig.swc.wheat.timecourse.no_smooth.free_y)
print(fig.lentil.timecourse.free_y)
print(fig.average.profile.per.last.weeks)
print(persp3D(z = my.list["Wheat.Scout.aCO2.wet"][[1]],
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        colvar = Scout.aCO2.wet.minus.Yitpi.aCO2.wet,
        col = my.dif,
        theta = 66, #clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = TRUE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [m3 m-3]",
        zlim = c(0, 1), #ticktype="detailed",
        main = "Scout aCO2 wet",
        clab = "Scout - Yitpi [m3 m-3]"))
dev.off()


