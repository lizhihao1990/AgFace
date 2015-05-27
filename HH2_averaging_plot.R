library(ggplot2)

setwd("~/AgFace/2014/HH2_Soil_moisture")

load("PR2_soil_moisture.RData")
mydata <- sm

# averaging plot
mydata$Day <- format(mydata$Time, "%Y-%m-%d")
mydata$Day <- as.Date(mydata$Day)
mydata$Week <- as.numeric(format(mydata$Time, "%W"))
mydata$Weekname <- paste("Week", mydata$Week, sep = " ")


# key harvest dates
DC30.date <- as.Date("2014-08-13")
DC65.date <- as.Date("2014-10-07")
DC90.date <- as.Date("2014-11-19")

Key.dates <- c(DC30.date, DC65.date, DC90.date)

#figures
p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ], 
            aes(x = Day, y = mV))
 p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_sdl", mult = 1, 
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
#  p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = FALSE)
  p <- p + facet_grid(Depth ~ Cultivar)
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = "Soil moisture expressed as raw milliVolts, default calibration [mV]")
p

p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ], 
            aes(x = Day, y = mV))
  p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_sdl", mult = 1, geom = "linerange", 
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = FALSE)
  p <- p + facet_grid(Depth ~ Cultivar)
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = "Soil moisture expressed as raw milliVolts, default calibration [mV]")
p

ggsave(file = "Smoothed_soil_moisture_profiles_wheat_2014.pdf",
       width = 9, height = 7)
       
p <- ggplot(mydata[mydata$Crop == "Lentil" &
                   !is.na(mydata$Crop), ], 
            aes(x = Day, y = mV))
  p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_cl_normal", geom = "linerange", mult = 1,
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = TRUE)
  p <- p + facet_grid(Depth ~ Cultivar)
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                x = "Date",
                y = "Soil moisture expressed as raw milliVolts, default calibration [mV]")
p

ggsave(file = "Smoothed_soil_moisture_profiles_lentil_2014.pdf",
       width = 9, height = 7)

# average profile for each week
p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ], 
            aes(y = mV, x = Depth))
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
                x = "Soil moisture expressed as raw milliVolts, default calibration [mV]")
p

# attempt to create a plot map
positions <- read.csv("HH2_Soilmoisture_tubes_lookup_table_with_coordinates.csv")

to_keep <- c("Year", "Ring", "PlotID", "Variety", "HH2_tube_number", "Position_x", "Position_y")
positions <- positions[, names(positions) %in% to_keep]

mydata2 <- merge(mydata, positions,
                 all.x = TRUE,
                 by.x = c("Year", "Ring", "PlotID", "Cultivar", "Sample"),
                 by.y = c("Year", "Ring", "PlotID", "Variety", "HH2_tube_number"))

p <- ggplot(mydata2[mydata2$Crop == "Wheat" &
                   !is.na(mydata2$Crop) &
                   #mydata2$Ring >= 10 &
                   mydata2$Depth == 300, ],
                   aes(x = Position_x, y = Position_y, z = mV))
  p <- p + geom_tile(aes(fill = mV))
  p <- p + scale_fill_gradient(low = "red", high = "blue")
  p <- p + facet_grid(Depth ~ Week)
  p <- p + theme_bw()
p

# average soil moisture per ring
p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ],
            aes(x = as.factor(Ring), y = mV))
  p <- p + geom_boxplot()
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
p

p <- ggplot(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ],
                   aes(x = mV))
  p <- p + geom_histogram()
  p <- p + facet_grid(Depth ~ Ring * Tube_treatment)
  p <- p + theme_bw()
p


library(plyr)
library(reshape2)
my.summary <- ddply(mydata[mydata$Crop == "Wheat" &
                   !is.na(mydata$Crop), ],
                    .(Ring, Depth, Week, Tube_treatment),
                    summarise,
                    mean = mean(mV, na.rm = TRUE),
                    sd   = sd(mV,   na.rm = TRUE))

p <- ggplot(my.summary, aes(x = Ring, y = mean))
  p <- p + geom_bar(aes(fill = as.factor(Depth)), stat = "identity", position = "dodge")
  p <- p + facet_grid(Tube_treatment ~ Week)
  p <- p + theme_bw()
p

library(plot3D)

m3 <- mydata[, c("CO2", "Cultivar", "Crop", "Tube_treatment", "Week", "Depth", "mV")]

m3.cast <- dcast(m3, value.var = "mV",
                Crop + Cultivar + CO2 + Tube_treatment + Depth ~ Week,
                fun.aggregate = mean)

MySubsets <- function(data) {
  require(plyr)
  my.list <- dlply(data,
                   .(Crop, Cultivar, CO2, Tube_treatment),
                   function(x) {
                   my.df <- x[, c("Depth", "Week", "mV")]
                   my.df.cast <- dcast(my.df, value.var = "mV",
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
        theta = 66, clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = FALSE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [mV]",
        zlim = c(0, 1000), #ticktype="detailed",
        main = my.names)
}

pdf(file = "Soil_moisture_over_time_and_depth.pdf")
	for (i in 1:15) {
		print(My3D(my.list[i]))}
dev.off()

# differences between specific treatments

Scout.aCO2.wet.minus.dry <- my.list["Wheat.Scout.aCO2.wet"][[1]] - my.list["Wheat.Scout.aCO2.ambient"][[1]]

Scout.eCO2.wet.minus.dry <- my.list["Wheat.Scout.eCO2.wet"][[1]] - my.list["Wheat.Scout.eCO2.ambient"][[1]]

pdf(file = "Scout_differences_due_to irrigation.pdf",
    width = 15, height = 9)
par(mfrow=c(1,2))
print(persp3D(z = my.list["Wheat.Scout.aCO2.ambient"][[1]],
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        colvar = Scout.aCO2.wet.minus.dry,
        col = my.dif,
        theta = 66, #clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = TRUE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [mV]",
        zlim = c(0, 1000), #ticktype="detailed",
        main = "Wheat.Scout.aCO2.ambient",
        clab = "Difference wet minus dry [mv]"))

print(persp3D(z = my.list["Wheat.Scout.eCO2.ambient"][[1]],
        contour = TRUE, image = TRUE, shade = 0.1,
        box = TRUE, axes = TRUE,
        colvar = Scout.eCO2.wet.minus.dry,
        col = my.dif,
        theta = 66, #clab = "mV",
        phi = 15, expand = 0.8,
        facets = TRUE, colkey = TRUE,
        xlab = "Depth [mm]",
        ylab = "Week",
        zlab = "Soil moisture [mV]",
        zlim = c(0, 1000), #ticktype="detailed",
        main = "Wheat.Scout.eCO2.ambient",
        clab = "Difference wet minus dry [mv]"))
dev.off()
        

library(lattice)
wireframe(mV ~ Depth * Week,
          data = m3,
          zlim = range(seq(0, 1000,by = 100)),
          scales = list(arrows=TRUE, cex= .85, col = "black", font = 1, tck = 1),
          drape = TRUE, colorkey = FALSE, shade = TRUE,
          aspect= c(1.6, 1))

library(rgl)
rgl.surface(m3$mV, m3$Depth, m3$Week)
