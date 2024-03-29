# HH2 visualisation and summary

HH2Visual <- function(data) {

require(ggplot2)

# reference tubes
ref.tube <- data[data$Sample < 3 |
                 data$Sample == 51, ]

p <- ggplot(ref.tube[ref.tube$Tube_treatmen != "ambient",],
            aes(x = Time, y = mV))
  p <- p + geom_line(aes(colour = Tube_treatment))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + facet_grid(Depth ~ Tube_treatment)
  p <- p + theme_bw()
  p <- p + labs(title = "Reference profile tubes timeseries for each depth",
                y = "Raw signal [mV], higher values indicate more moisture")
p
fig.ref.ts <- p

# time series per sensor, depth and ring
p <- ggplot(data[!is.na(data$Ring), ], aes(x = Time, y = mV))
  p <- p + geom_line(aes(colour = as.factor(Sample)))
  #p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + facet_grid(Depth ~ Ring)
  p <- p + theme_bw()
  p <- p + theme(legend.position = "none",
                 axis.text.x = element_text(size = rel(0.5), angle = 90))
  p <- p + labs(title = "Time series, all 16 rings, all depths",
                y = "Raw signal [mV], higher values indicate more moisture")
p
fig.sensors.ts <- p


# time series per sensor, depth and ring
# wet plots and root plots only
Plots.wet.root <- data[grep("Wet|Root", data$PlotID), ] 

p <- ggplot(Plots.wet.root[!is.na(Plots.wet.root$Ring), ],
            aes(x = Time, y = mV))
  p <- p + geom_line(aes(# colour = as.factor(Sample), 
                         colour = Cultivar, 
                         linetype = Tube_treatment))
#  p <- p + geom_point(aes(# colour = as.factor(Sample),
#                          colour = Cultivar,
#                          shape = Cultivar))
  p <- p + facet_grid(Depth ~ Ring * Tube_treatment)
  p <- p + theme_bw()
  p <- p + theme(#legend.position = "none",
                 axis.text.x = element_text(size = rel(0.5), angle = 90),
                 strip.text  = element_text(size = rel(0.6)))
  p <- p + labs(title = "Wheat rings, Scout/Yitpi - 'root' and 'wet' plots",
                y = "Raw signal [mV], higher values indicate more moisture")
p
fig.sensors.ts.wet <- p


# calibration figure
p <- ggplot(data, aes(x = mV, y = Percent_Vol))
  p <- p + geom_line()
  p <- p + geom_point()
  p <- p + theme_bw()
  p <- p + labs(title = "Raw signal vs default volumetric water content",
                x = "Raw signal [mV]",
                y = "Volumetric water content [%], using default conversion table of instrument")
p
fig.calib <- p 

# figure for last measurement date
latest.date <- max(data$Time)
latest.day  <- trunc(as.POSIXct(latest.date, format = "%Y-%M-%D"), "day")

# create a title text for the following figure
my.title <- paste("Latest profiles, measured on", latest.day, sep = " ")

# create a filename
my.filename <- paste("Soil_moisture_profiles", latest.day, sep = "_")
my.filename <- paste(my.filename, "pdf", sep = ".")

# select data from last day only
data.latest <- data[trunc(data$Time, "day") == latest.day, ]

p <- ggplot(data.latest[!is.na(data.latest$Ring), ], aes(x = mV, y = Depth))
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + scale_y_reverse()
  p <- p + facet_wrap( ~ Ring)
  p <- p + theme_bw()
  p <- p + theme(legend.position = "none")
  p <- p + labs(title = my.title,
                y = "Depth [mm]",
                x = "Raw signal [mV], higher value indicates more moisture")
p
fig.mV <- p

p <- ggplot(data.latest[!is.na(data.latest$Ring), ], aes(x = Percent_Vol, y = Depth))
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + scale_y_reverse()
  p <- p + facet_wrap( ~ Ring)
  p <- p + theme_bw()
  p <- p + theme(legend.position = "none")
  p <- p + labs(title = my.title,
                y = "Depth [mm]",
                x = "Volumetric water content [%]")
p
fig.percentVol <- p

# averaging plot
data$Day <- format(data$Time, "%Y-%m-%d")
data$Day <- as.Date(data$Day)
data$Week <- as.numeric(format(data$Time, "%W"))
data$Weekname <- paste("Week", data$Week, sep = " ")


# key harvest dates
DC30.date <- as.Date("2014-08-13")
DC65.date <- as.Date("2014-10-07")
DC90.date <- as.Date("2014-11-19")

Key.dates <- c(DC30.date, DC65.date, DC90.date)

#figures
p <- ggplot(data[data$Crop == "Wheat" &
                   !is.na(data$Crop), ], 
            aes(x = Day, y = mV))
  p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_sdl", mult = 1, geom = "linerange", 
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = FALSE)
  p <- p + facet_grid(Depth ~ Cultivar)
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                title = "Wheat, loess-smoothed data (error bars are sd)",
                x = "Date",
                y = "Soil moisture expressed as raw milliVolts, default calibration [mV]")
p
fig.smooth.wheat <- p

# lentil smoothed plot      
p <- ggplot(data[data$Crop == "Lentil" &
                 !is.na(data$Crop), ], 
            aes(x = Day, y = mV))
  p <- p + stat_summary(aes(colour = CO2, shape = Tube_treatment, linetype = Tube_treatment), 
                        fun.data = "mean_cl_normal", geom = "linerange", mult = 1,
                        position = position_jitter(width = 0.25), alpha = 0.5)
  p <- p + geom_vline(xintercept = as.numeric(Key.dates), colour = "grey")
  p <- p + geom_smooth(aes(colour = CO2, linetype = Tube_treatment), se = TRUE)
  p <- p + facet_grid(Depth ~ Cultivar)
  p <- p + theme_bw()
  p <- p + labs(linetype = "Water supply",
                title = "Lentil, loess-smoothed data (error bars are 95% conf.int)",
                x = "Date",
                y = "Soil moisture expressed as raw milliVolts, default calibration [mV]")
p

fig.smooth.lentil <- p

# average profile for each week
p <- ggplot(data[data$Crop == "Wheat" &
                   !is.na(data$Crop), ], 
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
  p <- p + theme(#legend.position = "none",
                 axis.text.x = element_text(size = rel(0.5), angle = 90),
                 strip.text  = element_text(size = rel(0.6)))
  p <- p + labs(linetype = "Water supply",
                title = "Wheat, weekly profiles",
                x = "Depth [mm]",
                x = "Soil moisture expressed as raw milliVolts, default calibration [mV]")
p

fig.weekly.profiles.wheat <- p

# attempt to create a plot map
positions <- read.csv("HH2_Soilmoisture_tubes_lookup_table_with_coordinates.csv")

to_keep <- c("Year", "Ring", "PlotID", "Variety", "HH2_tube_number", "Position_x", "Position_y")
positions <- positions[, names(positions) %in% to_keep]

data2 <- merge(data, positions,
                 all.x = TRUE,
                 by.x = c("Year", "Ring", "PlotID", "Cultivar", "Sample"),
                 by.y = c("Year", "Ring", "PlotID", "Variety", "HH2_tube_number"))

p <- ggplot(data2[data2$Crop == "Wheat" &
                   !is.na(data2$Crop) &
                   data2$Ring >= 10 &
                   data2$Depth == 300, ],
                   aes(x = Position_x, y = Position_y, z = mV))
  p <- p + geom_tile(aes(fill = mV))
  p <- p + scale_fill_gradient(low = "red", high = "blue")
  p <- p + facet_grid(Depth ~ Weekname)
  p <- p + theme_bw()
  p <- p + labs(title = "Wheat, crude plot map of tube positions, 300 mm depth",
                x = "Dummy coordinates West - East",
                y = "Dummy coordinates South - North")
p

fig.map.wheat <- p

# average soil moisture per ring
#p <- ggplot(data[data$Crop == "Wheat" &
#                   !is.na(data$Crop), ],
#            aes(x = as.factor(Ring), y = mV))
#  p <- p + geom_boxplot()
#  p <- p + facet_grid(Depth ~ .)
#  p <- p + theme_bw()
#p

#p <- ggplot(data[data$Crop == "Wheat" &
#                   !is.na(data$Crop), ],
#                   aes(x = mV))
#  p <- p + geom_histogram()
#  p <- p + facet_grid(Depth ~ Ring * Tube_treatment)
#  p <- p + theme_bw()
#p


#library(plyr)
#my.summary <- ddply(data[data$Crop == "Wheat" &
#                   !is.na(data$Crop), ],
#                    .(Ring, Depth, Week, Tube_treatment),
#                    summarise,
#                    mean = mean(mV, na.rm = TRUE),
#                    sd   = sd(mV,   na.rm = TRUE))

#p <- ggplot(my.summary, aes(x = Ring, y = mean))
#  p <- p + geom_bar(aes(fill = as.factor(Depth)), stat = "identity", position = "dodge")
#  p <- p + facet_grid(Tube_treatment ~ Weekname)
#  p <- p + theme_bw()
#p



# create summary file
pdf(file = my.filename,
    width = 9, height = 7)
print(fig.ref.ts)
print(fig.calib)
print(fig.sensors.ts)
print(fig.sensors.ts.wet)
print(fig.mV)
print(fig.percentVol)
print(fig.smooth.wheat)
print(fig.smooth.lentil)
print(fig.weekly.profiles.wheat)
print(fig.map.wheat)
dev.off()
}
