# using the CampbellLogger library to import files

library(CampbellLogger)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

df <- CampbellAllImport(log.interval = "Hourly")

df.cast <- CampbellCast(df)

ephemeral.times <- CampbellSunriseSunset(df)

my.time.to.plot <- 2000
MyRecentPlot("Soil_Avg", my.time.to.plot, df.cast,
             yscale_min = 0, yscale_max = 0.5,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("Batt_volt_Min", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = FALSE, cartesian = TRUE)

df.cast$Date <- as.Date(df.cast$TIMESTAMP, tz = "Australia/Melbourne")
df.cast$Hour <- format(df.cast$TIMESTAMP, "%H")

# SYS1 TDR4 knocked or moved on June 25, 9 am?
# SYS4 TDR4 knocked on June 30, 11 am?
my.logger1.time <- 166
MyRecentPlot("Soil_Avg", my.logger1.time, df.cast, #logger = "SYS1",
             yscale_min = 0, yscale_max = 0.5,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("Batt_volt_Min", my.logger1.time, df.cast, #logger = "SYS3",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = FALSE, cartesian = TRUE)
#library(ggplot2)
#p <- ggplot(df[df$Batt_volt_Min < 12 & df$SYSTEM == "SYS3", ], aes(x = Batt_volt_Min, y = Soil_Avg_3_))
#  p <- p + geom_point()
#  p <- p + facet_grid(SYSTEM ~ .)
#  p <- p + theme_bw()
#p

df.cast.clean <- df.cast
df.cast.clean$Soil_Avg[df.cast.clean$SYSTEM == "SYS1" &
                       (df.cast.clean$Soil_Avg < 0.1 | 
                       df.cast.clean$Soil_Avg > 1)] <- NA

library(ggplot2)

# translate SYSTEMs to Rings
SYS.numbers <- c(1:8)
SYS.numbers <- paste("SYS", SYS.numbers, sep = "")
Ring.numbers <- c(3, 4, 6, 7, 10, 11, 15, 16)
SYS.Ring <- data.frame(SYSTEM = as.factor(SYS.numbers),
                       Ring = as.factor(Ring.numbers))

df.cast.clean <- merge(df.cast.clean, SYS.Ring)

df.cast.clean <- df.cast.clean[df.cast.clean$TIMESTAMP > as.POSIXct("2015-06-11"), ]
df.cast.clean$Soil_Avg[df.cast.clean$SYSTEM == "SYS3" &
                       df.cast.clean$SensorID == "3"] <- NA
source("~/AgFace/R_scripts/MyThemes.R")
my.sun <- CampbellSunriseSunset(df[df$TIMESTAMP > as.POSIXct("2015-06-10"), ])

my.sunset <- my.sun$sunset[1:length(my.sun$sunrise) - 1]
my.sunrise <- my.sun$sunrise[2:length(my.sun$sunrise)]
#        p <- p + ggplot2::annotate("rect", xmin = my.sunset, 
#            xmax = my.sunrise, ymin = my.min, ymax = my.max, 
#            fill = "grey", alpha = 0.1)

p <- ggplot(df.cast.clean, 
             aes(x = TIMESTAMP, y = Soil_Avg * 100))
 # p <- p + geom_point()
  p <- p + geom_hline(yintercept = seq(20, 40, by = 5), colour = "light grey", alpha = 0.3)
  p <- p + stat_summary(aes(fill = Ring), fun.data = "mean_cl_normal", mult = 1, geom = "ribbon", alpha = 0.1)
  p <- p + stat_summary(aes(colour = Ring), fun.data = "mean_sdl", mult = 1, geom = "line")
  p <- p + annotate("rect", xmin = my.sunset, 
            xmax = my.sunrise, ymin = 20, ymax = 45, 
            fill = "grey", alpha = 0.1)
  p <- p + scale_colour_brewer(palette = "Set2")
  p <- p + scale_fill_brewer(palette = "Set2")
  p <- p + labs(y = "Mean soil moisture from TDRs, (%)")
  p <- p + theme_my
  p <- p + coord_cartesian(ylim = c(20, 45))
p

my.width = 32
my.height = 18

pdf(file = "TDR_figures.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(p)
dev.off()
