# using the CampbellLogger library to import files

library(CampbellLogger)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

df <- CampbellAllImport(log.interval = "Hourly", #logger.name = "SYS1",
                        time.zone = "GMT",
                        logger.folder = "~/AgFace/2015/Campbell_logger/logger_data_in_season")
#individual import for trouble shooting

#my.folder <- "/home/loewi/AgFace/2015/Campbell_logger/logger_data"
#my.file <- "SYS2_Hourly.dat"
#my.import <- paste(my.folder, my.file, sep = "/")

#x <- CampbellFileImport(my.import)
#my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#x <- read.csv(my.import, skip = 4, header = F)
#names(x) <- names(my.header)
#x$Date <- as.POSIXct(x$TIMESTAMP, tz = "Australia/Melbourne")

# export data
write.csv(df,
          file = "Hourly_Campbell_logger_data.csv",
          row.names = FALSE, na = "")

df.cast <- CampbellCast(df)

ephemeral.times <- CampbellSunriseSunset(df)

my.time.to.plot <- 100
MyRecentPlot("Soil_Avg", my.time.to.plot, df.cast,
             yscale_min = 0, yscale_max = 0.5,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("PAR_Avg", my.time.to.plot/2, df.cast,
             yscale_min = 0, yscale_max = 10,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("PAR_Avg", my.time.to.plot, df.cast, logger = "SYS2", 
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("Batt_volt_Min", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = FALSE, cartesian = TRUE)

df.cast$Date <- as.Date(df.cast$TIMESTAMP)#, tz = "GMT")
df.cast$Hour <- format(df.cast$TIMESTAMP, "%H")
ephemeral.times$sunrise <- ephemeral.times$sunrise + 60*60*10
ephemeral.times$sunset <- ephemeral.times$sunset + 60*60*10

# SYS1 TDR4 knocked or moved on June 25, 9 am?
# SYS4 TDR4 knocked on June 30, 11 am?
my.logger1.time <- 288
MyRecentPlot("Soil_Avg", my.logger1.time, df.cast, #logger = "SYS1",
             yscale_min = 0, yscale_max = 0.45,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("Batt_volt_Min", my.logger1.time, df.cast, #logger = "SYS3",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = FALSE, cartesian = TRUE)
my.temp.time <- 96
MyRecentPlot("Hum_Avg", my.temp.time, df.cast, #logger = "SYS3",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("Temp_Avg", my.temp.time, df.cast, #logger = "SYS3",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
             
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
wet.SensorID <- c("1", "2")
dry.SensorID <- c("3", "4")
SYS.Ring <- data.frame(SYSTEM = as.factor(SYS.numbers),
                       Ring = as.factor(Ring.numbers))

SYS.numbers <- c(1:8)
SYS.numbers <- paste("SYS", SYS.numbers, sep = "")
Ring.numbers <- c(3, 4, 6, 7, 10, 11, 15, 16)
CO2.treat  <- c("aCO2", "eCO2", "eCO2", "aCO2", "eCO2", "aCO2", "aCO2", "eCO2")
SYS.Ring <- data.frame(SYSTEM = as.factor(SYS.numbers),
                       Ring = as.factor(Ring.numbers),
                       CO2_treatment = as.factor(CO2.treat))
df.cast.clean <- merge(df.cast.clean, SYS.Ring)

df.cast.clean$Irrigation <- NA
df.cast.clean$Irrigation[df.cast.clean$SensorID %in% wet.SensorID] <- "wet"
df.cast.clean$Irrigation[df.cast.clean$SensorID %in% dry.SensorID] <- "rainfed"
df.cast.clean$Irrigation <- as.factor(df.cast.clean$Irrigation)

df.cast.clean <- df.cast.clean[df.cast.clean$TIMESTAMP > as.POSIXct("2015-06-11"), ]
df.cast.clean$Soil_Avg[df.cast.clean$SYSTEM == "SYS3" &
                       df.cast.clean$SensorID == "3"] <- NA

# import TDR treatment information
TDR.treat <- read.csv("~/AgFace/2015/TDR/TDRs_in_TraitFace.csv")
TDR.treat$Ring <- as.factor(as.character(TDR.treat$Ring))
TDR.treat$CO2_treatment <- gsub("Ambient", "aCO2", TDR.treat$CO2_treatment)
TDR.treat$CO2_treatment <- gsub("Elevated", "eCO2", TDR.treat$CO2_treatment)
TDR.treat$CO2_treatment <- as.factor(TDR.treat$CO2_treatment)
TDR.treat$System <- NULL
TDR.treat.TDR_no <- as.factor(as.character(TDR.treat$TDR_no))
names(TDR.treat) <- gsub("TDR_no", "SensorID", names(TDR.treat))

df.cast.clean <- merge(df.cast.clean, TDR.treat)

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
  p <- p + geom_hline(yintercept = seq(10, 40, by = 5), colour = "light grey", alpha = 0.3)
  p <- p + stat_summary(aes(fill = Ring), fun.data = "mean_cl_normal", mult = 1, geom = "ribbon", alpha = 0.1)
  p <- p + stat_summary(aes(colour = Ring, linetype = Irrigation), 
                        fun.data = "mean_sdl", mult = 1, geom = "line")
  p <- p + annotate("rect", xmin = my.sunset, 
            xmax = my.sunrise, ymin = 0, ymax = 45, 
            fill = "grey", alpha = 0.1)
  p <- p + scale_colour_brewer(palette = "Set2")
  p <- p + scale_fill_brewer(palette = "Set2")
  p <- p + labs(y = "Mean soil moisture from TDRs, (%)")
  p <- p + theme_my
  p <- p + coord_cartesian(ylim = c(0, 45))
p

p <- ggplot(df.cast.clean, 
             aes(x = TIMESTAMP, y = Soil_Avg * 100))
 # p <- p + geom_point()
  p <- p + geom_hline(yintercept = seq(5, 40, by = 5), colour = "light grey", alpha = 0.3)
  p <- p + stat_summary(aes(fill = CO2_treatment), fun.data = "mean_cl_normal", mult = 1, geom = "ribbon", alpha = 0.1)
  p <- p + stat_summary(aes(colour = CO2_treatment, linetype = Irrigation), 
                        fun.data = "mean_sdl", mult = 1, geom = "line")
#  p <- p + annotate("rect", xmin = my.sunset, 
#            xmax = my.sunrise, ymin = 0, ymax = 45, 
#            fill = "grey", alpha = 0.1)
  p <- p + scale_colour_brewer(palette = "Set2")
  p <- p + scale_fill_brewer(palette = "Set2")
  p <- p + labs(y = "Mean soil moisture from TDRs, (%)")
  p <- p + facet_grid(Cultivar ~ Irrigation)
  p <- p + theme_my
  #p <- p + coord_cartesian(ylim = c(0, 45))
  p <- p + scale_y_continuous(limits = c(0, 45))
p


my.width = 32
my.height = 18

pdf(file = "TDR_figures.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(p)
dev.off()

PlotAll <- function(data, time, sensor) {
      require(ggplot2)
      MyRecentPlot(sensor, time, data,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian =TRUE)
}

para.names <- names(df.cast)[4:8]

fig.out <- lapply(para.names, function(x) PlotAll(df.cast, 4200, x))

pdf(file = "Hourly_figures.pdf")
print(fig.out)
dev.off()
