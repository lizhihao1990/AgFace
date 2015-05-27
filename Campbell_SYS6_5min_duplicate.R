# Analysis of Campbell sensors

setwd("~/AgFace/2015/Campbell_logger/Transmissions/Sap_flow_SYS6")

library(ggplot2)
library(plyr)
library(grid)
#library(gridExtra)
library(RAtmosphere)
library(reshape2)

source("~/AgFace/R_scripts/import_Campbell.R")

df <- CampbellAllImport(log.interval = "5Min", 
                 logger.folder = "~/AgFace/2015/Campbell_logger/Transmissions/Sap_flow_SYS6")

setwd("~/AgFace/2015/Campbell_logger/Transmissions/Sap_flow_SYS6")

# limit data to April 2015 and later
my.start.date <- as.POSIXct("2015-04-01", tz = "Australia/Melbourne")

df <- df[df$TIMESTAMP >= my.start.date, ]

df.melt <- melt(df, id.vars = c("SYSTEM", "TIMESTAMP"))
#my.out <- lapply(df.melt$variable, GetSensorID)
df.melt <- df.melt[df.melt$variable != "RECORD", ]


my.names <- ddply(df.melt,
                .(SYSTEM, variable),
               function(x) GetSensorID(x$variable))
#my.names <- do.call(rbind, my.out)

df.melt.merge <- merge(df.melt, my.names,
                       by.x = c("SYSTEM", "variable"),
                       by.y = c("SYSTEM", "variable"))
df.melt.merge <- unique(df.melt.merge)
df.melt <- df.melt.merge

df.melt$variable <- NULL
df.melt$FullName <- NULL


df.2 <- dcast(df.melt,
              SYSTEM + TIMESTAMP + SensorID ~ SensorName)

# Agface field site position
# -36.751367, 142.114477

# Creswick position
# -37.423003, 143.900472
# d <- as.numeric(format(Kshapp.stop, "%j"))
Sys.setenv(TZ='Australia/Melbourne')
#suncalc(d, Lat = -37.423003, Long = 143.90, UTC = TRUE)

Agface.loc <- matrix(c(142.114477, -37.423003), nrow = 1)

my.hour <- 60 * 60

NoDaylightSaving <- function(my.time) {
  my.hour <- 60 * 60
  out.time <- my.time - my.hour
}

CalcSunriseSunset <- function(my.date, spatial.loc = Agface.loc) {
   # calculate sunrise and sunset for a location
   # requires maptools
   library(maptools)
   spatial.loc <- SpatialPoints(spatial.loc,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
   my.sunrise <- sunriset(spatial.loc, 
                          my.date, 
                          direction = "sunrise", 
                          POSIXct.out = TRUE)
   my.sunset  <- sunriset(spatial.loc, 
                          my.date, 
                          direction = "sunset",  
                          POSIXct.out = TRUE)
   # strip decimal information
   my.sunrise <- my.sunrise$time
   my.sunset  <- my.sunset$time
   
   sunrise <- my.sunrise
   sunset  <- my.sunset
   
   out <- data.frame(sunrise = sunrise,
                     sunset  = sunset)
     
   return(out)
}

# calculate the sunrise and sunset times for each day in the data frame
my.calendar.days <- format(df.melt$TIMESTAMP, "%Y-%m-%d")
my.calendar.days <- as.POSIXct(unique(my.calendar.days))

my.calendar.days <- data.frame(Date = my.calendar.days)

ephemeral.times <- ddply(my.calendar.days,
                        .(Date),
                        .progress = "text",
                        function(x) CalcSunriseSunset(x$Date, 
                                     spatial.loc = Agface.loc))

# correct for daylight saving
# not needed any more after 
#ephemeral.times$sunrise <- ephemeral.times$sunrise - my.hour
#ephemeral.times$sunset  <- ephemeral.times$sunset  - my.hour

my.time.to.plot <- 170

sap <- MyRecentPlot("Sapflow_Avg", my.time.to.plot, df.2, 
             yscale_min = 0, yscale_max = 7, 
             sensor.colour = TRUE, cartesian = FALSE)
sap

dT <- MyRecentPlot("dT_Avg", my.time.to.plot, df.2, 
             yscale_min = NA, yscale_max = NA, 
             sensor.colour = TRUE, cartesian = FALSE)
dT

Qr <- MyRecentPlot("Qr_Avg", my.time.to.plot, df.2, 
             yscale_min = 0.03, yscale_max = 0.08, 
             sensor.colour = TRUE, cartesian = FALSE)
Qr

Qf <- MyRecentPlot("Qf_Avg", my.time.to.plot, df.2, 
             yscale_min = NA, yscale_max = NA, 
             sensor.colour = TRUE, cartesian = FALSE)
Qf

Kshapp.time.to.plot <- 6
Ka <- MyRecentPlot("Kshapp_Avg", Kshapp.time.to.plot, df.2, 
             yscale_min = 0, yscale_max = 0.45, 
             sensor.colour = TRUE, cartesian = FALSE)
Ka

Ka <- MyKshPlot(df.2, ylim = c(-1, 1))
Ka

