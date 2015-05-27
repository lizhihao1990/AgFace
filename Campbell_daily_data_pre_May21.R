# Analysis of Campbell sensors

setwd("~/AgFace/2015/Campbell_logger/Transmissions")


library(plyr)
source("~/AgFace/R_scripts/import_Campbell.R")

df <- CampbellAllImport(log.interval = "Daily",
                        logger.folder = "~/AgFace/2015/Campbell_logger/logger_data_pre-May21/logger_data_2015-05-21_17_15_before_5min_table")

##the.folder <- "/run/user/1000/gvfs/dav:host=agface.dnsdynamic.net,ssl=true,user=markus,prefix=%2Fowncloud%2Fremote.php%2Fwebdav/Shared/current_season_data/Campbell_loggers/logger_data"
##the.folder <- "~/AgFace/2014/Campbell_logger/Transmissions/2014_10_28_sapflow_install/logger_data"
#the.folder <- "~/AgFace/2014/Campbell_logger/logger_data"
#setwd(the.folder)

#my.header   <- read.csv("SYS8_5Min.dat", skip = 1)
#my.header   <- names(my.header)
#my.descript <- read.csv("SYS8_5Min.dat", skip = 3)
#my.descript <- names(my.descript)
#df <- read.csv("SYS8_5Min.dat", skip = 4, na.strings = "NAN")

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

library(reshape2)
df.melt <- melt(df, id.vars = c("SYSTEM", "TIMESTAMP"))

library(RAtmosphere)

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
                        function(x) CalcSunriseSunset(x$Date, spatial.loc = Agface.loc))

# correct for daylight saving
ephemeral.times$sunrise <- ephemeral.times$sunrise - my.hour
ephemeral.times$sunset  <- ephemeral.times$sunset  - my.hour



MyRecentPlot <- function(para, hours, data, logger = NA, yscale_min = NA, yscale_max = NA) {
    require(ggplot2)
    # function to plot a specific parameter for the last x hours
    
    # determine if all logger data should be used or only one specific logger
    if (is.na(logger) == TRUE){
        # do nothing
    } else {
       # subset data to use the named system only
       print(logger)
       data <- data[data$SYSTEM == logger, ]
    }
    
    # determine time frame to display
    my.time    <- hours * 60 * 60 # conversion from hours to seconds
    last.time  <- max(data$TIMESTAMP, na.rm = TRUE)
    start.time <- last.time - my.time
    
    my.data <- data[data$TIMESTAMP > start.time &
                    data$TIMESTAMP <= last.time, ]
    
    my.para <- which(names(data) == para)
    
    # get rid of Infinite data that mess up the determination of the scales
    my.infinites <- is.infinite(my.data[, my.para])
    my.data.clean <- my.data[which(my.infinites == FALSE), ]
    
    # calculate min and max values for the scaling
    if (is.na(yscale_min) == TRUE) {
	    my.max <- max(my.data.clean[, my.para], na.rm = TRUE)
	    my.min <- min(my.data.clean[, my.para], na.rm = TRUE)
	    } else {
	    my.max <- yscale_max
	    my.min <- yscale_min
	    }
    
    # put the figure together
    p <- ggplot(data, aes_string(x = "TIMESTAMP", y = para))
      p <- p + annotate("rect", 
          xmin = ephemeral.times$sunset[1:length(ephemeral.times$sunrise) - 1], 
          xmax = ephemeral.times$sunrise[2:length(ephemeral.times$sunrise)], 
          ymin = my.min, ymax = my.max,
          fill = "grey", alpha = 0.1)
      p <- p + geom_line()
      p <- p + coord_cartesian(xlim = c(start.time, last.time),
                               ylim = c(my.min, my.max))
      p <- p + labs(y = para)
      p <- p + facet_grid(SYSTEM ~ ., scales = "free_y")
      p <- p + theme_bw()
    return(p)
}

my.time.to.plot <- 230

MyRecentPlot("Batt_volt_Min", my.time.to.plot, df, yscale_min = NA, yscale_max = NA)
#ggsave(file = "Min_battery_May12_to_May21_SYS4_out_of_juice.pdf", width = 9, height = 7)

