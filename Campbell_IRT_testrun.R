# Analysis of Campbell sensors

setwd("~/AgFace/2014/Campbell_logger/Transmissions")


library(plyr)
library(ggplot2)
source("~/AgFace/R_scripts/import_Campbell.R")

df <- CampbellAllImport(log.interval = "5Min", logger.name = "SYS2")

setwd("~/AgFace/2014/Campbell_logger/Transmissions")

# my.names <- paste(my.header, my.descript, sep = "_")
#names(df) <- my.header

#names(df) <- gsub("\\.", "_", names(df))

#df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "Australia/Melbourne")

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
Creswick.loc <- matrix(c(143.899631, -37.422249), nrow = 1)
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


# IRTs that were doing something meaningful from Feb 4 on
# Horizontal 1: head wheat
# Narrow 4 bean
# Narrow 7 bean

MyRecentPlot <- function(para, hours, data, logger = NA, yscale_min = NA, yscale_max = NA) {
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

my.time.to.plot <- 1024
my.max <- 40
a <- MyRecentPlot("IR_Narrow_Avg_7_", my.time.to.plot, df, 
             logger = "SYS2",
             yscale_min = 0, yscale_max = my.max)
b <- MyRecentPlot("IR_Narrow_Avg_4_", my.time.to.plot, df, 
             logger = "SYS2",
             yscale_min = 0, yscale_max = my.max)
c <- MyRecentPlot("IR_Horz_Avg_1_", my.time.to.plot, df, 
             logger = "SYS2",
             yscale_min = 0, yscale_max = my.max)

# assemble figures 
a <- ggplotGrob(a)
b <- ggplotGrob(b)
c <- ggplotGrob(c)

library(gridExtra)
pdf(file = "IRT_glasshouse.pdf", width = 19, height = 17)
grid.draw(rbind(a, b, c, size = "first"))
#grid.arrange(a, b, c, d, e, ncol = 1)
dev.off()

p <- ggplot(df, aes(x = IR_Horz_Avg_1_, y = IR_Narrow_Avg_7_))
  p <- p + geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed")
  p <- p + geom_point(alpha = 0.01)
  p <- p + geom_smooth(method = "lm")
  p <- p + scale_x_continuous(limits = c(0, 60))
  p <- p + scale_y_continuous(limits = c(0, 60))
p

