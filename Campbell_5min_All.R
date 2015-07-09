# Analysis of Campbell sensors

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

library(ggplot2)
library(plyr)
library(grid)
#library(gridExtra)
# library(RAtmosphere)
library(reshape2)

source("~/AgFace/R_scripts/import_Campbell.R")

df <- CampbellAllImport(log.interval = "5Min")

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

# limit data to April 2015 and later
my.start.date <- as.POSIXct("2015-04-01", tz = "Australia/Melbourne")

df <- df[df$TIMESTAMP >= my.start.date, ]

write.csv(df, file = "df.csv", row.names = F)

df.melt <- melt(df, id.vars = c("SYSTEM", "TIMESTAMP"))
#my.out <- lapply(df.melt$variable, GetSensorID)

df.melt <- df.melt[df.melt$variable != "RECORD", ]
#df.melt <- df.melt[df.melt$variable != "Batt_volt_Min", ]

my.names <- ddply(df.melt,
                .(SYSTEM, variable),
               function(x) GetSensorID(x$variable))
my.names$SensorID <- as.factor(as.character(my.names$SensorID))
#my.names <- do.call(rbind, my.out)

#df.melt.merge <- merge(df.melt, my.names,
#                       by.x = c("SYSTEM", "variable"),
#                       by.y = c("SYSTEM", "variable"))
df.melt.join <- join(df.melt, my.names,
                     by = c("SYSTEM", "variable"),
                     match = "first")
                     
#df.melt.merge <- unique(df.melt.merge)
df.melt <- unique(df.melt.join[df.melt.join$SYSTEM == "SYS7", ])
#dim(df.melt)
#dim(unique(df.melt))
# df.melt <- df.melt.merge

df.melt$variable <- NULL
df.melt$FullName <- NULL


df.2 <- dcast(df.melt,
              SYSTEM + TIMESTAMP + SensorID ~ SensorName)
write.csv(df.2, file = "df2.csv", row.names = F)
write.csv(df.melt, file = "melt.csv", row.names = F)

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

MyPlot <- function(data) {
     my.label <- unique(data$SensorName)
     p <- ggplot(data, aes(x = TIMESTAMP, y = value))
     p <- p + geom_line()
     p <- p + labs(y = my.label)
     p <- p + facet_grid(SYSTEM ~ .)
     return(p)
}
library(plyr)
plot.list <- dlply(df.melt,
                   .(SYSTEM, SensorName),
                   function(x) MyPlot(x))

#pdf(file = "Plots.pdf")
#print(plot.list)
#dev.off()


#df.2 <- df.2[df.2$SYSTEM != "SYS3", ]
df.2 <- df.2

my.time.to.plot <- 36

MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.2, 
             yscale_min = 0, yscale_max = 25,
             sensor.colour = TRUE, cartesian = TRUE)

sap <- MyRecentPlot("Sapflow_Avg", my.time.to.plot, df.2, 
             yscale_min = -1, yscale_max = 20, 
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
             
#MyRecentPlot("Sapflow_SGA2_2_Avg", my.time.to.plot, df, yscale_min = -1, yscale_max = 25)
#MyRecentPlot("Kshapp_SGA2_1_Avg", 36, df, yscale_min = 0.2, yscale_max = 0.45)
#MyRecentPlot("Kshapp_SGA2_2_Avg", 36, df, yscale_min = 0.2, yscale_max = 0.45)


a <- MyRecentPlot("Sapflow_SGA2_1_Avg", my.time.to.plot, df, yscale_min = 0, yscale_max = 45)

b <- MyRecentPlot("dT_SGA2_1_Avg", my.time.to.plot, df)

c <- MyRecentPlot("Qf_SGA2_1_Avg", my.time.to.plot, df, yscale_min = NA, yscale_max = NA)

d <- MyRecentPlot("Qr_SGA2_1_Avg", my.time.to.plot, df, yscale_min = NA, yscale_max = NA)


e <- MyRecentPlot("RawCh_SGA2_1_Avg", my.time.to.plot, df, yscale_min = NA, yscale_max = NA)
# dev.new()
f <- MyRecentPlot("Pin_SGA2_1_Avg", my.time.to.plot, df, yscale_min = 0.04, yscale_max = 0.06)

a <- ggplotGrob(a)
b <- ggplotGrob(b)
c <- ggplotGrob(c)
d <- ggplotGrob(d)
e <- ggplotGrob(e)
f <- ggplotGrob(f)


pdf(file = "Sapflow_example_All.pdf", width = 19, height = 17)
grid.draw(rbind(c, d, e, b, a, size = "first"))
#grid.arrange(a, b, c, d, e, ncol = 1)
dev.off()

the.date <- as.POSIXct("2015-05-09 00:00:00", tz = "Australia/Melbourne")

the.end <- df.2[df.2$TIMESTAMP < the.date, ]

p <- ggplot(df.2[df.2$TIMESTAMP > the.date, ],
            aes(x = TIMESTAMP, y = Sapflow_Avg))
  p <- p + geom_point()
  p <- p + facet_grid(SYSTEM ~ .)
p

MyRecentPlot("Batt_volt_Min", my.time.to.plot, df.2, 
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
#ggsave(file = "Battery_Voltage.pdf", width = 9, height = 7)
