# Analysis of Campbell sensors

setwd("~/AgFace/2014/Campbell_logger/Transmissions")


library(plyr)

#the.folder <- "/run/user/1000/gvfs/dav:host=agface.dnsdynamic.net,ssl=true,user=markus,prefix=%2Fowncloud%2Fremote.php%2Fwebdav/Shared/current_season_data/Campbell_loggers/logger_data"
#the.folder <- "~/AgFace/2014/Campbell_logger/Transmissions/2014_10_28_sapflow_install/logger_data"
the.folder <- "~/AgFace/2014/Campbell_logger/logger_data"
setwd(the.folder)

my.header   <- read.csv("SYS1_5Min.dat", skip = 1)
my.header   <- names(my.header)
my.descript <- read.csv("SYS1_5Min.dat", skip = 3)
my.descript <- names(my.descript)
df <- read.csv("SYS1_5Min.dat", skip = 4, na.strings = "NAN")

setwd("~/AgFace/2014/Campbell_logger/Transmissions")


# my.names <- paste(my.header, my.descript, sep = "_")
names(df) <- my.header

names(df) <- gsub("\\.", "_", names(df))

df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "Australia/Melbourne")

library(reshape2)
df.melt <- melt(df, id.vars = "TIMESTAMP")

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


library(ggplot2)

my.start.time <- as.POSIXct("2015-01-15 02:30:00",
                            origin = "1970-01-01",
                            tz = "Australia/Melbourne")
my.end.time   <- as.POSIXct("2015-01-16 18:00:00",
                            origin = "1970-01-01",
                            tz = "Australia/Melbourne")

p <- ggplot(df.melt[df.melt$variable == "SYS1_Qf_SGA2_2_Avg" |
                    df.melt$variable == "SYS1_Sapflow_SGA2_2_Avg" | 
                    df.melt$variable == "SYS1_dT_SGA2_2_Avg", ],
            aes(x = TIMESTAMP, y = value))
   p <- p + geom_line(SYS1_Qf_SGA2_2_Avg / (SYS1_dT_SGA2_2_Avg) * 4.186)
   p <- p + geom_line()
   p <- p + coord_cartesian(xlim = c(my.start.time, my.end.time))#,
                            #ylim = c(0.0350, 0.04))
   p <- p + facet_grid(variable ~ ., scales = "free_y")
   p <- p + geom_vline(aes(xintercept = as.numeric(sunrise)), data = ephemeral.times, colour = "orange")
   p <- p + geom_vline(aes(xintercept = as.numeric(sunset)),  data = ephemeral.times, colour = "light blue")
   p <- p + theme_bw()
p

# unfiltered sapflow
p <- ggplot(df, aes(x = TIMESTAMP))
  p <- p + annotate("rect", 
          xmin = ephemeral.times$sunset[1:length(ephemeral.times$sunrise) - 1], 
          xmax = ephemeral.times$sunrise[2:length(ephemeral.times$sunrise)], 
          ymin = 0, ymax = 5,
          fill = "grey", alpha = 0.1)
  p <- p + geom_line(aes(y = SYS1_Qf_SGA2_2_Avg*50), colour = "blue")
  p <- p + geom_line(aes(y = SYS1_dT_SGA2_2_Avg), colour = "red")
  p <- p + geom_line(aes(y = SYS1_Qf_SGA2_2_Avg / (SYS1_dT_SGA2_2_Avg) * 4.186))
  p <- p + geom_line(aes(y = SYS1_Qr_SGA2_2_Avg * 1000), colour = "orange")
  p <- p + scale_y_continuous(limits = c(0, 5))
  p <- p + coord_cartesian(xlim = c(my.start.time, my.end.time))
#  p <- p + geom_vline(aes(xintercept = as.numeric(sunrise)), 
#                      data = ephemeral.times, colour = "orange")
#   p <- p + geom_vline(aes(xintercept = as.numeric(sunset)),  
#                      data = ephemeral.times, colour = "light blue")
   p <- p + theme_bw()
p

p <- ggplot(df, aes(x = TIMESTAMP ))
  p <- p + geom_line(aes(y = SYS1_Qr_SGA2_1_Avg), colour = "red")
  p <- p + geom_line(aes(y = SYS1_Qr_SGA2_2_Avg), colour = "blue")
  p <- p + geom_line(aes(y = SYS1_Qf_SGA2_2_Avg), colour = "orange")
  p <- p + geom_line(aes(y = SYS1_Pin_SGA2_2_Avg), colour = "yellow")
  p <- p + annotate("rect", 
          xmin = ephemeral.times$sunset[1:length(ephemeral.times$sunrise) - 1], 
          xmax = ephemeral.times$sunrise[2:length(ephemeral.times$sunrise)], 
          ymin = 0, ymax = 0.05,
          fill = "grey", alpha = 0.1)
  p <- p + coord_cartesian(xlim = c(my.start.time, my.end.time))
  p <- p + scale_y_continuous(limits = c(0, 0.05))
  p <- p + theme_bw()
p


# KShapp plot
Kshapp.start = as.POSIXct("2015-01-19 00:00:00",
                            origin = "1970-01-01",
                            tz = "Australia/Melbourne")
Kshapp.stop  = as.POSIXct("2015-01-19 12:00:00",
                            origin = "1970-01-01",
                            tz = "Australia/Melbourne")

Kshapp.cut <- df.melt[df.melt$TIMESTAMP > Kshapp.start &
                      df.melt$TIMESTAMP < Kshapp.stop, ]

Kshapp.cut[Kshapp.cut$variable == "SYS1_Kshapp_SGA2_1_Avg" |
           Kshapp.cut$variable == "SYS1_Kshapp_SGA2_2_Avg", ]

Predawn.avg <- ddply(Kshapp.cut,
                     .(variable),
                     summarise,
                     mean = mean(value, na.rm = TRUE),
                     sd   = sd(value,   na.rm = TRUE))

my.Predawn.Kshapp <- grep("Kshapp", Predawn.avg$variable)

Predawn.avg[my.Predawn.Kshapp, ]

p <- ggplot(Kshapp.cut[(Kshapp.cut$variable == "SYS1_Kshapp_SGA2_1_Avg" |
                       Kshapp.cut$variable == "SYS1_Kshapp_SGA2_2_Avg" |
                       Kshapp.cut$variable == "SYS1_RawCh_SGA2_1_Avg"  |
                       Kshapp.cut$variable == "SYS1_RawCh_SGA2_2_Avg") &
                       (as.numeric(format(Kshapp.cut$TIMESTAMP, "%H")) >= 5 &
                       as.numeric(format(Kshapp.cut$TIMESTAMP, "%H")) <= 6), ],
            aes(x = TIMESTAMP, y = value))
   p <- p + geom_line(aes(colour = variable)) + geom_point(aes(colour = variable))
   p <- p + facet_grid(variable ~ ., scales = "free_y")
#   p <- p + coord_cartesian(xlim = c(Kshapp.start, Kshapp.stop),
#                            ylim = c(-5, 5))
p

MyPlot <- function(data) {
     my.label <- unique(data$variable)
     p <- ggplot(data, aes(x = TIMESTAMP, y = value))
     p <- p + geom_line()
     p <- p + labs(y = my.label)
     return(p)
}
library(plyr)
plot.list <- dlply(df.melt,
                   .(variable),
                   function(x) MyPlot(x))
pdf(file = "Plots.pdf")
print(plot.list)
dev.off()

# sapflow calculations
# list of sapflow parameters
sapflow.names <- c("TIME", "_Raw", "Sapflow", "Kshapp", "dT", "Qf", "Qr", "Pin", "Qv")

# list of data frame names hat match the sapflow parameters
sapflow.names.index <- unlist(lapply(sapflow.names, function(x) grep(x, names(df))))

# create a data frame for sap flow only
sap <- df[, sapflow.names.index]

#	--	2mm stem gauges	--
# Const HeaterResistor_SGA2_1 = 90.8
# Const HeaterResistor_SGA2_2 = 106.3

Heat.resist.ohm <- 90.8

# Cross sectional area of the stem (in cm2)
#' 0.12566cm2 is entered by default for the 2mm stem gauges
#' These values need to be updated regularly (installation and maintenance of the stem gauges)
#'	--	2mm stem gauges	--
#Const CrossSectArea_SGA2_1 = 0.12566
#Const CrossSectArea_SGA2_2 = 0.12566

dia <- 0.3 #mm
area <- pi * (dia/2)^2
area

#' Thermal conductivity (W/m*K) of the studied plants
#' This number is generally 0.54 W/m*K for herbaceous plants, 0.42 W/m*K for woody plants
#' and 0.28 W/m*K for hollow stems.
#' Value entered by default 0.54 W/m*K - this value will need to be adjusted
#'	--	2mm stem gauges	--
#Const ThermalCond_SGA2_1 = 0.28
#Const ThermalCond_SGA2_2 = 0.28

#' Thermocouple gap (cm) - information given by the manuals
#'	--	2mm stem gauges	--
#Const TCGap_SGA2 = 0.1 '(1 cm) as per dynamax manual

# Qr = Ch * Ksh
# Ch is from SYS1_RawCh_SGA2_1
# Ksh is set to 0.125

Ksh <- 0.26 # for system 2, estimated from Kshapp Nov 11 to 14

my.Gap  <- 0 # cm
my.cond <- 0.28 # (W/mK)
my.area <- 0.070 # m

# calculate Qv
# Qv = ((Bh - Ah) / (4 * my.Gap)) * my.area * my.cond
sap$myQv <- ((sap$SYS1_RawBh_SGA2_2_Avg - sap$SYS1_RawAh_SGA2_2_Avg) / ( 4 * my.Gap)) * my.area * my.cond
sap$myQv <- (sap$SYS1_RawBh_SGA2_2_Avg - sap$SYS1_RawAh_SGA2_2_Avg) / my.area * my.cond
plot(myQv ~ SYS1_Qv_SGA2_2_Avg, data = sap)

# calculate Qr
sap$myQr <- sap$SYS1_RawCh_SGA2_2_Avg * Ksh
plot(myQr ~ SYS1_Qr_SGA2_2_Avg, data = sap)

# calculate Qf
# Qf = Pin - Qv - Qr
sap$myQf <- sap$SYS1_Pin_SGA2_2_Avg - sap$SYS1_Qv_SGA2_2_Avg - sap$myQr
plot(myQf ~ SYS1_Qf_SGA2_2_Avg, data = sap)

# my dT
#DT <- ((Ah + Bh) / 2) * 25
sap$myDT <- ((sap$SYS1_RawAh_SGA2_2_Avg + sap$SYS1_RawBh_SGA2_2_Avg) / 2) * 25
plot(myDT ~ SYS1_dT_SGA2_2_Avg, data = sap)

# calculate sapflow
sap$mysapflow <- sap$myQf / (sap$SYS1_dT_SGA2_2_Avg * 4.186)

plot(mysapflow ~ SYS1_Sapflow_SGA2_2_Avg, data = sap)
plot(sap$SYS1_dT_SGA2_2_Avg ~ TIMESTAMP, data = sap)
p <- ggplot(sap, aes(x = TIMESTAMP, ))
  #p <- p + geom_line(aes(y = SYS1_dT_SGA2_1_Avg), colour = "red")
  p <- p + geom_line(aes(y = SYS1_dT_SGA2_2_Avg), colour = "blue")
#  p <- p + geom_line(aes(y = SYS1_Pin_SGA2_2_Avg), colour = "purple")
#  p <- p + geom_line(aes(y = mysapflow), colour = "brown")
  p <- p + geom_line(aes(y = myQf), colour = "orange")
#  p <- p + geom_line(aes(y = myQr), colour = "yellow")
#  p <- p + geom_line(aes(y = SYS1_Pin_SGA2_1_Avg), colour = "green")
#  p <- p + geom_line(aes(y = myQv), colour = "black")
#  p <- p + coord_cartesian(ylim = c(-0.025, 0.0))
#  p <- p + scale_y_continuous(limits=c(-0.025, 0))
p

plot(mysapflow ~ TIMESTAMP, data = sap[sap$TIMESTAMP > Kshapp.start &
                                       sap$TIMESTAMP < Kshapp.stop, ],
     ylim = c(-0.2, 0.2))
write.table(sap[sap$TIMESTAMP > Kshapp.start &
                sap$TIMESTAMP < Kshapp.stop, ],
            file = "sapflow.csv", row.names = FALSE, sep = ",")
            
# Grapical selection
#iplot
library(iplots)
#iplot(df$TIMESTAMP, df$SYS1_dT_SGA2_2_Avg)
#iset.selected()
#my.selected <- iset.selected()
#df[my.selected, ]


MyRecentPlot <- function(para, hours, data, yscale_min = NA, yscale_max = NA) {
    # function to plot a specific paramter for the last x hours
    
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
      p <- p + geom_line()
      p <- p + coord_cartesian(xlim = c(start.time, last.time),
                               ylim = c(my.min, my.max))
      p <- p + labs(y = para)
      p <- p + theme_bw()
    return(p)
}

MyRecentPlot("SYS1_Sapflow_SGA2_1_Avg", 128, df, yscale_min = -5, yscale_max = 5)
#dev.new()
a <- MyRecentPlot("SYS1_Sapflow_SGA2_2_Avg", 128, df, yscale_min = -2.5, yscale_max = 2.5)
# dev.new()
b <- MyRecentPlot("SYS1_dT_SGA2_2_Avg", 128, df)
# dev.new()
c <- MyRecentPlot("SYS1_Qf_SGA2_2_Avg", 128, df, yscale_min = NA, yscale_max = NA)
# dev.new()
d <- MyRecentPlot("SYS1_Qr_SGA2_2_Avg", 128, df, yscale_min = 0.049, yscale_max = 0.059)
# dev.new()
#e <- MyRecentPlot("SYS1_Kshapp_SGA2_1_Avg", 128, df, yscale_min = 0, yscale_max = 0.05)
e <- MyRecentPlot("SYS1_RawCh_SGA2_2_Avg", 128, df, yscale_min = NA, yscale_max = NA)
# dev.new()
f <- MyRecentPlot("SYS1_Pin_SGA2_1_Avg", 128, df, yscale_min = 0.04, yscale_max = 0.06)

a <- ggplotGrob(a)
b <- ggplotGrob(b)
c <- ggplotGrob(c)
d <- ggplotGrob(d)
e <- ggplotGrob(e)
f <- ggplotGrob(f)

library(gridExtra)
pdf(file = "Sapflow_example.pdf", width = 19, height = 17)
grid.draw(rbind(c, d, e, b, a, size = "first"))
#grid.arrange(a, b, c, d, e, ncol = 1)
dev.off()
