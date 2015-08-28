# using the CampbellLogger library to import files

library(CampbellLogger)
library(doMC)
#library(doParallel)

# set up cluster for parallel computing
registerDoMC(4)

#cl <- makeCluster(4)
#registerDoParallel(cl)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

#individual import for trouble shooting
#my.folder <- "/home/loewi/AgFace/2015/Campbell_logger/logger_data"
#my.file <- "SYS8_5Min.dat"
#my.import <- paste(my.folder, my.file, sep = "/")

#x <- CampbellFileImport(my.import)
#my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#x <- read.csv(my.import, skip = 4, header = F)
#names(x) <- names(my.header)
#x$Date <- as.POSIXct(x$TIMESTAMP, tz = "Australia/Melbourne")

# end of individual import

df <- CampbellAllImport(log.interval = "5Min", use.parallel = TRUE)

df.cast <- CampbellCast(df, use.parallel = TRUE)

ephemeral.times <- CampbellSunriseSunset(df)

my.time.to.plot <- 1000
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = -6.5, yscale_max = 36,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast,
             yscale_min = -20, yscale_max = 36,
             sensor.colour = TRUE, cartesian =TRUE)


