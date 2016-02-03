# using the CampbellLogger library to import files

library(CampbellLogger)
#library(doParallel)
library(doMC)
registerDoMC(4)
# set up cluster for parallel computing
#cl <- makeCluster(4)
#registerDoParallel(cl)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")
#system.time(
#df.a <- CampbellAllImport(log.interval = "15Min", use.parallel = FALSE)
#)


#individual import for trouble shooting
#my.folder <- "/home/loewi/AgFace/2015/Campbell_logger/logger_data"
#my.files <- list.files(path = my.folder, "*_15Min.dat")

#library(plyr)
#my.list <- ldply(my.files,
#                 #.progress = "text",
#                 function(y) {
#                 print(y)
#                 my.import <- paste(my.folder, y, sep = "/")
#                 x <- CampbellFileImport(my.import)
#                 my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#                 names(x) <- names(my.header)
#                 x$Date <- as.POSIXct(x$TIMESTAMP, tz = "GMT")
#                 return(x)
#                 })

#my.file <- "SYS7_15Min.dat"
#my.import <- paste(my.folder, my.file, sep = "/")

#x <- CampbellFileImport(my.import)
#my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#x <- read.csv(my.import, skip = 4, header = F)
#names(x) <- names(my.header)
#x$Date <- as.POSIXct(x$TIMESTAMP, tz = "GMT")

# end of individual import

system.time(
df <- CampbellAllImport(log.interval = "15Min", use.parallel = TRUE, #logger.name = "SYS8",
                        time.zone = "GMT")
)

# export data
write.csv(df,
          file = "15Min_Campbell_logger_data.csv",
          row.names = FALSE, na = "")

#system.time(
#df.cast <- CampbellCast(df), use.parallel = FALSE)
#)

system.time(
df.cast <- CampbellCast(df, use.parallel = TRUE)
)

ephemeral.times <- CampbellSunriseSunset(df)
ephemeral.times$sunrise <- ephemeral.times$sunrise + 60*60*10
ephemeral.times$sunset  <- ephemeral.times$sunset  + 60*60*10

my.time.to.plot <- 550
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = -10, yscale_max = 50,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast,
             yscale_min = -10, yscale_max = 50,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("Batt_volt_Min", my.time.to.plot, df.cast,
             yscale_min = -10, yscale_max = 50,
             sensor.colour = TRUE, cartesian = TRUE)

PlotAll <- function(data, time, sensor) {
      print(sensor)
      require(ggplot2)
      my.col <- which(names(data) == sensor)
      print(my.col)
      if (sum(is.na(data[, my.col])) == length(data[, my.col])) {
        print("No data")
	out <- ggplot(data) + geom_blank()
      } else {
         
	out <- MyRecentPlot(sensor, time, data,
		     yscale_min = NA, yscale_max = NA,
		     sensor.colour = TRUE, cartesian =TRUE)
      }
      return(out)
}

para.names <- names(df.cast)[4:length(names(df.cast))]

fig.out <- lapply(para.names, function(x) PlotAll(df.cast, 4200, x))

pdf(file = "15Min_figures_gh.pdf")
print(fig.out)
dev.off()

#my.hum.time <-48
#MyRecentPlot("Hum_Avg", my.hum.time, df.cast, #logger = "SYS8",
#             yscale_min = 0, yscale_max = 105,
#             sensor.colour = TRUE, cartesian =TRUE)
#MyRecentPlot("Temp_Avg", my.hum.time, df.cast, logger = "SYS1",
#             yscale_min = NA, yscale_max = NA,
#             sensor.colour = TRUE, cartesian =TRUE)
#MyRecentPlot("PAR_Avg", my.par.time, df.cast,
#             yscale_min = 0, yscale_max = 10,
#             sensor.colour = TRUE, cartesian = FALSE)
