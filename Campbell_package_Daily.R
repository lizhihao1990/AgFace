# using the CampbellLogger library to import files

library(CampbellLogger)
#library(doParallel)
library(doMC)
registerDoMC(4)
# set up cluster for parallel computing
#cl <- makeCluster(4)
#registerDoParallel(cl)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")


#individual import for trouble shooting
#my.folder <- "/home/loewi/AgFace/2015/Campbell_logger/logger_data"
#my.files <- list.files(path = my.folder, "*_Daily.dat")

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

#my.file <- "SYS7_Daily.dat"
#my.import <- paste(my.folder, my.file, sep = "/")

#x <- CampbellFileImport(my.import)
#my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#x <- read.csv(my.import, skip = 4, header = F)
#names(x) <- names(my.header)
#x$Date <- as.POSIXct(x$TIMESTAMP, tz = "GMT")

# end of individual import

system.time(
df <- CampbellAllImport(log.interval = "Daily", use.parallel = FALSE, #logger.name = "SYS1",
                        time.zone = "GMT")
)

# export data
write.csv(df,
          file = "Daily_Campbell_logger_data.csv",
          row.names = FALSE, na = "")

# get rid of unnecessary parameters
#df <- df[, -which(names(df) == "PTemp"), ]
#df <- df[, -which(names(df) == "LithiumBatt_Min"), ]

# get rid of unequal repeats
# Batt_volt_Min is "0" for some bad repeats. Removing them.
df <- df[df$Batt_volt_Min != 0, ]

# IR_Narrow_Min_1_ is "0" for bad repeats. Removing them.
df <- df[df$IR_Narrow_Min_1_ != 0, ]
df <- df[df$IR_Narrow_Min_2_ != 0, ]
df <- df[df$IR_Narrow_Min_6_ != -217.6, ]
df <- df[df$Soil_Avg_4_ != -146 & !is.na(df$Soil_Avg_4_), ]
df <- df[df$Soil_Avg_2_ != 11.3 & !is.na(df$Soil_Avg_2_), ]
df <- df[df$Soil_Avg_2_ != 28.47 & !is.na(df$Soil_Avg_2_), ]
df <- df[df$Soil_Avg_2_ != 31.94, ]
df <- df[df$Soil_Avg_2_ != 223, ]
df <- df[df$Soil_Avg_3_ != 18.73, ]
df <- df[df$Soil_Avg_3_ != 24.18, ]
df <- df[df$Soil_Avg_3_ != 20.45, ]
df <- df[df$IR_Horz_Max_1_ != -8034, ]
#df <- df[!(df$IR_Narrow_Avg_4_ == 12.8 &
#         df$TIMESTAMP == as.POSIXct("2015-10-22") & df$SYSTEM == "SYS3") & !is.na(df$IR_Narrow_Avg_4), ]
df <- df[df$Soil_Avg_4_ != -1107, ]
df <- df[df$TIMESTAMP < as.POSIXct("2051-01-01"), ]

full.dates <- seq(from = min(df$TIMESTAMP), to = max(df$TIMESTAMP), by = "day")
full.dates <- as.data.frame(full.dates)
names(full.dates) <- "TIMESTAMP"

df <- merge(df, full.dates, all.y = TRUE)

system.time(
df.cast <- CampbellCast(df, use.parallel = FALSE)
)


ephemeral.times <- CampbellSunriseSunset(df)
ephemeral.times$sunrise <- ephemeral.times$sunrise + 60*60*10
ephemeral.times$sunset <- ephemeral.times$sunset + 60*60*10



my.time.to.plot <- 400
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = 0, yscale_max = 50,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast,
             yscale_min = -10, yscale_max = 50,
             sensor.colour = TRUE, cartesian =TRUE)

my.hum.time <- 4200
MyRecentPlot("Soil_Avg", my.hum.time, df.cast, #logger = "SYS8",
             yscale_min = 0, yscale_max = 1,
             sensor.colour = TRUE, cartesian =TRUE)

PlotAll <- function(data, time, sensor) {
      require(ggplot2)
      MyRecentPlot(sensor, time, data,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian =TRUE)
}

para.names <- names(df.cast)[4:length(names(df.cast))]

fig.out <- lapply(para.names, function(x) PlotAll(df.cast, 4200, x))

pdf(file = "Daily_figures.pdf")
print(fig.out)
dev.off()
