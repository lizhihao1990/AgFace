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

system.time(
df <- CampbellAllImport(log.interval = "15Min", use.parallel = TRUE,
                        time.zone = "GMT")
)

#system.time(
#df.cast <- CampbellCast(df), use.parallel = FALSE)
#)

system.time(
df.cast <- CampbellCast(df, use.parallel = TRUE)
)

ephemeral.times <- CampbellSunriseSunset(df)
ephemeral.times$sunrise <- ephemeral.times$sunrise + 60*60*10
ephemeral.times$sunset <- ephemeral.times$sunset + 60*60*10

my.time.to.plot <- 48
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = 0, yscale_max = 50,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian =TRUE)

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
