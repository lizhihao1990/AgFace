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
df <- CampbellAllImport(log.interval = "15Min", use.parallel = TRUE)
)

#system.time(
#df.cast <- CampbellCast(df), use.parallel = FALSE)
#)

system.time(
df.cast <- CampbellCast(df, use.parallel = TRUE)
)

ephemeral.times <- CampbellSunriseSunset(df)

my.time.to.plot <- 96
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = -4, yscale_max = 35,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast,
             yscale_min = -4, yscale_max = 35,
             sensor.colour = TRUE, cartesian =TRUE)
