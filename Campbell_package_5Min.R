# using the CampbellLogger library to import files

library(CampbellLogger)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

df <- CampbellAllImport(log.interval = "5Min")

df.cast <- CampbellCast(df)

ephemeral.times <- CampbellSunriseSunset(df)

my.time.to.plot <- 148
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
