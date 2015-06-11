# using the CampbellLogger library to import files

library(CampbellLogger)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

df <- CampbellAllImport(log.interval = "Hourly")

df.cast <- CampbellCast(df)

ephemeral.times <- CampbellSunriseSunset(df)

my.time.to.plot <- 48
MyRecentPlot("Soil_Avg", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE, ephemeral.time = TRUE)
MyRecentPlot("Batt_volt_Min", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
