# using the CampbellLogger library to import files

library(CampbellLogger)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

df <- CampbellAllImport(log.interval = "Hourly")

df.cast <- CampbellCast(df)

ephemeral.times <- CampbellSunriseSunset(df)

my.time.to.plot <- 144
MyRecentPlot("Soil_Avg", my.time.to.plot, df.cast,
             yscale_min = 0, yscale_max = 0.4,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("Batt_volt_Min", my.time.to.plot, df.cast,
             yscale_min = NA, yscale_max = NA,
             sensor.colour = FALSE, cartesian = TRUE)
