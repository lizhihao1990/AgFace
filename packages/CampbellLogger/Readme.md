CampbellLogger
==============

R-package to import, merge, and visualise Campbell Scientific *.dat files

See 

	help(package = CampbellLogger) 

for details on the functions provided by this package.

### Installation

Installation straight from github via

```{r}
install_github("MarkusLoew/AgFace/packages/CampbellLogger")
```

Installation under Windows might require the installation of Rtools. There will be a prompt for it if needed.

### Example usage

```{r}
# using the CampbellLogger library to import files

library(CampbellLogger)

df <- CampbellAllImport(logger.folder = "~/AgFace/2015/Campbell_logger/logger_data", 
                        log.interval = "Hourly")

# put all sensors of similar type together
df.cast <- CampbellCast(df)

# calculate sunrise and sunset times for all dates in df
ephemeral.times <- CampbellSunriseSunset(df)

# create figure for the last 48 hours, for the soil moisture sensors,
# implement a y-axis range from 0 to 0.35, 
# identify each sensor per system by colour, 
# discard data outside of the chosen y-axis scale, 
# and include information on day- / night-time.

MyRecentPlot(para = "Soil_Avg", 
             hours = 48, 
             data = df.cast,
             yscale_min = 0, 
             yscale_max = 0.35,
             sensor.colour = TRUE,
             cartesian = TRUE,
             ephemeral.time = TRUE,
             ephemeral.object = ephemeral.times)
```
