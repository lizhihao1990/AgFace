# combine soil moisture data from 2011 and 2012

# based on scripts "Grav_Soil_moisture_2011|2012.R"

# Markus Löw, Feb 2015

setwd("~/AgFace/2011_2012/Soil_moisture")

# load existing workspaces
load("../../2011/Soil_moisture_2011/2011_soil_moisture_workspace.RData")
load("../../2012/Soil_moisture_2012/2012_soil_moisture_workspace.RData")

# get rid of a few 2011 things
df.2011$Remark <- NULL
df.2011$Trial  <- NULL

# combine the 2011 and 2012 data
df <- rbind(df.2011, df.2012)

with(df, table(Sample_time, Year, CO2_treatment))

df$Ring <- as.numeric(as.character(df$Ring))

df.3 <- df
df.3$dup <- duplicated(df.3)
write.table(df.3, file = "df.csv", row.names = FALSE, sep = ",")

# merge with Cultivar information from Agface helper script
df <- merge(df, PlotRingCult,
            by.x = c("Year", "Ring", "CO2_treatment", "Plot", "Irrigation"),
            by.y = c("Year", "RingID", "CO2", "PlotID", "Irrigation"),
            all.x = TRUE)
# df.2$dup <- duplicated(df.2)
# write.table(df.2, file = "df2.csv", row.names = FALSE, sep = ",")

# get rid of bulk wheat plots in 2012
# those are the plots that can't be matched to a cultivar from the PlotRingCult data frame
df.incl.bulk <- df
df <- df[!is.na(df.2$Cultivar), ]

# re-order data frame

df.Silver <- df[, c("Year", "Ring", "Plot", "CO2_treatment", "Irrigation", "TOS", "Cultivar", "Depth", "Sample_time", "RingPos", "Sample.quality", "Volumetric.water.content")]

df.Silver <- df.Silver[df.Silver$Cultivar == "Silverstar" |
                       df.Silver$Cultivar == "SSR T65", ]
# export overall file
write.table(df,
            file = "Soil_moisture_content_2011_2012.csv",
            sep = ",", row.names = FALSE, na = "")

names(df.Silver) <- gsub("Volumetric.water.content", "Volumetric.water.content_mm_mm-2", names(df.Silver))
# sort by depth
df.Silver <- df.Silver[with(df.Silver, order(Year, Ring, Cultivar, Depth)), ]
# export a file for Silverstar/SSRT only
write.table(df.Silver,
            file = "Soil_moisture_content_2011_2012_Silverstar_tin.csv",
            sep = ",", row.names = FALSE, na = "")

# calculate sowing or harvest soil moisture
library(plyr)

# calculate sum of water content over whole profile
profile.sum  <- ddply(df,
               .(Year, Ring, CO2_treatment, Plot, Irrigation, Cultivar, Sample_time),
               summarise,
               Profile_sum_vol_water_cont = sum(Volumetric.water.content, na.rm = TRUE))

# calculate water use
# initial minus final soil moisture deltaW
deltaW <- ddply(profile.sum,
            .(Year, Ring, CO2_treatment, Plot, Irrigation, Cultivar),
            function(x){
            # separate sowing and harvest
            sow <- x[x$Sample_time == "Sowing", ]
            har <- x[x$Sample_time == "Harvest", ]
            deltaW <- sow$Profile_sum_vol_water_cont - har$Profile_sum_vol_water_cont
            names(deltaW) <- "deltaW"
            return(deltaW)
            })

# subtract rainfall that fell between sowing and harvest
# date of soil moisture data 2011:
sowing.sample.date.2011 <- "2011-12-12"
harvest.sample.date.2011 <- "2011-05-18" # actual range given as "16-19/5/11" in data from Russel

# date of soil moisture data 2011
harvest.sample.date.2012 <- "2012-12-17" # actual date given as Dec 17 - Jan 3 2013: 
sowing.sample.date <- "2012-05-21" # actual date given as: "21/5/12"

sowing.date.2011 <- as.POSIXct("2011-05-25", tz = "Australia/Melbourne")
harvest.date.2011 <- as.POSIXct("2011-12-05", tz = "Australia/Melbourne")

sowing.date.2012 <- as.POSIXct("2012-05-30", tz = "Australia/Melbourne")
harvest.date.2012 <- as.POSIXct("2012-12-05", tz = "Australia/Melbourne")

# load workspace with rainfall and irrigation data
load("~/AgFace/Weather/Daily_weather_2011_2012.RData")
daily_weather_2011_2012$Year <- as.factor(format(daily_weather_2011_2012$Date, "%Y"))

# calculate in-season rainfall between sowing and harvest date
in.season.rain.2011 <- sum(daily_weather_2011_2012$RainTot..mm.[
                          daily_weather_2011_2012$Date >= as.Date(sowing.date.2011) &
                          daily_weather_2011_2012$Date <= as.Date(harvest.date.2011)], na.rm = TRUE)

in.season.rain.2012 <- sum(daily_weather_2011_2012$RainTot..mm.[
                          daily_weather_2011_2012$Date >= as.Date(sowing.date.2012) &
                          daily_weather_2011_2012$Date <= as.Date(harvest.date.2012)], na.rm = TRUE)

# load irrigation information
#load("~/AgFace/Topics/Met_Irri/Meteo_Irrigation_info.RData")
#Met.Irri <- Met.Irri[Met.Irri$Year == 2011 | Met.Irri$Year == 2012, ]

irri.amount.2011 <- 100 # mm from Met.Irri data
irri.amount.2012 <- 120 # mm from Met.Irri data

in.season.water.2011 <- in.season.rain.2011 + irri.amount.2011
in.season.water.2012 <- in.season.rain.2012 + irri.amount.2012

# rainfall and irrigation data frame
rain.irri <- data.frame(Year = rep(c(2011, 2012), 2),
                        Irrigation = c("rainfed", "rainfed", "supp", "supp"),
                        Tot.in.season.water.received = c(in.season.rain.2011,
                                           in.season.water.2011,
                                           in.season.rain.2012,
                                           in.season.water.2012))

# merge rainfall and irrigation information with delta W
water.use <- merge(deltaW, rain.irri)

# water use is deltaW plus rainfall plus irrigation
water.use$water.use <- with(water.use, deltaW + Tot.in.season.water.received)


