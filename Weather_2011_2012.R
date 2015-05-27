# Look at Met data 2011 & 12

# load libraries
require(xlsx)
require(reshape)
require(ggplot2)
require(plyr)

# set working directory
setwd("~/AgFace/Weather")

# grab the data
# /home/loewi/AgFace/2011/Weather_data_2011
df2011 <- read.xlsx("../2011/Weather_data_2011/2011_FACE_weather_PBC_summary.xls",
                    sheetName = "Met11_Final summary_daily")

df2012 <- read.xlsx("../2012/Weather_data_2012/2012_FACE_weather_PBC_summary.xls",
                    sheetName = "Met12_Final summary_daily")

# Parameter names seem to be identical
names(df2011) %in% names(df2012)
names(df2012) %in% names(df2011)

# why are there missing values in the data?
df2011[is.na(df2011$Date), ]
df2012[is.na(df2012$Date), ]

# seems like there are empty rows beyond Dec 31
nrow(df2012)

# na.omit removes too much
# df2012 <- na.omit(df2012)

df2012 <- df2012[!is.na(df2012$Date), ]

#putting them together
df <- rbind(df2011, df2012)

# checking the number of samples - one per day for two years
nrow(df) / 2

# data cleaning:
# Minimum temperature of -40 is unrealistic
df$Min.AirTemp..degC.[df$Min.AirTemp..degC. == -40 &
                      !is.na(df$Min.AirTemp..degC.)] <- NA
# Discarding maximum temperatures above 45°C
df$Max.AirTemp..degC.[df$Max.AirTemp..degC. > 45 &
                      !is.na(df$Max.AirTemp..degC.)] <- NA

# dates with missing minimum temperature
# 2011-02-05
# 2012-02-01 # all data missing
# 2012-02-02 # all data missing

# prepare for efficient plotting
# getting rid of Time - it is all 9:00 anyway
df$Time <- NULL
df.melt <- melt(df, id = c("Date"))

my.plots <- dlply(df.melt,
                  .(variable),
                  function(x) {
                  my.ylabel <- unique(x$variable)
                  p <- ggplot(x, aes(x = Date, y = value))
                   p <- p + geom_line()
                   p <- p + theme_bw()
                   p <- p + labs(y = my.ylabel)
                  return(p)
                  })
pdf(file = "Weather_plots_2011_2012.pdf",
    width = 9, height = 7)
    print(my.plots)
dev.off()

# export the daily_weather_data
daily_weather_2011_2012 <- df

save(daily_weather_2011_2012, file = "Daily_weather_2011_2012.RData", compress = TRUE)
