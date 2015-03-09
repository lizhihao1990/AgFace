# import example 5 min data file

# visualise 5 min weather data Agface
library(reshape2)
library(plyr)
library(ggplot2)

setwd("~/Desktop")

df <- read.csv("5minute_data_buffer-12-16 Feb.csv")

# convert Date to date
df$Time <- paste(df$Date, df$Time, sep = " ")
df$Date <- as.Date(df$Date, format = "%d/%m/%Y", 
                   tz = "Australia/Melbourne")
# convert time to time
df$Time <- as.POSIXct(df$Time, 
                      format = "%d/%m/%Y %I:%M:%S %p", 
                      tz = "Australia/Melbourne")

df.melt <- melt(df,
                id = c("Date", "Time"))

my.time <- c("12/02/2015 1:05:00 am",
             "12/02/2015 1:05:00 pm")
as.POSIXct(my.time, format = "%d/%m/%Y %I:%M:%S %p")


p <- ggplot(df, aes(x = Time, y = Ave.VPD..kPa. ))
     p <- p + geom_point()
p
