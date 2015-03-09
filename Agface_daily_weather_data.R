# visualise daily weather data Agface
library(reshape2)
library(plyr)
library(ggplot2)

setwd("~/AgFace/2014/Weather_data")

df <- read.csv("Jan-DecHorMEADailyAve2014.csv")

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

#p <- ggplot(df, aes(x = Time, y = RainTot..mm.))
#     p <- p + geom_line()
#     p <- p + geom_text(label = "Hi", aes(x = mean(df$Time), y = sum(df$RainTot..mm.)))
#p

# In season data:
start.season <- "2014-05-29"
start.season.pos <- as.POSIXct(start.season, tz = "Australia/Melbourne")
end.season <- "2014-11-19"
end.season.pos <- as.POSIXct(start.season, tz = "Australia/Melbourne")
my.start <- "2014-05-29"

MyPlot <- function(data, my.start = start.season, my.end = end.season) {
  require(ggplot2)
  my.label    <- unique(data$variable)
  last.date   <- max(data$Time)
  sum.list    <- c("RainTot..mm.", "Frost..hrs.")
  my.sum      <- sum(data$value, na.rm = TRUE)
  my.mean     <- mean(data$value, na.rm = TRUE)
  my.max      <- max(data$value, na.rm = TRUE)
  my.min      <- min(data$value, na.rm = TRUE)
  my.meantime <- mean(data$Time, na.rm = TRUE)
  my.max.75   <- 0.75 * my.max
  my.sum.season <- sum(data$value[data$Time > as.POSIXct(my.start, tz = "Australia/Melbourne") &
  data$Time < as.POSIXct(my.end, tz = "Australia/Melbourne")], 
  na.rm = TRUE)
  my.mean.season <- mean(data$value[data$Time > as.POSIXct(my.start, tz = "Australia/Melbourne") &
  data$Time < as.POSIXct(my.end, tz = "Australia/Melbourne")], na.rm = TRUE)
  
  p <- ggplot(data, aes(x = Time, y = value))
    p <- p + geom_line()

# before season grey marker    
    p <- p + annotate("rect",
                      xmin = as.POSIXct("2014-01-01", tz = "Australia/Melbourne"),
                      xmax = as.POSIXct(my.start, tz = "Australia/Melbourne"),
                      ymin = my.min - (0.1 * my.min),
                      ymax = my.max + (0.1 * my.max),
                      alpha = 0.2)
    p <- p + annotate("text",
                      x = as.POSIXct("2014-03-10"),
                      y = my.max - (0.1 * my.max),
                      label = "Before sowing", colour = "whitesmoke")
# After season grey marker
    p <- p + annotate("rect",
                      xmin = as.POSIXct(my.end, tz = "Australia/Melbourne"),
                      #xmax = as.POSIXct(my.start, tz = "Australia/Melbourne"),
                      xmax = last.date,
                      ymin = my.min - (0.1 * my.min),
                      ymax = my.max + (0.1 * my.max),
                      alpha = 0.2)
    p <- p + annotate("text",
                      x = as.POSIXct("2014-11-28"),
                      y = my.max - (0.1 * my.max),
                      label = "After season", colour = "whitesmoke")
    if(my.label %in% sum.list) {
       p <- p + annotate("text", 
                x = my.meantime, 
                y = my.max.75, colour = "red", 
                label = paste("Sum =", round(my.sum, 3), sep = " "))
       p <- p + annotate("text", 
                x = my.meantime, 
                y = my.max.75 + (0.1 * my.max.75)  , colour = "green", 
                label = paste("In season Sum =", round(my.sum.season, 3), sep = " "))
    } else {
       p <- p + geom_hline(yintercept = my.mean, colour = "red")
       p <- p + annotate("text", 
                label = paste("Mean =", round(my.mean, 3), sep = " "), 
                x = my.meantime, y = my.max.75, colour = "red")
       p <- p + annotate("text", 
                x = my.meantime, 
                y = my.max.75 + (0.1 * my.max.75)  , colour = "green", 
                label = paste("In season Mean =", round(my.mean.season, 3), sep = " "))
    }
    p <- p + labs(y = my.label,
                  x = "Date")
    p <- p + theme_bw()
  return(p)
}

MyPlotList <- dlply(df.melt,
                    .(variable),
                    function(x) {                   
                    MyPlot(x, my.start = start.season, my.end = end.season)})

pdf(file = "2014_daily_weather_data.pdf",
    width = 9, height = 9)
print(MyPlotList)
dev.off()

in.season <- df.melt[df.melt$Time > as.POSIXct(start.season, tz = "Australia/Melbourne") &
                     df.melt$Time < as.POSIXct(end.season, tz = "Australia/Melbourne"), ]

in.season$Weeknumber <- as.numeric(format(in.season$Time, "%W"))
in.season$DOY <- as.numeric(format(in.season$Time, "%j"))

# number of unique days
length(sort(unique(in.season$DOY)))
# number of unique weeks
length(sort(unique(in.season$Weeknumber)))
