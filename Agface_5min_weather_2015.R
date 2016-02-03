# import 5Min weather data from Mahabuburs AgFace weather station

# Markus Löw, Dec 2015

library(reshape2)
library(ggplot2)
library(plyr)

setwd("~/AgFace/2015/Weather")

df <- read.csv("2015 - 05 Min Ave.csv")

df$DateTime <- paste(df$Date, df$Time, sep = " ")

df$DateTime <- as.POSIXct(df$DateTime, format = "%d/%m/%Y %I:%M:%S %p", tz = "GMT")

df.melt <- melt(df,
                id.vars = c("Date", "Time", "DateTime"))

# In season data:
start.season <- "2015-05-26"
start.season.pos <- as.POSIXct(start.season, tz = "Australia/Melbourne")
end.season <- "2015-11-18"
end.season.pos <- as.POSIXct(start.season, tz = "Australia/Melbourne")
my.start <- "2015-05-26"

MyPlot <- function(data, my.start = start.season, my.end = end.season) {
  require(ggplot2)
  my.label    <- unique(data$variable)
  last.date   <- max(data$DateTime)
  sum.list    <- c("Total.Rain..mm.", "Frost..hrs.")
  my.sum      <- sum(data$value, na.rm = TRUE)
  my.mean     <- mean(data$value, na.rm = TRUE)
  my.max      <- max(data$value, na.rm = TRUE)
  my.min      <- min(data$value, na.rm = TRUE)
  my.meantime <- mean(data$DateTime, na.rm = TRUE)
  my.max.75   <- 0.75 * my.max
  my.sum.season <- sum(data$value[data$DateTime > as.POSIXct(my.start, tz = "Australia/Melbourne") &
  data$Time < as.POSIXct(my.end, tz = "Australia/Melbourne")], 
  na.rm = TRUE)
  my.mean.season <- mean(data$value[data$DateTime > as.POSIXct(my.start, tz = "Australia/Melbourne") &
  data$Time < as.POSIXct(my.end, tz = "Australia/Melbourne")], na.rm = TRUE)
  
  p <- ggplot(data, aes(x = DateTime, y = value))
    p <- p + geom_line()

# before season grey marker    
    p <- p + annotate("rect",
                      xmin = as.POSIXct("2015-01-01", tz = "Australia/Melbourne"),
                      xmax = as.POSIXct(my.start, tz = "Australia/Melbourne"),
                      ymin = my.min - (0.1 * my.min),
                      ymax = my.max + (0.1 * my.max),
                      alpha = 0.2)
    p <- p + annotate("text",
                      x = as.POSIXct("2015-03-10"),
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
                      x = as.POSIXct("2015-11-29"),
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

pdf(file = "2015_5Min_weather_data.pdf",
    width = 9, height = 9)
print(MyPlotList)
dev.off()

# winddirection plot
p <- ggplot(df, aes(x = Ave.WndDir..deg., y = Ave.WndSpd..m.s.))
  p <- p + geom_line(alpha = 0.8)
  p <- p + coord_polar()
  p <- p + scale_x_continuous(breaks = c(0, 90, 180, 270, 360))
  p <- p + theme_bw()
p

# renaming object for re-use with other data sets
Five.Min.weather.2015 <- df.melt
save(Five.Min.weather.2015, file = "5Min_weather_2015.RData", compress = TRUE)
