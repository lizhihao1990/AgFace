# Lysimeter import

library(reshape2)
library(ggplot2)

setwd("~/AgFace/2015/Lysimeter")

df <- read.csv("Lysimeter_2015_06_29.csv")

df$DateTime <- with(df, paste(Date, Time, sep = " "))

# date.format <- "%d/%m/%Y %I:%M:%S %p"
date.format <- "%d/%m/%Y %H:%M:%S"

df$DateTime <- as.POSIXct(df$DateTime, format = date.format)


df$Date <- NULL
df$Time <- NULL

df.melt <- melt(df,
                id.vars = c("DateTime"))

cut.off.date.low <- as.POSIXct("2015-06-26 09:40:00")
cut.off.date.high <- as.POSIXct("2015-06-26 14:56:00")

#df.melt.current <- df.melt[df.melt$DateTime > cut.off.date.low &
#                           df.melt$DateTime < cut.off.date.high, ]

df.melt.current <- df.melt

# replace exact "0" with NA
# df.melt.current$value[df.melt.current$value == 0] <- NA

p <- ggplot(df.melt.current, aes(x = DateTime, y = value))
  p <- p + geom_line(aes(colour = variable))
  p <- p + facet_grid(variable ~ .)#, scale = "free_y")
  p <- p + theme_bw()
p

MyPlot <- function(data) {
     my.label <- unique(data$variable)
     my.label <- gsub("\\.", " ", my.label)
     my.length <- length(data$value)
     my.NA <- sum(is.na(data$value))
     
     if (my.NA != my.length) {
     
     p <- ggplot(data, aes(x = DateTime, y = value))
     p <- p + geom_line()
     p <- p + geom_vline(xintercept = as.numeric(c(cut.off.date.low, cut.off.date.high)), colour = "red")
     p <- p + labs(y = my.label)
     #p <- p + coord_cartesian(ylim = c(110, 120))
     p <- p + facet_grid(variable ~ .)
     p <- p + theme_bw()
     return(p)
     } else {
     p <- ggplot(data, aes(x = 1, y = 1))
     p <- p + geom_blank()
     p <- p + labs(y = my.label)
     p <- p + theme_bw()
     return(p)
     }
     
}
library(plyr)
plot.list <- dlply(df.melt.current,
                   .(variable),
                   function(x) MyPlot(x))

pdf(file = "Plots.pdf")
print(plot.list)
dev.off()
