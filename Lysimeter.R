# Lysimeter import

library(reshape2)
library(ggplot2)
library(plyr)

setwd("~/AgFace/2015/Lysimeter")

df <- read.csv("Lysimeter.csv")

df$DateTime <- with(df, paste(Date, Time, sep = " "))

# date.format <- "%d/%m/%Y %I:%M:%S %p"
date.format <- "%d/%m/%Y %H:%M:%S"

df$DateTime <- as.POSIXct(df$DateTime, format = date.format)


df$Date <- NULL
df$Time <- NULL

df.melt <- melt(df,
                id.vars = c("DateTime"))

sowing.date <- as.POSIXct("2015-05-27")
cut.off.date.low <- as.POSIXct("2015-05-20 00:00:00")
cut.off.date.high <- as.POSIXct("2015-12-07 18:00:00")

df.melt.current <- df.melt[df.melt$DateTime > sowing.date  &
                           df.melt$DateTime < cut.off.date.high, ]

# df.melt.current <- df.melt


# replace exact "0" with NA
df.melt.current$value[df.melt.current$value == 0] <- NA

p <- ggplot(df.melt.current, aes(x = DateTime, y = value))
  p <- p + geom_line(aes(colour = variable))
  p <- p + facet_grid(variable ~ ., scale = "free_y")
  p <- p + theme_bw()
#p

# get starting weights
# get the average weight of the core on the day before sowing
day.before.sowing <- as.POSIXct("2015-05-26")

pre.experiment <- df.melt[format(df.melt$DateTime, "%Y-%m-%d") == "2015-05-27", ]

pre.sowing.weight <- ddply(pre.experiment,
                           .(variable),
                           summarise,
                           mean.weight = mean(value, na.rm = TRUE))

# scale 7 was malfunctioning
scale.7.online <- df.melt.current$DateTime[df.melt.current$variable == "Ring.6.Scale.7..kg." &
                                    df.melt.current$value > 1]
scale.7.online <- scale.7.online[!is.na(scale.7.online)]
scale.7.online.day <- unique(format(scale.7.online, "%Y-%m-%d"))[1]

scale7.first.day <- df.melt.current[df.melt.current$variable == "Ring.6.Scale.7..kg." &
                            format(df.melt.current$DateTime, "%Y-%m-%d") == scale.7.online.day, ]
# get rid of bad readings for this scale and day
scale7.first.day <- scale7.first.day[scale7.first.day$value > 1, ]
scale7.first.weight <- mean(scale7.first.day$value, na.rm = TRUE)

# use the first-day value to replace the value at sowing time
pre.sowing.weight$mean.weight[pre.sowing.weight$variable == "Ring.6.Scale.7..kg."] <- scale7.first.weight


# same procedure for Ring.5.Scale.4..kg
scale.4.online <- df.melt.current$DateTime[df.melt.current$variable == "Ring.5.Scale.4..kg." &
                                    df.melt.current$value > 1]
scale.4.online <- scale.7.online[!is.na(scale.4.online)]
scale.4.online.day <- unique(format(scale.4.online, "%Y-%m-%d"))[1]

scale4.first.day <- df.melt.current[df.melt.current$variable == "Ring.5.Scale.4..kg." &
                            format(df.melt.current$DateTime, "%Y-%m-%d") == scale.7.online.day, ]
# get rid of bad readings for this scale and day
scale4.first.day <- scale4.first.day[scale4.first.day$value > 1, ]
scale4.first.weight <- mean(scale4.first.day$value, na.rm = TRUE)

# use the first-day value to replace the value at sowing time
pre.sowing.weight$mean.weight[pre.sowing.weight$variable == "Ring.5.Scale.4..kg."] <- scale4.first.weight

# re-name pre-sowing weights
names(pre.sowing.weight) <- c("variable", "pre.sowing.weight")

# merge weigths with pre-sowing weights
rel.weights <- merge(df.melt.current, pre.sowing.weight)
rel.weights$rel.weight <- (rel.weights$value / rel.weights$pre.sowing.weight) * 100

p <- ggplot(df.melt.current[df.melt.current$DateTime < as.POSIXct("2015-06-04"), ],
            aes(x = DateTime, y = value))
  p <- p + geom_line()#aes(colour = variable))
  p <- p + facet_grid(. ~ variable)
  p <- p + scale_y_continuous(limits = c(0, 140))
  p <- p + theme_bw()
p

 
# only for some scales
show.scales <- c("Ring\\.5\\.Scale\\.4\\.\\.kg\\.")
#show.scales <- c("Ring\\.6\\.Scale\\.7\\.\\.kg\\.")
#show.scales <- c("\\.Q\\.", "\\.R\\.", "\\.S\\.", "\\.T\\.")
found <- lapply(show.scales, function(x) grep(x, df.melt.current$variable))
found <- unlist(found)

some.scales <- df.melt.current[found, ]

p <- ggplot(some.scales, aes(x = DateTime, y = value))
  p <- p + geom_line(aes(colour = variable))
  #p <- p + facet_grid(variable ~ ., scale = "free_y")
  p <- p + theme_bw()
p

#ggsave(file = "Scales_in_Ring3.pdf",
#       width = 9, height = 7)

reconnected.all.32.scales <- as.numeric(as.POSIXct("2015-11-24 15:00:00"))

MyPlot <- function(data, para) {
     my.label <- unique(data$variable)
     my.label <- gsub("\\.", " ", my.label)
     my.length <- length(data$value)
     my.NA <- sum(is.na(data$value))
     
     if (my.NA != my.length) {
    	     p <- ggplot(data, aes_string(x = "DateTime", y = para))
	     p <- p + geom_line()
	     #p <- p + geom_vline(xintercept = as.numeric(c(cut.off.date.low, cut.off.date.high)), colour = "red")
	     p <- p + geom_vline(xintercept = reconnected.all.32.scales, colour = "red")
	     p <- p + labs(y = my.label)
	     if (para == "value") {
	         p <- p + coord_cartesian(ylim = c(105, 135))
	     } else {
	         p <- p + coord_cartesian(ylim = c(90, 110))
	     }
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
                   function(x) MyPlot(x, "value"))

pdf(file = "Plots.pdf")
print(plot.list)
dev.off()

# relative figure
plot.list.rel <- dlply(rel.weights,
                   .(variable),
                   function(x) MyPlot(x, "rel.weight"))

pdf(file = "Plots_relative.pdf")
print(plot.list.rel)
dev.off()

#write.csv(df.melt.current, file = "Lysimeter_weights.csv",
#          row.names = F)
