# Lysimeter import

library(reshape2)
library(ggplot2)

setwd("~/AgFace/2015/Lysimeter")

df <- read.csv("Lysimeter_2015-07-23.csv")

df$DateTime <- with(df, paste(Date, Time, sep = " "))

# date.format <- "%d/%m/%Y %I:%M:%S %p"
date.format <- "%d/%m/%Y %H:%M:%S"

df$DateTime <- as.POSIXct(df$DateTime, format = date.format)


df$Date <- NULL
df$Time <- NULL

df.melt <- melt(df,
                id.vars = c("DateTime"))

cut.off.date.low <- as.POSIXct("2015-07-19 00:00:00")
cut.off.date.high <- as.POSIXct("2015-07-23 18:00:00")

df.melt.current <- df.melt[df.melt$DateTime > cut.off.date.low &
                           df.melt$DateTime < cut.off.date.high, ]

# df.melt.current <- df.melt

# replace exact "0" with NA
# df.melt.current$value[df.melt.current$value == 0] <- NA

p <- ggplot(df.melt.current, aes(x = DateTime, y = value))
  p <- p + geom_line(aes(colour = variable))
  p <- p + facet_grid(variable ~ ., scale = "free_y")
  p <- p + theme_bw()
p

      
# only for some scales
show.scales <- c("\\.Q\\.", "\\.R\\.", "\\.S\\.", "\\.T\\.")
found <- lapply(show.scales, function(x) grep(x, df.melt.current$variable))
found <- unlist(found)

some.scales <- df.melt.current[found, ]

p <- ggplot(some.scales, aes(x = DateTime, y = value))
  p <- p + geom_line(aes(colour = variable))
  #p <- p + facet_grid(variable ~ ., scale = "free_y")
  p <- p + theme_bw()
p

ggsave(file = "Scales_in_Ring3.pdf",
       width = 9, height = 7)

MyPlot <- function(data) {
     my.label <- unique(data$variable)
     my.label <- gsub("\\.", " ", my.label)
     my.length <- length(data$value)
     my.NA <- sum(is.na(data$value))
     
     if (my.NA != my.length) {
    	     p <- ggplot(data, aes(x = DateTime, y = value))
	     p <- p + geom_line()
	     #p <- p + geom_vline(xintercept = as.numeric(c(cut.off.date.low, cut.off.date.high)), colour = "red")
	     p <- p + labs(y = my.label)
	     #p <- p + coord_cartesian(ylim = c(105, 140))
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

write.csv(df.melt.current, file = "Lysimeter_weights.csv",
          row.names = F)
