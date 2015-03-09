# Analysis of Campbell sensors

setwd("~/AgFace/2014/Campbell_logger/Lab_test")

my.header   <- read.csv("CR1000_3_SYS3_5Min.dat", skip = 1)
my.header   <- names(my.header)
my.descript <- read.csv("CR1000_3_SYS3_5Min.dat", skip = 3)
my.descript <- names(my.descript)
df <- read.csv("CR1000_3_SYS3_5Min.dat", skip = 4, na.strings = "NAN")

# my.names <- paste(my.header, my.descript, sep = "_")
names(df) <- my.header

names(df) <- gsub("\\.", "_", names(df))

df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "Australia/Melbourne")

my.logic <- c("SYS1_RawCh_SGA2_1_Avg", "SYS1_RawAh_SGA2_1_Avg", "SYS1_RawBh_SGA2_1_Avg", "SYS1_RawCh_SGA2_2_Avg", "SYS1_RawAh_SGA2_2_Avg", "SYS1_RawBh_SGA2_2_Avg", "SYS1_Sapflow_SGA2_1_Avg", "SYS1_RawCh_SGA2_1_Avg", "SYS1_RawAh_SGA2_1_Avg", "SYS1_RawBh_SGA2_1_Avg", "SYS1_RawCh_SGA2_2_Avg", "SYS1_RawAh_SGA2_2_Avg", "SYS1_RawBh_SGA2_2_Avg", "SYS1_Sapflow_SGA2_1_Avg", "SYS1_Kshapp_SGA2_1_Avg", "SYS1_dT_SGA2_1_Avg", "SYS1_Qv_SGA2_1_Avg", "SYS1_Qr_SGA2_1_Avg", "SYS1_Qf_SGA2_1_Avg", "SYS1_Sapflow_SGA2_2_Avg", "SYS1_Kshapp_SGA2_2_Avg", "SYS1_dT_SGA2_2_Avg", "SYS1_Qv_SGA2_2_Avg", "SYS1_Qr_SGA2_2_Avg", "SYS1_Qf_SGA2_2_Avg", "SYS1_Kshapp_SGA2_1_Avg", "SYS1_dT_SGA2_1_Avg", "SYS1_Qv_SGA2_1_Avg", "SYS1_Qr_SGA2_1_Avg", "SYS1_Qf_SGA2_1_Avg", "SYS1_Sapflow_SGA2_2_Avg", "SYS1_Kshapp_SGA2_2_Avg", "SYS1_dT_SGA2_2_Avg", "SYS1_Qv_SGA2_2_Avg", "SYS1_Qr_SGA2_2_Avg", "SYS1_Qf_SGA2_2_Avg")

df.orig <- df
df <- df[, !names(df) %in% my.logic]

# delete everything before January 6, only keeping the data from the test in the lab
cut.off.date <- as.POSIXct("2015-01-06 00:00:00", tz = "Australia/Melbourne")
df <- df[df$TIMESTAMP > cut.off.date, ]

library(reshape2)
df.melt <- melt(df, id.vars = "TIMESTAMP")


library(ggplot2)

p <- ggplot(df.melt[df.melt$variable == "SYS1_IR_Narrow_Std_2_", ],
            aes(x = TIMESTAMP, y = value))
   p <- p + geom_line()
p

MyPlot <- function(data) {
     my.label <- unique(data$variable)
     p <- ggplot(data, aes(x = TIMESTAMP, y = value))
     p <- p + geom_line()
     p <- p + labs(y = my.label)
     return(p)
}
library(plyr)
plot.list <- dlply(df.melt,
                   .(variable),
                   function(x) MyPlot(x))
pdf(file = "Plots.pdf")
print(plot.list)
dev.off()

my.start.time <- as.POSIXct("2015-01-10 00:00:00", tz = "Australia/Melbourne")
my.end.time <- as.POSIXct("2015-01-14 00:00:00", tz = "Australia/Melbourne")

to.plot <- c("SYS3_Qr_SGA2_1_Avg",
             "SYS3_Qf_SGA2_1_Avg",
             "SYS3_Pin_SGA2_1_Avg",
             "SYS3_Sapflow_SGA2_1_Avg")

p <- ggplot(df.melt[df.melt$variable %in% to.plot, ], 
            aes(x = TIMESTAMP, y = value))
  p <- p + geom_line(aes(colour = variable))
  p <- p + coord_cartesian(xlim = c(my.start.time, my.end.time))
  p <- p + scale_y_continuous(limits = c(-1, 5))
  p <- p + facet_grid(variable ~ ., scale = "free_y")
  p <- p + theme_bw()
p

