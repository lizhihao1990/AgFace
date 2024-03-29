# analyse test soil moisture data

library(ggplot2)

source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")


setwd("~/AgFace/2014/HH2_Soil_moisture")
load("HH2_Tubes_lookup_table.RData")

# import file
HH2 <- HH2Import("2014-07-07_orig.CSV")

# special case in this file: all readings were taken in tube 51
# Sample IDs in the file are wrong!
HH2$Sample <- 51

# assign IDs and treatments, etc

sm <- merge(HH2, TubeIDs,
            by.x = "Sample",
            by.y = "HH2_tube_number")

sm <- sm[with(sm, order(Sample, Depth)), ]

p <- ggplot(sm, aes(x = mV, y = Depth))
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + scale_y_reverse()
  p <- p + theme_bw()
  p <- p + theme(legend.position = "none")
  p <- p + labs(title = "Tube 51, 2014-07-07")
p
fig.mV <- p

p <- ggplot(sm, aes(x = mV, y = Percent_Vol))
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + theme_bw()
  p <- p + theme(legend.position = "none")
  p <- p + labs(title = "Tube 51, 2014-07-07")
p
fig.calib <- p


pdf(file = "Tube_51_Soil_moisture_figures.pdf",
    width = 7, height = 7)
print(fig.mV)
print(fig.calib)
dev.off()
