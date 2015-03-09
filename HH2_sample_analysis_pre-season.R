# analyse test soil moisture data

library(ggplot2)

source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")


setwd("~/AgFace/2015/HH2_Soil_moisture/Pre-season_calibration/")

# import file
HH2 <- HH2Import("2014-07-03.csv")

# assign IDs and treatments, etc

sm <- merge(HH2, TubeIDs,
            by.x = "Sample",
            by.y = "HH2_tube_number")
sm <- unique(sm)
sm <- sm[with(sm, order(Sample, Depth)), ]

p <- ggplot(sm, aes(x = mV, y = Percent_Vol))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_line(aes(colour = Tube_treatment))
#  p <- p + facet_wrap( ~ Ring)
  p <- p + theme_bw()
  p <- p + labs(title = "millivolt vs %Vol")
p
fig.calib <- p

p <- ggplot(sm[sm$Sample < 3, ], aes(x = mV, y = Depth))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_path(aes(colour = Tube_treatment))
  p <- p + scale_y_reverse()
  p <- p + facet_wrap( ~ Ring)
  p <- p + theme_bw()
  p <- p + labs(title = "Reference tubes")
p
fig.ref <- p

p <- ggplot(sm[!is.na(sm$Ring), ], aes(x = mV, y = Depth))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + geom_path(aes(colour = as.factor(Sample)))
  p <- p + scale_y_reverse()
  p <- p + facet_wrap( ~ Ring)
  p <- p + theme_bw()
  p <- p + theme(legend.position = "none")
  p <- p + labs(title = "All rings, all sensors")
p
fig.mV <- p

pdf(file = "Soil_moisture_figures.pdf",
    width = 7, height = 7)
print(fig.ref)
print(fig.mV)
print(fig.calib)
dev.off()

# export data as RData frame
save(sm, file = "HH2_soil_moisture.RData", compress = TRUE)
