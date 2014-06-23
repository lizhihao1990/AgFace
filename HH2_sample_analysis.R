# analyse test soil moisture data

library(ggplot2)

# import file
setwd("~/AgFace/2014/testfiles")
my.file <- HH2Import("19June14.CSV")

p <- ggplot(my.file, aes(x = Percent_Vol, y = Depth))
  p <- p + geom_line(aes(linetype = Plot,  colour = as.factor(Sample)))
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + scale_y_reverse()
  p <- p + theme_bw()
p
fig.vol <- p


p <- ggplot(my.file, aes(x = mV, y = Percent_Vol))
  p <- p + geom_line(aes(linetype = Plot,  colour = as.factor(Sample)))
  p <- p + theme_bw()
p

fig.calib <- p

pdf(file = "Soil_moisture_figures.pdf",
    width = 7, height = 7)
print(fig.vol)
print(fig.calib)
dev.off()
