# test measuremnt of Absorptivity

setwd("~/AgFace/2013/Spectra/Absorptivity_test")

refl  <- read.csv("test100006.asd.txt", sep = "\t")
trans <- read.csv("test100007.asd.txt", sep = "\t")
names(refl)[2] <- "Reflectance"
names(trans)[2] <- "Transmission"

my.data <- merge(refl, trans)

my.offset <- range(my.data$Transmission)

library(scales)
my.data$Norm_Ref <- rescale(my.data$Reflectance,  to = c(0, 1))
my.data$Norm_Tra <- rescale(my.data$Transmission, to = c(0, 1))

# Force all data to the range from 0 to 1
# before processing

my.data$Abs <- my.data$Norm_Ref - my.data$Norm_Tra

library(ggplot2)
p <- ggplot(my.data, aes(x = Wavelength))
 p <- p + geom_line(aes(y = Abs), colour = "blue")
 p <- p + geom_line(aes(y = Reflectance), colour = "red")
 p <- p + geom_line(aes(y = Transmission), colour = "green")
p

library(reshape)
my.data.melt <- melt(my.data, id = "Wavelength")

norm.data <- my.data.melt[my.data.melt$variable == "Norm_Ref" |
                          my.data.melt$variable == "Norm_Tra" |
                          my.data.melt$variable == "Abs", ]

p <- ggplot(norm.data, aes(x = Wavelength, y = value))
 p <- p + geom_line(aes(colour = variable))
p
