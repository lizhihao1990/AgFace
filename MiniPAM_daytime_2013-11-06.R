# Daytime measurement analysis Oct 16, 2013
# one flash at ambient light level, second flash after 30 sec of light level 10 (Act-int)

setwd("~/AgFace/2013/MiniPAM/2013-11-06_daytime")

# load helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")
# call loading script
source("~/AgFace/R_scripts/MiniPAM_minicom_import.R")

# load libraries
library(ggplot2)
library(reshape)
library(plyr)

# import data
daytime <- LoadMiniPAM("minicom.cap")

# get rid of failed measurements with no leaf
daytime <- daytime[!daytime$F == 0 & !daytime$Fm. == 0, ]


# Meta data
# create a ring number
daytime$Ring <- 0

# Assign heat treatments
standard_heat_treatment_description <- c(rep("ambient temperature", 2),
                                        rep("elevated temperature", 2),
                                        rep("ambient temperature", 2),
                                        rep("elevated temperature", 2))

daytime$Ring[daytime$Mark[1:58] %in% c("A", "B")] <- 2
daytime$Ring[daytime$Mark[1:58] %in% c("C", "D", "E")] <- 4

# all data for Mark "D" are bad. Deleted
daytime <- daytime[!(daytime$Mark == "D" & daytime$Ring == 4), ]

daytime$Ring[daytime$Mark[1:58] %in% c("F", "G", "H", "I")] <- 7
daytime$Ring[daytime$Mark[1:58] %in% c("J", "K", "L", "M")] <- 6
daytime$Ring[daytime$Mark[1:58] %in% c("N", "O", "P", "Q")] <- 10
daytime$Ring[daytime$Mark[1:58] %in% c("R", "S")] <- 11
# here is another T, but now it belongs to Ring 16
daytime$Ring[daytime$Mark == "T"] <- c(rep(11, 4), 16, 16)

daytime$Ring[daytime$Mark[1:58] %in% c("U", "V", "W")] <- 16
daytime$Ring[daytime$Mark[1:58] %in% c("X", "Y", "Z")] <- 15

daytime$Ring[daytime$Date > as.POSIXct("2013-11-06 13:44:30", tz = "Australia/Melbourne")] <- c(15, 15, rep(2, 4), 99)
# get rid of the bad measurement marked as ring 99
daytime <- daytime[!daytime$Ring == 99, ]

# add the heat treatment information
daytime$Heat_treat <- NA
daytime$Heat_treat[daytime$Ring >= 4 ] <- standard_heat_treatment_description
daytime$Heat_treat[daytime$Ring == 2 ] <- standard_heat_treatment_description

daytime$Ring <- as.factor(daytime$Ring)
daytime$Heat_treat <- as.factor(daytime$Heat_treat)
# CO2 treatment
daytime$CO2 <- NA
daytime$CO2[daytime$Ring %in% ambient_rings_2013]  <- "aCO2"
daytime$CO2[daytime$Ring %in% elevated_rings_2013] <- "eCO2"
daytime$CO2 <- as.factor(daytime$CO2)

# Assigning plot identifiers
daytime$Plot <- "zzz"

daytime$Plot <- as.factor(daytime$Plot)

# identification of first flash at ambient light vs high light
daytime$Light <- rep(c("ambient light", "high light"), nrow(daytime)/2)
daytime$Light <- as.factor(daytime$Light)


# aggregate the values per Ring, and Light
# cast and melt
# getting rid of variables that are not needed
get_rid_of <- c("H.M.S", "D.M.Y", "No.", "Date")
daytime.small <- daytime[, !(names(daytime) %in% get_rid_of)]

daytime.melt <- melt(daytime.small)

daytime.cast <- cast(daytime.melt,
                Ring + CO2 + Heat_treat + Light ~ variable,
                mean)


# analysis
# PAR levels
p <- ggplot(daytime.cast, aes(x = Heat_treat, y = PAR))
        p <- p + stat_summary(aes(colour = Heat_treat), 
                              fun.data = "mean_sdl", mult = 1)
        p <- p + theme_bw()
p

# light levels between chambers and rings
light.difference.aov <- aov(PAR ~ Heat_treat, data = daytime.cast)
summary(light.difference.aov)

p <- ggplot(daytime.cast, aes(x = PAR, y = Yield))
     p <- p + geom_point(aes(colour = CO2))
     p <- p + geom_smooth(aes(colour = CO2), method = "lm")
     p <- p + facet_grid(Light ~ Heat_treat)
     p <- p + theme_bw()
     p <- p + labs(y = expression(phi[PS2]))
p

p <- ggplot(daytime.cast, aes(x = PAR, y = ETR))
     p <- p + geom_point(aes(colour = CO2))
     p <- p + geom_smooth(aes(colour = CO2), method = "lm")
     p <- p + facet_grid(Light ~ Heat_treat)
     p <- p + theme_bw()
     p <- p + labs(y = expression("ETR ["~mu*mol~m^-1~s^-2~"]"))
p

p <- ggplot(daytime.cast, aes(x = PAR, y = Yield))
     #p <- p + geom_point(aes(colour = Light))
     p <- p + geom_line(aes(colour = Ring))

     p <- p + facet_grid(Heat_treat ~ CO2)
     p <- p + theme_bw()
     p <- p + labs(y = expression(PS[II]~operating~efficiency~phi[PS2]))
p

phiPS2_slopes <- p

# calculate slope per Ring

my.slopes <- ddply(daytime.cast,
              .(Ring, CO2, Heat_treat),
              function(x) {
              my.lm <- lm(Yield ~ PAR, data = x)
              my.slope <- coef(my.lm)[2]
              names(my.slope) <- "Yield_slope"
              return(my.slope)
              })

# Anova on slopes
#my.aov <- aov(Yield_slope ~ CO2 * Heat_treat, data = my.slopes[!my.slopes$Ring == 4, ])
my.slope.aov <- aov(Yield_slope ~ CO2 * Heat_treat, data = my.slopes)
summary(my.slope.aov)

# compare Yield regarding CO2 and heat treatments under ambient light
my.aov <- aov(Yield ~ CO2 * Heat_treat, 
              data = daytime.cast[daytime.cast$Light == "ambient light", ])
summary(my.aov)
ambient_light_aov <- my.aov

TukeyHSD(my.aov)
my.aov <- aov(Yield ~ CO2 * Heat_treat, 
              data = daytime.cast[daytime.cast$Light == "high light", ])
summary(my.aov)
TukeyHSD(my.aov)

# boxplot
p <- ggplot(daytime.cast, 
            aes(x = CO2, y = Yield))
        p <- p + geom_boxplot(aes(fill = Heat_treat))
        p <- p + facet_grid(. ~ Light)
        p <- p + theme_bw()
        p <- p + labs(y = expression(PS[II]~operating~efficiency~phi[PS2]),
                      x = expression(CO[2]~treatment))
p
ambient_and_high_phiPS2 <- p

p <- ggplot(daytime.cast, aes(x = PAR, y = Temp))
        p <- p + geom_point(aes(colour = CO2))
        p <- p + facet_grid(. ~ Light)
        p <- p + theme_bw()
p

# boxplot
p <- ggplot(daytime.cast, 
            aes(x = CO2, y = Temp))
        p <- p + geom_boxplot(aes(colour = Heat_treat))
        p <- p + facet_grid(. ~ Light)
        p <- p + theme_bw()
        p <- p + labs(y = expression(Leaf~temperature~"["~phantom(0)*degree~C~"]"),
                      x = expression(CO[2]~treatment))
p
ambient_and_high_leaf_temperatures <- p
ggsave(file = "Daytime_leaf_temperatures_ambient_and_high_light.pdf",
       width = 7, height = 5)
# compare Leaf temperature regarding CO2 and heat treatments under ambient light
my.Temp.aov <- aov(Temp ~ CO2 * Heat_treat, 
              data = daytime.cast[daytime.cast$Light == "ambient light", ])
summary(my.Temp.aov)

my.Temp_both.aov <- aov(Temp ~ CO2 * Heat_treat, 
              data = daytime.cast)
summary(my.Temp_both.aov)
