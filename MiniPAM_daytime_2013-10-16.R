# Daytime measurement analysis Oct 16, 2013
# one flash at ambient light level, second flash after 30 sec of light level 10 (Act-int)

setwd("~/AgFace/2013/MiniPAM/2013-10-16/Mid-day")

# load helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")
# call loading script
source("~/AgFace/R_scripts/MiniPAM_minicom_import.R")

# load libraries
library(ggplot2)
library(reshape)

# import data
daytime <- LoadMiniPAM("minicom.cap")

# get rid of failed measurements with no leaf
daytime <- daytime[!daytime$F == 0 & !daytime$Fm. == 0, ]


# Meta data
# First measurement "A" bad, was test measurement
# Second measurement "A" ok
#A Ring 11, plot J, outside
#B Ring 11, plot J, chamber
#C Ring 11, plot U, chamber
#D Ring 11, plot U, outside

# create a ring number
daytime$Ring <- 0

# Assign the ring numbers based on protocol
daytime$Ring[daytime$Mark %in% LETTERS[1:4]]   <- 11
daytime$Ring[daytime$Mark %in% LETTERS[5:8]]   <- 10
daytime$Ring[daytime$Mark %in% LETTERS[9:12]]  <- 15
daytime$Ring[daytime$Mark %in% LETTERS[13:16]] <- 16
daytime$Ring[daytime$Mark %in% LETTERS[17:20]] <- 7
daytime$Ring[daytime$Mark %in% LETTERS[21:24]] <- 6
daytime$Ring[daytime$Mark == "Y"] <- 2
daytime$Ring[daytime$Mark == "Z"] <- 4
daytime$Ring <- as.factor(daytime$Ring)

# CO2 treatment
daytime$CO2 <- NA
daytime$CO2[daytime$Ring %in% ambient_rings_2013]  <- "aCO2"
daytime$CO2[daytime$Ring %in% elevated_rings_2013] <- "eCO2"
daytime$CO2 <- as.factor(daytime$CO2)


# Plot identifiers
daytime[daytime$Ring == 11, ] # was the first ring, has some test-measurements

# First measurement A, had "low signal error" changed meas int
daytime <- daytime[!daytime$No. <= 4, ]

# Assigning plot identifiers
daytime$Plot <- "zzz"

daytime$Plot[(daytime$Mark == "A" | daytime$Mark == "B") & daytime$Ring == 11] <- "J"
daytime$Plot[(daytime$Mark == "C" | daytime$Mark == "D") & daytime$Ring == 11] <- "U"
daytime$Plot[(daytime$Mark == "E" | daytime$Mark == "F") & daytime$Ring == 10] <- "G"
daytime$Plot[(daytime$Mark == "G" | daytime$Mark == "H") & daytime$Ring == 10] <- "O"
daytime$Plot[(daytime$Mark == "I" | daytime$Mark == "J") & daytime$Ring == 15] <- "F"
daytime$Plot[(daytime$Mark == "K" | daytime$Mark == "L") & daytime$Ring == 15] <- "N"
daytime$Plot[(daytime$Mark == "M" | daytime$Mark == "N") & daytime$Ring == 16] <- "F"
daytime$Plot[(daytime$Mark == "O" | daytime$Mark == "P") & daytime$Ring == 16] <- "N"
daytime$Plot[(daytime$Mark == "Q" | daytime$Mark == "R") & daytime$Ring == 7]  <- "J"
daytime$Plot[(daytime$Mark == "S" | daytime$Mark == "T") & daytime$Ring == 7]  <- "R"
daytime$Plot[(daytime$Mark == "U" | daytime$Mark == "V") & daytime$Ring == 6]  <- "F"
daytime$Plot[(daytime$Mark == "W" | daytime$Mark == "X") & daytime$Ring == 6]  <- "S"
daytime$Plot[daytime$Mark == "Y" & daytime$Ring == 2]  <- c(rep("C", 4), rep("V", 4))
daytime$Plot[daytime$Mark == "Z" & daytime$Ring == 4]  <- c(rep("S", 4), rep("K", 4))
daytime$Plot <- as.factor(daytime$Plot)

# Assign heat treatments
standard_heat_treatment_description <- c(rep("elevated temperature", 2),
                                        rep("ambient temperature", 2),
                                        rep("elevated temperature", 2),
                                        rep("ambient temperature", 2))
                                        
daytime$Heat_treat <- NA
daytime$Heat_treat[daytime$Ring == 11] <- c(rep("ambient temperature", 2), 
                                  rep("elevated temperature", 4),
                                  rep("ambient temperature", 2))
daytime$Heat_treat[daytime$Ring == 10] <- standard_heat_treatment_description
daytime$Heat_treat[daytime$Ring == 15] <- standard_heat_treatment_description
daytime$Heat_treat[daytime$Ring == 16] <- standard_heat_treatment_description
daytime$Heat_treat[daytime$Ring == 7]  <- standard_heat_treatment_description

# extra measuremnt when the changing of the MiniPAM mark went wrong:
# getting rid of measurement No 52, Ring
daytime <- daytime[!daytime$No. == 52, ]

daytime$Heat_treat[daytime$Ring == 6]  <- standard_heat_treatment_description
daytime$Heat_treat[daytime$Ring == 2]  <- standard_heat_treatment_description
daytime$Heat_treat[daytime$Ring == 4]  <- standard_heat_treatment_description

daytime$Heat_treat <- as.factor(daytime$Heat_treat)

# identification of first flash at ambient light vs high light
daytime$Light <- rep(c("ambient light", "high light"), nrow(daytime)/2)
daytime$Light <- as.factor(daytime$Light)

# Battery at end of measurement still at 12.4V, int. Temp 37°C, Zero off 9, sat width 0.8
# Damp 2, Gain 1, Meas int 4
# Start ~ 13:00, finish 14:30

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
     p <- p + labs(y = expression(phi[PS2]))
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

#[daytime.cast$Light == "ambient light", ]

# boxplot
p <- ggplot(daytime.cast, 
            aes(x = CO2, y = Yield))
        p <- p + geom_boxplot(aes(fill = Heat_treat))
        p <- p + facet_grid(. ~ Light)
        p <- p + theme_bw()
        p <- p + labs(y = expression(phi[PS2]),
                      x = expression(CO[2]~treatment))
p
ambient_and_high_phiPS2 <- p

# boxplot
p <- ggplot(daytime.cast, 
            aes(x = CO2, y = Temp))
        p <- p + geom_boxplot(aes(fill = Heat_treat))
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
