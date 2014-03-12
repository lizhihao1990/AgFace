# Pre-dawn measurement analysis Oct 17, 2013
# rapid light response

setwd("~/AgFace/2013/MiniPAM/2013-10-17_predawn")

# load helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")
# call loading script
source("~/AgFace/R_scripts/MiniPAM_minicom_import.R")

# load libraries
library(ggplot2)
library(reshape)
library(plyr)


# import data
df <- LoadMiniPAM("minicom.cap")

# get rid of failed measurements with no leaf
df <- df[!df$F == 0 & !df$Fm. == 0, ]

# Meta data
# from lab notebook:
# wrong button pressed for first measurement Fv/Fm. label H
# getting rid of it
df <- df[-65, ]

# the initial Fv/Fm measurements have the label "l".
# They should have the same label as the rapid light response they belong to.

# get the next label for each "l" label
NextLabel <- function(marker, badlabel, position = 1) {
        my.bad  <- which(marker == badlabel)
        marker[my.bad] <- marker[my.bad + position]
        return(marker)
}

df$Mark <- NextLabel(df$Mark, "l")

# Get the experimental data
# A: Ring 2, C, outside
# B: Ring 2, C, inside
# C: Ring 4, K, outside
# D: Ring 4, K, inside
# E: Ring 7, J, outside
# F, Ring 7, J, inside
# G, Ring 6, F, outside
# H, Ring 6, F, inside
# I, ring 10, G, outside
# J, Ring 10, G, inside
# K, ring 11, J, outside
# L, Ring 11, J, inside
# M, Ring 16, F, outside
# N, Ring 16, F, inside
# O, Ring 15, F, outside
# P, Ring 15, F, inside

df$Ring <- NA
df$Ring[df$Mark == "A" | df$Mark == "B"] <- 2
df$Ring[df$Mark == "C" | df$Mark == "D"] <- 4
df$Ring[df$Mark == "E" | df$Mark == "F"] <- 7
df$Ring[df$Mark == "G" | df$Mark == "H"] <- 6
df$Ring[df$Mark == "I" | df$Mark == "J"] <- 10
df$Ring[df$Mark == "K" | df$Mark == "L"] <- 11
df$Ring[df$Mark == "M" | df$Mark == "N"] <- 16
df$Ring[df$Mark == "O" | df$Mark == "P"] <- 15
df$Ring <- as.factor(df$Ring)

# CO2 treatment
df$CO2 <- NA
df$CO2[df$Ring %in% ambient_rings_2013]  <- "aCO2"
df$CO2[df$Ring %in% elevated_rings_2013] <- "eCO2"
df$CO2 <- as.factor(df$CO2)

# Plot information
df$Plot <- NA
df$Plot[df$Mark == "A" | df$Mark == "B"] <- "C"
df$Plot[df$Mark == "C" | df$Mark == "D"] <- "K"
df$Plot[df$Mark == "E" | df$Mark == "F"] <- "J"
df$Plot[df$Mark == "G" | df$Mark == "H"] <- "F"
df$Plot[df$Mark == "I" | df$Mark == "J"] <- "G"
df$Plot[df$Mark == "K" | df$Mark == "L"] <- "J"
df$Plot[df$Mark == "M" | df$Mark == "N"] <- "F"
df$Plot[df$Mark == "O" | df$Mark == "P"] <- "F"
df$Plot <- as.factor(df$Plot)

# heat treatment, as the experiment went without a hitch,
# using the fixed order, 9 measurements per Marker
my.heat.treat <- c(rep("ambient temperature", 9),
                   rep("elevated temperature", 9))

df$Heat_treat <- my.heat.treat

p <- ggplot(df, aes(x = PAR, y = Yield))
        p <- p + geom_line(aes(colour = Ring))
        #p <- p + geom_smooth()
        p <- p + facet_grid(Heat_treat ~ CO2)
        p <- p + theme_bw()
        p <- p + labs(x = expression("PPFD ["~mu*mol~m^-1~s^-2~"]"),
                      y = expression("PSII operating efficiency "~phi[PSII]))
p
phi_PSII_plot <- p

p <- ggplot(df, aes(x = PAR, y = ETR))
        p <- p + geom_line(aes(colour = Ring))
        p <- p + geom_smooth()
        p <- p + facet_grid(Heat_treat ~ CO2)
        p <- p + theme_bw()
p

# Waiting-in-line fit
# Ritchie (2008)
# f(x) = ETR_max * k * PAR * exp(1 - k * x)

#define starting values for the curve fitting
#MiniPAM values:
# ETR_factor <- 300
# k <- 0.000009

nls.fit <- ddply(df,
            .(CO2, Heat_treat, Mark),
            function(x) {
            
            nls.fit <- nls(ETR ~ ETR_factor * k * PAR * exp(1 - k * PAR),
                       data = x,
                       start = c(ETR_factor = 10, k = 0.000009),
                       control = nls.control(maxiter = 700,
                                    warnOnly = FALSE,
                                    minFactor = 6.10352e-09))
            
            my.ETR <- coef(nls.fit)[1]
            my.k   <- coef(nls.fit)[2]
            my.output <- data.frame(ETRf = my.ETR, k = my.k)   
            return(my.output)
            }
)
nls.fit$k_ratio <- 1/nls.fit$k

my.ETRf.aov <- aov(ETRf ~ CO2 * Heat_treat, data = nls.fit)
summary(my.ETRf.aov)

my.kw.aov <- aov(k_ratio ~ CO2 * Heat_treat, data = nls.fit)
summary(my.kw.aov)

mean_kw <- ddply(nls.fit,
                 .(CO2, Heat_treat),
                 summarise,
                 Mean_ETRf = mean(ETRf),
                 SD_ETRf = sd(ETRf),
                 Mean_kw = mean(k_ratio),
                 SD_kw = sd(k_ratio))

p <- ggplot(nls.fit, aes(x = CO2, y = ETRf))
        p <- p + geom_boxplot(aes(colour = Heat_treat))
        p <- p + theme_bw()
        p <- p + labs(y = expression("maximum ETR ("~ETR[f]~")"),
                      x = expression(CO[2]~"treatment"))
p

max_ETR_plot <- p

p <- ggplot(nls.fit, aes(x = CO2, y = k_ratio))
        p <- p + geom_boxplot(aes(colour = Heat_treat))
        p <- p + theme_bw()
        p <- p + labs(y = expression(PPFD~at~maximum~ETR~"("~1/k[w]~")"),
                      x = expression(CO[2]~"treatment"))
p

kw_plot <- p


#p <- ggplot(df, aes(x = PAR, y = ETR))
#        p <- p + stat_summary(fun.data = "mean_sdl", mult = 1)
#p
                
p <- ggplot(df, aes(x = PAR, y = ETR))
        # p <- p + stat_summary(fun.data = "mean_sdl", mult = 1)
        p <- p + geom_point(aes(colour = Ring))
        p <- p + geom_smooth(colour = "black", se = F,
                 method="nls", 
                 formula = y ~ ETR_factor * k * x * exp(1 - k * x),
                 start = c(ETR_factor = 10, k = 0.000009),
                 control = nls.control(maxiter = 700,
                             warnOnly = FALSE,
                             minFactor = 6.10352e-09))
        p <- p + facet_grid(Heat_treat ~ CO2)
        p <- p + theme_bw()
        p <- p + labs(y = expression("ETR ["~mu*mol~m^-1~s^-2~"]"),
                      x = expression("PPFD ["~mu*mol~m^-1~s^-2~"]"))
p
ETR_light_response <- p

p <- ggplot(df[df$PAR == 0, ], aes(x = CO2, y = Yield))
     p <- p + geom_boxplot(aes(colour = Heat_treat))
#     p <- p + stat_summary(aes(colour = Heat_treat),
#                           fun.data = "mean_sdl", mult = 1,
#                           position = position_dodge(width = 0.90))
     p <- p + theme_bw()
     #p <- p + labs(y = expression(phi[PS2]))
     p <- p + labs(x = expression(CO[2]~"treatment"),
                y = expression("Dark adapted maximum quantum yield"~F[v]/F[m]))
p
FvFm_boxplot <- p

my.FvFm_aov <- aov(Yield ~ CO2 * Heat_treat, data = df[df$PAR == 0, ])
summary(my.FvFm_aov)
