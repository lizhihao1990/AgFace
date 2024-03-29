# Pre-dawn data from Nov 6, 2013

setwd("~/AgFace/2013/MiniPAM/2013-11-06_pre_dawn/")

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
# wrong button for first sample, another Fv/Fm was done at the start of the light response. This one is redundant, therefore removed.

df <- df[-2, ]

# the initial Fv/Fm measurements have the label "l".
# They should have the same label as the rapid light response they belong to.
df$Mark <- NextLabel(df$Mark, "l")

# add ring numbers, CO2 treatment, Plot, heat treatment, and Heat_shock_number
df$Ring <- NA
df$CO2  <- NA
df$Plot <- NA
df$Heat_treat <- NA
df$Heat_shock <- NA

# heat treatment, fixed order.
# using the fixed order, 9 measurements per and previous heat shock
my.heat.treat <- c(rep("ambient temperature", 9),
                   rep("elevated temperature", 9),
                   rep("prev. elevated temperature", 9))


df$Ring[1:27]  <- 2
df$Plot[1:9]   <- "A"
df$Plot[10:18] <- "A"
df$Plot[19:27] <- "C"
df$Heat_shock[1:18]  <- "15 days post-anthesis"
df$Heat_shock[19:27] <- "5 days pre-anthesis"
df$Heat_treat[1:27]  <- my.heat.treat 


df$Ring[28:54] <- 4
df$Plot[28:45] <- "B"
df$Plot[46:54] <- "K"
df$Heat_shock[28:45] <- "15 days post-anthesis"
df$Heat_shock[46:54] <- "5 days pre-anthesis"
df$Heat_treat[28:54] <- my.heat.treat

df$Ring[55:81] <- 7
df$Plot[55:72] <- "F"
df$Plot[73:81] <- "J"
df$Heat_shock[55:72] <- "15 days post-anthesis"
df$Heat_shock[73:81] <- "5 days pre-anthesis"
df$Heat_treat[55:81] <- my.heat.treat

df$Ring[82:108] <- 6
df$Plot[df$Mark == "I" & df$Ring == 6] <- "J"
df$Plot[df$Mark == "J" & df$Ring == 6] <- "J"
df$Plot[df$Mark == "K" & df$Ring == 6] <- "F"
df$Heat_shock[df$Plot == "J" & df$Ring == 6] <- "15 days post-anthesis"
df$Heat_shock[df$Plot == "F" & df$Ring == 6] <- "5 days pre-anthesis"
df$Heat_treat[82:108] <- my.heat.treat

# one erroneous measurement (wrong button)
df <- df[!(df$F == 337 & df$Fm. == 2086.),]

df$Ring[109:135] <- 10
df$Plot[df$Mark == "K" & df$Ring == 10] <- "K"
df$Plot[df$Mark == "L" & df$Ring == 10] <- "K"
df$Plot[df$Mark == "M" & df$Ring == 10] <- "G"
df$Heat_shock[df$Plot == "K" & df$Ring == 10] <- "15 days post-anthesis"
df$Heat_shock[df$Plot == "G" & df$Ring == 10] <- "5 days pre-anthesis"
df$Heat_treat[109:135] <- my.heat.treat

df$Ring[136:162] <- 11
df$Plot[df$Mark == "N"] <- "C"
df$Plot[df$Mark == "O"] <- "C"
df$Plot[df$Mark == "P"] <- "J"
df$Heat_shock[df$Plot == "C" & df$Ring == 11] <- "15 days post-anthesis"
df$Heat_shock[df$Plot == "J" & df$Ring == 11] <- "5 days pre-anthesis"
df$Heat_treat[136:162] <- my.heat.treat

df$Ring[163:189] <- 16
df$Plot[df$Mark == "Q" & df$Ring == 16] <- "J"
df$Plot[df$Mark == "R" & df$Ring == 16] <- "J"
df$Plot[df$Mark == "S" & df$Ring == 16] <- "F"
df$Heat_shock[df$Plot == "J" & df$Ring == 16] <- "15 days post-anthesis"
df$Heat_shock[df$Plot == "F" & df$Ring == 16] <- "5 days pre-anthesis"
df$Heat_treat[163:189] <- my.heat.treat

# signal low warning in Ring 15
df <- df[!(df$F == 119 & df$Fm. == 695), ]

df$Ring[190:216] <- 15
df$Plot[df$Mark == "T" & df$Ring == 15] <- "J"
df$Plot[df$Mark == "U" & df$Ring == 15] <- "J"
df$Plot[df$Mark == "V" & df$Ring == 15] <- "F"
df$Heat_shock[df$Plot == "J" & df$Ring == 15] <- "15 days post-anthesis"
df$Heat_shock[df$Plot == "F" & df$Ring == 15] <- "5 days pre-anthesis"
df$Heat_treat[190:216] <- my.heat.treat

# re-create a new unique Marker for each sample
my.markers <- unlist(lapply(LETTERS[1:24], function(x) rep(x, 9)))

df$Mark <- my.markers

# CO2 information
df$CO2[df$Ring %in% ambient_rings_2013]  <- "aCO2"
df$CO2[df$Ring %in% elevated_rings_2013] <- "eCO2"

# convert all meta data to factor
df[, c("Mark", "Ring", "CO2", "Plot", "Heat_treat", "Heat_shock")] <- lapply(df[, c("Mark", "Ring", "CO2", "Plot", "Heat_treat", "Heat_shock")], as.factor )

# re-order Heat_shock
df$Heat_shock <- factor(df$Heat_shock, levels = c("5 days pre-anthesis", "15 days post-anthesis"))

# some graphs
p <- ggplot(df, aes(x = PAR, y = Yield))
        p <- p + geom_line(aes(colour = Ring))
        #p <- p + geom_smooth()
        p <- p + facet_grid(Heat_treat ~ CO2)
        p <- p + theme_bw()
        p <- p + labs(x = expression("PPFD ["~mu*mol~m^-1~s^-2~"]"),
                      y = expression("PSII operating efficiency"~phi[PSII]))
p
phi_PSII_plot_time2 <- p

nls.fit <- ddply(df,
            .(CO2, Heat_treat, Heat_shock, Mark),
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

p <- ggplot(nls.fit, aes(x = CO2, y = ETRf))
        p <- p + geom_boxplot(aes(colour = Heat_treat))
        p <- p + theme_bw()
        p <- p + labs(y = expression("maximum ETR ("~ETR[f]~")"),
                      x = expression(CO[2]~"treatment"))
p
max_ETR_plot <- p

p <- ggplot(nls.fit, aes(x = CO2, y = k_ratio))
        p <- p + geom_boxplot(aes(colour = Heat_treat), outlier.colour = NA)
        p <- p + theme_bw()
        p <- p + labs(y = expression(PPFD~at~maximum~ETR~"("~1/k[w]~")"),
                      x = expression(CO[2]~"treatment"))
p
kw_plot <- p


my.ETRf.aov.time2 <- aov(ETRf ~ CO2 * Heat_treat, 
                     data = nls.fit[nls.fit$Heat_shock == "15 days post-anthesis",])
summary(my.ETRf.aov.time2)

my.ETRf.aov.time3 <- aov(ETRf ~ CO2 * Heat_treat, 
                     data = nls.fit)
summary(my.ETRf.aov.time3)


my.kw.aov <- aov(k_ratio ~ CO2 * Heat_treat, 
             data = nls.fit[nls.fit$Heat_shock == "15 days post-anthesis",])
summary(my.kw.aov)

# difference between current ambient and previously shocked plants
recovery.ETRf.aov <- aov(ETRf ~ CO2 * Heat_shock,
                    data = nls.fit[nls.fit$Heat_treat != "elevated temperature",])
summary(recovery.ETRf.aov)

recovery.k_ratio.aov <- aov(k_ratio ~ CO2 * Heat_shock,
                    data = nls.fit[nls.fit$Heat_treat != "elevated temperature",])
summary(recovery.k_ratio.aov)


p <- ggplot(nls.fit[nls.fit$Heat_treat != "elevated temperature",], 
            aes(x = Heat_shock, y = ETRf))
     p <- p + geom_boxplot(aes(colour = CO2))
     p <- p + geom_point(aes(colour = CO2), position = position_dodge(height = 0.8, width = 0.7))
     p <- p + theme_bw()
     p <- p + labs(x = "Heat shock applied")
p

p <- ggplot(nls.fit[nls.fit$Heat_treat != "elevated temperature",], 
            aes(x = Heat_shock, y = k_ratio))
     p <- p + geom_boxplot(aes(colour = CO2))
     p <- p + theme_bw()
     p <- p + labs(x = "Heat shock applied")
p

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

p <- ggplot(df[df$PAR <= 1, ], aes(x = CO2, y = Yield))
     p <- p + geom_boxplot(aes(colour = Heat_treat), outlier.colour = NA)
#     p <- p + geom_point(aes(colour = Heat_treat), 
#                         position = position_dodge(height = 0.8, width = 0.7))
#     p <- p + stat_summary(aes(colour = Heat_treat),
#                           fun.data = "mean_sdl", mult = 1,
#                           position = position_dodge(width = 0.90))
     p <- p + theme_bw()
     #p <- p + labs(y = expression(phi[PS2]))
     p <- p + labs(x = expression(CO[2]~"treatment"),
                y = expression("Dark adapted maximum quantum yield"~F[v]/F[m]))
p
FvFm_boxplot <- p

my.FvFm_aov <- aov(Yield ~ CO2 * Heat_treat, 
                   data = df[df$PAR <= 1 & df$Heat_shock == "15 days post-anthesis", ])
summary(my.FvFm_aov)
