# import and analyse 2013 Night-time respiration

library(ggplot2)
library(plyr)
setwd("~/AgFace/2013/Nighttime_Respiration/2013_10_30")

# load helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# read all csv files
my.files <- list.files(pattern = "*.csv")

df <- ldply(my.files, function(x){
        filename <- x
        if (!exists ("y")){
                y <- read.delim(x,
                             skip = 14,
                             header = TRUE)
                y$Filename <- as.factor(filename)}
                
        else {
                z <- read.delim(x,
                                skip = 14,
                                header = TRUE)
                z$Filename <- as.factor(filename)
                y <- rbind(y, z)}
        return(y)
})

# get rid of comment rows
df <- df[!is.na(df$CO2R), ]

# clean up the meta data
# remove whitespace
df[, c("variety", "organ", "ring")] <- lapply(df[, c("variety", "organ", "ring")],
       function(x) {
       x <- gsub(" ", "", x)
       })

# CO2 treatment
df$CO2 <- NA
df$CO2[df$ring %in% ambient_rings_2013]  <- "aCO2"
df$CO2[df$ring %in% elevated_rings_2013] <- "eCO2"
df$CO2 <- as.factor(df$CO2)

# calculate averages per CO2 treatment, cultivar, and leaf
# six logs per leaf to get one mean value

df.mean <- aggregate(df, by=list(CO2_treat = df$CO2, 
                                 Cultivar = df$variety, 
                                 Leaf = df$organ), mean)

p <- ggplot(df.mean, aes(x = Cultivar, y = Photo))
   p <- p + geom_boxplot(aes(colour = CO2_treat))
   p <- p + geom_point(aes(colour = CO2_treat), position = position_dodge(width = 0.90))
   p <- p + theme_bw()
   p <- p + labs(y = expression(CO[2]~uptake~rate~at~night),
                 x = "Cultivar")
p
respiration_boxplot <- p

r.aov <- aov(Photo ~ CO2_treat * Cultivar, data = df.mean)
summary(r.aov)
