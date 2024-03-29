# import and analyse 2013 Cc data

library(ggplot2)
library(plyr)
setwd("/home/loewi/AgFace/2013/Cc/")

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
df$ring <- as.numeric(as.character(df$ring))
# CO2 treatment
df$CO2 <- NA
df$CO2[df$ring %in% ambient_rings_2013]  <- "aCO2"
df$CO2[df$ring %in% elevated_rings_2013] <- "eCO2"
df$CO2 <- as.factor(df$CO2)

# some plots


p <- ggplot(df, aes(x = FTime, y = Ci))
   p <- p + geom_point(aes(colour = organ))
   p <- p + facet_grid(. ~ Filename)
   p <- p + theme_bw()
p

p <- ggplot(df, aes(x = Ci, y = Photo))
   p <- p + geom_point(aes(colour = CO2))
   p <- p + facet_grid(. ~ Filename)
   p <- p + theme_bw()
p

p <- ggplot(df, aes(x = Ci, y = Photo))
   p <- p + geom_point(aes(colour = organ))
   p <- p + facet_grid(. ~ CO2)
   p <- p + theme_bw()
p

head(df[df$organ == "medium_light", ])

