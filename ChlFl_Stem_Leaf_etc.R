# Analysis of AgFACE data from 2010
# Chlorophyll fluorescence measurements for Sabine

# set working directory
setwd("~/AgFace/Leaf_Stem_Spike_Awn_2010")

# load libraries
require(ggplot2)
require(gridExtra)


# import data
# from another folder
df <- read.csv("../2010/chlorophyll_fluorescence_summary_Oct-Nov_2010/all_files_combined.csv")

# re-format the data as usable date
df$date <- as.POSIXct(as.character(df$date), tz="GMT", format="%Y-%m-%d")

# add CO2 treatment information based on ring number
ambient_rings  <- c(1, 8, 12, 14)
elevated_rings <- c(3, 5, 9, 13)

# create column for CO2_treatment with initial information
df$CO2_treatment <- "none"

# re-code CO2_treatment based on ring numbers
df$CO2_treatment[which(df$ring %in% ambient_rings )] <- "ambient"
df$CO2_treatment[which(df$ring %in% elevated_rings)] <- "elevated"

# specify "CO2_treatment" as factor
df$CO2_treatment <- as.factor(df$CO2_treatment)

# create information on measurement conditions
df$is_growth_CO2 <- "Not at growth CO2"
df$is_growth_CO2[(df$CO2_treatment == "elevated" & 
                  df$CO2R > 500)   | 
                  df$CO2_treatment == "ambient"] <- "At growth CO2"
# recode as factor
df$is_growth_CO2 <- as.factor(df$is_growth_CO2)

## create sub-sets of data
#at_growth_CO2 <- subset(df, 
#                       (CO2_treatment == "elevated" & 
#                        CO2R > 500)   | 
#                        CO2_treatment == "ambient")

# Create some graphs with ggplot

# PhiPS2
p <- ggplot(df, aes(x = date, y = PhiPS2))
        p <- p + stat_summary(aes(colour = organ), 
                              fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + stat_summary(aes(colour = organ), 
                              fun.data = mean_sdl, mult = 1)
        p <- p + facet_grid(variety ~ is_growth_CO2)
        p <- p + theme_bw()
p

PhiPS2_graph <- p

# qP
p <- ggplot(df, aes(x = date, y = qP))
        p <- p + stat_summary(aes(colour = organ), 
                              fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + stat_summary(aes(colour = organ), 
                              fun.data = mean_sdl, mult = 1)
        p <- p + facet_grid(variety ~ is_growth_CO2)
        p <- p + theme_bw()
        p <- p + ylim(0, 3.5)
p

qP_graph <- p

# qN
p <- ggplot(df, aes(x = date, y = qN))
        p <- p + stat_summary(aes(colour = organ), 
                              fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + stat_summary(aes(colour = organ), 
                              fun.data = mean_sdl, mult = 1)
        p <- p + facet_grid(variety ~ is_growth_CO2)
        p <- p + theme_bw()
p

qN_graph <- p

# arrange several plots on one page (using gridExtra package)
grid.arrange(PhiPS2_graph, qP_graph, qN_graph,
             ncol = 2, nrow = 2)
