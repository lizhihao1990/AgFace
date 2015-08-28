# HH2 workflow

# 1) call import script
# 2) call processing script
# 3) call summarise script

# call helper scripts
source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")
source("~/AgFace/R_scripts/HH2_PR2_treatment_info_2015.R")
source("~/AgFace/R_scripts/HH2_file_processing_2015.R")
source("~/AgFace/R_scripts/HH2_visualisation_and_summary_2015.R")

# set working directory
setwd("~/AgFace/2015/SoilFace/PR2")

#1) HH2_soil_moisture_import.R
df <- HH2Import("2015-07-15_PR2_SoilFace.csv", 
                sensor.type = "PR2")

rings <- 1:8
ringIDs <- lapply(rings, function(x) rep(x, 4))
RingIDs <- unlist(ringIDs)
df$Ring <- RingIDs
df$Ring <- as.factor(df$Ring)

library(ggplot2)
p <- ggplot(df, aes(x = Ring, y = Percent_Vol))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1)
  #p <- p + geom_boxplot(aes(colour = Ring))
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
p

my.aov <- aov(Percent_Vol ~ Ring, data = df)
summary(my.aov)
# re-order mydf
mydf <- df[with(df, order(Sample, Depth)), ]


p <- ggplot(mydf, aes(x = Percent_Vol, y = Depth))
  p <- p + geom_path(aes(colour = Ring))
  p <- p + scale_y_reverse()
  p <- p + facet_grid(. ~ Ring)
  p <- p + theme_bw()
p

#2) HH2_file_processing.R
#sm <- HH2SoilMoistureProcess(my.data, is.first = TRUE)

# next file
my.data2 <- HH2Import("PR2-20150713.csv", sensor.type = "PR2")

sm <- HH2SoilMoistureProcess(my.data2, is.first = FALSE)

# on July 13, 2015, Russel compared probes
#You will note that reading 107 -110 are done with the old probe that has a broken band. 
#Note the following 
#Reading 107 relates to Sth sump Tube 1 
#Reading 108 relates to Sth sump tube 2 
#Reading 109 relates to Ring 3 Plot 1 (W) 
#Reading 110 relates to Ring 3 Plot 2 (W) 

# extract probe comparison data
probe.comp <- sm[sm$Time > as.POSIXct("2015-07-13 00:00:00"), ]
probe.comp$Device[probe.comp$Sample > 106] <- 2
probe.comp$Sample[probe.comp$Sample == 107] <- 1
probe.comp$Sample[probe.comp$Sample == 108] <- 2
probe.comp$Sample[probe.comp$Sample == 109] <- 3
probe.comp$Sample[probe.comp$Sample == 110] <- 4
probe.comp <- probe.comp[probe.comp$Sample < 5, ]

to.keep <- c("Sample","Depth", "Device", "Percent_Vol", "mV")
small <- probe.comp[, names(probe.comp) %in% to.keep]

library(reshape2)
small.melt <- melt(small,
                   id.vars = c("Sample", "Device", "Depth"))

small.cast <- dcast(small.melt,
                    Sample + Depth ~ variable + Device)
                    
library(ggplot2)
p <- ggplot(small.cast, aes(x = Percent_Vol_1, y = Percent_Vol_2))
  p <- p + geom_abline(slope = 1, yintercept = 0)
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_point(aes(colour = as.factor(Sample)))
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
  p <- p + labs(title = "Comparison old and new profile probe",
                x = "Soil moisture (%), old probe",
                y = "Soil moisture (%), new probe",
                colour = "Tube#")
p
ggsave(file = "Comparison_old_new_profile_probe.pdf",
       width = 13, height = 8)

#write.table(d.melt, file = "out.csv", row.names = F, sep = ",")
#write.table(probe.comp, file = "out.csv", row.names = F, sep = ",")
write.table(sm, file = "Soil_moisture.csv", row.names = F, sep = ",")

#3) HH2_visualisation_and_summary.R
# HH2Visual(mydata)


