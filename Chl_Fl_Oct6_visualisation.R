# Chlorophyll fluorescence overview Oct 6, 2010
# Data from Sabine
# some guesswork went into identifying the samples
# this R script visualises the data to help with correctly identifying the samples.

# set working directory
setwd("~/AgFace/2010/Data_files_for_Markus")

# load packages
require(ggplot2)

# import data
df <- read.csv("Chl_fl_Sabine_06_Oct_assembled_by_ML.csv")

# define the time as POSIXct (to help identify the samples)
df$HHMMSS <- as.POSIXct(as.character(df$HHMMSS), format = "%H:%M:%S")

# define Date as POSIXct
df$Date <- as.POSIXct(as.character(df$Date), tz = "Australia/Melbourne")

# define ring number as factor
df$Ring <- as.factor(df$Ring)

# graph PhiPS2
p <- ggplot(df, aes(x = Ring, y = PhiPS2))
        p <- p + geom_boxplot(aes(fill = Cultivar))
        p <- p + geom_jitter(aes(colour = Cultivar))
p

# plot based on time to identify groups of samples
# expecting two-minute intervall between samples that belong together
p <- ggplot(df, aes(x = HHMMSS, y = PhiPS2))
        p <- p + geom_point(aes(colour = Ring, shape = Cultivar))
        p <- p + geom_line(aes(colour = Ring))
p

# First two samples identified as Janz, some question regarding the flash-information missing in the original file.
# Ring 1: missing "Janz" samples, but twice the Zebu samples
# based on the pattern of the assessment, first two measurements in a ring are Janz, second ones are Zebu
df$Cultivar[df$Remark == "ML testrun"] <- as.factor("Janz")
df$Ring[df$Remark == "ML testrun"]     <- as.factor(1)

# re-do the factor-levels
df$Cultivar <- as.factor(as.character(df$Cultivar))
df$Ring     <- as.factor(as.numeric(as.character(df$Ring)))

# Ring 8 has too many samples for Zebu. All declining over the measurements
# First one removed as indicated it was replaced by "new sample"
df$bad_data <- FALSE
df$bad_data[df$Ring == 8 & df$Remark == "l"] <- TRUE

# removing the first Zebu due to an existing "new sample"
df$bad_data[df$Ring == 8 & df$Remark == "" & df$Cultivar == "Zebu"] <- TRUE

# plot based on time to identify groups of samples, using "good data" only
p <- ggplot(df[df$bad_data == FALSE, ], aes(x = HHMMSS, y = PhiPS2))
        p <- p + geom_point(aes(colour = Ring, shape = Cultivar))
        p <- p + geom_line(aes(colour = Ring))
p

# Ring 13: lots to do, some guesswork involved
# first samples: not clear, why there was repetition needed. Maybe low Ci and Cond?
# fourth sample has much higher Ci
# new guess: as Ring 14 Zebu is missing, first two samples from Ring 13 are from ring 14.
# the timing of the measurement confirms this. Therefore
df$Cultivar[df$Cultivar == "Janz" & 
           df$Ring == 13 &
           df$FTime < 9692 ] <- as.factor("Zebu")
df$Ring[df$Cultivar == "Zebu" & 
           df$Ring == 13 &
           df$FTime < 9692 ] <- 14

# Back to Ring 13
# both samples for Janz look ok, using the second one assuming that there was a decision made in the field to redo ring 13 Janz.
df$bad_data[df$Ring == 13 & df$Cultivar == "Janz" & df$FTime < 11018] <- TRUE

# prepare data for further use in other scripts
df_orig_Oct06 <- df
Chl_fl_data_Oct06 <- df[df$bad_data == FALSE & !df$Cultivar == "Hartog", ]
rm(df)

# overview on samples
tapply(Chl_fl_data_Oct06$Photo, 
       list(Chl_fl_data_Oct06$Ring, Chl_fl_data_Oct06$Cultivar), 
       length)


# save the data
save.image("Chl_fl_data_Oct06.RData", compress = TRUE)
