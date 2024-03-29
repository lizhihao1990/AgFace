# Analyse chlorophyll fluorescence data from Oct - Nov 2010

# import libraries
require(plyr)
require(reshape)
require(ggplot2)

# set working directory
setwd("~/AgFace/2010/chlorophyll_fluorescence_summary_Oct-Nov_2010")

# call the AgFACE helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# import data
df <- read.csv("all_files_combined.csv")

# get rid of a stry column
df$X <- NULL

# change some column headers to upper case
names(df) <- gsub("date", "Date", names(df))
names(df) <- gsub("variety", "Cultivar", names(df))
names(df) <- gsub("organ", "Organ", names(df))
names(df) <- gsub("ring", "Ring", names(df))

# capitalise the Cultivar names
df$Cultivar <- gsub("zebu", "Zebu", df$Cultivar)
df$Cultivar <- gsub("janz", "Janz", df$Cultivar)

# capitalise the Organ names (using function from helper script)
df$Organ <- .simpleCap(df$Organ)

# define as factor instead of character
df$Organ    <- as.factor(df$Organ)
df$Cultivar <- as.factor(df$Cultivar)

# format the date as a real date
df$Date <- as.POSIXct(as.character(df$Date), 
                      format = "%Y-%m-%d",
                      tz = "Australia/Melbourne")

# define ring as a factor
df$Ring <- as.factor(df$Ring)

# import the additional data from Sabine, measured on Oct6 (leaves only, slightly different protocol). Data pre-processed in separate script "Chl_Fl_Oct6_visualisation.R"
load("../Data_files_for_Markus/Chl_fl_data_Oct06.RData")

# get rid of the column that was introduced during quality control of Oct 06 data
Chl_fl_data_Oct06$bad_data <- NULL

# have to get rid of "remark" column as well for compatibility
Chl_fl_data_Oct06$Remark <- NULL

# convert the HHMMSS column back to factor
Chl_fl_data_Oct06$HHMMSS <- as.factor(
                strftime(Chl_fl_data_Oct06$HHMMSS,"%H:%M:%S"))

# merge the data from Oct 06 with the other data
df <- rbind(df, Chl_fl_data_Oct06)

# re-do the factor levels for Cultivar to get rid of left-overs
df$Cultivar <- as.factor(as.character(df$Cultivar))

# add the CO2 treatment information
# create column for CO2_treatment with initial information

df$CO2_treatment <- "none"
df$CO2_treatment[which(df$Ring %in% ambient_rings_2010 )] <- "ambient"
df$CO2_treatment[which(df$Ring %in% elevated_rings_2010)] <- "elevated"

# specify "CO2_treatment" as factor
df$CO2_treatment <- as.factor(df$CO2_treatment)

# add the information on the CO2-settings
# some samples were measured at reciprocal CO2 concentrations
df$is_growth_CO2 <- "unknown growth CO2"
df$is_growth_CO2[(df$CO2_treatment == "elevated" & 
                  df$CO2R > 500)   | 
                  (df$CO2_treatment == "ambient" &
                   df$CO2R < 400)] <- "At growth CO2"
# on Oct 6, 2010, Sabine did something similar. However, the ambient plants were measured at elevated CO2
df$is_growth_CO2[df$CO2_treatment == "ambient" & 
                 df$CO2R > 500] <- "Not at growth CO2"
df$is_growth_CO2[df$CO2_treatment == "elevated" & 
                 df$CO2R < 500] <- "Not at growth CO2"

df$is_growth_CO2 <- as.factor(as.character(df$is_growth_CO2))


# keep a copy of the original data frame
df.orig <- df

# re-ordering the columns in the data frame to make further processing easier
df <- df[, c(1:4, 53, 54, 5:52)]

# find out which colums are defined as logical or as factor
# these columns will mess up the later "melt" statement.
col.num.logical <- sapply(df[, 1:ncol(df)], is.logical)

# get an index of column that are not logical
col.num <- which(col.num.logical == FALSE)
df <- df[, col.num] # keep only the non-logical columns

# get an index of column that are not factor, but keeping the descriptive columns
col.num.factor  <- sapply(df[, 1:ncol(df)], is.factor)
col.num <- which(col.num.factor == FALSE)
df.new <- df[, col.num] # keep only the non-logical columns
df <- cbind(df[, 2:6], df.new) # combine with the descriptive columns
rm(df.new)
# now we only have numeric and integer columns left in the data frame

# check the sample size for each ring
ddply(df,
      .(Date, Ring, Organ, Cultivar, CO2_treatment, is_growth_CO2),
      summarise,
      available_samples = length(Photo))
# result: samples size is n = 1 for each experimental factor. All good.

# Check some individual cases for quality assurance. Optional.
#df[df$Ring     == 3 & 
#   df$Date     == as.POSIXct("2010-11-19") & 
#   df$Organ    == "Leaf" & 
#   df$Cultivar == "Janz", ]

## get rid of unnecessary columns
#columns_to_keep <- c("Date", "Ring", "Cultivar", "CO2_treatment", "Organ", "is_growth_CO2", "Photo", "Cond", "Ci", "Fo.", "Fs", "PhiPS2", "qP", "qN")

## create a small data frame with the relevant parameters only
#df <- df[, columns_to_keep]


df[df$Organ == "Leaf" & df$Photo == min(df$Photo[df$Organ == "Leaf"]), ]
df[df$Organ == "Leaf" & df$Date == as.POSIXct("2010-11-19"), ]

# On Nov 19, some bad gas exchange data measured on "dead" leaves
# Ci and Photo affected
df$Photo[df$Photo < -20 & df$Ci > 1000 & df$Organ == "Leaf"] <- NA
df$Ci[is.na(df$Photo) & df$Ci > 1000 & df$Organ == "Leaf"] <- NA
df$Photo[df$Photo < -6.4 & df$Organ == "Leaf" & df$Ci > 800 & df$is_growth_CO2 == "Not at growth CO2"] <- NA
df$Ci[is.na(df$Photo) & df$Organ == "Leaf" & df$Ci > 800 & df$is_growth_CO2 == "Not at growth CO2"] <- NA

df$Ci[df$Ci > 1000 & !(is.na(df$Ci)) & df$is_growth_CO2 == "At growth CO2" & df$Organ == "Leaf"] <- NA

# getting rid of stomatal conductance smaller than 0
df$Cond[df$Cond < 0 ] <- NA


df$Photo[df$Photo > 28 &
      !is.na(df$Photo) &
      df$Ci < -1000 &
      !is.na(df$Ci) &
      df$Organ == "Leaf"] <- NA

df$Ci[is.na(df$Photo) &
      df$Ci < -1000 &
      !is.na(df$Ci) &
      df$Organ == "Leaf"] <- NA

# negative chlorophyll fluorescence values set to NA
# rather dangerous! But neccessary due to low signal-to-noise ratios
df$Fv..Fm.[df$Fv..Fm. < 0] <- NA
df$PhiPS2[df$PhiPS2 < 0]   <- NA
df$qP[df$qP < 0]           <- NA
df$qP[df$qP > 1]           <- NA

# test graph
p <- ggplot(df[df$Organ == "Leaf" &
               df$is_growth_CO2 == "At growth CO2", ],
               aes(x = Ci, y = PhiPS2))
        p <- p + stat_summary(aes(colour = CO2_treatment), 
                              fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour = CO2_treatment), 
                              fun.data = mean_sdl, mult = 1,
                              geom = "line")
        #p <- p + ylim(c(0,10))
        p <- p + facet_grid(Organ ~ Cultivar, scales = "free_y")
        p <- p + theme_bw()
p

# Prepare data for batch plotting
df.melt <- melt(df,
                id = c("Date", "Cultivar", "Organ", "Ring", "CO2_treatment", "is_growth_CO2"))

# do some batch graphs
# create one plot per parameter - only for data measured at growth CO2 levels
my_plots <- dlply(df.melt[df.melt$is_growth_CO2 == "At growth CO2",],
                  .(variable),
                  function(x) {
                  MyMultPlot(x, x$variable, "Date", "value", "CO2_treatment", "Organ", "Cultivar")})

pdf(file = "Chlorophyll_fluorescence_Oct_Nov_2010_at_growth_CO2_level.pdf")
        print(my_plots)
dev.off()

# now again for the reciprocal CO2 levels
my_plots <- dlply(df.melt[df.melt$is_growth_CO2 == "Not at growth CO2",],
                  .(variable),
                  function(x) {
                  MyMultPlot(x, x$variable, "Date", "value", "CO2_treatment", "Organ", "Cultivar")})

pdf(file = "Chlorophyll_fluorescence_Oct_Nov_2010_not_at_growth_CO2_level.pdf")
        print(my_plots)
dev.off()


# re-name the main data table
Chlorophyll_fluorescence_data <- df

# save the data
save.image("Chlorophyll_fluorescence_2010.RData", compress = TRUE)
