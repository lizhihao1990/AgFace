# Import the Plant Production data 2007 - 2009
# based on the revised Plant Production file sent by Glenn on August 22, 2013
# new file sent on Oct 24.

# Next version of the Plant Production file from Glenn, February 10, 2014:

#As I mentioned here in an updated (final!) data set for AOV analysis.  You can copy and paste the data into your template and run as is. 

#Here is what I did: 
#As before, please analyzed just the columns for DC90 that are in bold and the two columns in DC65 (spikelets/head, spikelet wt).  I added two more variables for analysis:  DC65 crop height and DC90 g/tiller (they are in blue).  The changes I made are in red.  If text is red then this is different from the previous data set.  If the entire cell is red this is because I have removed the data and it is to be treated as missing for the purposes of analysis.  Column names in red mean I have changed the names from the previous analysis. 

#Call or email if you have questions.  As before, please produce box plots, AOV and means tables. 

#Thanks, 
#Glenn 

# New in May 2014
# data from 2007 to 2013 became available from Glenn
# two parameters (Spikelets and Screenings) have been corrected in the new data
# these corrected parameters are extracted from the 2007 to 2013 data and imported 
# and merged with the 2007 to 2009 data

# require(xlsx)
require(ggplot2)
require(plyr)
require(reshape)

# set working directory
setwd("~/AgFace/2007_2009")

# df <- read.xlsx("Markus_2007-2009_22Aug2013.xlsx",
#                 sheetName = "Sheet1")

# new file from Glenn
#df <- read.csv("Markus_2007-2009_22Aug2013.csv",
#                header = TRUE)

#df <- read.csv("Markus_2007-2009_24Oct2013.csv",
#                header = TRUE)

# Yet another new version of the data 
#df <- read.csv("Markus_2007-2009_Feb2014.csv",
#                header = TRUE)
# Yet another another version of the data
#df <- read.csv("Markus_2007-2009_14Feb2014.csv",
#                header = TRUE)
# Yet yet another version of the data
df <- read.csv("Markus_2007-2009_20Mar2014.csv",
                header = TRUE)

# Yet yet another version of the data
df <- read.csv("Markus_2007-2009_29May2014.csv",
                header = TRUE)

# get rid of empty rows
df <- df[!df$CO2.TOS.Irr.Cultivar.DATABASE == "", ]

# correct wrong-imported columns
# all imported nicely from .csv
df.orig <- df
# keep a copy of the imported data

# getting rid of empty columns
X.names <- grep("^X.", names(df))
df <- df[, -X.names]

# one left-over "X" column
df$X <- NULL

# getting rid of the unnecessary columns - one by one
df$Crop <- NULL
df$NA.  <- NULL
df$Bay  <- NULL
df$Bay.1  <- NULL
df$CO2.TOS.Irr.Cultivar.DATABASE.1 <- NULL
df$CO2.TOS.Irr.Cultivar.DATABASE.2 <- NULL
df$CO2.TOS.Irr.Cultivar.DATABASE.3 <- NULL
df$CO2.TOS.Irr.Cultivar.DATABASE.4 <- NULL
df$RingID.1 <- NULL
df$RingID.2 <- NULL
df$RingID.3 <- NULL
df$RingID.3 <- NULL
df$RingID.4 <- NULL
df$RingTrt.1 <- NULL
df$RingTrt.2 <- NULL
df$HalfRingID.1 <- NULL
df$HalfRingID.2 <- NULL

# remove additional columns - getting sick of individual removel
to_remove <- c("Plot.Order.1", "TrialID.1", "Year.1",  "PlotID.1",  "CO2.1", "Irrigation.1", 
"TOS.1", "PlotTrt.1", "Sampling.1", "Cultivar.1", "Stage.1", "TrialName.1", "Plot.Order.2", "TrialID.2", "Year.2", "PlotID.2", "CO2.2", "Irrigation.2", "TOS.2", "PlotTrt.2", "Sampling.2", "Cultivar.2", "Stage.2", "TrialName.2")
to_remove <- unlist(lapply(to_remove, function(x) {grep(x, names(df))}))

# get rid of the specified columns
df <- df[, -to_remove]

# Email from Glenn regarding problems in the data file

##On Fri, 2013-08-23 at 13:06 +1000, Glenn.Fitzgerald@depi.vic.gov.au wrote:
##Markus,
##> 
##> Thanks for the sharp eye.  A number of the descriptive columns have been created at different times by different people.  The ones that are correct are the columns with the complete treatments listed under "database" title (sorry, don't have access to my files this minute) eg, eCO2 YitpiN0 TOS1 Sup (or something like this).  I would parse these columns and create your own individual treatment columns based on these.  Walpuep had only CO2, YitpiN0 and TOS.  No irrigation. Only ring number and sampling time (eg DC30) are important for Walpeup. 
##> 
##> Yes, I sorted all the data within each year so that each row in the file represents the same treatment across the sampling times.  Going on this you should be able to organize them.
##> 
##> Hope that helps.
##> 
##> Glenn

# that mean throwing away all columns that are not needed.
# Only ones to keep are
# CO2.TOS.Irr.Cultivar.DATABASE 
# TrialID
# RingID (PlotID and HalfRingID not reliable for DC65)
# Year
# Stage (Stage has to be created on the fly as it is not reliable in the original file!)

# cut the data into three data frames, one for each DC.
# col 56: start of DC65
# col 112: start of DC30
# only keeping the above mentioned columns
# all hard-coded! Danger when the original data file changes

# email from GLenn Tue, 25 Mar 2014 12:20:52 +1100
#Markus, 

#I have advanced my paper a bit more.  I am attaching an updated data set.  The columns ET, EU, EV have changes.  ET and EU just have new names.  Column EV is a new data set (all changes in red).  Could you run the analysis on the data in EV?  CO2XTOSXIrrigationXCultivar for Horsham and CO2XTOS at Walpeup with stats and mean tables. 

#Cheers, 

#Glenn 


DC30 <- df[, c(1:55)]
DC30$Stage <- as.factor("DC30")
#DC30 <- DC30[, -c(1, 6:13, 16)]


DC65 <- df[, c(1:16, 56:96)]
DC65$Stage <- as.factor("DC65")
#DC65 <- DC65[, -c(1, 6:13, 16)]

DC90 <- df[, c(1:16, 97:ncol(df))]
DC90$Stage <- as.factor("DC90")
#DC90 <- DC90[, -c(1, 6:13, 16)]


# Duplicate treatment descriptors
names(df)[grep("\\.1$", names(df))]

# get rid of the suffixes
names(DC65) <- gsub("\\.1$", "", names(DC65))
names(DC65) <- gsub("\\.2$", "", names(DC65))

names(DC90) <- gsub("\\.1$", "", names(DC90))
names(DC90) <- gsub("\\.2$", "", names(DC90))
names(DC90) <- gsub("\\.3$", "", names(DC90))

# remove the stage identifier form the column names
names(DC30) <- gsub("^DC30.", "", names(DC30))
names(DC65) <- gsub("^DC65.", "", names(DC65))
names(DC90) <- gsub("^DC90.", "", names(DC90))


# DC30 names that are not in DC65
add.names <- names(DC30)[!(names(DC30) %in% names(DC65))]

# add empty columns to be added to DC65
add.cols <- DC30[, add.names]

columns <- ncol(add.cols)
rows    <- nrow(add.cols)

dummy <- as.data.frame(matrix(nrow = rows, ncol = columns))
names(dummy) <- add.names

DC65d <- cbind(DC65, dummy)

# DC65 names that are not in DC30
add.names <- names(DC65)[!(names(DC65) %in% names(DC30))]

add.cols <- DC65[, add.names]

columns <- ncol(add.cols)
rows    <- nrow(add.cols)

dummy <- as.data.frame(matrix(nrow = rows, ncol = columns))
names(dummy) <- add.names

DC30d <- cbind(DC30, dummy)

# merge the data from DC30d and DC65d
DC3065 <- merge(DC30d, DC65d, all = TRUE)

# DC90 names that are not in DC3065
add.names <- names(DC90)[!(names(DC90) %in% names(DC3065))]

add.cols <- DC90[, add.names]

columns <- ncol(add.cols)
rows    <- nrow(add.cols)

dummy <- as.data.frame(matrix(nrow = rows, ncol = columns))
names(dummy) <- add.names

DC3065d <- cbind(DC3065, dummy)

# DC3065 names that are not in DC90
add.names <- names(DC3065)[!(names(DC3065) %in% names(DC90))]

add.cols <- DC3065[, add.names]

columns <- ncol(add.cols)
rows    <- nrow(add.cols) - nrow(DC90)

dummy <- as.data.frame(matrix(nrow = rows, ncol = columns))
names(dummy) <- add.names

DC90d <- cbind(DC90, dummy)

# put all growth stages together
DCall <- merge(DC3065d, DC90d, all = TRUE)

# Re-create the treatments from the entry in the database column
strsplit(as.character(DCall$CO2.TOS.Irr.Cultivar.DATABASE), " ")
DCall$CO2 <- sapply(
             strsplit(
                      as.character(DCall$CO2.TOS.Irr.Cultivar.DATABASE), " "), 
                      `[`, 1)
DCall$TOS <- sapply(
             strsplit(
                      as.character(DCall$CO2.TOS.Irr.Cultivar.DATABASE), " "), 
                      `[`, 2)

DCall$Irrigation[DCall$TrialID == "Horsham"] <- sapply(
            strsplit(
                      as.character(
                      DCall$CO2.TOS.Irr.Cultivar.DATABASE[DCall$TrialID == "Horsham"]), " "), 
                      `[`, 3)

DCall$Irrigation[DCall$TrialID == "Walpeup"] <- "Rain"

# some hoops to jump through to get the Cultivar information complete
DCall$Cultivar <- as.character(DCall$Cultivar)

DCall$Cultivar[DCall$TrialID == "Horsham"] <- sapply(
            strsplit(
                      as.character(
                      DCall$CO2.TOS.Irr.Cultivar.DATABASE[DCall$TrialID == "Horsham"]), " "), 
                      `[`, 4)

DCall$Cultivar[DCall$TrialID == "Walpeup"] <- sapply(
            strsplit(
                      as.character(
                      DCall$CO2.TOS.Irr.Cultivar.DATABASE[DCall$TrialID == "Walpeup"]), " "), 
                      `[`, 3)

# get rid of Nitrogen information as part of the Cultivar
# Nitrogen information is given in another column
DCall$Cultivar <- gsub("YitpiN\\+", "Yitpi", DCall$Cultivar)
DCall$Cultivar <- gsub("YitpiN0", "Yitpi", DCall$Cultivar)

# grab the Nitrogen information as well!!
DCall$Nitrogen <- "N0"
DCall$Nitrogen[grep("N0", DCall$CO2.TOS.Irr.Cultivar.DATABASE)] <- "N0"
DCall$Nitrogen[grep("N\\+", DCall$CO2.TOS.Irr.Cultivar.DATABASE)] <- "N+"

DCall[, c("Cultivar", "CO2", "TOS", "Irrigation", "Nitrogen")] <- lapply(DCall[, c("Cultivar", "CO2", "TOS", "Irrigation", "Nitrogen")], as.factor)

# Re-order Nitrogen_treatment, as N0 should be sorted before N+
DCall$Nitrogen <- factor(DCall$Nitrogen, levels = c("N0", "N+"))

# Re-arrange the treatment columns
DCall <- DCall[, c(1:10, 99, 11:98)]


# export to csv
write.table(DCall,
            file = "DC30and65and90_Horsham_Walpeup_2007_2009.csv",
            row.names = F,
            sep = ",",
            na = "")

save.image("../Plant_Production/Plant_Production_2007_2009_Glenn_May29_2014.RData", compress = TRUE)


# not needed any more - data corrected in the 2007 to 2009 file


# import the newly corrected data from Glenn May 2014
# merge DCall with the corrected screenlets and screenings data from GLenn May 2014
load("~/AgFace/2007_2009/Substitute_data_Glenn.RData")

# delete the "wrong" data from DCall
# keep a copy of the un-altered DCall data frame
DCall.pre.change <- DCall
correct_paras <- c("Screenings...2mm.....", "Spikelet.wt..g.")
to_delete <- names(DCall) %in% correct_paras
# DCall <- DCall[, !(to_delete)]

# prepare substitute data to match DCall
substitute_data <- substitute_data[, !(names(substitute_data) %in% c("PlotID.1", "PlotID.2"))]
names(substitute_data) <- gsub("Ntreat", "Nitrogen", names(substitute_data))
substitute_data$Crop <- NULL
substitute_data$RingTrt <- NULL
substitute_data$PlotTrt <- NULL
substitute_data$CO2.TOS.Irr.Cultivar.DATABASE <- gsub("_", " ", substitute_data$CO2.TOS.Irr.Cultivar.DATABASE)
substitute_data$CO2.TOS.Irr.Cultivar.DATABASE <- as.factor(as.character(substitute_data$CO2.TOS.Irr.Cultivar.DATABASE))
substitute_data$PlotID <- as.factor(as.character(substitute_data$PlotID))
substitute_data$Stage <- gsub("DC31", "DC30", substitute_data$Stage)
substitute_data$Stage <- as.factor(substitute_data$Stage)
substitute_data$Sampling <- NULL
substitute_data$Plot.Order <- NULL
substitute_data$HalfRingID[substitute_data$TrialID == "Walpeup"] <- NA
# re-order N factor levels
substitute_data$Nitrogen <- factor(substitute_data$Nitrogen, levels = c("N0", "N+"))

# get rid of some DCall data
DCall$RingTrt <- NULL
DCall$PlotTrt <- NULL
DCall$TrialName <- NULL
DCall$Sampling <- NULL
DCall$Plot.Order <- NULL

# halfringID data for Walpeup is sometimes wrong and never helpful, removing it
DCall$HalfRingID[DCall$TrialID == "Walpeup"] <- NA

# merge the data with the 'old' 2007 to 2009 data
myDCall <- merge(DCall, substitute_data,
                 all.x = TRUE)

# comparison of the spikelet data
my.subs <- substitute_data
names(my.subs) <- gsub("Spikelet.wt..g.", "Spikelet.wt..g.new", names(my.subs))
my.comp <- merge(DCall, my.subs,
                 all.x = TRUE,
                 all.y = TRUE)

write.table(DCall.pre.change, file = "old_data.csv", sep = ",", row.names = FALSE)
write.table(substitute_data, file = "new_data.csv", sep = ",", row.names = FALSE)

write.table(my.comp, file = "Comparison_old_new.csv", sep = ",", row.names = FALSE)

library(ggplot2)
p <- ggplot(my.comp, aes(x = Spikelet.wt..g., y = Spikelet.wt..g.new))
  p <-p + geom_point()
p
ggsave(file = "Comparison-old_new_spikelet_data.pdf", width = 7, height = 7)

# compare the amount of available samples
sum(!is.na(my.comp$Spikelet.wt..g.new))
sum(!is.na(my.comp$Spikelet.wt..g.))
