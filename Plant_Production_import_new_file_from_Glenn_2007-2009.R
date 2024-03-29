# Import the Plant Production data 2007 - 2009
# based on the revised Plant Production file sent by Glenn on August 22, 2013
# new file sent on Oct 24.

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

df <- read.csv("Markus_2007-2009_24Oct2013.csv",
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

# getting rid of the unnecessary columns
df$Crop <- NULL
df$NA.  <- NULL
df$Bay  <- NULL
df$Bay.1  <- NULL
df$CO2.TOS.Irr.Cultivar.DATABASE.1 <- NULL
df$RingID.1 <- NULL
df$CO2.TOS.Irr.Cultivar.DATABASE.4 <- NULL
df$RingID.4 <- NULL

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

DC30 <- df[, c(1:55)]
DC30$Stage <- as.factor("DC30")
DC30 <- DC30[, -c(1, 6:13, 16)]


DC65 <- df[, 56:112]
DC65$Stage.1 <- as.factor("DC65")
DC65 <- DC65[, -c(1, 6:13, 16)]

DC90 <- df[, 113:ncol(df)]
DC90$Stage.2 <- as.factor("DC90")
DC90 <- DC90[, -c(1, 6:13, 16)]


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
DCall <- DCall[, c(1:6, 84:87, 7:83)]

# export to csv
write.table(DCall,
            file = "DC30and65and90_Horsham_Walpeup_2007_2009.csv",
            row.names = F,
            sep = ",",
            na = "")

save.image("../Plant_Production/Plant_Production_2007_2009_Glenn_Oct24_2013.RData", compress = TRUE)
