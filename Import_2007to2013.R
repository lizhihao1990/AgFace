# Import 2007 - 2013 Agface data

setwd("~/AgFace/2007_2013")

#df <- read.csv("Markus_2007-2013_24Apr2014.csv")
df <- read.csv("Markus_2007-2013_27May2014.csv")


# create a table of the potential changes of PlotIDs within a season
# the experimental design of the rings in 2007, 2008, 2009, used multiple plots to provide enough space for taking samples of each cultivar. The Plot ID that identifes each Cultivar changes over the season.
# From 2010 on, the PlotIDs for a given cultivar were not changed any more within a season.
# names(df)[grep("PlotID", names(df))]

# Conversation with Glenn around May 29, 2014: HalfringIDs for Walpeup are different from Halfring IDs in Horsham. Removing them to avoid confusion.

# extract all PlotIDs, their years, and rings
all.Plot.IDs <- data.frame(df$TrialID, df$Year, df$RingID, df$PlotID, df$PlotID.1, df$PlotID.2)
names(all.Plot.IDs) <- gsub("df\\.", "", names(all.Plot.IDs))

# create a indicator to tell if the plot id changed
all.Plot.IDs$PlotID_is_unique <- ifelse(
                                 all.Plot.IDs$PlotID   == all.Plot.IDs$PlotID.1 &
                                 all.Plot.IDs$PlotID.1 == all.Plot.IDs$PlotID.2,
                                 TRUE, FALSE)

write.table(all.Plot.IDs, 
            file = "PlotID_succession.csv", 
            row.names = FALSE, sep = ",", quote = FALSE)
            
# re-order data frame and identify redundant columns
duplicate.columns.to.keep <- c("PlotID.1", "PlotID.2")

# identify parameters with one or more numbers at the end of their name after a "."
# those might be duplicate names (identified by .1, .2, etc)
to.delete <- names(df)[grep("\\.[[:digit:]]+$", names(df))]

# in addition, get rid of the columns without proper names (identified by "X")
to.delete <- c(to.delete, "X")

# create a list of PloIDs to keep
to.delete <- to.delete[!(to.delete %in% duplicate.columns.to.keep)]
to.delete <- names(df) %in% to.delete

# keep a copy of the original data frame
df.orig <- df

# remove the redundant and unwanted columns
df <- df[, c(names(df)[to.delete == FALSE])]

# remove HalfringIDs from Walpeup data and re-factor
df$HalfRingID[df$TrialID == "Walpeup"] <- NA
df$HalfRingID[df$HalfRingID == ""] <- NA
df$HalfRingID <- as.factor(as.character(df$HalfRingID))

# correct typos
df$TOS <- gsub(" ", "", df$TOS)
df$TOS <- as.factor(df$TOS)

df$Sampling <- gsub("g", "G", df$Sampling)
df$Sampling <- as.factor(df$Sampling)

df$PlotTrt[df$PlotTrt == ""] <- NA

# correct wrong cultivar entries
# the data in the database column is the "gold" standard
# split the database entry to create a table with the treatment components
# only for 2007, other years are supposedly OK
# first, get rid of blanks in the database column
df$CO2.TOS.Irr.Cultivar.DATABASE <- gsub(" ", "_", df$CO2.TOS.Irr.Cultivar.DATABASE)

#splitted.A <- lapply(strsplit(df$CO2.TOS.Irr.Cultivar.DATABASE[df$Year == 2007], 
#                   split="_"), "[", 1)
#splitted.B <- lapply(strsplit(df$CO2.TOS.Irr.Cultivar.DATABASE[df$Year == 2007], 
#                   split="_"), "[", 2)
#splitted.C <- lapply(strsplit(df$CO2.TOS.Irr.Cultivar.DATABASE[df$Year == 2007], 
#                   split="_"), "[", 3)
splitted.D <- lapply(strsplit(df$CO2.TOS.Irr.Cultivar.DATABASE[df$Year == 2007], 
                   split="_"), "[", 4)

df$Cultivar[df$Year == 2007] <- gsub("N0|N\\+", "", splitted.D)
df$Cultivar <- as.factor(df$Cultivar)
df$CO2.TOS.Irr.Cultivar.DATABASE <- as.factor(df$CO2.TOS.Irr.Cultivar.DATABASE)

# extract the Nitrogen information from PlotTrt
# assuming if there is no information, it has no additional nitrogen
df$Ntreat <- "N0"
df$Ntreat[grep("N\\+", df$PlotTrt)] <- "N+"
df$Ntreat <- as.factor(df$Ntreat)

# identify measured parameters (to distinguish from descriptive parameters)
# the measured parameters start with "DC" in their name
measured.para <- names(df)[grep("^DC", names(df))]

# by exclusion, the other parameters are descriptors
descript.para <- names(df)[!names(df) %in% measured.para]

# re-organise the data frame - to have all descriptors in front
df <- df[, c(descript.para, measured.para)]

# re-order the descriptors to have all PlotIDs next to each other
PlotID.names <- names(df)[grep("PlotID", names(df))]
non.PlotIDs <-  descript.para[!descript.para %in% PlotID.names]
df <- df[, c(non.PlotIDs, PlotID.names, measured.para)]

# get the re-ordered descriptors
descript.para.reord <- names(df)[!names(df) %in% measured.para]

# re-shuffling the "wide" data to "long" format
# are there any non-numeric columns in the data frame? Have to remove those before melt() can do its magic

my.nonnumerics <- sapply(df[, measured.para], is.numeric)
my.characters  <- sapply(df[, measured.para], is.character)

# show the non-numeric paramters
my.nonnumerics[my.nonnumerics == FALSE]
my.characters[my.characters == TRUE]

# ok, df$DC90.WUE has a problem with a division by zero message from the spreadsheet.
# replacing with "NA"
df$DC90.WUE[df$DC90.WUE == "#DIV/0!"] <- NA
df$DC90.WUE[df$DC90.WUE == ""] <- NA

# force them to be numeric
df$DC90.WU..mm. <- as.numeric(df$DC90.WU..mm.)
df$DC90.WUE     <- as.numeric(df$DC90.WUE)

library(reshape2)
# put data in "long" format
df.melt <- melt(df,
                id = descript.para.reord)

# re-creating the "Stage" descriptor from parameter names
df.melt$Stage <- as.character(df.melt$Stage)
df.melt$Stage[grep("^DC31", df.melt$variable)] <- "DC31"
df.melt$Stage[grep("^DC65", df.melt$variable)] <- "DC65"
df.melt$Stage[grep("^DC90", df.melt$variable)] <- "DC90"
df.melt$Stage <- as.factor(df.melt$Stage)

# get rid of the Stage-indicator in the parameter names
# too much hassle to get it done with apply, but here the somewhat working idea
get.rid.of <- c("DC31\\.", "DC65\\.", "DC90\\.")
# out <- sapply(get.rid.of, function(x) {
#                           gsub(x, "", df.melt$variable)})

# manually
df.melt$variable <- gsub(get.rid.of[1], "", df.melt$variable)
df.melt$variable <- gsub(get.rid.of[2], "", df.melt$variable)
df.melt$variable <- gsub(get.rid.of[3], "", df.melt$variable)
df.melt$variable <- as.factor(df.melt$variable)

# reshape the data to wide format, merging all data for all stages under their common parameter name
# if there are typos in the parameter names, the same parameter will be put into several columns
# no check for typos in parameter names for now!
Agface <- dcast(df.melt,
                formula = ... ~ variable)

# code to check for descrepancies between Cultivar and PlotTrt
# vectorised to only work on samples with an existing PlotTrt entry
# not needed any more as Cultivar names are now correct (see code above regarding database entry)
#Agface$Cultivar_is_same[is.na(Agface$PlotTrt) == FALSE] <- ifelse(
#  gsub("N0|N\\+", "", 
#  Agface$PlotTrt[is.na(Agface$PlotTrt) == FALSE]) == 
#  Agface$Cultivar[is.na(Agface$PlotTrt) == FALSE],
#  TRUE, FALSE)

# export the data as csv file
write.table(Agface, 
            file = "Agface_Plant_production_2007_to_2013.csv", 
            row.names = FALSE, sep = ",", na = "")

# save as a Rdata for further use
save.image(file = "Agface_Plant_production_2007_to_2013.RData", 
     compress = TRUE)

# comparison with old data fro Spikelets and screenings
myAgface <- Agface[Agface$Year >= 2007 & Agface$Year <= 2009, ]
