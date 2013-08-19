# prepare Walpeup 2008-2009 data
# format has to match plant production data from Horsham


# load packages
require(xlsx)


# set working directory to the general Plant production folder
setwd("~/AgFace/Plant_Production")

# load AgFACE helper script
# source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# these files have been shuffled around outside of R to match the desired format
walp2008 <- read.xlsx("../2008/Walpeup_FACE_2008_MRS_Plant_production_summary_5_markus.xls",
                      sheetName = "Final")

walp2009 <- read.xlsx("../2009/Walpeup_FACE_2009_MRS_Plant_Production_Summary_5_markus.xls",
                      sheetName = "Final")

# WUE contains "Div/0" errors from excel. Those get imported with an error message. Replacing the error message with "NA"
imp_errors <- grep("jav", walp2008$WUE)
walp2008$WUE[imp_errors] <- as.numeric(NA)
walp2008$WUE <- as.numeric(walp2008$WUE)

imp_errors <- grep("jav", walp2009$WUE)
walp2009$WUE[imp_errors] <- as.numeric(NA)
walp2009$WUE <- as.numeric(walp2009$WUE)

# and for Dead.leaf.green.leaf in 2009
imp_errors <- grep("jav", walp2009$Dead.leaf.green.leaf)
walp2009$Dead.leaf.green.leaf[imp_errors] <- as.numeric(NA)
walp2009$Dead.leaf.green.leaf <- as.numeric(walp2009$Dead.leaf.green.leaf)

# getting rid of empty columns
# rather radical, just looking for columns with names that start with "NA." after import
cnames2008 <- names(walp2008)
noname2008 <- grep("^NA.", cnames2008)

walp2008 <- walp2008[, -noname2008]

cnames2009 <- names(walp2009)
noname2009 <- grep("^NA.", cnames2009)

walp2009 <- walp2009[, -noname2009]

# according to Glenn (afternoon chat Monday, July 29), everyting after Seeds.Floret is growth rate-specific and not needed for now. Might be re-calculated later.

to_delete <- c("Duration.EM.to.DC30..d.",
                "Duration.DC30.DC65..d.",
                "Duration.DC65.DC90..d.",
                "GR.Em.to.DC30.g.m2.d",
                "GR.DC30.DC65.g.m2.d",
                "GR.DC65.DC90.g.m2.d",
                "Relative.GR.DC30.DC65..g.m2.d.g.",
                "Relative.GR.DC65.DC90..g.m2.d.g.",
                "Specific.GR.Em.DC30..g.m2.d.LAI",
                "Specific.GR.DC30.DC65..g.m2.d.LAI",
                "N.uptake.Rate.DC30.DC65.g.m2.d",
                "N.Uptake.Rate.DC65.DC90..g.m2.d.")

cols_to_delete <- which(names(walp2008) %in% to_delete)
walp2008 <- walp2008[, -cols_to_delete]

cols_to_delete <- which(names(walp2009) %in% to_delete)
walp2009 <- walp2009[, -cols_to_delete]

# check some columns betweenteh two years
names(walp2008)[161]
names(walp2009)[161]
names(walp2008)[171]
names(walp2009)[171]
names(walp2008)[63]
names(walp2009)[63]

# there are only spelling errors left by now (hopefully)
# using the names from 2008 and apply them to 2009
walp2008.orig <- walp2008
walp2009.orig <- walp2009

# here goes
names(walp2009) <- names(walp2008)


# put the data together
walp <- rbind(walp2008, walp2009)

# add the NDVI.volume. column to match the format of Horsham
walp$NDVI.volume. <- as.numeric(NA)


# correct some inconsistencies in the data
walp$TOS <- as.factor(as.character(gsub(" ", "", walp$TOS))) # get rid of spaces
walp$Stage <- as.factor(as.character(gsub(" ", "", walp$Stage)))
walp$Year <- as.factor(walp$Year)
walp$PlotTrt <- as.factor("YitpiN0")
walp$Sampling <- as.factor(gsub("G", "g", walp$Sampling))
walp$Sampling <- as.factor(gsub("M", "m", walp$Sampling))
walp$CO2[walp$RingTrt == "CO2"]    <- "eCO2"
walp$CO2[walp$RingTrt == "No CO2"] <- "aCO2"
walp$CO2 <- as.factor(walp$CO2)
walp$Irrigation <- as.factor("Rain")
walp$TrialName <- as.factor("Walpeup")
walp$RingID <- as.factor(walp$RingID)

# dump the file to disk for manual comparison
write.table(walp2008.orig,
            file = "re-formatted_Walpeup_2008.csv",
            sep = ",",
            row.names = FALSE)
write.table(walp2009.orig,
            file = "re-formatted_Walpeup_2009.csv",
            sep = ",",
            row.names = FALSE)

write.table(walp,
            file = "Walpeup_2008_2009.csv",
            sep = ",",
            row.names = FALSE)

save.image("Walpeup_2008_2009.RData", compress = TRUE)
