#####################################
# Assign Plot IDs and treatments to 
# the soil moisture tube ID numbers
#
# Markus Löw, July 2014
# based on tube ID numbers from Russel
# received July 4, 2014
#####################################

setwd("~/AgFace/2014/HH2_Soil_moisture")

TubeIDs <- read.csv("2014_PR2_Soil_Moisture_ID_numbers.csv")

TubeIDs$Year <- 2014
TubeIDs$Ring <- sapply(
                strsplit(
                as.character(TubeIDs$Ring.ID..), " "), "[", 5)
TubeIDs$Ring <- as.numeric(TubeIDs$Ring)
TubeIDs$HH2_tube_number <- sapply(
                           strsplit(
                           as.character(TubeIDs$PR.2.Tube..Number), " "), "[", 7)
TubeIDs$HH2_tube_number <- as.numeric(TubeIDs$HH2_tube_number)

TubeIDs$Treatment <- as.character(TubeIDs$Treatment)

# additional columns
TubeIDs$Tube_treatment <- NA
TubeIDs$Tube_treatment[TubeIDs$Treatment != "Dry Soil" | 
                       TubeIDs$Treatment != "Wet Soil" ] <- "ambient"
TubeIDs$Tube_treatment[TubeIDs$HH2_tube_number ==  1] <- "dry"
TubeIDs$Tube_treatment[TubeIDs$HH2_tube_number ==  2] <- "wet"
TubeIDs$Tube_treatment[TubeIDs$HH2_tube_number == 51] <- "ambient"
TubeIDs$Tube_treatment[grep("Wet", TubeIDs$Treatment)] <- "wet"
TubeIDs$Tube_treatment <- as.factor(TubeIDs$Tube_treatment)

# Treatment names clean-up
TubeIDs$Treatment <- gsub(" ", "", TubeIDs$Treatment)
TubeIDs$Treatment <- gsub("Roots", "Root", TubeIDs$Treatment)
TubeIDs$Treatment <- gsub("wet", "Wet", TubeIDs$Treatment)
TubeIDs$Treatment[TubeIDs$Treatment == "Baresoil"] <- NA

TubeIDs$Treatment <- as.factor(TubeIDs$Treatment)

# Adding in identifier for the species
TubeIDs$Crop <- "Wheat"
TubeIDs$Crop[TubeIDs$Treatment == "Lentil"] <- "Lentil"
TubeIDs$Crop[TubeIDs$HH2_tube_number <= 2 |
             TubeIDs$HH2_tube_number == 51] <- NA
TubeIDs$Crop <- as.factor(TubeIDs$Crop)

# for consistency with other data sets, a treatment info and PlotID is needed
TubeIDs$CO2 <- NA
TubeIDs$Irrigation <- NA

# get lentil field layout
 source("~/AgFace/R_scripts/Lentil_helper.R")

lentil.layout$CO2 <- NULL

# merge TubeIDs with lentil plot information
TubeIDs <- merge(TubeIDs, lentil.layout,
            by.x = c("Ring", "Variety"),
            by.y = c("Ring", "Cultivar"),
            all.x = TRUE)

# add PlotIDs for lentils to the treatment column
TubeIDs$Treatment <- as.character(TubeIDs$Treatment)
TubeIDs$Treatment[!is.na(TubeIDs$PlotId)] <- as.character(TubeIDs$PlotId[!is.na(TubeIDs$PlotId)])
TubeIDs$Treatment <- as.factor(TubeIDs$Treatment)

# Lentil PlotID is now redundant
TubeIDs$PlotId <- NULL

# renaming the Treatment column
names(TubeIDs) <- gsub("Treatment", "PlotID", names(TubeIDs))

names(TubeIDs) <- gsub("Variety", "Cultivar", names(TubeIDs))

names.to.keep <- c("Crop", "Year", "Ring", "PlotID", "CO2", "Irrigation", "Cultivar", "HH2_tube_number", "Tube_treatment")

TubeIDs.orig <- TubeIDs

# select columns to keep
TubeIDs <- TubeIDs[, names(TubeIDs) %in% names.to.keep]

# reorder columns
TubeIDs <- TubeIDs[, names.to.keep]

# merge with position information
positions <- read.csv("HH2_Soilmoisture_tubes_lookup_table_with_coordinates.csv")

positions$Variety <- NULL

TubeIDs <- merge(TubeIDs, positions,
                  all.x = TRUE)

write.table(TubeIDs,
            file = "PR2_Soilmoisture_tubes_lookup_table.csv",
            sep = ",", row.names = FALSE)

save(TubeIDs, 
     file = "PR2_Tubes_lookup_table.RData", 
     compress = TRUE)
