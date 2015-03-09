# merge and analyse 2012 sowing and harvest Agface soil moisture data

# Markus Löw, Feb 2015

# data from Russel Argall, received Feb 11, 2015

setwd("~/AgFace/2012/Soil_moisture_2012")

# import Agface helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# ==== Sowing soil moisture =======

# import sowing soil oisture data
df.sowing <- read.csv("2012_Sowing_grav_soil_moisture.csv")

# get rid of field pea data, and repeats
df.sowing <- df.sowing[df.sowing$Crop == "Wheat", ]

# get rid of "Site"
df.sowing$Site <- NULL

# rename a few things
names(df.sowing) <- gsub("Ring.No", "Ring", names(df.sowing))

# Add CO2 treatment information
df.sowing$CO2_treatment <- NA
df.sowing$CO2_treatment[df.sowing$Ring %in% ambient_rings_2012] <- "aCO2"
df.sowing$CO2_treatment[df.sowing$Ring %in% elevated_rings_2012] <- "eCO2"

# Add halfring ID
df.sowing$RingPos <- NA
df.sowing$RingPos[df.sowing$Plot %in% common_plot_names_east] <- "E"
df.sowing$RingPos[df.sowing$Plot %in% common_plot_names_west] <- "W"

# Irrigation information
irrigated <- interaction(supplement_loc_2012$Ring, supplement_loc_2012$supp)
irrigated <- gsub("east", "E", irrigated)
irrigated <- gsub("west", "W", irrigated)

# Add irrigation information based on halfring
df.sowing$Irrigation <- "rainfed"
df.sowing$Irrigation[interaction(df.sowing$Ring, df.sowing$RingPos) %in% irrigated] <- "supp"

change.to.factor <- c("Sample", "Ring", "CO2_treatment", "Irrigation", "RingPos")
df.sowing[, names(df.sowing) %in% change.to.factor] <- lapply(df.sowing[, names(df.sowing) %in% change.to.factor], as.factor)


# ==== Harvest soil moisture =======

df.harvest <- read.csv("2012_Harvest_grav_soil_moisture.csv")

# get rid of everthing that is not wheat
df.harvest <- df.harvest[df.harvest$Crop == "Wheat", ]

# cleanup of names
df.harvest$Year <- gsub(" AGFACE ", "", df.harvest$Year)
df.harvest$Year <- gsub("HSM", "", df.harvest$Year)
df.harvest$Sample <- gsub("Soil Moisture Bulk", "", df.harvest$Sample)
names(df.harvest) <- gsub("X", "Remark", names(df.harvest))

# get rid of meaningless columns
df.harvest$Site   <- NULL
df.harvest$Sample <- NULL

# rename a few things
names(df.harvest) <- gsub("Ring.No", "Ring", names(df.harvest))

# Add CO2 treatment information
df.harvest$CO2_treatment <- NA
df.harvest$CO2_treatment[df.harvest$Ring %in% ambient_rings_2012] <- "aCO2"
df.harvest$CO2_treatment[df.harvest$Ring %in% elevated_rings_2012] <- "eCO2"

# Add halfring ID
df.harvest$RingPos <- NA
df.harvest$RingPos[df.harvest$Plot %in% common_plot_names_east] <- "E"
df.harvest$RingPos[df.harvest$Plot %in% common_plot_names_west] <- "W"

# Add irrigation information based on halfring
df.harvest$Irrigation <- "rainfed"
df.harvest$Irrigation[interaction(df.harvest$Ring, df.harvest$RingPos) %in% irrigated] <- "supp"

# define the correct data type for some parameters
# change to factor
change.to.factor <- c("Sample", "Ring", "CO2_treatment", "Irrigation", "RingPos")
df.harvest[, names(df.harvest) %in% change.to.factor] <- lapply(df.harvest[, names(df.harvest) %in% change.to.factor], as.factor)

# change to numeric
df.harvest$Year <- as.numeric(as.character(df.harvest$Year))

# ======== Prepare sowing and harvest data frame for merging
# check names
names(df.harvest) <- gsub("wt", "Wt", names(df.harvest))

#df.sowing$Depth <- as.character(df.sowing$Depth)
#df.harvest$Depth <- as.character(df.harvest$Depth)

# ===== merge data ===

df <- rbind(df.sowing, df.harvest)

# sort factor levels
df$Depth <- factor(as.character(df$Depth),
                   levels = c("0-10", "10-20", "20-40", 
                              "40-60", "60-80", "80-100", 
                              "100-120"))

# calculate soil core length for each depth
unique.depths <- as.character(sort(unique(df$Depth)))
sdepths <- strsplit(unique.depths, split = "-")

ulimit <- as.numeric(unlist(lapply(sdepths, "[", 1)))
llimit <- as.numeric(unlist(lapply(sdepths, "[", 2)))
core.length <- llimit - ulimit

core.info <- data.frame(Depth = unique.depths,
                        core.length = core.length)
core.info$Depth <- factor(as.character(core.info$Depth),
                   levels = c("0-10", "10-20", "20-40", 
                              "40-60", "60-80", "80-100", 
                              "100-120"))

# merge core.info with df to add length of soil core to each sample
df <- merge(df, core.info)


# calculate bulk density, 
# assuming corer radius is 2.15 cm
# volumetric water content theta = (mass.wet - mass.dry) / (density of water pw * Volume.wet)
# or
# bulk density" dry weight / volume"
# different bulk densities at different depths
# then bulk multiplied with gravimetric to get to volumetric water
# to get bulk density in g/cm3

corer.radius <- 2.15 #cm
df$corer.volume <- pi * corer.radius^2 * df$core.length # V = pi * r^2 * h # in cm^3

df$Bulk.density <- with(df, Dry.Wt / corer.volume)

# according to Roger Armstrong and in line with the handling of 2014 soil mositure data
# discarding samples that are below or above:
# bulk densities > 2 g cm3 get discarded
# bulk densities < 1 g cm3 get discarded

# my decision to discard samples with grav water content above 50

df$Sample.quality <- "Good data"
df$Sample.quality[df$Bulk.density > 2] <- "Unlikely high bulk density, discard sample"
df$Sample.quality[df$Bulk.density < 1] <- "Unlikely low bulk density, discard sample"
#df$Sample.quality[df$Grav_water < 15] <- "Unlikley low gravimetric water content, discard"
df$Sample.quality[df$Grav_water > 50] <- "Unlikley high gravimetric water content, discard"
df$Sample.quality <- as.factor(df$Sample.quality)

# calculate volumetric soil moisture
# bulk density multiplied with gravimetric water content to get to volumetric water
df$Volumetric.water.content <- with(df, Bulk.density * Grav_water)
df$Volumetric.water.content[df$Sample.quality != "Good data"] <- NA

#df[df$Grav_water > 30 & !is.na(df$Grav_water), ]
# rename things ready for export
df.sowing.2012 <- df.sowing
df.harvest.2012 <- df.harvest
df.2012 <- df

save.image("2012_soil_moisture_workspace.RData", compress = TRUE)
