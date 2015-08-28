#' Process soil moisture files: Add treatment information to the samples based on access tube number. Merge new data with previous data.
#' 
#' @description Add treatment information (CO2 treatment, Cultivar, PlotID, Ring, Irrigation regime) to the samples. Optionally, the samples are merged with exisiting data.
#' @param mydf Data frame with a HH2 file for a PR2 profile probe
#' @param is.first Logical. If FALSE, the samples in mydf are merged with existing PR2 data that are stored in a PR2_soil_moisture.RData file in the current working directory. When TRUE, a new PR2_soil_moisture.RData will be created. Defaults to FALSE.
#' @param pre.season Logical. If FALSE, treatment information is added to each sample based on the PR2 sample number. Defaults to FALSE.
#' @param remove.out.of.range Logical. If TRUE, samples that are markes as erroneous in the "Vol_Error" column of the data are removed from the millivolt (mV) data as well. Defaults to TRUE.
#' @return Returns the mydf data frame with added treatment information and (optionally) out of range values removed.

HH2SoilMoistureProcess <- function(mydf, is.first = FALSE, pre.season = FALSE, remove.out.of.range = TRUE) {
 # add treatment information to a HH2 soil moisture data frame
 # load data frame with previously existing data
 # merge new and old data
 # export data frame, replacing the previously existing data frame
 
 if (remove.out.of.range == TRUE) {
 # remove out of range data by replacing the bad mV values with NA based on the Percent_Vol data.
 oor <- which(is.na(mydf$Percent_Vol))
 mydf$mV[oor] <- NA
 }
 
 if (pre.season == FALSE) {
 # load treatment information data frame
 # load("PR2_Tubes_lookup_table.RData") # from 2014 procedure
 source("~/AgFace/R_scripts/HH2_PR2_treatment_info_2015.R")
 
 # merge data with treatment information
 mydf <- merge(mydf, PR2.treat.info,
           by.x = "Sample",
           by.y = "PR2ID",
           all.x = TRUE)
 
 # re-order mydf
 mydf <- mydf[with(mydf, order(Sample, Depth)), ]
 
  # re-arrange columns
 mydf <- mydf[, c("Year", "Trial", "Time", "Crop", "Cultivar", "Ring", "PlotID", "CO2_treatment", "Irrigation", "Device", "Sample", "Treatment", "Plot", "Depth", "Percent_Vol", "Vol_Error", "mV", "mV_Error")]
 }
 # still carrying the useless "Plot" column around that we don't use in the field
 # should it be removed? Yes!
 mydf$Plot <- NULL

 if (is.first == TRUE) {
     sm <- mydf
 } else {
 # load existing data (data frame called "sm")
 load("PR2_soil_moisture.RData")

# if (pre.season == FALSE) {
 # changing Ring 10 wet2 to "wet"
# sm$Tube_treatment[!is.na(sm$Ring) &
#                   sm$Ring == 10 &
#                   sm$PlotID == "Wet2"] <- "wet"
# sm$Tube_treatment <- as.factor(sm$Tube_treatment)
#}
 # add mydf to the bottom of sm

 sm <- rbind(sm, mydf)
 } 
 # make sure we get rid of duplicates, in case the script was run multiple times
 sm <- unique(sm)

# if (pre.season == FALSE) {
# # add CO2, and other treatment information
# source("~/AgFace/R_scripts/AgFace_helper_scripts.R")
# 
# sm$CO2 <- NA
# sm$CO2[sm$Ring %in% ambient_rings_2014]  <- "aCO2"
# sm$CO2[sm$Ring %in% elevated_rings_2014] <- "eCO2"
# sm$CO2 <- as.factor(sm$CO2)
# 
# # make sure we get rid of duplicates, in case the script was run multiple times
# sm <- unique(sm)
# }

# make sure we get rid of duplicates, in case the script was run multiple times
 sm <- unique(sm)
 
 # save the soil moisture data file to disk
 save(sm, file = "PR2_soil_moisture.RData", compress = TRUE)
 write.table(sm, 
             file      = "PR2_soil_moisture.csv",
             row.names = FALSE,
             sep       = ",",
             na        = "")
 return(sm)
}
