# Process soil moisture files

# import file, add the data to an exisiting data frame

HH2SoilMoistureProcess <- function(mydf, is.first = FALSE, pre.season = FALSE) {
 # add treatment information to a HH2 soil moisture data frame
 # load data frame with previously existing data
 # merge new and old data
 # export data frame, replacing the previously existing data frame
 
 if (pre.season == FALSE) {
 # load treatment information data frame
 load("PR2_Tubes_lookup_table.RData")
 
 # merge data with treatment information
 mydf <- merge(mydf, TubeIDs,
           by.x = "Sample",
           by.y = "HH2_tube_number")
 # re-order mydf
 mydf <- mydf[with(mydf, order(Sample, Depth)), ]
 
  # re-arrange columns
 mydf <- mydf[, c("Year", "Time", "Crop", "Cultivar", "Ring", "PlotID", "CO2", "Irrigation", "Device", "Sample", "Tube_treatment", "Plot", "Depth", "Percent_Vol", "Vol_Error", "mV", "mV_Error")]
 }
 # still carrying the useless "Plot" column around that we don't use in the field
 # should it be removed? Yes!
 mydf$Plot <- NULL

 if (is.first == TRUE) {
     sm <- mydf
 } else {
 # load existing data (data frame called "sm")
 load("PR2_soil_moisture.RData")
 if (pre.season == FALSE) {
 # changing Ring 10 wet2 to "wet"
 sm$Tube_treatment[!is.na(sm$Ring) &
                   sm$Ring == 10 &
                   sm$PlotID == "Wet2"] <- "wet"
 sm$Tube_treatment <- as.factor(sm$Tube_treatment)
}
 # add mydf to the bottom of sm
 sm <- rbind(sm, mydf)
 } 
 # make sure we get rid of duplicates, in case the script was run multiple times
 sm <- unique(sm)

 if (pre.season == FALSE) {
 # add CO2, and other treatment information
 source("~/AgFace/R_scripts/AgFace_helper_scripts.R")
 
 sm$CO2 <- NA
 sm$CO2[sm$Ring %in% ambient_rings_2014]  <- "aCO2"
 sm$CO2[sm$Ring %in% elevated_rings_2014] <- "eCO2"
 sm$CO2 <- as.factor(sm$CO2)
 
 # make sure we get rid of duplicates, in case the script was run multiple times
 sm <- unique(sm)
 }
 # save the soil moisture data file to disk
 save(sm, file = "PR2_soil_moisture.RData", compress = TRUE)
 write.table(sm, 
             file      = "PR2_soil_moisture.csv",
             row.names = FALSE,
             sep       = ",",
             na        = "")
 return(sm)
}
