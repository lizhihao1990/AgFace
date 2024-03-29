# HH2 ML3 workflow

# 1) call import script
# 2) call processing script


# call helper scripts
source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")
source("~/AgFace/R_scripts/HH2_file_processing.R")
source("~/AgFace/R_scripts/HH2_visualisation_and_summary.R")

# set working directory
setwd("~/AgFace/2015/HH2_Soil_moisture/Pre-season_calibration/ML3")

#1) HH2_soil_moisture_import.R
#mydata <- HH2Import("2015-01-12-PR2-Sumps.csv")
mydata <- HH2Import("ML3-2015-03-05.csv", sensor.type = "ML3")

#2) HH2_file_processing.R
mydata <- HH2SoilMoistureProcess(mydata, is.first = TRUE, pre.season = TRUE)

ML3.precal <- mydata
