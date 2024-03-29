# HH2 workflow

# 1) call import script
# 2) call processing script
# 3) call summarise script

# call helper scripts
source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")
source("~/AgFace/R_scripts/HH2_file_processing.R")
source("~/AgFace/R_scripts/HH2_visualisation_and_summary.R")

# set working directory
setwd("~/AgFace/2014/HH2_Soil_moisture")

#1) HH2_soil_moisture_import.R
mydata <- HH2Import("PR2-2014-12-02.csv", sensor.type = "PR2")

#2) HH2_file_processing.R
mydata <- HH2SoilMoistureProcess(mydata, is.first = FALSE)

#3) HH2_visualisation_and_summary.R
HH2Visual(mydata)
