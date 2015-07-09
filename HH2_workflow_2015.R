# HH2 workflow

# 1) call import script
# 2) call processing script
# 3) call summarise script

# call helper scripts
source("~/AgFace/R_scripts/HH2_soil_moisture_import.R")
source("~/AgFace/R_scripts/HH2_PR2_treatment_info_2015.R")
source("~/AgFace/R_scripts/HH2_file_processing_2015.R")
source("~/AgFace/R_scripts/HH2_visualisation_and_summary_2015.R")

# set working directory
setwd("~/AgFace/2015/PR2_Soil_moisture")

#1) HH2_soil_moisture_import.R
#my.data <- HH2Import("PR2_20150611.csv", sensor.type = "PR2")

#2) HH2_file_processing.R
#sm <- HH2SoilMoistureProcess(my.data, is.first = TRUE)

# next file
my.data2 <- HH2Import("PR2_20150706.csv", sensor.type = "PR2")

sm <- HH2SoilMoistureProcess(my.data2, is.first = FALSE)

write.table(sm, file = "out.csv", row.names = F, sep = ",")

#3) HH2_visualisation_and_summary.R
# HH2Visual(mydata)


