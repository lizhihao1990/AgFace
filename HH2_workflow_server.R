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

# find newest file in current folder
df <- file.info(list.files())

# df is a dataframe with 10 variables
# one variable is 'mtime' the file modification POSIX date
# using mtime to identify the name of the file with the latest mtime
latest.row  <- which.max(df$mtime)
latest.file <- rownames(df[latest.row, ])

#1) HH2_soil_moisture_import.R
mydata <- HH2Import(latest.file)

#2) HH2_file_processing.R
mydata <- HH2SoilMoistureProcess(mydata, is.first = FALSE)

#3) HH2_visualisation_and_summary.R
HH2Visual(mydata)

