# Lentil experimental setup 2014
# Based on file "Lentil2014SoilMoisture.xlsx"

setwd("~/AgFace/2014/Trial_layout")

ll <- read.csv("2014_Lentil_layout.csv")

ll$CO2 <- gsub("Ambient",  "aCO2", ll$CO2)
ll$CO2 <- gsub("Elevated", "eCO2", ll$CO2)
ll$CO2 <- as.factor(ll$CO2)

# show dput version of the data frame:
# the dput version is added to the Agface_lentil_helper_script.R
dput(ll)
