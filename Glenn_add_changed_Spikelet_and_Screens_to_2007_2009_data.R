# Get the changes in DC 65 Spikelet wt (g) and DC90 Screenings (<2 mm) (%)
# into the data file for Glenn simple Anova data

# set working directory
setwd("~/AgFace/2007_2009")

# load previously imported data
load("~/AgFace/2007_2013/Agface_Plant_production_2007_to_2013.RData")

# paras to  extract
# DC 65 Spikelet wt (g) and DC90 Screenings (<2 mm) (%)
to_extract <- c("Spikelet.wt..g.", "Screenings...2mm.....")

to_keep <- c(descript.para, to_extract)

# extract the columns that Glenns wants
substitute_data <- Agface[, names(Agface) %in% to_keep]

# restrict the new data to the years of the original data 
substitute_data <- substitute_data[substitute_data$Year >= 2007 &
                                   substitute_data$Year <= 2009, ]

save(substitute_data, 
     file = "Substitute_data_Glenn.RData", 
     compress = TRUE)
