# Analyse Nitrogen and Sulfur data from Oct - Nov 2010

# import libraries
require(plyr)
require(reshape)
require(ggplot2)

# set working directory
setwd("~/AgFace/2010/Data_files_for_Markus/Nitrogen_and_Sulfur")

# call the AgFACE helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# import data
df <- read.csv("N_and_S_data.csv")

# capitalise the Organ names (using function from helper script)
df$Organ <- .simpleCap(df$Organ)

# replace "Grain" with "Seed" for consistency with other data
df$Organ <- gsub("Grain", "Seed", df$Organ)

# define as factor instead of character
df$Organ    <- as.factor(df$Organ)

# add/modify the CO2 treatment information
df$CO2_treatment <- gsub("A", "ambient",  df$CO2_treatment)
df$CO2_treatment <- gsub("E", "elevated", df$CO2_treatment)

# specify "CO2_treatment" as factor
df$CO2_treatment <- as.factor(df$CO2_treatment)

# format the date as a real date
df$Date <- as.POSIXct(as.character(df$Date), 
                      format = "%d/%m/%Y",
                      tz = "Australia/Melbourne")

# Prepare data for batch plotting
df.melt <- melt(df,
                id = c("Sample_ID", "Date", "Cultivar", "Organ", "Ring", "Plot", "CO2_treatment"))

# do some batch graphs
# create one plot per parameter - only for data measured at growth CO2 levels
my_plots <- dlply(df.melt,
                  .(variable),
                  function(x) {
                  MyMultPlot(x, x$variable, "Date", "value", "CO2_treatment", "Organ", "Cultivar")})

pdf(file = "Nitrogen_and_Sulfur_Oct_Nov_2010.pdf")
        print(my_plots)
dev.off()

# re-name the main data table
Nitrogen_Sulfur_data <- df

# save the data
save.image("Nitrogen_and_Sulfur_2010.RData", compress = TRUE)
