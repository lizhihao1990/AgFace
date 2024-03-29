# import and create overview graphs for Antioxidant data

# data from file "Gesamtdaten antioxidants.xlsx" 

# Markus Löw, June 2013


# load packages
require(reshape)
require(ggplot2)

# set working directory
setwd("~/AgFace/2010/Data_files_for_Markus/Antioxidants")

# load the helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# import data from csv file
# this csv file has been manually assembled from "Gesamtdaten antioxidants.xlsx"
df <- read.csv("Antioxidant_data_from_Gesamtdaten_antioxidants.csv")

# June 24, 2013: phonecall with Sabine regarding different dates for Stems compared to other organs: correct sampling dates are: 28/10, 19/11/, 30/11 (and 6/10 for some)

# June 28, 2013: meeting with Sabine. Sample from 19/11, Zebu, Stem, has erroneoulsy a value of "0" for GSH and ASC. Should be treated as missing sample. Value deleted in original data.

# format the date is a real date
df$Date <- as.POSIXct(as.character(df$Date), 
                      format = "%d/%m/%Y",
                      tz = "Australia/Melbourne")

# add the CO2 treatment information
# create column for CO2_treatment with initial information

df$CO2_treatment <- "none"
df$CO2_treatment[which(df$Ring %in% ambient_rings_2010 )] <- "ambient"
df$CO2_treatment[which(df$Ring %in% elevated_rings_2010)] <- "elevated"

# specify "CO2_treatment" as factor
df$CO2_treatment <- as.factor(df$CO2_treatment)

# Prepare data for batch plotting
df.melt <- melt(df,
                id = c("Date", "Ring", "Organ", "Sample_ID", "Cultivar", "CO2_treatment"),
                measured = names(df)[6:9])

# create one plot per parameter
my_plots <- dlply(df.melt,
                  .(variable),
                  function(x) {
                  MyMultPlot(x, x$variable, "Date", "value", "CO2_treatment", "Organ", "Cultivar")})

pdf(file = "Antioxidants_Oct_Nov_2010.pdf")
        print(my_plots)
dev.off()

# only the leaves - just to confirm and compare the graph with the graph from the original files
p <- ggplot(df[df$Organ == "Leaf", ], aes(x = Date, y = GSH_nmol_per_g))
        p <- p + stat_summary(aes(colour = CO2_treatment, linetype = Cultivar), 
                              fun.data = mean_sdl, mult = 1 )
        p <- p + stat_summary(aes(colour = CO2_treatment, linetype = Cultivar), 
                              fun.data = mean_sdl, mult = 1, geom = "line" )
        p <- p + theme_bw()
p
# Result of the comparison: graph is identical with the one shown in the original data (xlsx file).

# Some simple stats
my.anova <- aov(GSH_nmol_per_g ~ (CO2_treatment * Cultivar) + Date,
                data = df[df$Organ == "Leaf", ])
summary(my.anova)

# Custom anova function to avoid failures with missing samples
MyAov <- function(x) {
          samples <- unique(x$value)
          ifelse((length(samples) > 1), 
          out.aov <- summary(aov(x$value ~ x$Cultivar * x$CO2_treatment + x$Date)), 
          out.aov <- NULL)
}

# using dlply to run anovas on all parameters
# prone to hick-ups with missing sampes
#my.anovas <- dlply(df.melt[df.melt$Organ == "Seed", ],
#                  .(variable, Organ),
#                  function(x) MyAov(x))

# better, simpler solution to run multiple Anovas                
# out <- sapply(split(df.melt, list(df.melt$Organ)), function(x) summary(aov(x$value ~ x$Cultivar * x$CO2_treatment + x$Date)))

my.anovas <- sapply(split(df.melt, list(df.melt$Organ)), MyAov)

# re-name the main data table
Antioxidants_data <- df

# save the data
save.image("Antioxidants_2010.RData", compress = TRUE)
