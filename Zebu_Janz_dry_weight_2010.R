# Process data from file "Zebu-Janz_dry_weight-width.xlsx"

# The individual data sheets from this file have been soted as individual csv-files.
# File too large (17 MB for so few data) for automatic xlsx to csv conversion. Therefore manual export of each relevant sheet.

# Sheet 1: Phonecall with Sabine June 20, "D" in stem menor and mayor is largest and smallest diameter, data was processed by Julio, therefore the spanish column names. 16 missing values for stem and head, as those organs were not processed on Oct 6, 2010. 
# Second phonecall June 20: Plot "Y" in ring 14 is probably a typo, might be "V". However, does not matter, asl all data is from the reinfed plots, the plot numbers are not needed to analyse the data. They are just used as references.


# import packages
require(reshape)
require(ggplot2)

# call the standard AgFACE R scripts
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# set working directory
setwd("~/AgFace/2010/Data_files_for_Markus/Zebu-Janz_dry_weight_width")

# import csv file
df <- read.csv("Zebu-Janz_dry_weight-width_Sheet1.csv")

# rename the spanish column headers
names(df) <- gsub("Stem_minor_D_width_mm", 
                  "Stem_smallest_diameter_mm", 
                  names(df))

names(df) <- gsub("Stem_mayor_D_width_mm", 
                  "Stem_largest_diameter_mm", 
                  names(df))

# rename "Genotype" to "Cultivar" to match other data sets
names(df) <- gsub("Genotype", "Cultivar", names(df))

# format the date is a real date
df$Date <- as.POSIXct(as.character(df$Date), 
                      format = "%d/%m/%Y",
                      tz = "Australia/Melbourne")

# define "Ring" as factor instead of numeric
df$Ring <- as.factor(df$Ring)

# capitalise the Cultivar names (using function from helper script)
df$Cultivar <- tolower(df$Cultivar)
df$Cultivar <- .simpleCap(df$Cultivar)
df$Cultivar <- as.factor(df$Cultivar)

# add CO2 treatment information based on ring number
# now part of the AgFACE helper scripts
# ambient_rings_2010  <- c(1, 8, 12, 14)
# elevated_rings_2010 <- c(3, 5, 9, 13)

# create column for CO2_treatment with initial information
df$CO2_treatment <- "none"
# re-code CO2_treatment based on ring numbers
df$CO2_treatment[which(df$Ring %in% ambient_rings_2010 )] <- "ambient"
df$CO2_treatment[which(df$Ring %in% elevated_rings_2010)] <- "elevated"

# specify "CO2_treatment" as factor
df$CO2_treatment <- as.factor(df$CO2_treatment)

# calculate SLA
# leaf samples were standardised to 3 cm length
# Leaf area (m) = leaf width (mm) / 10000 * 0.03 m
# SLA (m2 kg-1) = Leaf area (m) / Dry weight (kg)

# df$Leaf_width_m <- df$Leaf_width_mm / 10000

Leaf_sample_length_m <- 0.03

# not storing the Leaf area in the dataframe
Leaf_area_m2 <- df$Leaf_width_mm / 10000 * Leaf_sample_length_m

df$SLA_m2_per_kg <- Leaf_area_m2 / (df$Three_cm_leaf_dry_weight_g / 1000)

# check some results
#df[df$Date == as.POSIXct("2010-10-06") & 
#   df$Genotype == "JANZ" &
#   df$Ring == 1, ]


# create graphs for each relevant paramter

# reshape date into the "long" format"
df.melt <- melt(df,
                id = c("Date", "Ring", "Plot", "Cultivar", "CO2_treatment"))

# create plot function (now part of the AgFACE helper functions)
#my_mult_plot <- function(dataframe, label, xaxis, yaxis, treatment_sep, facet_var) {
#  axis_label <- unique(label)
#  # workaround for specifying facets from strings in a function from Hadley Wickham:
#  # http://r.789695.n4.nabble.com/ggplot2-proper-use-of-facet-grid-inside-a-function-td906018.html
#  facets <- facet_grid(paste(facet_var, "~ .")) 
#  
#  p <- ggplot(dataframe, aes_string(x = xaxis, y = yaxis))
#       p <- p + stat_summary(aes_string(colour = treatment_sep),
#                             fun.data = mean_sdl, mult = 1)
#       p <- p + stat_summary(aes_string(colour = treatment_sep),
#                             fun.data = mean_sdl, mult = 1, geom = "line")
#       p <- p + ylab(axis_label)
#       # p <- p + facet_wrap(facet_var ~ .)
#       p <- p + facets
#       p <- p + theme_bw()
#       
#  return(p)
#}


# create one plot per parameter
my_plots <- dlply(df.melt,
                  .(variable),
                  function(x) {
                  MyMultPlot(x, x$variable, "Date", "value", "CO2_treatment", "Cultivar", ".")})

pdf(file = "Dry_weight_and_width_graphs.pdf")
        print(my_plots)
dev.off()

# re-organise the data to match the format of the data from ohter experiments
# i.e. a "Organ" column is needed

# extract information on the organ from the "variable" colum in the "df.melt" data frame
leaf_samples <- grep("Leaf", df.melt$variable, ignore.case = TRUE)
stem_samples <- grep("Stem", df.melt$variable, ignore.case = TRUE)
head_samples <- grep("Head", df.melt$variable, ignore.case = TRUE)
SLA_samples  <- grep("SLA", df.melt$variable)

df.melt$Organ <- "None"
df.melt$Organ[leaf_samples] <- "Leaf"
df.melt$Organ[SLA_samples]  <- "Leaf" #  adding SLA to the "Leaf" group
df.melt$Organ[stem_samples] <- "Stem"
df.melt$Organ[head_samples] <- "Head"
df.melt$Organ <- as.factor(df.melt$Organ)

# cast the "df.melt" data frame
# the wide format matches the data from other experiments
df.wide <- cast(df.melt)

# re-name the data table for export
Dry_weight_and_width_data <- df.wide

# save the data
save.image("Dry_weight_and_width_2010.RData", compress = TRUE)
