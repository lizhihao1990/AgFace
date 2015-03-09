# Analyse example data from Joe Panozzo following phonecall Jan 30, 2015

# set working directory, set to match your file structure
setwd("~/AgFace/2014/Agronomy")

# libraries
library(reshape2)
library(plyr)

# import data
df <- read.csv("FACE_Wheat_2014-15_Quadrat_Quality_Data_270115.csv",
               skip = 1)

# define experimental factors
df$Ring <- as.factor(df$Ring)
df$Plot <- as.factor(df$Plot)

# rename misleading Samples to the actual thing, i.e Cultivar
names(df) <- gsub("SAMPLE", "Cultivar", names(df))

# Treatent definitions are actually traits
names(df) <- gsub("Treatment.Definitions", "Trait", names(df))

# in the spreadsheet, the column "Treatment" refers actually to PlotIDs
names(df) <- gsub("Treatment", "PlotID", names(df))

# rename paramters to something more meaningful
names(df) <- gsub("GWT", "Thousand_grains_weight", names(df))
names(df) <- gsub("SCR", "Screenings", names(df))
names(df) <- gsub("GP", "Grain protein", names(df))

# get rid of forbidden symbols
df$Trait <- gsub("\"", "", df$Trait)
df$Trait <- as.factor(df$Trait)

# remove empty, unnecessary lines
df <- df[df$CO2 != "", ]

# reshape data to have all parameters in long format to allow quick batch processing
df.melt <- melt(df, 
                id = c("BARCODE", "Cultivar", "Ring", "Plot", "CO2", "PlotID", "Trait", "Grain.Type", "Quality.Comments"),
                measured = c("GWT", "SCR", "GP"))

# define statistical model
# as discussed on the phone, a simple anova suffices, because
# no missing data, no special nesting (just within rings), only one sampling point

# nested anova
my.model <- formula(value ~ CO2 * Cultivar + Error(Ring))

# test multiple traits in a loop
# return results as a list

# have to get rid of "Pests" and Generational response experiment, as only one Cultivar is used
df.melt <- df.melt[df.melt$Trait != "Pests", ]
df.melt <- df.melt[df.melt$Trait != "Generational Response", ]

# get rid of all experiments without data
df.melt <- df.melt[!is.na(df.melt$value),  ]

# [df.melt$Treatment.Definitions == "Pests", ]
my.result <- dlply(df.melt,
                   .(Trait, variable),
                   function(x){
                   my.aov <- summary(aov(my.model, data = x))
                   }
                   )
my.result

# export anova text file
sink(file = "Panozzo_List_of_Anova_results.txt")
print(my.result)
sink()

# function to create boxplots
my.plot <- function(data) {
    require(ggplot2)
    
    my.para <- unique(data$variable)
    my.trait <- unique(data$Trait)
    my.y.label <- interaction(my.trait, my.para)
    p <- ggplot(data, aes(x = Cultivar, y = value))
      p <- p + geom_boxplot(aes(fill = CO2))
      p <- p + theme_bw()
      p <- p + labs(y = my.y.label)
    return(p)
}

# create boxplots for all traits
my.plots <- dlply(df.melt,
                  .(Trait, variable),
                  function(x) my.plot(x))

# export boxplots as pdf file
pdf(file = "Panozzo_Boxplots.pdf",
    width = 9, height = 7)
print(my.plots)
dev.off()

