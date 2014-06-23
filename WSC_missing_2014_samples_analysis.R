# WSC analysis for missing samples 2012

setwd("~/AgFace/2012/Carbohydrates/missing_samples_analysis_April_2014")

# import microtiter plate data
my.files <- list.files(pattern = ".asc")

my.data <- lapply(my.files, function(x) {
        print(x) ## progess indicator
        cur.file <- read.csv(file = x, sep = "\t", skip = 2)
        # cut away the annotations on the bottom of the files
        cur.file <- cur.file[1:8, ]
        # add the file name to the data frame
        cur.file$Name <- x
        return(cur.file)
})

# convert the list elements to one element
my.data <- do.call("rbind", my.data)

# as there was text in each column all columns were imported as factor.
# now changing the relevant columns to numeric
my.data[, c(1:14)] <- lapply(my.data[, c(1:14)], as.character)
my.data[, c(2:14)] <- lapply(my.data[, c(2:14)], as.numeric)

# Get rid of some parts of the file name
my.data$Name <- gsub("Markus11April14_", "", my.data$Name)
my.data$Name <- gsub("\\.asc", "", my.data$Name)
my.data$Name <- as.factor(my.data$Name)

# add a more specific name to the row-vector
names(my.data)[1] <- "Row"

# First overview
plot(my.data$X1)
points(my.data$X2, col = "red")

# Stock-concentrations calculations
# in ml
my.data$Stock <- rep(c(1.5, 1.25, 1, 0.5, 0.25, 0.1, 0.05, 0), 2)
my.data$H20   <- rep(c(0.5, 0.75, 1, 1.5, 1.75, 1.9, 1.95, 2), 2)
my.data$Final_volume <- 2   # ml 
my.data$Fruc_orig    <- 2   # mg
my.data$Standard_Conc_mg_ml  <- my.data$Fruc_orig * (my.data$Stock/my.data$Final_volume)


library(reshape)
my.data.melt <- melt(my.data, id = c("Row", "Name"))

calib.data <- my.data.melt[my.data.melt$variable == "X1" | 
                           my.data.melt$variable == "X2" | 
                           my.data.melt$variable == "Standard_Conc_mg_ml", ]
calib.data$Standard <- calib.data$value[calib.data$variable == "Standard_Conc_mg_ml"]
calib.data <- calib.data[calib.data$variable != "Standard_Conc_mg_ml", ]

my.lm <- lm(value ~ Standard, data = calib.data)
summary(my.lm)

# indicate good and bad calibration values - pipetting errors
calib.data$Good_data    <- TRUE
calib.data$Good_data[3] <- FALSE
calib.data$Good_data[4] <- FALSE

# establishing a relationship between standards and absorption via linear regression 
my.lm <- lm(Standard ~ value, data = calib.data[calib.data$Good_data == TRUE, ])
summary(my.lm)

my.intercept <- coef(my.lm)[1]
my.slope     <- coef(my.lm)[2]

# Calibration curve
library(ggplot2)
p <- ggplot(calib.data, aes(x = Row, y = Standard))
 p <- p + geom_point()
p

p <- ggplot(calib.data[calib.data$Good_data == TRUE, ], 
            aes(x = value, y = Standard))
 p <- p + geom_point(aes(colour = Name))
 p <- p + geom_smooth(method = "lm") 
 p <- p + labs(x = "measured standards [extinction units]",
               y = "Known concentration (mg/ml)")
 p <- p + theme_bw()
p

ggsave(file = "WSC_Standards_April11_2014.pdf", width = 9, height = 7)


# untangle the microtiterplate samples, assign sample IDs
# convert rows and columns to a list

# re-create sample order
plate.numbers <- rep(1:72, each = 2)
plate.letters <- rep(c("A", "B"), 72)
plate.ID <- paste(plate.numbers, plate.letters, sep = "")

# to have a name for each well in the plate, have to add empty names
my.length.names  <- length(plate.ID)
wells.in.plate   <- 192
calibs.in.plate  <- 32
samples.in.plate <- wells.in.plate - calibs.in.plate
missing.samples.in.plate <- samples.in.plate - my.length.names

# merge plate IDs with the "empty" sample names
plate.ID <- c(plate.ID, rep("Empty", missing.samples.in.plate))

# grab the actual samples out of the titer plate data, without calibrations, etc
my.samples <- my.data[, c(4:15)]
my.samples$X <- NULL
my.samples.p1 <- my.samples[my.samples$Name == "plate1", -11]
my.samples.p2 <- my.samples[my.samples$Name == "plate2", -11]

# sample layout of the plate is
# 1A
# 1B
# 2A
# 2B

# convert data frames to vectors
# done separately for each plate to make sure not to mess up the sample ID to sample association
my.samples.p1 <- unlist(as.list(my.samples.p1))
my.samples.p2 <- unlist(as.list(my.samples.p2))

# create a data frame with Sample ID and extinction
my.samples.named <- data.frame(Extinction = c(my.samples.p1, my.samples.p2),
                               Sample_ID  = plate.ID)

# process sample list. This list has weights and detailed info on treatment for each sample.

# import sample weights
samples.list <- read.csv("Missing_samples_analysis.csv")

library(reshape)
samples.melt <- melt(samples.list, 
                     id = c("ID", "Year", "Cultivar", "Ring", "Plot", "Organ", "Remark"),
                     measured = c("Weight_A_mg", "Weight_B_mg"))
# give the melted values a meaningful name
names(samples.melt) <- gsub("value", "Sample_weight_mg", names(samples.melt))

# assign letters A and B to the two repeats from one sample
samples.melt$Sample_ID_Letter <- "A"
samples.melt$Sample_ID_Letter[samples.melt$variable == "Weight_B_mg"] <- "B"

# create the sample ID by merging sample number with repeat letter
samples.melt$Sample_ID <- paste(samples.melt$ID, samples.melt$Sample_ID_Letter, sep = "")
samples.melt$Sample_ID <- as.factor(samples.melt$Sample_ID)
samples.melt$Sample_ID_Letter <- NULL

# merge weights and extinctions
# calculate concentrations mg/ml and mg/mg
# following the existing calculations - "5" is the dilution factor
my.WSC <- merge(samples.melt, my.samples.named)
my.WSC$WSC_mg.ml <- my.WSC$Extinction * my.slope - my.intercept
my.WSC$WSC_mg.mg <- my.WSC$WSC_mg.ml * 5 / my.WSC$Sample_weight_mg

# add treatment information
# run the AgFace helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

my.WSC$CO2 <- NA
my.WSC$CO2[my.WSC$Ring %in% ambient_rings_2012]  <- "aCO2"
my.WSC$CO2[my.WSC$Ring %in% elevated_rings_2012] <- "eCO2"
my.WSC$CO2 <- as.factor(my.WSC$CO2)

# add irrigation information
# have to find a way to vectorise the function

library(plyr)
my.Irrigation <- ddply(my.WSC,
                      .(Year, Ring, Plot),
                      function(x) 
                      .PlotIsIrrigated(x$Plot, x$Ring, x$Year))
my.Irrigation$Irrigation <- as.factor(my.Irrigation$Irrigation)

my.WSC <- merge(my.WSC, my.Irrigation)

my.WSC$Irrigation <- as.factor(my.WSC$Irrigation)

# add the cultivar/traits information
my.WSC$my.Traits <- NA
my.WSC$my.Traits[grep("SB",     my.WSC$Cultivar)] <- "SB"
my.WSC$my.Traits[grep("Silver", my.WSC$Cultivar)] <- "Silverstar"
my.WSC$my.Traits[grep("SSR",    my.WSC$Cultivar)] <- "Silverstar"
my.WSC$my.Traits <- as.factor(my.WSC$my.Traits)

# export the data
# write.table(my.WSC, file = "myWSC.csv", sep = ",", row.names = F)

# aggregation of the two repeats per sample
names(my.WSC) <- gsub("variable", "Weight_index", names(my.WSC))
my.WSC.melt <- melt(my.WSC,
                    id = c("Year", "Ring", "Plot", "Sample_ID", "ID", 
                           "Cultivar", "Organ", "Remark", "Weight_index", 
                           "CO2", "Irrigation", "my.Traits"),
                    measured = c("WSC_mg.ml", "WSC_mg.mg", "Sample_weight_mg", 
                                 "Extinction"))

my.WSC.means <- cast(my.WSC.melt,
                     ID + Year + Ring + Plot + Cultivar + Organ + 
                     Remark + CO2 + Irrigation + my.Traits ~ variable,
                     fun.aggregate = mean)
my.WSC.means <- as.data.frame(my.WSC.means)

# add an identifier regarding who did the analysis
#my.WSC.means$Operator <- as.factor("Markus")

# plot
p <- ggplot(my.WSC.means, aes(x = Irrigation, y = WSC_mg.mg))
  p <- p + geom_boxplot(aes(fill = Cultivar, linetype = CO2))
  p <- p + facet_grid(. ~ Organ)
  p <- p + theme_bw()
p

# Import the exisiting samples for 2012
# load("~/AgFace/Topics/Tillering_2011_2012/Carbohydrate_tillering_workspace.RData")
load(file = "~/AgFace/Topics/Tillering_2011_2012/WSC/Carbohydrate_merged_stages_and_years.RData")

# data frame with the existing data is "Carbs"

#Carbs$Operator <- as.factor("French interns")

my.WSC.merge <- merge(my.WSC.means, Carbs[Carbs$Stage == "DC65", ])
my.WSC.repeats <- my.WSC.merge[my.WSC.merge$Remark == "Repeat", ]

# comparison exisiting data vs newly measured data
p <- ggplot(my.WSC.repeats, 
            aes(x = Conc..mg.mg. * 1.25, y = WSC_mg.mg))
  p <- p + geom_abline(yintercept = 0, slope = 1)
  p <- p + geom_point(aes(colour = Organ))
  p <- p + geom_smooth(method = "lm")
  p <- p + labs(y = "Maryse, Markus [mg/mg]",
                x = "French interns [mg/mg]")
  #p <- p + facet_grid(. ~ my.Traits)
  p <- p + theme_bw()
p

ggsave(file = "WSC_comparison_repeated_samples.pdf", width = 9, height = 7)

my.lm <- lm(WSC_mg.mg ~ Conc..mg.mg., data = my.WSC.repeats )
summary(my.lm)

corr.coef <- coef(my.lm)

# linear correlation between the new lab analysis and the previous one
# reveals an off set
# ok, so there is the need for a correction
# The previous lab analysis used a wrong dilution factor of "4". 
# From the provided protocol this factor should be "5".
# Therefore correcting all previous data with a factor of 1.25
# as the previous data has more samples than the new data, the new data get corrected 
# towards the previous data using the linear correlation coefficient for the intercept.

# corrected repeated samples plot

# testing the correction with the repeated samples only.
my.WSC.repeats.cor <- my.WSC.repeats

# accounting for the wrong dilution in the previous data
dilution_cor_factor <- 1.25
my.WSC.repeats.cor$Conc..mg.mg.cor <- my.WSC.repeats.cor$Conc..mg.mg. * dilution_cor_factor

my.WSC.repeats.cor$WSC_mg.mg.cor <- (my.WSC.repeats.cor$WSC_mg.mg - corr.coef[1])

p <- ggplot(my.WSC.repeats.cor, 
            aes(x = Conc..mg.mg.cor, y = WSC_mg.mg.cor))
  p <- p + geom_abline(yintercept = 0, slope = 1)
  p <- p + geom_point(aes(colour = Organ))
  p <- p + geom_smooth(method = "lm")
  p <- p + labs(y = "corrected Maryse, Markus [mg/mg]",
                x = "corrected French interns [mg/mg]")
  #p <- p + facet_grid(. ~ my.Traits)
  p <- p + theme_bw()
p

ggsave(file = "WSC_comparison_repeated_samples_after_correction.pdf", width = 9, height = 7)

# correcting the dilution in the data from the French interns
Carbs$Conc..mg.mg. <- Carbs$Conc..mg.mg. * dilution_cor_factor

# now appylying the corrections to the full data file for the missing samples
# accounting for the offset between the new analysis and the old one
# old analysis is used as the gold standard, as more data "old" samples.
my.WSC.means$WSC_mg.mg <- my.WSC.means$WSC_mg.mg - corr.coef[1]

# rename objects to match the existing data
names(my.WSC.means) <- gsub("WSC_mg.mg", "Conc..mg.mg.", names(my.WSC.means))
names(my.WSC.means) <- gsub("my.Traits", "Trait", names(my.WSC.means))

# get rid of the repeated samples
my.WSC.for_merge <- my.WSC.means[my.WSC.means$Remark != "Repeat", ]
my.WSC.for_merge$WSC_mg.ml <- NULL
my.WSC.for_merge$Sample_weight_mg <- NULL
my.WSC.for_merge$Extinction <- NULL
my.WSC.for_merge$ID <- NULL
my.WSC.for_merge$Remark <- NULL
my.WSC.for_merge$Stage <- as.factor("DC65")

# add plot IDs
my.WSC.for_merge$RingPos <- NA
my.WSC.for_merge$RingPos[my.WSC.for_merge$Plot %in% common_plot_names_east] <- "E"
my.WSC.for_merge$RingPos[my.WSC.for_merge$Plot %in% common_plot_names_west] <- "W"
my.WSC.for_merge$RingPos <- as.factor(my.WSC.for_merge$RingPos)
my.WSC.for_merge$my.HalfringID <- interaction(my.WSC.for_merge$Year,
                                              my.WSC.for_merge$Ring,
                                              my.WSC.for_merge$RingPos)
my.WSC.for_merge$my.HalfringID <- as.factor(my.WSC.for_merge$my.HalfringID)

# other additions
my.WSC.for_merge$Ring <- as.factor(my.WSC.for_merge$Ring)
my.WSC.for_merge$Year <- as.factor(my.WSC.for_merge$Year)
my.WSC.for_merge$Environment <- interaction(my.WSC.for_merge$Year, my.WSC.for_merge$Irrigation)
my.WSC.for_merge$Environment <- gsub("Supp", "Sup", my.WSC.for_merge$Environment)
my.WSC.for_merge$Environment     <- as.factor(my.WSC.for_merge$Environment)
my.WSC.for_merge$Ord.Environment <- as.factor(my.WSC.for_merge$Environment)

# Factor Order from tillering plant production data


my.WSC.for_merge$Ord.Environment <- factor(my.WSC.for_merge$Ord.Environment, 
               levels = c("2011.Rain", "2012.Rain", "2012.Sup", "2011.Sup"))

merged.Carbs <- rbind(Carbs, my.WSC.for_merge)

p <- ggplot(merged.Carbs, aes(x = Irrigation, y = Conc..mg.mg.))
  p <- p + geom_boxplot(aes(fill = Cultivar, linetype = CO2))
  p <- p + facet_grid(Stage ~ Organ)
  p <- p + theme_bw()
p

my.counts <- ddply(merged.Carbs,
                  .(Year, Ring, Plot, Stage, Cultivar, Organ, Irrigation, CO2),
                  summarise,
                  n = sum(!is.na(Conc..mg.mg.)))

my.counts[my.counts$n != 1, ]

# export the Carbohydrate data
write.table(merged.Carbs, file = "WSC_2011_2012.csv", 
            sep = ",", row.names = F)

# counts
p <- ggplot(merged.Carbs, aes(x = my.HalfringID))
  p <- p + geom_bar(stat="bin", aes(fill = CO2))
  p <- p + facet_grid(Stage ~ Organ)
  p <- p + theme_bw() + theme(axis.text.x = element_text(angle = 90))
p

# provide the "Carbs" data set
Carbs <- merged.Carbs

save.image(file = "~/AgFace/Topics/Tillering_2011_2012/WSC/Carbohydrate_tillering_workspace.RData", compress = TRUE)

# only save the "Carbs" Silverstart data frame
WSC_Silverstar <- Carbs[Carbs$Trait == "Silverstar", ]
#names(WSC_Silverstar) <- gsub("Conc..mg.mg.", "WSC_conc_mg_mg", names(WSC_Silverstar))
save(WSC_Silverstar, file = "WSC_Silverstar_2011_2012.RData", compress = TRUE)

library(plyr)

WSC_Silverstar <- Carbs[Carbs$Trait == "Silverstar", ]

out <- ddply(WSC_Silverstar,
             .(Year, Stage, Ring, Irrigation, Cultivar, Organ),
             summarise,
             sample_size = length(Conc..mg.mg.))

sort(unique(WSC_Silverstar$Ring[WSC_Silverstar$Year == 2012]))

new_WSC_Silverstar <- WSC_Silverstar[WSC_Silverstar$Stage == "DC65"&
                                     WSC_Silverstar$Organ == "Stem", ] 

with(new_WSC_Silverstar, table(Year, Ring))
