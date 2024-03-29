# Carbohydrate tillering merge and analysis

# libraries
require(ggplot2)
require(plyr)
require(reshape)
require(nlme)

# set working directory
setwd("~/AgFace/Topics/Tillering_2011_2012/WSC")

# import the data from
load("../../Carbohydrates_DC65_2009_2010_2011_2012/DC65_Carbohydrates_2012.RData")
load("../../Carbohydrates_DC31_2011_2012/CarbohydratesDC31_2011_2012.RData")
load("../../Carbohydrates_DC65_2009_2010_2011_2012/DC65_Carbohydrates_2011.RData")

CarbohydratesDC31$Year <- as.numeric(as.character(CarbohydratesDC31$Year))
DC65_2012$Year <- as.numeric(as.character(DC65_2012$Year))
Carb.DC65.2011.tin$Year <- as.numeric(as.character(Carb.DC65.2011.tin$Year))

# assemble the data
Carbs <- rbind(CarbohydratesDC31, DC65_2012)

Carb.DC65.2011.tin$sample.ID <- NULL

Carbs <- rbind(Carbs, Carb.DC65.2011.tin)

# the traits
Silverstars <- c("Silverstar", "SSR T65")
SBs <- c("SB062", "SB003")

# assign traits to cultivars
Carbs$Trait <- NA
Carbs$Trait[Carbs$Cultivar %in% Silverstars] <- "Silverstar"
Carbs$Trait[Carbs$Cultivar %in% SBs] <- "SB"
Carbs$Trait <- as.factor(Carbs$Trait)

# re-order cultivars
Carbs$Cultivar <- factor(Carbs$Cultivar, 
                         levels = c("SB003", "SB062", "Silverstar", "SSR T65"))

# re-name Environments to match other workspaces
Carbs$Environment <- gsub("Supp", "Sup", Carbs$Environment)

# format Carbs
Carbs$Ring <- as.factor(Carbs$Ring)
Carbs$Year <- as.factor(Carbs$Year)
Carbs$Environment     <- as.factor(Carbs$Environment)
Carbs$Ord.Environment <- as.factor(Carbs$Environment)

# Factor Order from tillering plant production data
Carbs$Ord.Environment <- factor(Carbs$Ord.Environment, 
               levels = c("2011.Rain", "2012.Rain", "2012.Sup", "2011.Sup"))

save.image(file = "Carbohydrate_merged_stages_and_years.RData", compress = TRUE)
