# Import carbohydrate data for 2011 and 2012

setwd("~/AgFace/Topics/Tillering_2011_2012")

# open workspaces
load("~/AgFace/Plant_Production/Silverstar_tin/Silverstar_tin_environment.RData")
load("~/AgFace/Topics/Tillering_2011_2012/WSC/Carbohydrate_tillering_workspace.RData")
load("~/AgFace/Topics/Tillering_2011_2012/WSC/WSC_lme_p_values.RData")

# keep some objects
to_keep <- c("df", "df.melt", "parameters_to_keep", "lme.out", "lme.res", "Carbs", "Carbs.melt", "Carbs.lme.out", "relev.p.values", "relev.p.values.Carbs")
rm(list = ls()[!(ls() %in% to_keep)])

# load libraries
require(ggplot2)
require(plyr)
require(reshape)

# load my ggplot themes
source("~/AgFace/R_scripts/MyThemes.R")

# get rid of "SB" data, only keep "tin"!
df      <- df[df$my.Trait           == "Silverstar", ]
df.melt <- df.melt[df.melt$my.Trait == "Silverstar", ]

Carbs      <- Carbs[Carbs$Trait           == "Silverstar", ]
Carbs.melt <- melt(Carbs)
Carbs.melt <- Carbs.melt[Carbs.melt$Trait == "Silverstar", ]

# re-name traits to my.Trait to match plant production data sets
names(Carbs)      <- gsub("Trait", "my.Trait", names(Carbs))
names(Carbs.melt) <- gsub("Trait", "my.Trait", names(Carbs.melt))

# for comparison here the parameters used by Glenn for the Plant production analysis.
Glenn_paras <- c("Spikelets.head", "Spikelet.wt..g.", "Crop.Height..cm.", "Emergence.Plants.m2", "AR.Dry.wt.area..g.m2.", "Plant.wt..g.plant.", "Tiller.wt..g.tiller.",  "Plants.m2..Quadrat.", "Tillers.m2..Quadrat.", "Tillers.plant..SS.", "Heads.m2..Quadrat.", "..Fertile.Tillers..Quadrat.SS.", "Heads.plant..SS.", "1000.Grain.Wt..g.", "Grains.m2", "Grains.plant", "Grains.tiller", "Grains.head", "Screenings...2mm.....", "Harvest.Index..AR.", "Milling.Yield....", "Yield..g.m2.", "Seeds.floret")



