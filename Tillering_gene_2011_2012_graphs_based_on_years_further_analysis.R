# Tillering 2011 2012 further analysis
# based on workspace created by script
# "Tillering_gene_2011_2012_graphs_based_on_years.R"
# analyse data based on water per period
# water per period from script "Weather_2011_2012_analysis.R"
# uses soil water profile sums from script "Water_use_calculation_2011_2012_from_grav_water.R"

library(ggplot2)

setwd("~/AgFace/Topics/Tillering_2011_2012")

# load workspace
load("Tin_2011_2012_plant_production_Carbs.RData")
load("~/AgFace/Weather/2011_2012/Tin_2011_2012_water_irri_per_period.RData")
load("~/AgFace/2011_2012/Soil_moisture/Tin_soilwater_use_2011_2012.RData")
# load my ggplot themes
source("~/AgFace/R_scripts/MyThemes.R")

# Standard error function
stderr <- function(x) {
          sqrt(var(x[!is.na(x)]) / length(x[!is.na(x)]))
}

# Then create a wrapper to *stderr* to make it compatible with *stat_summary*
# see values shown in ?stat_summary
my.stderr <- function(x) {
             meany <- mean(x)
             ymin  <- mean(x) - stderr(x)
             ymax  <- mean(x) + stderr(x)
             # assemble the named output
             out <- c(y = meany, ymin = ymin, ymax = ymax)
             return(out)
}

# graph output sizes
my.width  <- 20
my.height <- 12

# graph elements
CO2.label  <- expression(textstyle(CO[2]~treatment))
CO2.treats <- c(expression(textstyle(aCO[2])), 
                expression(textstyle(eCO[2])))
Cultivar.label <- expression(textstyle(Cultivar))

# correlations
# The low tillering trait is not independent of e. g. head size (in terms of weight? Grain numbers per head? Grain weights?) and stem carbohydrates. If there are fewer tillers there are bigger heads (this was already described somewhere - one of the early 'tin' papers I think - as "gigas" type) and possibly more stem carbohydrates. Are these relationships affected by elevated CO2; are they different between years etc. Explore! Maybe plot scattergraphs for those and have a look, or whatever.

# head size versus tillers
# Sub.sample.Head.dry.wt_g. vs Sub.sample.Fresh.Tiller.No.

# Heads vs tillers figure 
p <- ggplot(tin[tin$Stage != "DC31", ], 
     aes(x = Tillers.m2_SS.dry., y = Sub.sample.Head.dry.wt_g.))
  p <- p + geom_smooth(method = "lm",
                       aes(linetype = CO2), se = TRUE, 
                       colour = "black", show_guide = FALSE)
  p <- p + geom_point(aes(fill = CO2, shape = Cultivar))
  p <- p + scale_fill_manual(values = c("white", "black"), 
                               name = CO2.label,
                             labels = CO2.treats)
  p <- p + scale_shape_manual(values = c(21, 24), name = Cultivar.label)
  p <- p + guides(fill = guide_legend(override.aes = list(shape=c(22, 22))))
  p <- p + labs(x = expression("No. of tillers"~(Tillers~m^-2)),
                y = "Head dry weight (g)")
  p <- p + facet_grid(Stage ~ Year * Irrigation, scale = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.08, 0.85))
p
fig.heads.per.tiller <- p 

# Grains per head vs Tillers m2
p <- ggplot(tin[tin$Stage == "DC90", ], 
     aes(x = Tillers.m2_SS.dry., y = Grains.head_SS.))
  p <- p + geom_smooth(method = "lm",
                       aes(linetype = CO2), se = TRUE, 
                       colour = "black", show_guide = FALSE)
  p <- p + geom_point(aes(fill = CO2, shape = Cultivar))
  p <- p + scale_fill_manual(values = c("white", "black"), 
                               name = CO2.label,
                             labels = CO2.treats)
  p <- p + scale_shape_manual(values = c(21, 24), name = Cultivar.label)
  p <- p + guides(fill = guide_legend(override.aes = list(shape=c(22, 22))))
  p <- p + labs(x = expression("No. of tillers"~(Tillers~m^-2)),
                y = "Number of grains per head")
  p <- p + facet_grid(Stage ~ Year * Irrigation, scale = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.18, 0.85))
p
fig.grains.per.head.vs.tiller <- p 


# Grain weights vs Tillers m2
p <- ggplot(tin[tin$Stage == "DC90", ], 
     aes(x = Tillers.m2_SS.dry., y = SS.1000.Grain.wt_g.))
  p <- p + geom_smooth(method = "lm",
                       aes(linetype = CO2), se = TRUE, 
                       colour = "black", show_guide = FALSE)
  p <- p + geom_point(aes(fill = CO2, shape = Cultivar))
  p <- p + scale_fill_manual(values = c("white", "black"), 
                               name = CO2.label,
                             labels = CO2.treats)
  p <- p + scale_shape_manual(values = c(21, 24), name = Cultivar.label)
  p <- p + guides(fill = guide_legend(override.aes = list(shape=c(22, 22))))
  p <- p + labs(x = expression("No. of tillers"~(Tillers~m^-2)),
                y = "Thousand grain weight (g)")
  p <- p + facet_grid(Stage ~ Year * Irrigation, scale = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.18, 0.85))
p
fig.1000grain.weights.per.tiller <- p 

# Grain weights vs Stem carbohydrates
p <- ggplot(tin[tin$Stage != "DC90", ], 
     aes(x = Tillers.m2_SS.dry., y = Stem_Conc..mg.mg.))
  p <- p + geom_smooth(method = "lm",
                       aes(linetype = CO2), se = TRUE, 
                       colour = "black", show_guide = FALSE)
  p <- p + geom_point(aes(fill = CO2, shape = Cultivar))
  p <- p + scale_fill_manual(values = c("white", "black"), 
                               name = CO2.label,
                             labels = CO2.treats)
  p <- p + scale_shape_manual(values = c(21, 24), name = Cultivar.label)
  p <- p + guides(fill = guide_legend(override.aes = list(shape=c(22, 22))))
  #p <- p + scale_y_continuous(limits=c(0, 0.9))
  p <- p + labs(x = expression("No. of tillers"~(Tillers~m^-2)),
                y = expression("Carbohydrate concentration in stem"~(mg~mg^-1)))
  p <- p + facet_grid(Stage ~ Year * Irrigation, scales = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.18, 0.85))
p
fig.StemCarbs.per.tiller <- p 

# N in grains weights vs tillers
p <- ggplot(tin[tin$Stage == "DC90", ], 
     aes(x = Tillers.m2_SS.dry., y = X.N_Grain_scan_0_))
  p <- p + geom_smooth(method = "lm",
                       aes(linetype = CO2), se = TRUE, 
                       colour = "black", show_guide = FALSE)
  p <- p + geom_point(aes(fill = CO2, shape = Cultivar))
  p <- p + scale_fill_manual(values = c("white", "black"), 
                               name = CO2.label,
                             labels = CO2.treats)
  p <- p + scale_shape_manual(values = c(21, 24), name = Cultivar.label)
  p <- p + guides(fill = guide_legend(override.aes = list(shape=c(22, 22))))
  p <- p + labs(x = expression("No. of tillers"~(Tillers~m^-2)),
                y = "Nitrogen content (%N)")
  p <- p + facet_grid(Stage ~ Year * Irrigation, scales = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.90, 0.85))
p
fig.NinGrains.per.tiller <- p 


# --------------------------------------------------
# assemble figures
# --------------------------------------------------
library(plyr)
my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))
names(my.figures.list) <- my.figures

pdf(file = "Tillering_relationships_per_year.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(my.figures.list)
dev.off()

# ------------------------------------------------
# analyse data based on water received per period
# ------------------------------------------------

# using the water.per.period data frame
# creating a data frame that specifies the irrigation treatment
w.rainfed <- water.per.period
w.irri    <- water.per.period

# get rid of the Rainfall columns, only need water received
w.irri$Rainfall_received <- NULL
w.rainfed$Irrigation_received <- NULL
w.rainfed$Water_received <- NULL

w.irri$Irrigation_received <- NULL
w.irri$Rainfall_received <- NULL

names(w.irri) <- gsub("Water_received", "Period_water", names(w.irri))
names(w.rainfed) <- gsub("Rainfall_received", "Period_water", names(w.rainfed))

w.rainfed$Irrigation <- "rainfed"
w.irri$Irrigation <- "supp"

water.per.period.irri <- rbind(w.rainfed, w.irri)
water.per.period.irri$Year <- NA
water.per.period.irri$Year[grep("2011", water.per.period.irri$Period)] <- 2011
water.per.period.irri$Year[grep("2012", water.per.period.irri$Period)] <- 2012
water.per.period.irri$Cultivar <- as.character(water.per.period.irri$Cultivar)
water.per.period.irri$Cultivar[is.na(water.per.period.irri$Cultivar)] <- "Silverstar"
water.per.period.irri$Cultivar <- as.factor(water.per.period.irri$Cultivar)
water.per.period.irri$Period <- NULL
#water.per.period.irri$Period <- as.character(water.per.period.irri$Period)

# keep a table with the SSR T65 water data but remove SSR T65 form the general table
# duplicate the Silverstar table for SSR T65, then replace the few periods where Silverstar and SSR T65 differ in their growth stages
w.SSRT65 <- water.per.period.irri[water.per.period.irri$Cultivar == "SSR T65", ]

water.per.period.irri <- water.per.period.irri[water.per.period.irri$Cultivar != "SSR T65", ]

w.duplicate <- water.per.period.irri
w.duplicate$Cultivar <- "SSR T65"
w.duplicate <- w.duplicate[!(w.duplicate$Start.period == "sowing" &
                           w.duplicate$End.period == "DC31" &
                           w.duplicate$Year == 2012), ]
w.SSRT65 <- rbind(w.duplicate, w.SSRT65)
water.per.period.irri <- rbind(water.per.period.irri, w.SSRT65)

# ----------------------------------------------
# add soil mositure data
# ----------------------------------------------
tot.wat <- merge(water.per.period.irri, profile.sum.average.tin,
                 all.y = TRUE)

# get rid of the Harvest soil moisture
tot.wat <- tot.wat[!tot.wat$Sample_time == "Harvest", ]
tot.wat$SoilRainf <- tot.wat$Period_water + tot.wat$Profile_sum_vol_water_cont_mean

p <- ggplot(tot.wat, aes(x = End.period, y = SoilRainf))
  p <- p + geom_point(aes(colour = CO2_treatment))
  p <- p + facet_grid(Year ~ Irrigation * Cultivar)
p

write.table(tot.wat,
            file = "Soil_water_plus_rain_irri_tin_2011_2012.csv",
            sep = ",", row.names = FALSE)

# get total water at specific times in the season
tot.wat$End.period <- gsub("harvest", "DC90", tot.wat$End.period)
tot.wat.small <- tot.wat[, c("Cultivar", "Irrigation", "Year", "Start.period", "End.period", "CO2_treatment", "SoilRainf")]

# get rid of periods that don't start at sowing
tot.wat.small <- tot.wat.small[tot.wat.small$Start.period == "sowing", ]
tot.wat.small$Irrigation <- gsub("supp", "supplemental", tot.wat.small$Irrigation)
names(tot.wat.small) <- gsub("CO2_treatment", "CO2", names(tot.wat.small))
names(tot.wat.small) <- gsub("End.period", "Stage", names(tot.wat.small))

tin.water <- merge(tin, tot.wat.small,
              all = TRUE)
write.table(tinwater,
            file = "Tin_plant_prod_tot_water.csv",
            sep = ",", row.names = FALSE)

# figures based on water
p <- ggplot(tin.water[tin.water$Stage == "DC90", ], 
     aes(x = SoilRainf, y = Grains.head_SS.))
  p <- p + geom_smooth(method = "lm",
                       aes(linetype = CO2), se = TRUE, 
                       colour = "black", show_guide = FALSE)
  p <- p + geom_point(aes(fill = CO2, shape = Irrigation))
  p <- p + scale_fill_manual(values = c("white", "black"), 
                               name = CO2.label,
                             labels = CO2.treats)
  p <- p + scale_shape_manual(values = c(21, 24))
  p <- p + guides(fill = guide_legend(override.aes = list(shape=c(22, 22))))
  p <- p + labs(x = "Rainfall and soil water [mm]",
                y = "Number of grains per head")
  p <- p + facet_grid(Stage ~ Cultivar, scale = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.18, 0.85))
p

p <- ggplot(tin.water[tin.water$Stage == "DC90", ], 
     aes(x = SoilRainf, y = Yield_g.m2.))
  p <- p + geom_smooth(method = "lm",
                       aes(linetype = CO2), se = TRUE, 
                       colour = "black", show_guide = FALSE)
  #p <- p + geom_point(aes(fill = CO2, shape = Irrigation))
  p <- p + stat_summary(aes(fill = CO2, shape = Irrigation),
                        fun.data = "mean_sdl", mult = 1)
  p <- p + scale_fill_manual(values = c("white", "black"), 
                               name = CO2.label,
                             labels = CO2.treats)
  p <- p + scale_shape_manual(values = c(21, 24))
  p <- p + guides(fill = guide_legend(override.aes = list(shape=c(22, 22))))
  p <- p + labs(x = "Rainfall and soil water [mm]",
                y = "Yield [g m-2]")
  p <- p + facet_grid(Stage ~ Cultivar, scale = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.18, 0.85))
p
