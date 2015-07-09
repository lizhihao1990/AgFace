# Import carbohydrate data for 2011 and 2012

setwd("~/AgFace/Topics/Tillering_2011_2012")

# open workspaces
load("~/AgFace/Plant_Production/Silverstar_tin/Silverstar_tin_environment.RData")
load("~/AgFace/Topics/Tillering_2011_2012/WSC/Carbohydrate_tillering_workspace.RData")
load("~/AgFace/Topics/Tillering_2011_2012/WSC/WSC_lme_p_values.RData")
load("~/AgFace/2011_2012/Apsim_soil_moisture/RUE_at_0.94/Daily/Apsim_soil_water_biomass_principal_dates_2011_2012.RData")
# keep some objects
to_keep <- c("df", "df.melt", "parameters_to_keep", "lme.out", "lme.res", "Carbs", "Carbs.melt", "Carbs.lme.out", "relev.p.values", "relev.p.values.Carbs", "aps.principal.dates")
rm(list = ls()[!(ls() %in% to_keep)])

# load libraries
require(ggplot2)
require(plyr)
require(reshape)

# load my ggplot themes
source("~/AgFace/R_scripts/MyThemes.R")

aps.principal.dates <- aps.principal.dates[aps.principal.dates$Event != "Sowing", ]
aps.principal.dates$Event <- as.character(aps.principal.dates$Event)
aps.principal.dates$Event[aps.principal.dates$Event == "Harvest"] <- "DC90"
aps.principal.dates$Event <- as.factor(aps.principal.dates$Event)
names(aps.principal.dates) <- gsub("Event", "Stage", names(aps.principal.dates))
aps.principal.dates$Date <- NULL
aps.principal.dates$Title <- NULL
aps.principal.dates$fileName <- NULL
aps.principal.dates$RingPlot <- NULL

# get rid of "SB" data, only keep "tin"!
df      <- df[df$my.Trait           == "Silverstar", ]
df$RingTrt <- NULL
# add soil water information from Apsim
df <- merge(aps.principal.dates, df)
df.melt <- df.melt[df.melt$my.Trait == "Silverstar", ]

Carbs      <- Carbs[Carbs$Trait           == "Silverstar", ]
Carbs <- merge(aps.principal.dates, Carbs)
Carbs.melt <- melt(Carbs)
Carbs.melt <- Carbs.melt[Carbs.melt$Trait == "Silverstar", ]

# re-name traits to my.Trait to match plant production data sets
names(Carbs)      <- gsub("Trait", "my.Trait", names(Carbs))
names(Carbs.melt) <- gsub("Trait", "my.Trait", names(Carbs.melt))

# for comparison here the parameters used by Glenn for the Plant production analysis.
Glenn_paras <- c("Spikelets.head", "Spikelet.wt..g.", "Crop.Height..cm.", "Emergence.Plants.m2", "AR.Dry.wt.area..g.m2.", "Plant.wt..g.plant.", "Tiller.wt..g.tiller.",  "Plants.m2..Quadrat.", "Tillers.m2..Quadrat.", "Tillers.plant..SS.", "Heads.m2..Quadrat.", "..Fertile.Tillers..Quadrat.SS.", "Heads.plant..SS.", "1000.Grain.Wt..g.", "Grains.m2", "Grains.plant", "Grains.tiller", "Grains.head", "Screenings...2mm.....", "Harvest.Index..AR.", "Milling.Yield....", "Yield..g.m2.", "Seeds.floret")


# response function
# Calulates mean and SD per given parameter and split combination
CalcRes <- function(data, parameter, splits) {
   my.df <- data[data$variable == parameter, ]
   my.means <- ddply(my.df,
              splits,
              summarise,
              mean = mean(value, na.rm = TRUE),
              sd   = sd(value, na.rm = TRUE))
   return(my.means)
}


# Calculate percent reduction in SSR T65 compared to Silverstar
CalcPerc <- function(data, split) {
           ddply(data,
           split,
           function(x) {
           silv <- x$mean[x$Cultivar == "Silverstar"]
           sst  <- x$mean[x$Cultivar == "SSR T65"]
           perc <- sst/silv * 100 - 100
           names(perc) <- "Percent_reduction_in_rtin"
           return(perc)
           })}

# Calculate percent change due to CO2
CalcPercCO2 <- function(data, split) {
           ddply(data,
           split,
           function(x) {
           aCO2 <- x$mean[x$CO2 == "aCO2"]
           eCO2  <- x$mean[x$CO2 == "eCO2"]
           perc <- eCO2/aCO2 * 100 - 100
           names(perc) <- "Percent_change_in_eCO2"
           return(perc)
           })}


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
my.width  <- 17
my.height <- my.width

# graph elements
CO2.label  <- expression(bold(textstyle(CO[2]~treatment)))
CO2.treats <- c(expression(textstyle(aCO[2])), 
                expression(textstyle(eCO[2])))

id.vars <- names(df[1:22])
not.id <- c("RingID", "PlotID", "HalfRing", "Trial.ID", "Crop", "Bulk_Trait", "CO2.Irr.Cultivar.DATABASE", "my.HalfringID", "Qplot", "QHalfRing", "Qring", "Environment", "Ord.Environment", "Trmt.Order", "Plot.Order", "RingPos.")
to.keep <- id.vars[-(which(id.vars %in% not.id))]
get.rid <- which(names(df) %in% not.id)
df.prep <- df[, -get.rid]

df.prep.melt <- melt(df.prep,
                     id.vars = names(df.prep)[1:6])
                
df.mean <- cast(df.prep.melt,
                Year + Cultivar + CO2 + Irrigation + my.Trait + Stage ~ variable,
                fun.aggregate = c(mean, sd), na.rm = TRUE)
write.csv(df.mean, file = "Mean_tin.csv")

names(df.mean) <- gsub("\\.\\.", "_", names(df.mean))
names(df.mean) <- gsub("\\.", "_", names(df.mean))

MyEswPlot <- function(data, x = "esw_mean", x.sd = "esw_sd", y, y.sd) {
 require(ggplot2)
 
 limits <- aes(ymax = y + y.sd,
               ymin = y - y.sd) 

 p <- ggplot(data, aes(x = esw_mean, y = y))
   p <- p + geom_point()
   p <- p + geom_errorbar(limits)
 p
}
MyEswPlot(df.mean, y = "Tillers_m2_SS_dry__mean", y.sd = "Tillers_m2_SS_dry__sd")


# -------------------------------------------------------
# Some Plant production figures
# -------------------------------------------------------

limitsy <- aes(ymax = Tillers_m2_SS_dry__mean + Tillers_m2_SS_dry__sd,
               ymin = Tillers_m2_SS_dry__mean - Tillers_m2_SS_dry__sd)

limitsx <- aes(xmax = esw_mean + esw_sd,
               xmin = esw_mean - esw_sd)
              
p <- ggplot(df.mean[df.mean$Stage != "DC31", ], 
            aes(x = esw_mean, y = Tillers_m2_SS_dry__mean))
  p <- p + geom_errorbar(limitsy, colour = "grey")
  p <- p + geom_errorbarh(limitsx, colour = "grey")
  p <- p + geom_point(aes(fill = Cultivar, size = Year, shape = CO2))
  p <- p + theme_my
  p <- p + facet_grid(Stage ~ .)
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_size_manual(values = c(1.5, 2.5))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + scale_colour_manual(values = c("orange", "violet"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)),
                  size = guide_legend(override.aes = list(shape = 22)))
  p <- p + scale_x_continuous(limits = c(0, 250))
  p <- p + labs(y = expression("No. of tillers"~(Tillers~m^-2)),
                x = "Extractable soil water (mm)")
p
fig.Tiller.esw.mean <- p

p <- ggplot(df[df$Stage != "DC31", ],
            aes(x = esw, y = Tillers.m2..SS.dry.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2, size = Year))
  #p <- p + stat_summary()
  p <- p + facet_grid(Stage ~ .)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + scale_colour_manual(values = c("black", "black"))
  p <- p + scale_size_manual(values = c(1.5, 2.5))
  p <- p + scale_x_continuous(limits = c(0, 250))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)),
                  size = guide_legend(override.aes = list(shape = 22)))
  p <- p + labs(y = expression("No. of tillers"~(Tillers~m^-2)),
                x = "Extractable soil water (mm)")
p

# Tillering figure 
fig.number.tillers <- p 

library(gridExtra)
grid.arrange(fig.Tiller.esw.mean, fig.number.tillers, 
             ncol = 1)

pdf(file = "Means_or_individual.pdf", width = 13, height = 9)
grid.arrange(fig.Tiller.esw.mean, fig.number.tillers, 
             ncol = 1)
dev.off()

ggsave(file = "Fig.02.Tillering_se_per_Year_esw.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

#ggsave(file = "Fig.01.Tillering_se_poster_per_Year.pdf", 
#       width = my.width * 1.25, height = my.height * 0.75, 
#       units = "cm")

# Tillering does only respond to Cultivar. Not affected by any other factor
# percent reduction
till.means <- CalcRes(df.melt, "Tillers.m2..SS.dry.", c("CO2", "Year", "Irrigation", "Stage", "Cultivar"))

till.mean.change_perc <- till.means$mean[till.means$Cultivar == "SSR T65"] / 
                         till.means$mean[till.means$Cultivar == "Silverstar"] * 100 -100

till.perc <- CalcPerc(till.means, c("CO2", "Irrigation", "Stage", "Year"))
till.perc

till.percCO2 <- CalcPercCO2(till.means, c("Cultivar", "Irrigation", "Year", "Year"))
till.percCO2

ddply(till.percCO2,
      .(Cultivar),
      summarise,
      my.mean.Perc.Change = mean(Percent_change_in_eCO2))


# Yield response?
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = esw, y = Yield..g.m2.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(name = expression("Cultivar"),
                             values = c("white", "black"))
  p <- p + scale_colour_manual(name = expression("Cultivar"),
                               values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + labs(y = expression(Yield~(g~m^-2)),
                x = "Extractable soil water (mm)")
p
fig.yield.m2 <- p

ggsave(file = "Fig.04.Yield_se_per_Year_esw.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

#ggsave(file = "Fig.02.Yield_se_poster_per_Year.pdf", 
#       width = my.width * 1.25, height = my.height * 0.75, 
#       units = "cm")


yield.means <- CalcRes(df.melt, "Yield..g.m2.", 
                       c("CO2", "Year", "Irrigation", "Stage", "Cultivar"))
yield.means[!is.na(yield.means$mean), ]
dlply(yield.means[!is.na(yield.means$mean), ],
      .(CO2),
      function(x) {summary(x)})
      
summary(yield.means[!is.na(yield.means$mean), ])

yield.mean.change_perc <- yield.means$mean[yield.means$Cultivar == "SSR T65"] / 
                         yield.means$mean[yield.means$Cultivar == "Silverstar"] * 100 -100

yield.perc <- CalcPerc(yield.means, c("CO2", "Stage", "Year"))
yield.perc


# Grains per tiller"Grains.tiller..SS."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = esw, y = Grains.tiller_SS.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(name = expression("Cultivar"),
                             values = c("white", "black"))
  p <- p + scale_colour_manual(name = expression("Cultivar"),
                               values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
    p <- p + labs(y = "Grains per tiller",
                x = "Extractable soil water (mm)")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.38, 0.90))
p
fig.grains_per_tiller <- p

ggsave(file = "Grains_per_tiller_se_per_Year_esw.pdf", 
       width = my.width, height = my.height, 
       units = "cm")


grains.tiller.means <- CalcRes(df.melt, "Grains.tiller..SS.", 
                       c("CO2", "Year", "Irrigation", "Stage", "Cultivar"))
grains.tiller.means[!is.na(grains.tiller.means$mean), ]
dlply(grains.tiller.means[!is.na(grains.tiller.means$mean), ],
      .(CO2),
      function(x) {summary(x)})
dlply(grains.tiller.means[!is.na(grains.tiller.means$mean), ],
      .(Cultivar),
      function(x) {summary(x)})     
summary(grains.tiller.means[!is.na(grains.tiller.means$mean), ])

grains.tiller.perc <- CalcPerc(grains.tiller.means, c("CO2", "Irrigation", "Stage", "Year"))
grains.tiller.perc


# Grains per Head "Grains.head_SS."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = esw, y = Grains.head..SS.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + scale_colour_manual(values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + theme_my
  # p <- p + theme(legend.position = c(0.38, 0.90))
  p <- p + labs(y = "Grains per head", 
                x = "Extractable soil water (mm)")
p
fig.grains_per_head <- p

#ggsave(file = "Grains_per_head_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")


# Harvest index "Harvest.Index..AR."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = esw, y = Harvest.Index..AR.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + scale_colour_manual(values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + theme_my
  p <- p + labs(y = "Harvest index",
                x = "Extractable soil water (mm)")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.62, 0.15))
p
fig.harvest.index <- p

#ggsave(file = "Harvest_index_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")


# Dry weight of heads "Dry.wt.heads.in.AR.sample..g.m2."

p <- ggplot(df[df$Stage == "DC65", ], 
            aes(x = Cultivar, y = Dry.wt.heads.in.AR.sample..g.m2.))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression("Heads dry weight"~(g~m^2)))
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.62, 0.15))
p

fig.Dry.weight.heads <- p

#ggsave(file = "Dry_weight_heads_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")


# Spikes per head "Spikelets.head" 

p <- ggplot(df[df$Stage == "DC65", ], 
            aes(x = Cultivar, y = Spikelets.head))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = "Spikelets per head")
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.62, 0.15))
p
fig.spikelets.head <- p

#ggsave(file = "Spikelets_per_head_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")

# Leaf area "Green.leaf.area.pl..cm2." 

p <- ggplot(df[df$Stage == "DC65", ], 
            aes(x = Cultivar, y = Green.leaf.area.pl..cm2.))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression("Green leaf area per plant"~(cm^2)))
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.green.leaf.area <- p

#ggsave(file = "Green_leaf_area_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")


# 1000 grains weight "SS.1000.Grain.wt..g."
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = SS.1000.Grain.wt..g.))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression("1000 grains weight"~(g)))
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.thousand.grains <- p

#ggsave(file = "Thousand_grains_weight_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")

names(df) <- gsub("\\.\\.", "_", names(df))

# %N in grains "X.N_Grain..scan..0.."
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = esw, y = X.N_Grain_scan_0_))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(name = expression("Cultivar"),
                             values = c("white", "black"))
  p <- p + scale_colour_manual(name = expression("Cultivar"),
                               values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + labs(y = "Nitrogen content in grains (%)",
                x = "Extractable soil water (mm)")
  #p <- p + theme(legend.position = c(0.38, 0.2))
p

fig.percent.N.grains <- p

ggsave(file = "Percent_N_grains_se_per_Year_esw.pdf", 
       width = my.width, height = my.height, 
       units = "cm")


# "X.N_Plant..AR.biomass."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = esw, y = X.N_Plant_AR.biomass.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(name = expression("Cultivar"),
                             values = c("white", "black"))
  p <- p + scale_colour_manual(name = expression("Cultivar"),
                               values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + labs(y = "%N in plant (above root)",
                x = "Extractable soil water (mm)")
  #p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.38, 0.8))
p

fig.percent.N.plant <- p

ggsave(file = "Percent_N_plant_se_per_Year_esw.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

# above root biomass AR.Dry.wt.area..g.m2."
p <- ggplot(df[df$Stage != "DC31", ], 
            aes(x = esw, y = AR.Dry.wt.area..g.m2.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(name = expression("Cultivar"),
                             values = c("white", "black"))
  p <- p + scale_colour_manual(name = expression("Cultivar"),
                               values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + labs(y = expression("Above root plant biomass"~(g~m^2)),
                x = "Extractable soil water (mm)")
p
  
fig.above.root.biomass <- p

ggsave(file = "Fig.03.Above_root_biomass_se_per_Year.pdf", 
       width = my.width, height = my.height, 
       units = "cm")


above.root.means <- CalcRes(df.melt, "AR.Dry.wt.area..g.m2.", 
                       c("CO2", "Year", "Irrigation", "Stage", "Cultivar"))
above.root.means[!is.na(above.root.means$mean), ]
dlply(above.root.means[!is.na(above.root.means$mean), ],
      .(CO2, Stage),
      function(x) {summary(x)})
dlply(above.root.means[!is.na(above.root.means$mean), ],
      .(Cultivar, Stage),
      function(x) {summary(x)})     
summary(above.root.means[!is.na(above.root.means$mean), ])

above.root.perc <- CalcPerc(above.root.means, c("CO2", "Irrigation", "Stage", "Year"))
above.root.perc


# Leaf area index "GLAI..AR.dry." 

p <- ggplot(df[df$Stage == "DC65", ], 
            aes(x = Cultivar, y = GLAI_AR.dry.))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = "Leaf area index")
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.64, 0.85))
p

fig.LAI <- p

#ggsave(file = "Leaf_area_index_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")


# Head numbers

p <- ggplot(df[df$Stage == "DC65" | df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Heads.m2_SS.dry.))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression(Heads~per~m^2))
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.44, 0.90))
p

fig.Heads_p_m2 <- p

#ggsave(file = "Heads_per_m2_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")


# Grains per m2 Grains.m2
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Grains.m2))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression(Grains~per~m^2))
  p <- p + facet_grid(Stage ~ Year * Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.43, 0.90))
p

fig.Grains_p_m2 <- p

#ggsave(file = "Grains_per_m2_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")



# Nitrogen in leaves

p <- ggplot(df[df$Stage != "DC90", ], 
            aes(x = Cultivar, y = X.N_Leaf))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = "%N in leaves")
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.13, 0.60))
p

fig.N_leaves <- p

#ggsave(file = "Percent_N_in_leaves_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")

#ggsave(file = "Percent_N_in_leaves_se_poster_per_Year.pdf", 
#       width = my.width * 1.25, height = my.height * 0.75, 
#       units = "cm")

percN.means <- CalcRes(df.melt, "X.N_Leaf", c("CO2", "Year", "Irrigation", "Stage", "Cultivar"))

percN.mean.change_perc <- percN.means$mean[percN.means$Cultivar == "SSR T65"] / 
                          percN.means$mean[percN.means$Cultivar == "Silverstar"] * 100 -100

percN.perc <- CalcPerc(percN.means, c("CO2", "Stage", "Year", "Irrigation"))
percN.perc

percN.percCO2 <- CalcPercCO2(percN.means, c("Cultivar", "Stage", "Year", "Irrigation"))
percN.percCO2

ddply(percN.percCO2,
      .(Cultivar),
      summarise,
      my.mean.Perc.Change = mean(Percent_change_in_eCO2, na.rm = TRUE))


# N in heads

p <- ggplot(df[df$Stage == "DC65", ], 
            aes(x = Cultivar, y = X.N_Heads))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = "%N in heads")
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.64, 0.90))
p

fig.N_heads <- p

#ggsave(file = "Percent_N_in_heads_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")

# figure N in tillers

p <- ggplot(df[df$Stage != "DC90", ], 
            aes(x = Cultivar, y = X.N_Tiller))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = "%N in tiller")
  p <- p + facet_grid(Stage ~ Year * Irrigation)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.4, 0.60))
p

fig.N_Tiller <- p

#ggsave(file = "Percent_N_in_tillers_se_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")

# N-distribution

p <- ggplot(df.melt[df.melt$variable == "X.N_Leaf"  |
                    df.melt$variable == "X.N_Heads" |
                    df.melt$variable == "X.N_Straw" | 
                    df.melt$variable == "X.N_Tiller" |
                    df.melt$variable == "X.N_Grain..scan..0..", ], 
            aes(x = Stage, y = value))
  p <- p + stat_summary(aes(colour = Cultivar, shape = variable), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(colour = Cultivar, shape = variable), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + facet_grid(CO2 ~ Year * Irrigation)
  p <- p + labs(y = "%N")
  p <- p + theme_my
p

fig.N_over_stages <- p

#ggsave(file = "Percent_N_over_stages_multiple_organs_se_per_Year.pdf", 
#       width = my.width * 1.5, height = my.height, 
#       units = "cm")

# Added July 2014
# How different were the canopies?
# What paramter to use for that?
# "GLAI_AR.dry."  Green leaf area index already taken care of via "fig.LAI"                                  
# "GSAI_AR.dry."  
# "GAI_AR.dry."   Green area index
# "NDVI"
# "Green.cover.fraction_photos."



p <- ggplot(df[df$Stage != "DC90", ], 
            aes(x = Cultivar, y = GAI_AR.dry.))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  #p <- p + labs(y = "%N in tiller")
  p <- p + facet_grid(Stage ~ Year * Irrigation )
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.4, 0.90))
p
fig.GAI <- p


# Fun graphs
# mean data
df.mean <- cast(df.melt,
            Cultivar + CO2 + Stage + Year + Irrigation ~ variable,
            fun = c(mean, sd))
limits <- aes(xmax = Tillers.m2..SS.dry._mean + Tillers.m2..SS.dry._sd,
              xmin = Tillers.m2..SS.dry._mean - Tillers.m2..SS.dry._sd,
              ymax = Yield..g.m2._mean + Yield..g.m2._sd,
              ymin = Yield..g.m2._mean - Yield..g.m2._sd)
              
# Yield per tillers
p <- ggplot(df.mean[df.mean$Stage == "DC90", ],
            aes(x = Tillers.m2..SS.dry._mean, y = Yield..g.m2._mean))
  p <- p + geom_errorbar(limits, colour = "grey")
  p <- p + geom_errorbarh(limits, colour = "grey")
  p <- p + geom_point(aes(fill = CO2, shape = Year))
  #p <- p + geom_smooth(aes(linetype = CO2), method = "lm", colour = "black")
  p <- p + labs(y = expression(Yield~(g~m^-2)),
                x = expression("No. of tillers"~(Tillers~m^-2)))
  p <- p + scale_shape_manual(values = c(21:24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 21)))
  p <- p + guides(linetype = FALSE)
  p <- p + facet_grid(Irrigation ~ Cultivar)
  p <- p + theme_my
p

fig.yield.per.tiller <- p

#ggsave(file = "Yield_vs_tillers_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")

my.lm <- lm(Yield_g.m2. ~ Tillers.m2_SS.dry., data = df[df$Stage == "DC90", ])
summary(my.lm)

# Percent N grains vs %N in plant
p <- ggplot(df[df$Stage == "DC90", ],
            aes(x = X.N_Plant_AR.biomass., y = X.N_Grain_scan_0_))
  p <- p + geom_point(aes(fill = CO2, shape = Year))
  p <- p + geom_smooth(aes(linetype = CO2), method = "lm", colour = "black")
  p <- p + labs(y = "%N in grains",
                x = "%N in plant (above root)")
  p <- p + scale_shape_manual(values = c(21:24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 21)))
#  p <- p + guides(linetype = FALSE)
  p <- p + facet_grid(Irrigation ~ Cultivar)
  p <- p + theme_my
p

fig.N.grains.vs.N.plant <- p

#ggsave(file = "Fig.3.Percent_N_grains_plant_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")


# test if the confidence intervall of the slope includes 0 using the confint() function on a lm model
my.lm.Silver.eCO2.N <- lm(X.N_Plant_AR.biomass. ~ X.N_Grain_scan_0_,
        data = df[df$CO2 == "eCO2" &
                  df$Cultivar == "Silverstar",])
summary(my.lm.Silver.eCO2.N)
confint(my.lm.Silver.eCO2.N) # confint does not include 0

my.lm.SSRT.eCO2.N <- lm(X.N_Plant_AR.biomass. ~ X.N_Grain_scan_0_,
        data = df[df$CO2 == "eCO2" &
                  df$Cultivar == "SSR T65",])
summary(my.lm.SSRT.eCO2.N)
confint(my.lm.SSRT.eCO2.N) # confint does not include 0


# Yield vs Harvest index
p <- ggplot(df[df$Stage == "DC90", ],
            aes(x = Harvest.Index_AR., y = Yield_g.m2.))
  p <- p + geom_point(aes(fill = CO2, shape = Year))
  p <- p + geom_smooth(aes(linetype = CO2), method = "lm", colour = "black")
  p <- p + labs(y = expression(Yield~(g~m^-2)),
                x = "Harvest index")
  p <- p + scale_shape_manual(values = c(21:24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 21)))
  p <- p + guides(linetype = FALSE)
  p <- p + facet_grid(Irrigation ~ Cultivar)
  p <- p + theme_my
p

fig.yield.per.harvest.index <- p

#ggsave(file = "Yield_vs_harvest_index_per_Year.pdf", 
#       width = my.width, height = my.height, 
#       units = "cm")

my.lm <- lm(Yield_g.m2. ~ Harvest.Index_AR., data = df[df$Stage == "DC90", ])
summary(my.lm)



# --------------------------------------------------
# Carbohydrate figures
# --------------------------------------------------

p <- ggplot(Carbs[Carbs$Organ == "Stem", ],# &
#                  Carbs$Stage == "DC65", ],
            aes(x = esw, y = Conc..mg.mg.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + scale_colour_manual(values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + labs(y = expression(Stem~carbohydrate~concentration~(mg~mg^-1)),
                x = "Extractable soil water (mm)")
  p <- p + theme_my
  #p <- p + labs(y = expression(Stem~carbohydrate~concentration~(mg~mg^-1)))
  #p <- p + facet_grid(Stage ~ Year * Irrigation, scale = "free_y")
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.13, 0.85))
p

fig.Carbs.stem <- p

#ggsave(file = "Stem_Carbohydrate_tin_se_poster_per_Year.pdf", 
#       width = my.width * 1.25, height = my.height * 0.75, 
#       units = "cm")

p <- ggplot(Carbs[Carbs$Organ == "Leaf", ],
            aes(x = esw, y = Conc..mg.mg.))
  p <- p + geom_point(aes(colour = Cultivar, fill = Cultivar, shape = CO2))
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_my
  p <- p + scale_shape_manual(name = CO2.label, 
                               labels = CO2.treats,
                               values = c(21, 24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + scale_colour_manual(values = c("black", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 22)))
  p <- p + labs(y = expression(Stem~carbohydrate~concentration~(mg~mg^-1)),
                x = "Extractable soil water (mm)")
  p <- p + theme_my
  #p <- p + labs(y = expression(Stem~carbohydrate~concentration~(mg~mg^-1)))
  #p <- p + facet_grid(Stage ~ Year * Irrigation, scale = "free_y")
  p <- p + theme_my
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.4, 0.2))
p

fig.Carbs.leaf <- p

carbs.means <- CalcRes(Carbs.melt, "Conc..mg.mg.", 
                      c("CO2", "Year", "Irrigation", "Organ", "Stage", "Cultivar"))
carbs.means[!is.na(carbs.means$mean), ]
dlply(carbs.means[!is.na(carbs.means$mean), ],
      .(CO2, Stage, Organ),
      function(x) {summary(x)})
summary(carbs.means[!is.na(carbs.means$mean), ])

carbs.perc <- CalcPerc(carbs.means, c("CO2", "Organ", "Stage", "Year", "Irrigation"))
carbs.perc

ddply(carbs.perc,
      .(CO2, Organ),
      summarise,
      my.mean.percent_change_in_Cultivars = mean(Percent_reduction_in_rtin))

carbs.percCO2 <- CalcPercCO2(carbs.means, c("Cultivar", "Organ", "Stage", "Year", "Irrigation"))
carbs.percCO2

ddply(carbs.percCO2,
      .(Cultivar, Stage),
      summarise,
      my.mean.percent_change_in_eCO2 = mean(Percent_change_in_eCO2))

carbs.mean.change_perc <- carbs.means$mean[carbs.means$Cultivar == "SSR T65"] / 
                          carbs.means$mean[carbs.means$Cultivar == "Silverstar"] * 100 -100
carbs.mean.change_perc

# --------------------------------------------------
# assemble figures
# --------------------------------------------------
my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))
names(my.figures.list) <- my.figures

pdf(file = "Tillering_figures_per_year_Apsim.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(my.figures.list)
dev.off()

pdf(file = "Carbohydrates_leaf_stem_figures_per_year_Apsim.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(my.figures.list$fig.Carbs.leaf)
    print(my.figures.list$fig.Carbs.stem)
dev.off()

# merge df with Carbs
get.rid.of <- c("Trmt.Order", "Plot.Order", "CO2.Irr.Cultivar.DATABASE", "Trial.ID", "HalfRing", "RingTrt", "my.HalfringID", "Crop", "Qplot", "QHalfRing", "Qring", "Bulk_Trait", "Sample.date", "Sowing.date", "Environment", "Ord.Environment")

plantprod <- df[, !names(df) %in% get.rid.of]
plantprod$Irrigation <- gsub("Rain", "rainfed", plantprod$Irrigation)
plantprod$Irrigation <- gsub("Sup", "supplemental", plantprod$Irrigation)
names(plantprod) <- gsub("RingPos\\.", "RingPos", names(plantprod))

Carbs.mer <- Carbs[, !names(Carbs) %in% get.rid.of]
Carbs.mer$Stage <- gsub("DC30", "DC31", Carbs.mer$Stage)
Carbs.mer$Stage <- as.factor(Carbs.mer$Stage)
Carbs.mer$Irrigation <- gsub("Rain", "rainfed", Carbs.mer$Irrigation)
Carbs.mer$Irrigation <- gsub("Supp", "supplemental", Carbs.mer$Irrigation)
names(Carbs.mer) <- gsub("Plot", "PlotID", names(Carbs.mer))
names(Carbs.mer) <- gsub("Ring$", "RingID", names(Carbs.mer))

Carbs.mer.melt <- melt(Carbs.mer)
Carbs.mer.melt$variable <- paste(Carbs.mer.melt$Organ, Carbs.mer.melt$variable, sep = "_")
Carbs.mer.melt$Organ <- NULL
Carbs.mer.cast <- cast(Carbs.mer.melt)


tin <- merge(plantprod, Carbs.mer.cast,
             all = TRUE)
dim(plantprod) + dim(Carbs.mer) 
dim(tin)
write.table(tin, file = "Tin_plantprod_and_Carbs_Apsim.csv", row.names= F, sep = ",")

save(tin, file = "Tin_2011_2012_plant_production_Carbs_Apsim.RData", compress = TRUE)
