# Import carbohydrate data for 2011 and 2012

setwd("~/AgFace/Topics/Tillering_2011_2012")

# open workspace
load("~/AgFace/Plant_Production/Silverstar_tin/Silverstar_tin_environment.RData")

# keep some objects
to_keep <- c("df", "df.melt", "parameters_to_keep", "lme.out", "lme.res")
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

# for comparison here the paramters used by Glenn for the Plant production analysis.
Glenn_paras <- c("Spikelets.head", "Spikelet.wt..g.", "Crop.Height..cm.", "Emergence.Plants.m2", "AR.Dry.wt.area..g.m2.", "Plant.wt..g.plant.", "Tiller.wt..g.tiller.",  "Plants.m2..Quadrat.", "Tillers.m2..Quadrat.", "Tillers.plant..SS.", "Heads.m2..Quadrat.", "..Fertile.Tillers..Quadrat.SS.", "Heads.plant..SS.", "1000.Grain.Wt..g.", "Grains.m2", "Grains.plant", "Grains.tiller", "Grains.head", "Screenings...2mm.....", "Harvest.Index..AR.", "Milling.Yield....", "Yield..g.m2.", "Seeds.floret")


# response function
# Calulates mean and SD per given parameter and split combination
CalcRes <- function(data, parameter, splits) {
   my.df <- data[data$variable == parameter, ]
   my.means <- ddply(my.df,
              splits,
              summarise,
              mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE))
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


# Standard error fucntion
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
my.width <- 17
my.height <- my.width

# graph elements
CO2.label  <- expression(textstyle(CO[2]~treatment))
CO2.treats <- c(expression(textstyle(aCO[2])), 
                expression(textstyle(eCO[2])))

# -------------------------------------------------------
# Some figures

# Tillering figure 
p <- ggplot(df,#[df$Stage != "DC90", ], 
     aes(x = Cultivar, y = Tillers.m2..SS.dry.))
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
  p <- p + labs(y = expression("No. of tillers"~(Tillers~m^-2)))
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.88, 0.90))
p
fig.number.tillers <- p 

ggsave(file = "Fig.01.Tillering_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

# Tillering does only respond to Cultivar. Not affected by any other factor
# percent reduction
till.means <- CalcRes(df.melt, "Tillers.m2..SS.dry.", c("CO2", "Environment", "Stage", "Cultivar"))

till.mean.change_perc <- till.means$mean[till.means$Cultivar == "SSR T65"] / 
                         till.means$mean[till.means$Cultivar == "Silverstar"] * 100 -100

till.perc <- CalcPerc(till.means, c("CO2", "Stage", "Environment"))
till.perc

lme.out$Silverstar.DC31.Tillers.m2..SS.dry.
lme.out$Silverstar.DC65.Tillers.m2..SS.dry.
lme.out$Silverstar.DC90.Tillers.m2..SS.dry.

# tillering reduced between 44% and 56% depending on growth stage.

# Yield response?
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Yield..g.m2.))
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
  p <- p + labs(y = expression(Yield~(g~m^-2)))
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.90))
p
fig.yield.m2 <- p

ggsave(file = "Fig.02.Yield_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC90.Yield..g.m2.

# Yield is increased by eCO2 in each cultivar
# yield is higher in Silverstar compared to SSR T65
# yield is affected by environment

yield.means <- CalcRes(df.melt, "Yield..g.m2.", 
                       c("CO2", "Environment", "Stage", "Cultivar"))
yield.means[!is.na(yield.means$mean), ]
dlply(yield.means[!is.na(yield.means$mean), ],
      .(CO2),
      function(x) {summary(x)})
      
summary(yield.means[!is.na(yield.means$mean), ])

yield.mean.change_perc <- yield.means$mean[yield.means$Cultivar == "SSR T65"] / 
                         yield.means$mean[yield.means$Cultivar == "Silverstar"] * 100 -100

yield.perc <- CalcPerc(yield.means, c("CO2", "Stage", "Environment"))
yield.perc
# Yield is increased by eCO2 in each cultivar
# yield is higher in Silverstar compared to SSR T65 +22%
# yield is affected by environment


# Grains per tiller"Grains.tiller..SS."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Grains.tiller..SS.))
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
  p <- p + labs(y = "Grains per tiller")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.90))
p
fig.grains_per_tiller <- p

ggsave(file = "Grains_per_tiller_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC90.Grains.tiller

grains.tiller.means <- CalcRes(df.melt, "Grains.tiller..SS.", 
                       c("CO2", "Environment", "Stage", "Cultivar"))
grains.tiller.means[!is.na(grains.tiller.means$mean), ]
dlply(grains.tiller.means[!is.na(grains.tiller.means$mean), ],
      .(CO2),
      function(x) {summary(x)})
dlply(grains.tiller.means[!is.na(grains.tiller.means$mean), ],
      .(Cultivar),
      function(x) {summary(x)})     
summary(grains.tiller.means[!is.na(grains.tiller.means$mean), ])

grains.tiller.perc <- CalcPerc(grains.tiller.means, c("CO2", "Stage", "Environment"))
grains.tiller.perc



# Harvest index "Harvest.Index..AR."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Harvest.Index..AR.))
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
  p <- p + labs(y = "Harvest index")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.62, 0.15))
p
fig.harvest.index <- p

ggsave(file = "Harvest_index_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC90.Harvest.Index..AR.


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
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.62, 0.15))
p

fig.Dry.weight.heads <- p

ggsave(file = "Dry_weight_heads_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC65.Dry.wt.heads.in.AR.sample..g.m2.


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
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.62, 0.15))
p
fig.spikelets.head <- p

ggsave(file = "Spikelets_per_head_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC65.Spikelets.head

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
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.green.leaf.area <- p

ggsave(file = "Green_leaf_area_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC65.Green.leaf.area.pl..cm2.


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
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.thousand.grains <- p

ggsave(file = "Thousand_grains_weight_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC90.SS.1000.Grain.wt..g.

names(df) <- gsub("\\.\\.", "_", names(df))

# %N in grains "X.N_Grain..scan..0.."
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = X.N_Grain_scan_0_))
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
  p <- p + labs(y = "%N in grains")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.percent.N.grains <- p

ggsave(file = "Percent_N_grains_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC90.X.N_Grain..scan..0..

# "X.N_Plant..AR.biomass."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = X.N_Plant_AR.biomass.))
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
  p <- p + labs(y = "%N in plant (above root)")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.2))
p

fig.percent.N.plant <- p

ggsave(file = "Percent_N_plant_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC90.X.N_Plant..AR.biomass.


# above root biomass AR.Dry.wt.area..g.m2."
p <- ggplot(df[df$Stage != "DC31", ], 
            aes(x = Cultivar, y = AR.Dry.wt.area_g.m2.))
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
  p <- p + labs(y = expression("Above root plant biomass"~(g~m^2)))
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.above.root.biomass <- p

ggsave(file = "Above_root_biomass_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC31.AR.Dry.wt.area..g.m2.
lme.out$Silverstar.DC65.AR.Dry.wt.area..g.m2.
lme.out$Silverstar.DC90.AR.Dry.wt.area..g.m2.

above.root.means <- CalcRes(df.melt, "AR.Dry.wt.area..g.m2.", 
                       c("CO2", "Environment", "Stage", "Cultivar"))
above.root.means[!is.na(above.root.means$mean), ]
dlply(above.root.means[!is.na(above.root.means$mean), ],
      .(CO2, Stage),
      function(x) {summary(x)})
dlply(above.root.means[!is.na(above.root.means$mean), ],
      .(Cultivar, Stage),
      function(x) {summary(x)})     
summary(above.root.means[!is.na(above.root.means$mean), ])

above.root.perc <- CalcPerc(above.root.means, c("CO2", "Stage", "Environment"))
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
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.LAI <- p

ggsave(file = "Leaf_area_index_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$Silverstar.DC65.GLAI..AR.dry.

# Fun graphs
# mean data
df.mean <- cast(df.melt,
            Cultivar + CO2 + Stage + Environment ~ variable,
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
  p <- p + geom_point(aes(fill = CO2, shape = Environment))
  #p <- p + geom_smooth(aes(linetype = CO2), method = "lm", colour = "black")
  p <- p + labs(y = expression(Yield~(g~m^-2)),
                x = expression("No. of tillers"~(Tillers~m^-2)))
  p <- p + scale_shape_manual(values = c(21:24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 21)))
  p <- p + guides(linetype = FALSE)
  p <- p + facet_grid(. ~ Cultivar)
  p <- p + theme_my
p

fig.yield.per.tiller <- p

ggsave(file = "Yield_vs_tillers.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

my.lm <- lm(Yield_g.m2. ~ Tillers.m2_SS.dry., data = df[df$Stage == "DC90", ])
summary(my.lm)

# Percent N grains vs %N in plant
p <- ggplot(df[df$Stage == "DC90", ],
            aes(x = X.N_Plant_AR.biomass., y = X.N_Grain_scan_0_))
  p <- p + geom_point(aes(fill = CO2, shape = Environment))
  p <- p + geom_smooth(aes(linetype = CO2), method = "lm", colour = "black")
  p <- p + labs(y = "%N in grains",
                x = "%N in plant (above root)")
  p <- p + scale_shape_manual(values = c(21:24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 21)))
#  p <- p + guides(linetype = FALSE)
  p <- p + facet_grid(. ~ Cultivar)
  p <- p + theme_my
p

fig.N.grains.vs.N.plant <- p

ggsave(file = "Fig.3.Percent_N_grains_plant.pdf", 
       width = my.width, height = my.height, 
       units = "cm")
lme.out$Silverstar.DC90.X.N_Grain..scan..0..
lme.out$Silverstar.DC90.X.N_Plant..AR.biomass.

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
  p <- p + geom_point(aes(fill = CO2, shape = Environment))
  p <- p + geom_smooth(aes(linetype = CO2), method = "lm", colour = "black")
  p <- p + labs(y = expression(Yield~(g~m^-2)),
                x = "Harvest index")
  p <- p + scale_shape_manual(values = c(21:24))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 21)))
  p <- p + guides(linetype = FALSE)
  p <- p + facet_grid(. ~ Cultivar)
  p <- p + theme_my
p

fig.yield.per.harvest.index <- p

ggsave(file = "Yield_vs_harvest_index.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

my.lm <- lm(Yield_g.m2. ~ Harvest.Index_AR., data = df[df$Stage == "DC90", ])
summary(my.lm)

# yield vs water 
my.envs <- sort(unique(df$Ord.Environment))
water_received <- c(218, 180, 300, 318)

seasonal_water <- data.frame(Environment = my.envs,
                             Seasonal_water.mm = water_received)
df.melt.yield <- df.melt[df.melt$variable == "Yield..g.m2.", ]

df.yield.water <- merge(df.melt.yield, seasonal_water)

p <- ggplot(df.yield.water[df.yield.water$Stage == "DC90", ], 
            aes(x = Seasonal_water.mm, y = value))
  p <- p + geom_smooth(aes(colour = CO2, linetype = Cultivar), 
                       method = "lm")
  p <- p + geom_point(aes(fill = CO2, shape = Cultivar))
  p <- p + scale_fill_manual(values = c("white", "black"))
  p <- p + scale_shape_manual(values = c(21:24))
  p <- p + guides(fill = guide_legend(override.aes = list(shape = 21)))
  #p <- p + guides(linetype = FALSE)
  p <- p + labs(y = expression(Yield~(g~m^-2)),
                x = "Water received during season (mm)")
  p <- p + theme_my
 # p <- p + facet_grid(Cultivar ~ .)
p

fig.yield.per.water <- p

ggsave(file = "Fig.2b.Yield_vs_water.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

# test if the confidence intervall of the slope includes 0 using the confint() function on a lm model
my.lm.Silver.eCO2 <- lm(value ~ Seasonal_water.mm,
        data = df.yield.water[df.yield.water$CO2 == "eCO2" &
                              df.yield.water$Cultivar == "Silverstar",])
summary(my.lm.Silver.eCO2)
confint(my.lm.Silver.eCO2) # confint does not include 0

my.lm.Silver.aCO2 <- lm(value ~ Seasonal_water.mm,
        data = df.yield.water[df.yield.water$CO2 == "aCO2" &
                              df.yield.water$Cultivar == "Silverstar",])
summary(my.lm.Silver.aCO2)
confint(my.lm.Silver.aCO2) # includes 0

my.lm.SSRT65.aCO2 <- lm(value ~ Seasonal_water.mm,
        data = df.yield.water[df.yield.water$CO2 == "aCO2" &
                              df.yield.water$Cultivar == "SSR T65",])
summary(my.lm.SSRT65.aCO2)
confint(my.lm.SSRT65.aCO2) # confint includes 0



# assemble figures
my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))

pdf(file = "Tillering_figures.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(my.figures.list)
dev.off()


