# Tillering 2011 2012 further analysis
# based on workspace created by script
# "Tillering_gene_2011_2012_graphs_based_on_years.R"

library(ggplot2)

setwd("~/AgFace/Topics/Tillering_2011_2012")

# load workspace
load("Tin_2011_2012_plant_production_Carbs.RData")

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
