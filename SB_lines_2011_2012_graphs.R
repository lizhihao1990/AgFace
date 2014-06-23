# Import carbohydrate data for 2011 and 2012

setwd("~/AgFace/Topics/SB_lines_2011_2012")

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

# get rid of "Silverstar" data, only keep "SB"!
df      <- df[df$my.Trait           == "SB", ]
df.melt <- df.melt[df.melt$my.Trait == "SB", ]

Carbs      <- Carbs[Carbs$Trait           == "SB", ]
Carbs.melt <- melt(Carbs)
Carbs.melt <- Carbs.melt[Carbs.melt$Trait == "SB", ]

# re-name traits to my.Trait to match plant production data sets
names(Carbs)      <- gsub("Trait", "my.Trait", names(Carbs))
names(Carbs.melt) <- gsub("Trait", "my.Trait", names(Carbs.melt))

# re-name cultivars to get rid of "high" and "low"
df$Cultivar <- gsub(" low| high", "", df$Cultivar)
df$Cultivar <- as.factor(df$Cultivar)

# re-order environments to only reflect SB
df$Ord.Environment <- factor(df$Ord.Environment, 
                            levels = c("2012.Rain", "2011.Rain","2011.Sup", "2012.Sup"))
df.melt$Ord.Environment <- factor(df.melt$Ord.Environment, 
                            levels = c("2012.Rain", "2011.Rain","2011.Sup", "2012.Sup"))
Carbs$Ord.Environment <- factor(Carbs$Ord.Environment, 
                            levels = c("2012.Rain", "2011.Rain","2011.Sup", "2012.Sup"))
Carbs.melt$Ord.Environment <- factor(Carbs.melt$Ord.Environment, 
                            levels = c("2012.Rain", "2011.Rain","2011.Sup", "2012.Sup"))
                            
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
           sb003 <- x$mean[x$Cultivar == "SB003"]
           sb062  <- x$mean[x$Cultivar == "SB062"]
           perc <- sb062/sb003 * 100 - 100
           names(perc) <- "Percent_reduction_in_sb062"
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
CO2.label  <- expression(textstyle(CO[2]~treatment))
CO2.treats <- c(expression(textstyle(aCO[2])), 
                expression(textstyle(eCO[2])))

# Use UoM colours for lines
use_UoM_colour = FALSE
my.UoM.colour = "#003366"

my.colour_manual <- if(use_UoM_colour == TRUE) {
                       c(rep(my.UoM.colour, 2))
                     } else { 
                       c(rep("black", 2))}
# -------------------------------------------------------
# Some Plant production figures
# -------------------------------------------------------

# yield components
# heads per m2 (= fertile tillers per m2), 
# grain number per head (rather than grain number per m2), 
# and grain weight.

# Head numbers

p <- ggplot(df[df$Stage == "DC65" | df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Heads.m2..SS.dry.))
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
                        values = my.colour_manual)
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression(Heads~per~m^2),
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.4, 0.90))
p

fig.Heads_p_m2 <- p

ggsave(file = "Heads_per_m2_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$SB.DC65.Heads.m2..SS.dry.
lme.out$SB.DC90.Heads.m2..SS.dry.


# Grains per Head "Grains.head_SS."
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Grains.head..SS.))
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
  p <- p + labs(y = "Grains per head",
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.90))
p
fig.grains_per_head <- p

ggsave(file = "Grains_per_head_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

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
  p <- p + labs(y = expression("1000 grains weight"~(g)),
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.85))
p

fig.thousand.grains <- p

ggsave(file = "Thousand_grains_weight_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

lme.out$SB.DC90.SS.1000.Grain.wt..g.

# raster plot
p <- ggplot(df, aes(x = SS.1000.Grain.wt..g., y = Grains.head..SS.))
  p <- p + geom_point(aes(shape = Cultivar, fill = Heads.m2..SS.dry.))
  p <- p + scale_shape_manual(values = c(21, 22))
  p <- p + scale_fill_gradient(low = "white", high = "black")
  p <- p + facet_grid(. ~ CO2)
  p <- p + theme_my
p


# Additionallym incontrast to heads/m2, now tillers per m2


# Head numbers

p <- ggplot(df[df$Stage == "DC65" | df$Stage == "DC90", ], 
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
                        values = my.colour_manual)
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression(Tillers~per~m^2),
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.4, 0.90))
p

fig.Tillers_p_m2 <- p

ggsave(file = "Tillers_per_m2_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

# out of curiosity:
# ratio of heads and tillers
p <- ggplot(df[df$Stage != "DC31", ], 
            aes(x = Tillers.m2..SS.dry., y = Heads.m2..SS.dry.))
  p <- p + geom_point(aes(shape = Cultivar, colour = CO2))
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + labs(y = expression(Heads~per~m^2),
                x = expression(Tillers~per~m^2))
  p <- p + theme_my
p

fig.Tiller.Head.relationship <- p
ggsave(file = "Tillers_vs_Head_relationship.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

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
  p <- p + labs(y = expression(Yield~(g~m^-2)),
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.14, 0.8))
p
fig.yield.m2 <- p

ggsave(file = "Yield_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

ggsave(file = "Yield_se_poster.pdf", 
       width = my.width * 1.25, height = my.height * 0.75, 
       units = "cm")

names(df) <- gsub("X.N_Grain..scan..0..", "X.N_Grain_scan_0", names(df))
# %N in grains "X.N_Grain..scan..0.."
p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = X.N_Grain_scan_0))
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
  p <- p + labs(y = "%N in grains",
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  #p <- p + theme(legend.position = c(0.38, 0.85))
  p <- p + theme(legend.position = "none")
p

fig.percent.N.grains <- p

ggsave(file = "Percent_N_grains_se_poster.pdf", 
       width = my.width * 1.25, height = my.height * 0.75, 
       units = "cm")

lme.out$SB.DC90.X.N_Grain..scan..0..

# --------------------------------------------------
# Carbohydrate figures
# --------------------------------------------------

p <- ggplot(Carbs[Carbs$Organ == "Stem", ],# &
                  #Carbs$Stage == "DC65", ],
            aes(x = Cultivar, y = Conc..mg.mg.))
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
  p <- p + labs(y = expression(Stem~carbohydrate~concentration~(mg~mg^-1)),
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment, scale = "free_y")
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.63, 0.85))
p

fig.Carbs.stem <- p

ggsave(file = "Stem_WSC_SB.pdf", 
       width = my.width * 1.25, height = my.height * 0.75, 
       units = "cm")

p <- ggplot(Carbs[Carbs$Organ == "Leaf", ],
            aes(x = Cultivar, y = Conc..mg.mg.))
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
#  p <- p + annotate("text", y = 0.2, x = 0.5, 
#                    label = "Stat. sign effects:\n
#                             Environment < 0.05\n
#                             Line x Environment < 0.05", 
#                             hjust = 0, size = 3)
  p <- p + labs(y = expression(Leaf~carbohydrate~concentration~(mg~mg^-1)),
                x = "Line")
  p <- p + facet_grid(Stage ~ Ord.Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.63, 0.85))
p

fig.Carbs.leaf <- p

ggsave(file = "Leaf_WSC_SB.pdf", 
       width = my.width * 1.25, height = my.height * 0.75, 
       units = "cm")
       
# --------------------------------------------------
# assemble figures
# --------------------------------------------------
my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))
names(my.figures.list) <- my.figures

pdf(file = "SB_figures.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(my.figures.list)
dev.off()


