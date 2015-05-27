# Analysis of Yipti N0/N+ experiment
# data from the years 2007, 2008, 2009
# Data from Michael Tausz

# this script is based on the "Plant_Production_2007_2009.R" script.
# it loads the workspace created from this script

# set working directory
setwd("~/AgFace/Nitrogen_2007_2009")

# load workspace created by the "Plant_Production_2007_2009.R" script
load("../Plant_Production/Plant_Production_data_02.RData")

# the workspace has remnants from the general data import and data preparation
# getting rid of those to start with a clean plate.

# objects and data to keep in the workspace (just the data and the data in "long" format)
to_keep <- c("df", "df.melt")

# remove elements from the workspace that we do not need
rm(list = ls()[!(ls() %in% to_keep)])

# load libraries
require(plyr)
require(reshape)
require(ggplot2)
require(nlme)

# load AgFACE helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# load Yitpi N-experiment helper script
source("~/AgFace/R_scripts/Yitpi_N_experiment_2007_2009_helper_script.R")

# Data analysis

# Michael email July 1, 2013: One thing I would like to look at soon is a comparison between “Yitpi N0” and “Yitpi N+”, mainly with respect to N in leaves and grain. We have these variants in all three years.

# new subset with Yitpi only
yitpi <- df[(df$PlotTrt == "YitpiN0" | 
             df$PlotTrt == "YitpiN+" )&
             !is.na(df$Sample.Date), ]

# why is there PlotTrt "Janz" in the data for "Yitpi" Cultivars in 2007 (32 cases)??
# has been solved in the original "df" already (see Plant_Production script).
# yitpi[yitpi$PlotTrt == "Janz", ]
# yitpi <- yitpi[yitpi$PlotTrt != "Janz", ]

# display the 2007 data
yitpi[yitpi$Sample.Date < as.Date("2008-01-01"), ]

# create additional identifiers
# create new variable N_treat from PlotTrt - yes, it is redundant, but I want to separate nitrogen information from cultivar information.
yitpi$N_treat <- "none"
yitpi$N_treat[grep("N\\+", yitpi$PlotTrt)] <- "N+"
yitpi$N_treat[grep("N0",   yitpi$PlotTrt)] <- "N0"
yitpi$N_treat <- as.factor(yitpi$N_treat)

# specify factor levels explicitly (as lexically, "+" is before "0"), zero first seems more natural
yitpi$N_treat <- factor(yitpi$N_treat, levels = c("N0", "N+"))

# create Environment variable               
yitpi$Environment <- interaction(yitpi$Year, 
                                 yitpi$Irrigation,
                                 yitpi$TOS)

# create unique identifiers for halfrings per year, rings, an halfring
# Original HalfRingID is only "E" or "W", not explicit.
yitpi$my_halfring_ID <- interaction(yitpi$HalfRingID, 
                                    yitpi$RingID, 
                                    yitpi$Year)


# sort Environment by increasing Grain yield in aCO2
# Michael: I would group the environments according to their grain yield under aCO2 on the x axis. This should show us already lots!

Ord.Env <- ddply(yitpi[yitpi$CO2 == "aCO2", ],
              .(Environment),
                summarise,
                mean_yield = mean(Yield..g.m2., na.rm = TRUE))

OEnv <- factor(Ord.Env$Environment,
               levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
               ordered = TRUE)


#yitpi$Ord.Environment <- factor(yitpi$Environment,
#                                levels = unique(yitpi$Environment[order(yitpi$Yield..g.m2.)]),
#                                ordered = TRUE)

yitpi$Ord.Environment <- factor(yitpi$Environment,
                                levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
                                ordered = TRUE)

# re-order yitpi data frame
column_order <- c(1:20, 190, 191, 192, 193, 21:189)
yitpi <- yitpi[, column_order]

# put yitpi data in long format (Emergence.date is a problem, as it is a date, not numeric -  treating it as a descriptor for now, not a measured paramter)
yitpi.melt <- melt(yitpi,
                   id = names(yitpi[1:24]))


# create yitpi_plots
yitpi_plots <- dlply(yitpi.melt,
                  .(variable),
                  function(x) {
                  MyMultPlotPlantProd(x, x$variable, "Stage", "value", "CO2", "Irrigation", "N_treat", ".")})

#pdf(file = "yitpi.pdf")
#        print(yitpi_plots)
#dev.off()

save.image("Yitpi_N_analysis_00.RData", compress = TRUE)
# load("Yitpi_N_analysis_00.RData")

# get the paramter names that contain "N"
names(yitpi)[grep("N", names(yitpi))]

# some graphs
p <- ggplot(yitpi, aes(x = Stage, y = N_Leaf..g.m2.))
        p <- p + stat_summary(aes(colour = CO2), fun.dat = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour = CO2), fun.dat = mean_sdl, mult = 1,
                              geom = "line")
        p <- p + stat_summary(aes(shape = CO2, y = N_Grain..g.m2.), 
                              fun.dat = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(shape = CO2, y = N_Grain..g.m2.), 
                              fun.dat = mean_sdl, mult = 1, geom = "line")
        p <- p + facet_grid(PlotTrt ~ Irrigation)
p

# write data to disk
write.table(yitpi,
            file = "Yitpi_Plant_production_2007-2009.csv",
            row.names = FALSE,
            sep = ",",
            na = "")
            
# Some analysis principles:
# create 'Environment' variable from interaction(year x Irrigation x TOS)
# CO2 as fixed factor (independent)
# N0/N+ as fixed factor (split-plot)

# look into N-percentages first, later potentially N (g/m2)

# visualise the analysis
p <- ggplot(yitpi, aes(x = Year, y = X.N_Leaf))
        p <- p + stat_summary(aes(colour = CO2, shape = TOS, linetype = Stage), 
                              geom = "pointrange", 
                              #position = "jitter", 
                              position = position_jitter(width = 0.25),
                              fun.data = mean_sdl, mult = 1)
        p <- p + facet_grid(Irrigation ~ N_treat)
        p <- p + theme_bw()
p

p <- ggplot(yitpi, aes(x = Year, y = X.N_Grain..2010.scan..0..moisture.))
        p <- p + stat_summary(aes(colour = CO2, shape = TOS, linetype = Stage), 
                              geom = "pointrange", 
                              #position = "jitter", 
                              position = position_jitter(width = 0.25),
                              fun.data = mean_sdl, mult = 1)
        p <- p + facet_grid(Irrigation ~ N_treat)
        p <- p + theme_bw()
p



# Detailed analysis

# create "Environment" and halfring descriptor
# halfring has to be unique per Year, and Ring
#yitpi$Environment    <- interaction(yitpi$Year, yitpi$Irrigation, yitpi$TOS)
#yitpi$my_halfring_ID <- interaction(yitpi$HalfRingID, yitpi$RingID, yitpi$Year)

# Data analysis at stage
# DC30: above root biomass, stem, and leaf %N
# DC65: above root biomass, stem, and leaf %N
# DC90: above root biomass / straw, chaff, grain yield, Grain %N, Straw, Chaff

# Doing the analysis in a loop for all of the mentioned parameters.
# the redundant results or resutls from the wrong stage have to be discarded after the fact.

 
# get the data for a specific Growth stage
dc65yitpi <- yitpi[yitpi$Stage == "DC65", ]



# ANOVA design
# Nested design (split-plot): Fertilizer treatment nested withing Irrigation plots. CO2 not part of the split! 
# Nesting provided in the model error term from largest to smallest.
# CO2 shows up as within-error
my.aov <- aov(X.N_Leaf ~ N_treat * CO2 * Environment + Error(HalfRingID/Irrigation/ N_treat), 
              data = yitpi)


# using liner mixed effects model

# See book: http://link.springer.com.ezp.lib.unimelb.edu.au/book/10.1007/b98882/page/1

# agreed design:
# lme(AR.Dry.wt.area..g.m2. ~ N_treat * CO2 * Environment,
#                      random = ~ 1 | my_halfring_ID/N_treat, 

# aov.repeated <- aov(DV ~ IV1 * IV2 * Time + Error(Subject/Time), data=data)

# repeated measures anova
ARbio.aov <- aov(AR.Dry.wt.area..g.m2. ~ N_treat * CO2 * Environment + Error(my_halfring_ID/N_treat), data = dc65yitpi)
summary(ARbio.aov)

# Above root biomass
ARbio.lme <- lme(AR.Dry.wt.area..g.m2. ~ N_treat * CO2 * Environment,
                      random = ~ 1 | my_halfring_ID/N_treat, 
                      data = dc65yitpi,
                      na.action = na.omit)
summary(ARbio.lme)
anova(ARbio.lme)
m0.lme <- lm(AR.Dry.wt.area..g.m2. ~ N_treat * CO2 * Environment, 
                      data = dc65yitpi,
                      na.action = na.omit)

anova(ARbio.lme, m0.lme)

# extract random effects (Intercepts) for nesting component
ARbio.rinterc <- random.effects(ARbio.lme)
ARbio.rinterc$N_treat$'(Intercept)'
intervals(ARbio.lme)

# test difference of intercepts to 0
t.test(ARbio.rinterc$N_treat$'(Intercept)')
mean(ARbio.rinterc$N_treat$'(Intercept)')
x <- c(34, 67, 85, 103, 24)

# Leaf N percentage
Leaf_N.lme = lme(X.N_Leaf ~ N_treat * CO2 * Environment,
                      random = ~ 1 | my_halfring_ID / N_treat,
                      data = dc65yitpi,
                      na.action = na.omit)
summary(Leaf_N.lme)
anova(Leaf_N.lme)


# in a a loop:
# first create a list of the N-related parameters that we want:
my_N_variables <- c("Yield..g.m2.",
                    "AR.Dry.wt.area..g.m2.",
                    "X.N_Leaf",
                    "N_Leaf..g.m2.",
                    "X.N_Dead.leaf",
                    "N_Dead.leaf..g.m2.",
                    "X.N_Stem",
                    "N_Stem..g.m2.",
                    "X.N_Pseudo.stem",
                    "N_Pseudo.stem..g.m2.",
                    "X.N_Heads",
                    "N_Heads..g.m2.",
                    "X.N_Straw", 
                    "N_Straw..g.m2.",
                    "X.N_Chaff",
                    # "X.N_Grain..original.scan.", 
                    "X.N_Grain..2010.scan..0..moisture.",
                    "N_Grain..g.m2.",
                    "AR.total.N.uptake..g.m2.",
                    # "AG.total.N.uptake..g.m2.",
                    "X.N_Plant..AR.biomass.",
                    "X.N_Plant..AG.biomass.",
                    "SLN..gN.m2.",
                    "Straw.C.N", 
                    "N.HI..AR.", 
                    "NUE..yield.N.uptake.")

# run the analysis on all cases
lme.out <- dlply(yitpi.melt[yitpi.melt$variable %in% my_N_variables, ],
                 .(variable, Stage),
                 function(x) {
                 
                 out <- try(anova(lme(value ~ N_treat * CO2 * Environment,
                      random = ~ 1 | my_halfring_ID / N_treat,
                      data = x,
                      na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lme.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lme.out <- out}
                 })
sink("nlme_results.txt")
        print(lme.out)
sink()



# check missing cases
ddply(yitpi,
      .(Year, my_halfring_ID, Stage),
      summarise,
      available_samples = length(X.N_Leaf),
      missing_samples   = sum(is.na(X.N_Leaf)))

# graphs per Parameter and Year
p <- ggplot(yitpi, aes(x = Year, y = X.N_Chaff))
        p <- p + geom_boxplot(aes(fill = CO2, linetype = N_treat), outlier.colour = NA)
        p <- p + facet_grid(Stage ~ .)
        p <- p + theme_bw()
p


# Boxplots in a loop
my.boxplots <- dlply(yitpi.melt[yitpi.melt$variable %in% my_N_variables, ],
                  .(variable),
                  function(x) {
                  
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Ord.Environment", 
                            yaxis = "value", 
                            treatment_sep_a = "CO2", 
                            treatment_sep_b = "N_treat", 
                            facet_var_a = "Stage", 
                            facet_var_b = ".",
                            two_separators = TRUE)})

pdf("Yitpi_boxplots.pdf")
        print(my.boxplots)
dev.off()

p <- ggplot(yitpi, aes(x = N_treat, y = AR.Dry.wt.area..g.m2.))
        p <- p + stat_summary(aes(fill = CO2, colour = CO2),  
                                  fun.y = mean,
                                  mult = 1,
                                  geom = "bar",
                                  position = "dodge")
        p <- p + stat_summary(aes(colour = CO2), 
                                  fun.data = mean_sdl,
                                  mult = 1,
                                  geom = "linerange",
                                  position = position_dodge(width = 0.9))
        p <- p + scale_colour_manual(values = c("black", "black"), guide=FALSE)
        p <- p + scale_fill_manual(values = c("white", "grey"))
        p <- p + facet_grid(Stage ~ .)
p

ddply(yitpi,
     .(N_treat, CO2, Stage),
     summarise,
     mean = mean(AR.Dry.wt.area..g.m2., na.rm = TRUE),
     sd   = sd(AR.Dry.wt.area..g.m2., na.rm = TRUE))

tapply(yitpi$AR.Dry.wt.area..g.m2.,
       INDEX = list(yitpi$N_treat, yitpi$CO2, yitpi$Stage),
       fun = mean)

# Barplots

my.N.barplots <- dlply(yitpi.melt[yitpi.melt$variable %in% my_N_variables, ],
                  .(variable),
                  function(x) {
                  MyBarplot(x, x$variable, "N_treat", "value", "CO2", "Stage", ".")})

pdf("Yitpi_N_barplots.pdf")
        print(my.N.barplots)
dev.off()

# calculate difference between "X.N_Plant..AG.biomass." at DC31 and DC65 and between DC65 and DC90

DiffBetGrowthPeriod <- function(data, para, first.period = "DC30", second.period = "DC65", third.period = "DC90") {
  # Calculates the difference between a given parameter at two different growth stages
 
  # get rid of other growth period
  #data <- data[data$Stage %in% c(first.period, second.period), ]
  
  # get a handle on the column that holds the paramter in question
  my.column <- which(names(data) == para)
  value.first.period <- data[data$Stage == first.period, my.column]
 
  value.second.period <- data[data$Stage == second.period, my.column]

  value.third.period <- data[data$Stage == third.period, my.column]
  
  my.diff.second.minus.first <- value.second.period - value.first.period
  my.diff.third.minus.second <- value.third.period - value.second.period
  # assemble a result table
  my.table <- data.frame(first.period = value.first.period,
                         second.period = value.second.period,
                         third.period = value.third.period,
                         Diff.second.first = my.diff.second.minus.first,
                         Diff.third.second = my.diff.third.minus.second)
  my.name.second.minus.first <- paste(second.period, first.period, sep = "_minus_")
  my.name.third.minus.second <- paste(third.period, second.period, sep = "_minus_")
  names(my.table) <- c(first.period, second.period, third.period, my.name.second.minus.first, my.name.third.minus.second)
  return(my.table)
}

# using plyr to split the data and apply the DiffBetGrowthPeriod function
require(plyr)

out <- ddply(yitpi,
            .(Year, RingID, HalfRingID, CO2, Irrigation, TOS, Cultivar, N_treat),
            function(x) DiffBetGrowthPeriod(x, para = "N_Leaf..g.m2."))

write.table(out, file = "Differences_between_growth_stages.csv",
            sep = ",", row.names = FALSE, na = "")

# data checks
# N leaf (g m-2) should be %Nleaf * leaf biomass (g m-2) /100

my.nleaf.g.m2 <- with(yitpi, (X.N_Leaf * Dry.wt.leaf.in.AR.sample..g.m2.) /100)
summary(my.nleaf.g.m2)

# compare with N_Leaf..g.m2.
summary(yitpi$N_Leaf..g.m2.)
summary(yitpi$X.N_Leaf)

# N uptake into ABG biomass (or whatever it is called) should be the sum of N Leaf (per m2), and N stem (m2)

my.uptake <- with(yitpi[yitpi$Stage == "DC30", ], N_Leaf..g.m2. + N_Stem..g.m2.)
my.uptake
# comapre with AG.total.N.uptake..g.m2.
yitpi$AG.total.N.uptake..g.m2.[yitpi$Stage == "DC30"]
