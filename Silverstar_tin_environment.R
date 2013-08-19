# As discussed, I would like to look at the contrasting pairs “Silvertar” versus “Silverstar – tin” (or named as something with SST...) and the two “SB...” lines. There should be 4 environments across the tow years, just irrigated and non-irrigated, but no TOS.

# load libraries
require(xlsx)
require(reshape)
require(plyr)
require(nlme)

# set working directory
setwd("~/AgFace/Plant_Production/Silverstar_tin")

# load Yitpi N-experiment helper script for custom Boxplot function
source("~/AgFace/R_scripts/Yitpi_N_experiment_2007_2009_helper_script.R")


df2011 <- read.xlsx("../../2011/2011_AGFACE_Wheat_Plant_production_Summary_4_markus.xls",
                    sheetName = "Final")

df2012 <- read.xlsx("../../2012/2012_AGFACE_Wheat_Plant_production_Summary_2_markus.xls",
                    sheetName = "Final")

# WUE contains "Div/0" errors from excel. Those get imported with an error message. Replacing the error message with "NA"
imp_errors <- grep("jav", df2012$WUE)
df2012$WUE[imp_errors] <- as.numeric(NA)
df2012$WUE <- as.numeric(df2012$WUE)

# get rid of cases without database entry
df2011 <- df2011[!is.na(df2011$CO2.Irr.Cultivar.DATABASE), ]

# bold move: using the names of 2012 for 2011
names(df2011) <- names(df2012)

# put both years together
df <- rbind(df2011, df2012)

# add missing "RingPos." information
df$RingPos.[is.na(df$RingPos.) & df$HalfRing == 1] <- as.factor("W")
df$RingPos.[is.na(df$RingPos.) & df$HalfRing == 2] <- as.factor("E")


# data formatting
df[, 1:19] <- lapply(df[, 1:19], as.character)
df[, 1:19] <- lapply(df[, 1:19], as.factor)

# show the available Cultivars
sort(unique(df$Cultivar))

# we need "SB003 low"     "SB062 high"    "Silverstar"    "SSR T65"

# is there a usable yield column?
names(df)[grep("ield", names(df))]
# yes, as usual "Yield..g.m2."

ddply(df,
      .(Cultivar, Year, Stage, CO2, Irrigation),
      summarise,
      no_of_samples = length(Yield..g.m2.),
      mean          = mean(Yield..g.m2., na.rm = TRUE))

# create "Environments" - expecting four Environments
df$Environment <- interaction(df$Year, df$Irrigation, drop = TRUE)

# create halfringID
df$my.HalfringID <- interaction(df$Year, df$RingID, df$RingPos.)

# list of the cultivars for this comparison
my.cultivars <- c("SB003 low", "SB062 high", "Silverstar", "SSR T65")

my.SB.cultivars     <- c("SB003 low", "SB062 high")
my.Silver.cultivars <- c("Silverstar", "SSR T65")

df$my.Trait <- "none"
df$my.Trait[df$Cultivar %in% my.SB.cultivars]     <- "SB"
df$my.Trait[df$Cultivar %in% my.Silver.cultivars] <- "Silverstar"
df$my.Trait <- as.factor(df$my.Trait)


# create a list of parameters to avoid:
# some keywords: remainder, fresh weight, rows, edge,
bad.remainder   <- names(df)[grep("Remainder", names(df), ignore.case = TRUE)]
bad.freshweight <- names(df)[grep("fresh",     names(df), ignore.case = TRUE)]
bad.rows <- names(df)[grep("Row", names(df),  ignore.case = TRUE)]
bad.edge <- names(df)[grep("edge", names(df), ignore.case = TRUE)]

bad.paras <- c(bad.remainder, bad.freshweight, bad.rows, bad.edge)
good.paras <- names(df)[!names(df) %in% bad.paras]


Ord.Env <- ddply(df[df$CO2 == "aCO2" &
                    df$Cultivar == "Silverstar", ],
              .(Environment),
                summarise,
                mean_yield = mean(Yield..g.m2., na.rm = TRUE))

df$Ord.Environment <- factor(df$Environment,
                             levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
                             ordered = TRUE)

# re-order the columns to have all descriptors in front
df <- df[, c(1:21, 153, 154, 155, 156, 22:152)]

# have to sacrifice Emergence.date again - converting to numeric
df$Emergence.date <- as.numeric(df$Emergence.date)

# create long data format
df.melt <- melt(df,
                id = names(df)[1:25])



# create boxplots
# Boxplots in a loop for YitpiN0 only
my.boxplots <- dlply(df.melt[df.melt$Cultivar %in% my.cultivars &
                             df.melt$variable %in% good.paras, ],
                  .(variable),
                  function(x) {
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Ord.Environment", 
                            yaxis = "value", 
                            treatment_sep_a = "Cultivar", 
                            treatment_sep_b = "CO2", 
                            facet_var_a = "Stage", 
                            facet_var_b = "my.Trait",
                            two_separators = TRUE)})

pdf("Silverstar_SBx_Environments_boxplots.pdf")
        print(my.boxplots)
dev.off()

# Run an anova
# run the analysis on all cases for YitpiN0
aov.out <- dlply(df.melt[df.melt$Cultivar %in% my.cultivars&
                         df.melt$variable %in% good.paras, ],
                 .(my.Trait, variable, Stage),
                 function(x) {
                 
                 out <- try(summary(aov(value ~ CO2 * Cultivar * Environment + Error(my.HalfringID/Cultivar),
                                    data = x,
                                    na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      aov.out <- "not testable"} 
                 else {
                      print("successful fit")
                      aov.out <- out}
                 })
sink("Silverstar_SBx_Environments_Anova_results.txt")
        print(aov.out)
sink()

# Run nlme
# run the analysis on all cases
lme.out <- dlply(df.melt[df.melt$Cultivar %in% my.cultivars &
                         df.melt$variable %in% good.paras, ],
                 .(my.Trait, variable, Stage),
                 function(x) {
                 
                 out <- try(anova(lme(value ~ CO2 * Cultivar * Environment,
                      random = ~ 1 | my.HalfringID/Cultivar,
                      data = x,
                      na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lme.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lme.out <- out}
                 })
sink("Silverstar_SBx_Environments_lme_results.txt")
        print(lme.out)
sink()
