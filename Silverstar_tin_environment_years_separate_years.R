# As discussed, I would like to look at the contrasting pairs “Silvertar” versus “Silverstar – tin” (or named as something with SST...) and the two “SB...” lines. There should be 4 environments across the tow years, just irrigated and non-irrigated, but no TOS.

# load libraries
require(xlsx)
require(reshape)
require(plyr)
require(nlme)
require(car)

# set working directory
setwd("~/AgFace/Plant_Production/Silverstar_tin/separate_years")

# load Yitpi N-experiment helper script for custom Boxplot function
# load script for BoxCox transformation
source("~/AgFace/R_scripts/Yitpi_N_experiment_2007_2009_helper_script.R")
source("~/AgFace/R_scripts/BoxCox_transformation.R")

#df2011 <- read.xlsx("../../../2011/2011_AGFACE_Wheat_Plant_production_Summary_4_markus.xls",
#                    sheetName = "Final")

#df2012 <- read.xlsx("../../../2012/2012_AGFACE_Wheat_Plant_production_Summary_2_markus.xls",
#                    sheetName = "Final")

#save.image("workspace_00.RData", compress = TRUE)
load("workspace_00.RData")
# carbs
load("~/AgFace/Topics/Tillering_2011_2012/WSC/Carbohydrate_tillering_workspace.RData")

# WUE contains "Div/0" errors from excel. Those get imported with an error message from java. Replacing the error message with "NA"
imp_errors <- grep("jav", df2012$WUE)
df2012$WUE[imp_errors] <- as.numeric(NA)
df2012$WUE <- as.numeric(df2012$WUE)

# get rid of cases without database entry - those should not be there in the first place!
df2011 <- df2011[!is.na(df2011$CO2.Irr.Cultivar.DATABASE), ]

# bold move: using the names of 2012 for 2011. Confirmed procedure with Glenn.
names(df2011) <- names(df2012)

# put both years together
df <- rbind(df2011, df2012)

# add missing "RingPos." information
df$RingPos.[is.na(df$RingPos.) & df$HalfRing == 1] <- "W"
df$RingPos.[is.na(df$RingPos.) & df$HalfRing == 2] <- "E"
df$RingPos. <- as.factor(df$RingPos.)

# data formatting, apply the correct class to the descriptive parameters
df[, 1:19] <- lapply(df[, 1:19], as.character)
df[, 1:19] <- lapply(df[, 1:19], as.factor)

# show the available Cultivars
sort(unique(df$Cultivar))

# we need "SB003 low"     "SB062 high"    "Silverstar"    "SSR T65"


# import the Carbohydrates DC31 data for 2011 and 2012 and make sure the format matches
# well, keeping Carbohydrates separate, file formats are not easily compatible
# load("../../Topics/Carbohydrates_DC31_2011_2012/CarbohydratesDC31_2011_2012.RData", .GlobalEnv)


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

# create halfringID for nesting
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

# assemble a list of parameters that are not analysed
bad.paras <- c(bad.remainder, bad.freshweight, bad.rows, bad.edge)

# remaining parameters as automatically the ones to use later
good.paras <- names(df)[!names(df) %in% bad.paras]


# limiting the "good.paras" list to the parameters listed by Glenn:
#parameters_to_keep <- c("Emergence.Plants.m2", "AR.Dry.wt.area..g.m2.", "Plant.wt..g.plant.", "Plants.m2..Quadrat.", "Tillers.m2..Quadrat.", "Tillers.plant..Quadrat.", "Heads.m2..Quadrat.SS.", "Heads.plant..Quadrat.", "1000.Grain.Wt..g.", "Grains.m2", "Grains.plant..quadrat.", "Grains.tiller..quadrat.", "Grains.head..quadrat.", "Screenings...2mm.....", "Harvest.Index..AR.", "Yield..g.m2.", "Milling.Yield....", "Spikelets.head", "Spikelet.wt..g.", "Seeds.floret")
# list from Glenn does not work due to name-mismatch over the years


# parameters presented by S. Tausz-Posch et al. 2012:
# DC31
# Above ground biomass gm-2
# Leaf mass g m-2
# Stem mass g m-2
# LAI
# SLA cm2 g-1
# No of tillers m-2
DC31.paras <-  c("AR.Dry.wt.area..g.m2.",
                 "Dry.wt.leaf.in.AR.sample..g.m2.",
                 "Dry.wt.tiller.in.AR.sample..g.m2.",
                 "GLAI..AR.dry.",
                 "GAI..AR.dry.",
                 "SLA..SS.cm2.g.SS.dry.wt.")

# DC65
# Above ground biomass g m-2
# Leaf mass (green) g m-2
# Dead biomass g m-2
# Stem mass g m-2
# Spike mass g m-2
# Number of tillers m-2
# Number of spikes m-2
# SLA cm2 g-1
DC65.paras <- c("AR.Dry.wt.area..g.m2.",
                "Green.leaf.area.pl..cm2.",
                "Dry.wt.leaf.in.AR.sample..g.m2.",
                "Dry.wt.dead.leaf.in.AR.sample..g.m2.",
                "Dry.wt.tiller.in.AR.sample..g.m2.",
                "Dry.wt.heads.in.AR.sample..g.m2.",
                "Dry.wt.straw.in.AR.sample..g.m2.",
                "Tillers.m2..Quadrat...SS.fresh.plant.no.",
                "Tillers.m2..SS.dry.",
                #"Tillers.plant..Quadrant.plant.no...SS.dry.",
                "Dry.wt.heads.in.AR.sample..g.m2.",
                "Grains.tiller..SS.",
                "Heads.m2..SS.dry.")

# DC90
# Grain yield g m-2
# Above ground biomass g m-2
# Number of spikes m-2
# Number of tillers m-2
# Number of kernels spike-1
# Number of kernels g-1 spike
# Harvest index
# 1000 kernel weight
DC90.paras <- c("Yield..g.m2.",
                "Harvest.Index..AR.",
                "Spikelets.head",
                "Grains.head..SS.",
                "Grains.tiller..SS.",
                "SS.1000.Grain.wt..g.",
                "Grains.m2",
                "Heads.m2_SS.dry.")
#                "Chaff.g.m2")

Nitrogen.paras <- c("X.N_Tiller", 
                    "X.N_Leaf", 
                    "X.N_Heads",
                    "X.N_Grain..scan..0..",
                    "X.N_Straw",
                    "X.N_Plant..AR.biomass.")

parameters_to_keep <- c(DC31.paras, DC65.paras, DC90.paras, Nitrogen.paras)

parameters_to_keep <- sort(unique(parameters_to_keep))

# which parameters to use later
my.paras <- c("Yield..g.m2.", "Grains.m2",  "Grains.head..SS.", "Heads.m2..SS.dry.", "SS.1000.Grain.wt..g.", "X.N_Leaf", "X.N_Heads", "X.N_Grain..scan..0..", "WSC_conc_mg_per_mg", "WSC_conc_mg_per_mg_Stem", "WSC_conc_mg_per_mg_Leaf")
my.paras.good.names <- c("Yield", "Grains m^-2", "Grains per head", "Heads m^-2", "1000 grains weight", "%N Leaf", "%N Head", "%N Grain", "WSC concentration", "WSC conc mg per mg in Stem", "WSC conc mg per mg in Leaf")

# ==================changing from Environment approach to year approach ==========

# Order the Environments by increasing yield
# create a table with the Environments and corresponding yield
Ord.Yea <- ddply(df[df$CO2 == "aCO2" &
                    df$Cultivar == "Silverstar", ],
              .(Year),
                summarise,
                mean_yield = mean(Yield..g.m2., na.rm = TRUE))

# apply the ordering to the Environment factor in the data frame
df$Ord.Year <- factor(df$Year,
                             levels = Ord.Yea$Year[order(Ord.Yea$mean_yield)],
                             ordered = TRUE)

# re-order the columns to have all descriptors in front
df <- df[, c(1:21, 154, 155, 156, 157, 22:153)]

# have to sacrifice Emergence.date again - converting to numeric
df$Emergence.date <- as.numeric(df$Emergence.date)

# keep a copy of the data frame
df.orig <- df

# only keep the cultivars that we are interested
df <- df[df$my.Trait != "none", ]

# get rid of sowing and sample dates, as they coud be different per environment
df$Sample.date <- NA
df$Sowing.date <- NA

# create long data format
df.melt <- melt(df,
                id = names(df)[1:25])

# add an alphabetical order to the variables
df.melt$variable <- factor(df.melt$variable,
                            levels = sort(unique(as.character(df.melt$variable))))

# export tin data frame
write.table(df[df$my.Trait == "Silverstar", ],
            file = "tin_plant_production_data_2011_2012.csv",
            row.names = FALSE,
            sep = ",", na = "")

# import cumulative water amount per stage
load("~/AgFace/Topics/Met_Irri/Water_received_per_Stage_2011_2012.RData")
stage_water$Cultivar <- gsub("SB003", "SB003 low", stage_water$Cultivar)
stage_water$Cultivar <- gsub("SB062", "SB062 high", stage_water$Cultivar)
stage_water$Cultivar <- as.factor(stage_water$Cultivar)

names(stage_water) <- c("Cultivar", "Irrigation", "Year", "Stage", "DC_date", "Water_received_mm")
df.melt <- merge(df.melt, stage_water,
                 by.x = c("Year", "Irrigation", "Cultivar", "Stage"),
                 by.y = c("Year", "Irrigation", "Cultivar", "Stage"))

# create boxplots
# Boxplots in a loop
my.boxplots <- dlply(df.melt[df.melt$variable %in% parameters_to_keep, ],
                  .(variable),
                  function(x) {
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Ord.Year", 
                            yaxis = "value", 
                            treatment_sep_a = "Cultivar", 
                            treatment_sep_b = "CO2", 
                            facet_var_a = "Stage", 
                            facet_var_b = "my.Trait",
                            two_separators = TRUE,
                            the_main_title = "")})

pdf("Silverstar_SBx_Environments_boxplots_year.pdf")
        print(my.boxplots)
dev.off()


# getting rid of the stage water information again
df.melt$Water_received_mm <- NULL
df.melt$DC_Date <- NULL

# Test for normality and homogeneous variances
# Check the sample size
n_size.out <- ddply(df.melt[df.melt$variable %in% parameters_to_keep,],
                   .(my.Trait, Cultivar, Stage, variable),
                   summarise,
                   n = sum(!is.na(value)))

# Test for normality of the underlying population

shapiro.out <- ddply(df.melt[df.melt$variable %in% parameters_to_keep,],
                   .(my.Trait, Cultivar, Stage, variable),
                   function(x) {
                 
                 out <- try(shapiro.test(x$value))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      sha.out <- NA} 
                 else {
                      print("successful fit")
                      sha.out <- out$p.value}
                names(sha.out) <- "Shapiro_p_value"
                return(sha.out)
                 })

sample_and_shapiro <- merge(n_size.out, shapiro.out)

# Extract the levene result for each parameter
levene.out <- ddply(df.melt[df.melt$variable %in% parameters_to_keep,],
                   .(my.Trait, Cultivar, Stage, variable),
                   function(x) {
                 
                 out <- try(leveneTest(value ~ CO2 * Cultivar * Ord.Year,
                                    data = x))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lev.out <- NA} 
                 else {
                      print("successful fit")
                      lev.out <- out$`Pr(>F)`[1]}
                names(lev.out) <- "Levene_p_value"
                return(lev.out)
                 })

# Assemble a table on normality and homogeneity of variances
pre.tests <- merge(sample_and_shapiro, levene.out)

pre.tests$either_fail <- FALSE
pre.tests$either_fail[pre.tests$Shapiro_p_value < 0.05 |
                      pre.tests$Levene_p_value < 0.05] <- TRUE

pre.tests$Shap_Leve_fail <- FALSE
pre.tests$Shap_Leve_fail[pre.tests$Shapiro_p_value < 0.05 &
                         pre.tests$Levene_p_value < 0.05] <- TRUE

pre.tests$Shap_fail <- FALSE
pre.tests$Shap_fail[pre.tests$Shapiro_p_value < 0.05 ] <- TRUE

pre.tests$Leve_fail <- FALSE
pre.tests$Leve_fail[pre.tests$Levene_p_value < 0.05 ] <- TRUE

write.table(pre.tests,
            file = "Shapiro_wilk_Levene_result_raw_data.csv",
            row.names = FALSE,
            sep = ",",
            na = "")

# get the paramters that need transforming based on failed Levene
to_transform <- pre.tests[pre.tests$Leve_fail == TRUE, ]
to_transform <- to_transform[, c("my.Trait", "Cultivar", "Stage", "variable")]
nrow(to_transform)
paras.to_transform <- interaction(to_transform$my.Trait,
                                  to_transform$Cultivar,
                                  to_transform$Stage,
                                  to_transform$variable,
                                  drop = TRUE)
                               
length(sort(unique(paras.to_transform)))

save.image("Silverstar_tin_workspace_00_years_separate.RData")
# load("Silverstar_tin_workspace_00.RData")

df.melt.sep <- df.melt
df.melt.sep$inter <- interaction(df.melt.sep$my.Trait,
                                 df.melt.sep$Cultivar,
                                 df.melt.sep$Stage,
                                 df.melt.sep$variable)

df.melt.to_transform <- df.melt.sep[df.melt.sep$inter %in%             
                                    paras.to_transform, ]
df.melt.no_transform <- df.melt.sep[!df.melt.sep$inter %in%             
                                    paras.to_transform, ]

# Transform all data to achieve homogeneity of variances
# Levene test does not honour nesting. Only testing the three factor groups
df.melt.transformed  <- ddply(df.melt.to_transform,
                    .(my.Trait, Stage, variable),
                    function(x){
                    BoxcoxTrans(x, x$value, x$CO2, x$Cultivar, x$Year)
                    })

# checking the success of the transformtaion
after.tests <- ddply(df.melt.transformed,
                   .(my.Trait, Stage, variable),
                   function(x) {
                 
                 out <- try(leveneTest(trans_value ~ CO2 * Cultivar * Year,
                            data = x))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lev.out <- NA} 
                 else {
                      print("successful fit")
                      lev.out <- out$`Pr(>F)`[1]}
                names(lev.out) <- "Levene_p_value"
                return(lev.out)
                 })

# Check the success of the transformation
after.tests$Leve_fail <- FALSE
after.tests$Leve_fail[after.tests$Levene_p_value < 0.05 ] <- TRUE

write.table(after.tests,
            file = "After_transformation_Levene_results.csv",
            sep = ",",
            row.names = FALSE)

# put the no_transformed and transformed data back together
# add a column to the no_transform data to match the format of the transformed data frame.
df.melt.no_transform$trans_value <- df.melt.no_transform$value

# put the two data frames together
df.melt.all_transformed <- rbind(df.melt.no_transform, df.melt.transformed)

bad.paras <- interaction(after.tests$variable[after.tests$Leve_fail == TRUE],
                         after.tests$Stage[after.tests$Leve_fail == TRUE])

# export the "non-homogeneous" parameters
write.table(bad.paras,
            file = "Non-homogeneous_parameters.txt",
            row.names = FALSE,
            col.names = FALSE)


# Select the data that do not violate homogeneity of variances assumption            
df.melt.all_transformed$inter <- interaction(df.melt.all_transformed$variable,
                                             df.melt.all_transformed$Stage)

# Well, keeping all parameters in the analysis, despite homogeneity problems
# df.melt.homogeneous <- df.melt.all_transformed[!(df.melt.all_transformed$inter %in% bad.paras), ]

df.melt.homogeneous <- df.melt.all_transformed

# get rid of the interaction column - was only used for selection purposes
df.melt.all_transformed$inter <- NULL
df.melt.homogeneous$inter     <- NULL

write.table(df.melt.homogeneous,
            file = "transformed_values.csv",
            sep = ",", row.names = FALSE)

# unique ring IDs per Year
df.melt.homogeneous$Year.RingID <- interaction(df.melt.homogeneous$Year, 
                                               df.melt.homogeneous$RingID)

# Run an anova
# run the analysis on all homogeneous cases per Trait and stage

# Analysis February 2015, Michael:
# Let us also look at total rainfall, rainfall+irrigation sowing to DC31, Dc31 to DC65, and DC65 to maturity. Let's check either CO2 x cv effect for each treatment and year separately, or (probably better) cv x CO2 x treatment for each year to go with graphs.

#We thus would treat year as a fixed variable, as we only have one contrast. 

#I suspect that the relative increase due to irrigation is more relevant in the dry year, that may explain the bigger differences due to irrigation.

aov.out <- dlply(df.melt.homogeneous[df.melt.homogeneous$variable %in% parameters_to_keep, ],
                 .(Irrigation, my.Trait, Stage, variable),
                 function(x) {
                 
                 out <- try(summary(aov(value ~ CO2 * Cultivar * Year + 
                                        Error(Year.RingID/Cultivar),
                                    data = x,
                                    na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      aov.out <- "not testable"} 
                 else {
                      print("successful fit")
                      aov.out <- out}
                 })
sink("Silverstar_SBx_Year_Anova_results_years.txt")
        print(aov.out)
sink()

# Run nlme
# run the analysis on all cases
# Halfring design
#lme.out <- dlply(df.melt.homogeneous[df.melt.homogeneous$variable %in% parameters_to_keep, ],
#                 .(my.Trait, Stage, variable),
#                 function(x) {
#                 
#                 out <- try(anova(lme(value ~ CO2 * Cultivar * Environment,
#                      random = ~ 1 | my.HalfringID/Cultivar,
#                      data = x,
#                      na.action = na.omit)))
#                    
#                 if (inherits(out, "try-error")) {
#                      print("problem with fit")
#                      lme.out <- "not testable"} 
#                 else {
#                      print("successful fit")
#                      lme.out <- out}
#                 })

# Design separate per year, nesting within Ring
lme.out <- dlply(df.melt.homogeneous[df.melt.homogeneous$variable %in% parameters_to_keep &
df.melt.homogeneous$my.Trait == "Silverstar", ],
                 .(Irrigation, my.Trait, Stage, variable),
                 function(x) {
                 
                 out <- try(anova(lme(value ~ CO2 * Cultivar * Year,
                      random = ~ 1 | Year.RingID/Cultivar,
                      data = x,
                      na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lme.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lme.out <- out}
                 })
sink("Silverstar_Year_lme_results_CO2xCultivarxYear.txt")
        print(lme.out)
sink()

# aggregate p-value in a table

factor_names <- rownames(lme.out[[1]])
# x <- lme.out[[1]]
# y <- lme.out[[3]]

lme.res <- ldply(lme.out,
             function (x) {
             if (x == "not testable") {
                my.p <- c(rep(NA, 8)) 
             } else {
                my.p <- x$`p-value`
             }
             names(my.p) <- factor_names
             return(my.p)
             }
)

# get rid of samples that result in missing values
lme.res <- na.omit(lme.res)

# melt the data down for the graph
lme.res.melt <- melt(lme.res)
names(lme.res.melt)[5] <- "AOV_factor"

head(lme.res.melt[is.na(lme.res.melt$variable), ])

write.table(lme.res.melt,
            file = "SB_Silverstar_AOV_nested_Year_separate_pvalues.csv",
            row.names = FALSE, sep = ",")


# =================== Carbs stats ======================

Carbs$Year.RingID <- interaction(Carbs$Year, 
                                 Carbs$Ring)
Carbs$Irrigation <- gsub("Supp", "Sup", Carbs$Irrigation)
Carbs$Irrigation <- as.factor(Carbs$Irrigation)
Carbs$Stage <- gsub("DC30", "DC31", Carbs$Stage)
Carbs$Stage <- as.factor(Carbs$Stage)


# Design separate per year, nesting within Ring
lme.out.Carb <- dlply(Carbs[Carbs$Trait == "Silverstar", ],
                 .(Irrigation, Trait, Organ, Stage),
                 function(x) {
                 
                 out <- try(anova(lme(Conc..mg.mg. ~ CO2 * Cultivar * Year,
                      random = ~ 1 | Year.RingID/Cultivar,
                      data = x,
                      na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lme.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lme.out <- out}
                 })
sink("Silverstar_Year_lme_results_WSC_CO2xCultivarxYear.txt")
        print(lme.out.Carb)
sink()

factor_names <- rownames(lme.out.Carb[[1]])
# x <- lme.out[[1]]
# y <- lme.out[[3]]

lme.res.Carb <- ldply(lme.out.Carb,
             function (x) {
             if (x == "not testable") {
                my.p <- c(rep(NA, 8)) 
             } else {
                my.p <- x$`p-value`
             }
             names(my.p) <- factor_names
             return(my.p)
             }
)

# get rid of samples that result in missing values
lme.res.Carb <- na.omit(lme.res.Carb)

# melt the data down for the graph
lme.res.melt.Carb <- melt(lme.res.Carb)
names(lme.res.melt.Carb)[5] <- "AOV_factor"
lme.res.melt.Carb$variable <- "WSC_conc_mg_per_mg"
names(lme.res.melt.Carb) <- gsub("Trait", "my.Trait", names((lme.res.melt.Carb)))
lme.res.melt$Organ <- "Plant"
lme.res.both <- rbind(lme.res.melt, lme.res.melt.Carb)


# plot the p-values

relev.p.values <- lme.res.both[lme.res.both$AOV_factor != "(Intercept)" &
                               !is.na(lme.res.both$AOV_factor) & 
                               lme.res.both$value <= 0.05 &
                               !is.na(lme.res.both$Stage), ]
relev.p.values$variable <- factor(relev.p.values$variable,
                                  levels = sort(unique(as.character(relev.p.values$variable))))

relev.p.values.select <- relev.p.values[relev.p.values$variable %in% my.paras, ]

my.good.names.df <- data.frame(variable = my.paras,
                               good.name = my.paras.good.names)
relev.p.values.select <- merge(relev.p.values, my.good.names.df)

# export p-value table
library(reshape2)
relev.p.values.select.cast.CO2xCultxYear <- dcast(relev.p.values.select[relev.p.values.select$my.Trait == "Silverstar", ],
         Irrigation + Stage + Organ + good.name ~ AOV_factor)
write.table(relev.p.values.select.cast.CO2xCultxYear,
            file = "lme_result_CO2xCultxYear.csv",
            row.names = FALSE,
            sep = ",", na = "")
                                  
p <- ggplot(relev.p.values.select[relev.p.values.select$my.Trait == "Silverstar", ],
            aes(x = AOV_factor, y = good.name))
  p <- p + geom_point(aes(colour = value))
  p <- p + scale_colour_gradient(low = "green", high = "red", name = "p-value")
  p <- p + facet_grid(Stage ~ Irrigation)
  p <- p + theme_bw()
  p <- p + labs(x = "Factor",
                y = "Variable")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0),
                 axis.text.y = element_text(size = rel(0.6)),
                 legend.position = c(0.95, 0.2))
p

ggsave(file = "tin_p_value_plot_Year.pdf",
       width = 11, height = 9)

save.image(file = "Silverstar_tin_environment_Year.RData", compress = TRUE)

# ================= Years separate CO2 * Cultivar * Irri ==================
# Design separate per year, nesting within Ring
lme.out <- dlply(df.melt.homogeneous[df.melt.homogeneous$variable %in% parameters_to_keep &
df.melt.homogeneous$my.Trait == "Silverstar", ],
                 .(Year, Stage, variable),
                 function(x) {
                 
                 out <- try(anova(lme(value ~ CO2 * Cultivar * Irrigation,
                      random = ~ 1 | Year.RingID/Cultivar,
                      data = x,
                      na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lme.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lme.out <- out}
                 })
sink("Silverstar_Year_sep_CO2xCultivarxIrrigation_lme_results.txt")
        print(lme.out)
sink()

# aggregate p-value in a table

factor_names <- rownames(lme.out[[1]])
# x <- lme.out[[1]]
# y <- lme.out[[3]]

lme.res <- ldply(lme.out,
             function (x) {
             if (x == "not testable") {
                my.p <- c(rep(NA, 8)) 
             } else {
                my.p <- x$`p-value`
             }
             names(my.p) <- factor_names
             return(my.p)
             }
)

# get rid of samples that result in missing values
lme.res <- na.omit(lme.res)

# melt the data down for the graph
lme.res.melt <- melt(lme.res)
names(lme.res.melt)[4] <- "AOV_factor"

#head(lme.res.melt[is.na(lme.res.melt$variable), ])

#write.table(lme.res.melt,
#            file = "SB_Silverstar_AOV_nested_Year_separate_pvalues.csv",
#            row.names = FALSE, sep = ",")


# =================== Carbs stats ======================

# Design separate per year, nesting within Ring
lme.out.Carb <- dlply(Carbs[Carbs$Trait == "Silverstar", ],
                 .(Year, Trait, Organ, Stage),
                 function(x) {
                 
                 out <- try(anova(lme(Conc..mg.mg. ~ CO2 * Cultivar * Irrigation,
                      random = ~ 1 | Year.RingID/Cultivar,
                      data = x,
                      na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lme.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lme.out <- out}
                 })
sink("Silverstar_Year_sep_CO2xCultivarxIrrigation_lme_results_WSC.txt")
        print(lme.out.Carb)
sink()

factor_names <- rownames(lme.out.Carb[[1]])
# x <- lme.out[[1]]
# y <- lme.out[[3]]

lme.res.Carb <- ldply(lme.out.Carb,
             function (x) {
             if (x == "not testable") {
                my.p <- c(rep(NA, 8)) 
             } else {
                my.p <- x$`p-value`
             }
             names(my.p) <- factor_names
             return(my.p)
             }
)

# get rid of samples that result in missing values
lme.res.Carb <- na.omit(lme.res.Carb)

# melt the data down for the graph
lme.res.melt.Carb <- melt(lme.res.Carb)
names(lme.res.melt.Carb)[5] <- "AOV_factor"
lme.res.melt.Carb$my.Trait <- NULL
lme.res.melt.Carb$Trait <- NULL
lme.res.melt.Carb$variable <- "WSC_conc_mg_per_mg"
lme.res.melt.Carb$variable <- paste(lme.res.melt.Carb$variable, lme.res.melt.Carb$Organ, sep = "_")
lme.res.melt$Organ <- "Plant"
lme.res.both <- rbind(lme.res.melt, lme.res.melt.Carb)


# plot the p-values

relev.p.values <- lme.res.both[lme.res.both$AOV_factor != "(Intercept)" &
                               !is.na(lme.res.both$AOV_factor) & 
                               lme.res.both$value <= 0.05 &
                               !is.na(lme.res.both$Stage), ]
relev.p.values$variable <- factor(relev.p.values$variable,
                                  levels = sort(unique(as.character(relev.p.values$variable))))

relev.p.values.select <- relev.p.values[relev.p.values$variable %in% my.paras, ]

my.good.names.df <- data.frame(variable = my.paras,
                               good.name = my.paras.good.names)
relev.p.values.select <- merge(relev.p.values, my.good.names.df)

# export p-value table
library(reshape2)
relev.p.values.select.cast.CO2xCultxIrri <- dcast(relev.p.values.select,
         Year + Stage + Organ + good.name ~ AOV_factor)
write.table(relev.p.values.select.cast.CO2xCultxIrri,
            file = "lme_result_CO2xCultXIrri.csv",
            row.names = FALSE,
            sep = ",", na = "")

                                  
p <- ggplot(relev.p.values.select,
            aes(x = AOV_factor, y = good.name))
  p <- p + geom_point(aes(colour = value))
  p <- p + scale_colour_gradient(low = "green", high = "red", name = "p-value")
  p <- p + facet_grid(Stage ~ Year)
  p <- p + theme_bw()
  p <- p + labs(x = "Factor",
                y = "Variable")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0),
                 axis.text.y = element_text(size = rel(0.6)))
p

ggsave(file = "tin_p_value_plot_Year_sep_CO2xCultivarxIrrigation.pdf",
       width = 11, height = 9)

