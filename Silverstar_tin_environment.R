# As discussed, I would like to look at the contrasting pairs “Silvertar” versus “Silverstar – tin” (or named as something with SST...) and the two “SB...” lines. There should be 4 environments across the tow years, just irrigated and non-irrigated, but no TOS.

# load libraries
require(xlsx)
require(reshape)
require(plyr)
require(nlme)
require(car)

# set working directory
setwd("~/AgFace/Plant_Production/Silverstar_tin")

# load Yitpi N-experiment helper script for custom Boxplot function
# load script for BoxCox transformation
source("~/AgFace/R_scripts/Yitpi_N_experiment_2007_2009_helper_script.R")
source("~/AgFace/R_scripts/BoxCox_transformation.R")

#df2011 <- read.xlsx("../../2011/2011_AGFACE_Wheat_Plant_production_Summary_4_markus.xls",
#                    sheetName = "Final")

#df2012 <- read.xlsx("../../2012/2012_AGFACE_Wheat_Plant_production_Summary_2_markus.xls",
#                    sheetName = "Final")

#save.image("workspace_00.RData", compress = TRUE)
load("workspace_00.RData")


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

# Order the Environments by increasing yield
# create a table with the Environments and corresponding yield
Ord.Env <- ddply(df[df$CO2 == "aCO2" &
                    df$Cultivar == "Silverstar", ],
              .(Environment),
                summarise,
                mean_yield = mean(Yield..g.m2., na.rm = TRUE))

# apply the ordering to the Environment factor in the data frame
df$Ord.Environment <- factor(df$Environment,
                             levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
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
                            xaxis = "Ord.Environment", 
                            yaxis = "value", 
                            treatment_sep_a = "Cultivar", 
                            treatment_sep_b = "CO2", 
                            facet_var_a = "Stage", 
                            facet_var_b = "my.Trait",
                            two_separators = TRUE,
                            the_main_title = "")})

pdf("Silverstar_SBx_Environments_boxplots.pdf")
        print(my.boxplots)
dev.off()

# data expressed as function of water received instead of Environment
my.water_graph <- dlply(df.melt[df.melt$variable %in% parameters_to_keep, ],
                       .(variable),
                  function(x) {
                  current_para <- unique(x$variable)
                  p <- ggplot(x, aes(x = Water_received_mm, y = value))
                    p <- p + stat_summary(aes(colour = CO2, 
                                              shape = Cultivar, 
                                              linetype = as.factor(Year)),
                                        fun.data = "mean_sdl", mult = 1)
                    p <- p + stat_summary(aes(colour = CO2,
                                              shape = Cultivar,
                                              linetype = as.factor(Year)),
                                        fun.data = "mean_sdl", mult = 1, geom = "line")
                    p <- p + labs(y = current_para, x = "In-season water received [mm]")
                    p <- p + scale_x_continuous(limits = c(0, 350))
                    p <- p + facet_grid(Stage ~ my.Trait)
                    p <- p + theme_bw()
                  })

pdf("Silverstar_SBx_water_received.pdf")
        print(my.water_graph)
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
                 
                 out <- try(leveneTest(value ~ CO2 * Cultivar * Environment,
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

save.image("Silverstar_tin_workspace_00.RData")
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
                    BoxcoxTrans(x, x$value, x$CO2, x$Cultivar, x$Environment)
                    })

# checking the success of the transformtaion
after.tests <- ddply(df.melt.transformed,
                   .(my.Trait, Stage, variable),
                   function(x) {
                 
                 out <- try(leveneTest(trans_value ~ CO2 * Cultivar * Environment,
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

# Run an anova
# run the analysis on all homogeneous cases per Trait and stage
aov.out <- dlply(df.melt.homogeneous[df.melt.homogeneous$variable %in% parameters_to_keep, ],
                 .(my.Trait, Stage, variable),
                 function(x) {
                 
                 out <- try(summary(aov(value ~ CO2 * Cultivar * Environment + 
                                        Error(my.HalfringID/Cultivar),
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
lme.out <- dlply(df.melt.homogeneous[df.melt.homogeneous$variable %in% parameters_to_keep, ],
                 .(my.Trait, Stage, variable),
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

head(lme.res.melt[is.na(lme.res.melt$variable), ])

write.table(lme.res.melt,
            file = "lme_pvalues.csv",
            row.names = FALSE, sep = ",")

# plot the p-values

relev.p.values <- lme.res.melt[lme.res.melt$AOV_factor != "(Intercept)" &
                               !is.na(lme.res.melt$AOV_factor) & 
                               lme.res.melt$value <= 0.05 &
                               !is.na(lme.res.melt$Stage), ]
relev.p.values$variable <- factor(relev.p.values$variable,
                                  levels = sort(unique(as.character(relev.p.values$variable))))
p <- ggplot(relev.p.values,
            aes(x = AOV_factor, y = variable))
  p <- p + geom_point(aes(colour = value))
  p <- p + scale_colour_gradient(low = "green", high = "red", name = "p-value")
  p <- p + facet_grid(Stage ~ my.Trait)
  p <- p + theme_bw()
  p <- p + labs(x = "ANOVA Factor")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0),
                 axis.text.y = element_text(size = rel(0.6)))
p

ggsave(file = "Silverstar_Tin_lme_results.pdf",
       width = 11, height = 19)

save.image(file = "Silverstar_tin_environment.RData", compress = TRUE)

#p <- ggplot(df[df$Stage != "DC90", ], 
#                aes(x = Sub.sample.Fresh.Tiller.No., 
#                    y = Dry.wt.tiller.in.AR.sample..g.m2.))
#p <- p + geom_point(aes(colour = Cultivar))
#p <- p + geom_smooth(aes(colour = Cultivar), method = "lm")
#p <- p + facet_grid(Stage ~ my.Trait, scales = "free_y")
#p

