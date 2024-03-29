# Wheat in different environments
# integrationof Horsham and Walpeup data

# looking into responses of Yitpi in different environments

# based on scripts 
# "Plant_Production_2007_2009.R" and
# "Walpeup_data_shuffle.R"



# set the working directory within the Plant Production folder
setwd("/home/loewi/AgFace/Plant_Production/Environment_comparison")

# load exisiting workspaces
load("../Plant_Production_data_02.RData")
load("../Walpeup_2008_2009.RData")

# rename the Horsham data
hors <- df.full # using all the data to make column matichin easier. Includes logical columns.

# get rid of the objects from these worksapces we don't need
to_keep <- c("hors", "walp")
rm(list = ls()[!(ls() %in% to_keep)])

# load libraries
require(reshape)

# load Yitpi N-experiment helper script for custom Boxplot function
source("~/AgFace/R_scripts/Yitpi_N_experiment_2007_2009_helper_script.R")

# combine Horsham and Walpeup data
# checking for names that are not part of the Horsham data

# first, checking the numbers of parameters
length(names(hors))
length(names(walp))

# name comparison between Horsham and Walpeup - expecting different spelling between names
# e.g. "Grains.plant..quadrat." in Horsham, but "Grains.plant" in Walpeup

names(hors)[which(names(hors) %in% names(walp))]

comp.names <- data.frame(Horsham = names(hors),
                         Walpeup = names(walp))

write.table(comp.names,
            file = "Parameter_Name_comparison.csv",
            sep = ",",
            row.names = FALSE)

# there are differences in the spelling of parameter names
# manual compariosn of names indicates that the content of the columns is comparable between the sites
# using the names from Horsham for the Walpeup samples
names(walp) <- names(hors)

# putting the data together
df <- rbind(hors, walp)

# create the N_treat identifier
df$N_treat <- "N0"
df$N_treat[grep("N\\+", df$PlotTrt)] <- "N+"
df$N_treat[grep("N0",   df$PlotTrt)] <- "N0"
df$N_treat <- as.factor(df$N_treat)

# specify factor levels explicitly (as lexically, "+" is before "0"), zero first seems more natural
df$N_treat <- factor(df$N_treat, levels = c("N0", "N+"))

# create Environment variable               
df$Environment <- interaction(df$TrialID,
                              df$Year, 
                              df$Irrigation,
                              df$TOS,
                              drop = TRUE)

# sort Environment by increasing Grain yield in aCO2
# Michael: I would group the environments according to their grain yield under aCO2 on the x axis. This should show us already lots!

Ord.Env <- ddply(df[df$CO2 == "aCO2" &
                    df$PlotTrt == "YitpiN0" &
                    df$Cultivar == "Yitpi", ],
              .(Environment),
                summarise,
                mean_yield = mean(Yield..g.m2., na.rm = TRUE))

OEnv <- factor(Ord.Env$Environment,
               levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
               ordered = TRUE)


df$Ord.Environment <- factor(df$Environment,
                             levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
                             ordered = TRUE)

# re-organise the data frame, all descriptors in front
df <- df[, c(1:19, 196:198, 20:195)]

# re-format Emergence.date as numeric, so not to trip off the upcoming melt operation
df$Emergence.date <- as.numeric(df$Emergence.date)

# find out which colums are defined as logical, as date, or as factor
# these columns will mess up the later "melt" statement.
# logical first

# keep a copy of the full data before removing non-numeric columns
df.full <- df

col.num.logical <- sapply(df[, 1:ncol(df)], is.logical)

# get an index of column that are not logical
col.num <- which(col.num.logical == FALSE)
df <- df[, col.num] # keep only the non-logical columns

# get an index of column that are not factor, but keeping the descriptive columns
col.num.factor  <- sapply(df[, 1:ncol(df)], is.factor)
col.num <- which(col.num.factor == FALSE)
df.new <- df[, col.num] # keep only the non-logical columns
df <- cbind(df[, 1:22], df.new) # combine with the descriptive columns

# create the long format of the data
df.melt <- melt(df,
                id = names(df)[1:22])

# Boxplots in a loop for YitpiN0 only
my.boxplots <- dlply(df.melt[df.melt$PlotTrt == "YitpiN0" &
                             df.melt$Cultivar == "Yitpi", ],
                  .(variable),
                  function(x) {
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Ord.Environment", 
                            yaxis = "value", 
                            treatment_sep_a = "CO2", 
                            treatment_sep_b = NA, 
                            facet_var_a = "Stage", 
                            facet_var_b = ".",
                            two_separators = FALSE)})

# orig line: MyBoxplot(x, x$variable, "Ord.Environment", "value", "CO2", "CO2", "Stage", ".")})

pdf("Horsham_Walpeup_Environments_boxplots.pdf")
        print(my.boxplots)
dev.off()

# Run an anova
# run the analysis on all cases for YitpiN0
aov.out <- dlply(df.melt[df.melt$PlotTrt  == "YitpiN0" &
                         df.melt$Cultivar == "Yitpi", ],
                 .(variable, Stage),
                 function(x) {
                 
                 out <- try(summary(aov(value ~ CO2 * Environment,
                                    data = x,
                                    na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      aov.out <- "not testable"} 
                 else {
                      print("successful fit")
                      aov.out <- out}
                 })
sink("Horsham_Walpeup_Anova_results.txt")
        print(aov.out)
sink()
