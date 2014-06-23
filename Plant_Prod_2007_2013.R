# Analyse combined 2007 to 2013 Plant production data

# based on script "Import_2007_to2013.R"

setwd("~/AgFace/Topics/Plant_Pro_2007_to_2013")

# load previously imported data
load("~/AgFace/2007_2013/Agface_Plant_production_2007_to_2013.RData")

# keep only some objects from the workspace, delete the others
objects.to.keep <- c("df.melt", "Agface", "descript.para.reord")
rm(list = ls()[!(ls() %in% objects.to.keep)])

library(plyr)
library(ggplot2)
library(reshape2)

# call R scripts
source("~/AgFace/R_scripts/MyThemes.R")
source("~/AgFace/R_scripts/Rel_response_helper_script.R")

# create Environment variable
df.melt$Environment <- with(df.melt, interaction(TrialID,
                            Year, 
                            Irrigation,
                            TOS,
                            Ntreat),
                            drop = TRUE)

# sort Environment by increasing Grain yield in aCO2
Ord.Env <- ddply(df.melt[df.melt$CO2 == "aCO2" & 
                 df.melt$variable == "Yield..g.m2.", ],
                .(Environment),
                summarise,
                mean_yield = mean(value, na.rm = TRUE))

df.melt$Ord.Environment <- factor(df.melt$Environment,
                             levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
                             ordered = TRUE)

df.cast <- dcast(df.melt, formula = ... ~ variable)

df.summary <- ddply(df.melt,
                    .(TrialID, Year, CO2, Irrigation, TOS, Cultivar, Stage, Ntreat, Ord.Environment, variable),
                    summarise,
                    my.mean = mean(value, na.rm = TRUE),
                    my.sd   = sd(value, na.rm = TRUE))

# extract the yield data
df.yield <- df.summary[df.summary$variable == "Yield..g.m2.", ]
names(df.yield) <- gsub("my", "Yield", names(df.yield))

# to make sure the yield data can be associated with data from all growth stages
# have to get rid of variable and Stage information for the yield data
df.yield$variable <- NULL
df.yield$Stage <- NULL

# merge the yield data back with df.summary to have the yield column available
# now yield has its own column in the "long" data format.
df.plot <- merge(df.summary, df.yield)

MyPlots <- function(data, variable_column, separator1 = "CO2", separator2 = "Cultivar", xdata, xerror, ydata, yerror, cultivar = "Cultivar") {
      # get hold of the variable name
      var_col <- which(names(data) == variable_column)
      my.variable <- sort(unique(data[, var_col]))
      
      # get hold of the xdata column
      
      # check that there are any data in the data frame
      missing_data <- sum(is.na(data[, names(data) == ydata]))
      # was missing_data <- length(data$my.mean[is.na(data$my.mean)])
            
      # check the amount of unique cultivars in the data
      my.unique.cultivars <- length(unique(data[, names(data) == cultivar]))
      #print(my.unique.cultivars)
      
      # calculate error bars
      xerr_names <- names(data)[names(data) %in% c(xdata, xerror)]
      yerr_names <- names(data)[names(data) %in% c(ydata, yerror)]
      
      limitsx <- aes_string(xmin = paste(xerr_names, collapse = "-"), 
                            xmax = paste(xerr_names, collapse = "+"))

      limitsy <- aes_string(ymin = paste(yerr_names, collapse = "-"), 
                            ymax = paste(yerr_names, collapse = "+"))

      # create plot for variables with non-missing data
      if (nrow(data) != missing_data ) {
      p <- ggplot(data, aes_string(x = xdata, y = ydata))
        p <- p + geom_smooth(aes_string(colour = separator1), method = "lm")
        p <- p + geom_errorbarh(limitsx, width = 0.25, colour = "grey80", alpha = 0.5)
        p <- p + geom_errorbar(limitsy,  width = 0.25, colour = "grey80", alpha = 0.5)
        p <- p + geom_point(aes_string(colour = separator1, shape = separator2))
        p <- p + scale_shape_manual(values = LETTERS[1:my.unique.cultivars]) 
        p <- p + labs(y = my.variable)
        p <- p + facet_grid(Stage ~ Irrigation)
        p <- p + theme_my
      return(p)
      } else {
      # create a dummy plot for parameters with 0 samples
      p <- ggplot()
        p <- p + geom_text(data = NULL, aes(x = 1, y = 1), label = "missing data")
        p <- p + labs(y = my.variable)
        p <- p + theme_my
      return(p)
      }
}

# list of paramters to create graphs of, in case not all are of interest
my.to_plots <- c("AR.Dry.wt.area..g.m2.", 
                 "N_Grain..g.m2.",
                 "Yield..g.m2.")

# create plots for all
my.plots <- dlply(df.plot, #[df.plot$variable %in% my.to_plots, ],
                  .(variable),
                  function(x) {
                  MyPlots(data = x, 
                         variable_column = "variable",
                         separator1 = "CO2",
                         separator2 = "Cultivar", 
                         xdata  = "Yield.mean", 
                         xerror = "Yield.sd", 
                         ydata  = "my.mean", 
                         yerror = "my.sd")})

pdf("Absolute_vs_yield.pdf", width = 7, height = 7)
print(my.plots)
dev.off()

# Calculate relative response
rel.response <- ddply(df.melt,
                     .(TrialID, 
                       Ord.Environment, 
                       Year, 
                       Cultivar, 
                       TOS, 
                       Ntreat, 
                       Irrigation, 
                       Stage,
                       variable),
                     .progress = "text",
                     function(x) {
                     CalcPercent(x, "CO2", "value", "aCO2")})

# extract the yield data from the relative response data frame
df.yield.rel <- rel.response[rel.response$variable == "Yield..g.m2.", ]
names(df.yield.rel) <- gsub("reference", "Yield_aCO2", names(df.yield.rel))

# stripping down the yield data frame
df.yield.rel <- df.yield.rel[, c("TrialID", "Ord.Environment", "Year", "Cultivar", "TOS", "Ntreat", "Irrigation", "Yield_aCO2_mean_abs", "Yield_aCO2_sd_abs")]

# merge the yield data back into the rel.response data frame
# the yield is now a separate column and is matched to each sample irrespective of stage
rel.response <- merge(rel.response, df.yield.rel)

# renaming all variables to include the information that it's a ratio now
rel.response.variable.extension <- "as_ratio_of_aCO2"
rel.response$variable <- paste(rel.response$variable, rel.response.variable.extension, sep = "_")
rel.response$variable <- as.factor(rel.response$variable)

# Relative plots of measured parameters vs yield
my.plots <- dlply(rel.response, #[rel.response$variable %in% my.to_plots, ],
                  .(variable),
                  function(x) {
                  MyPlots(data = x, 
                         variable_column = "variable", 
                         separator1 = "Irrigation",
                         separator2 = "Cultivar",
                         xdata  = "Yield_aCO2_mean_abs", 
                         xerror = "Yield_aCO2_sd_abs",
                         ydata  = "response_mean_relative", 
                         yerror = "response_sd_relative")})

pdf("Rel_vs_abs_yield.pdf", width = 7, height = 7)
print(my.plots)
dev.off()

# keep an image of this workspace for later processing
save.image("Plant_Prod_2007_to_2013_abs_rel.RData", compress = TRUE)
