# ------------------------------
# Processing for Glenn
# ------------------------------

# create absolute relative grpahs of selected 
# parameters from the 2007 to 2013 Agface plant production data

# Only analysing a sub-set of the 2007 to 2013 data
setwd("~/AgFace/Topics/Plant_Pro_2007_to_2013")

# load workspace created by "Plant_Prod_2007_2013.R" script
load("Plant_Prod_2007_to_2013_abs_rel.RData")

# parameters for Glenn
#DC65 Spikelets/head	
#DC65 Spikelet wt (g) 
#DC65 Crop Height (cm)
#DC90 Emergence Plants/m2
#DC90 AR Dry wt/area (g/m2)	
#DC90 Single plant wt (g)	
#DC90 Single tiller wt (g)	
#DC90 Single head wt (g)	
#DC90 Plants/m2	
#DC90 Tillers/m2	
#DC90 Tillers/plant	
#DC90 Heads/m2	
#DC90 % Fertile Tillers	
#DC90 Heads/plant
#DC90 Adjusted 1000 Grain wt (g) - not screened	
#DC90 Adjusted 1000 Grain wt (g) - screened	
#DC90 Grains/m2	
#DC90 Grains/plant
#DC90 Grains/tiller	
#DC90 Grains/head	
#DC90 Test weight	
#DC90 Screenings (<2mm) (%)	
#DC90 Harvest Index
#DC90 Milling Yield (%)	
#DC90 Yield (g/m2)
#DC90 Seeds/floret

Glenn.paras <- sort(c(
        "Spikelets.head", 
        "Spikelet.wt..g.",
        "Crop.Height..cm.",
        "Emergence.Plants.m2",
        "AR.Dry.wt.area..g.m2.",
        "Single.head.wt..g.", 
        "Single.plant.wt..g.", 
        "Single.tiller.wt..g.", 
        "Plants.m2",
        "Tillers.m2", 
        "Tillers.plant",
        "Heads.m2",
        "..Fertile.Tillers",
        "Heads.plant",
        "Adjusted.1000.Grain.wt..g....not.screened" , 
        "Adjusted.1000.Grain.wt..g....screened",
        "Grains.m2",
        "Grains.plant",
        "Grains.tiller",
        "Grains.head", 
        "Test.weight",
        "Screenings...2mm.....",
        "Harvest.Index",
        "Milling.Yield....", 
        "Yield..g.m2.",
        "Seeds.floret"))

# Show the list of parameters on screen, one parameter per line
cat(Glenn.paras, sep = "\n")

# Glenn only wants a few Cultivars, stages, and treatments analysed
Glenn.cultivar <- c("Janz", "Yitpi")
Glenn.nitrogen <- c("N0")
Glenn.stages   <- c("DC65", "DC90")

# create subsets of df.plot and rel.response for Glenns purposes
Glenn.df.plot <- df.plot[df.plot$Cultivar %in% Glenn.cultivar &
                         df.plot$Ntreat   %in% Glenn.nitrogen &
                         df.plot$Stage    %in% Glenn.stages &
                         df.plot$variable %in% Glenn.paras, ]

# Number of samples per parameter and Environment
# should be max n = 1 for each.
Glenn.samples.Env <- ddply(Glenn.df.plot,
                          .(TrialID, Ord.Environment, CO2, Irrigation, TOS, Cultivar, Stage, variable),
                          summarise,
                          n = sum(!is.na(my.mean)))

Glenn.yield.samples <- Glenn.df.plot[Glenn.df.plot$variable == "Yield..g.m2.", ]

# create a version of Glenns parameter names that match the names in rel.response
Glenn.paras.rel <- paste(Glenn.paras, rel.response.variable.extension, sep = "_")

Glenn.rel.response <- rel.response[rel.response$Cultivar %in% Glenn.cultivar &
                         rel.response$Ntreat   %in% Glenn.nitrogen &
                         rel.response$Stage    %in% Glenn.stages &
                         rel.response$variable %in% Glenn.paras.rel, ]
Glenn.yield.rel.samples <- Glenn.rel.response[Glenn.rel.response$variable == paste("Yield..g.m2.", rel.response.variable.extension, sep = "_"), ]

# Unbalanced yield: 23 samples for "Rain", 19 for "Supp"! 42 samples in total
#p <- ggplot(Glenn.yield.rel.samples, aes(x = reference_mean_abs, y = response_mean_relative))
#  p <- p + geom_point(aes(colour = Irrigation))
#p

# create graphs for Glenns parameters
# create plots for all of Glenns scenarios
Glenn.plots.abs <- dlply(Glenn.df.plot,
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

pdf("Glenn_Absolute_vs_absolute_yield.pdf", width = 7, height = 7)
print(Glenn.plots.abs)
dev.off()

# Relative plots of measured parameters vs yield for Glenn
Glenn.plots.rel <- dlply(Glenn.rel.response,
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

pdf("Glenn_Relative_vs_absolute_yield.pdf", width = 7, height = 7)
print(Glenn.plots.rel)
dev.off()
