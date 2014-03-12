changes:

load("../../Plant_Production_2007_2009_Glenn_Feb10_2014.RData")

parameters_to_keep <- c("Spikelets.head", "Spikelet.wt..g.", "Crop.Height..cm.", "Emergence.Plants.m2", "AR.Dry.wt.area..g.m2.", "Plant.wt..g.plant.", "Tiller.wt..g.tiller.",  "Plants.m2..Quadrat.", "Tillers.m2..Quadrat.", "1000.Grain.Wt..g.", "Grains.m2", "Grains.plant..quadrat.", "Seeds.floret")

yj <- yj[, c(1:17, 98:99, 18:97)]

yj.melt <- melt(yj,
                id = names(yj)[1:19])

yj.sum <- cast(yj.melt[yj.melt$variable %in% parameters_to_keep, ], 


my.boxplots <- dlply(yj.melt[yj.melt$variable %in% parameters_to_keep, ],

yj.rel <- rel.response

yj.rel.melt <- melt(yj.rel,
                    id = names(yj.rel)[1:20])



my.boxplots <- dlply(rel.response[rel.response$variable %in% parameters_to_keep, ],

aov.out <- dlply(yj.homogeneous[yj.homogeneous$variable %in% parameters_to_keep, ],

aov.details.out <- ddply(yj.homogeneous[yj.homogeneous$variable %in% parameters_to_keep, ],                   

