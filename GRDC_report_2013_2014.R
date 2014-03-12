# graphs GRDC report 2013/2014
# for trait

# depends on R-script "Silverstar_tin_environment.R"!!
# depends on carbohydrate script

library(plyr)
library(ggplot2)

# set working directory
setwd("~/AgFace/Plant_Production/Silverstar_tin")
load(file = "Silverstar_tin_environment.RData")

# Create overview figure for
# * Yield (both Traits)
# * tillering (Silverstart lines)
# * carbohydrates (SB-lines)

# re-create the yield graph for the GRDC report as png
GRDC.para <- c("Yield..g.m2.")
GRDC.yield <- df.melt[df.melt$variable %in% GRDC.para, ]
my.scale.label <- expression(bold(CO[2]))

GRDC.plot <- dlply(GRDC.yield,
                   .(my.Trait),
                   function(x) {
             p <- ggplot(x, aes(x = Environment, y = value))
             p <- p + theme_bw()
#             p <- p + theme(panel.grid.major.x = element_blank(),
#                      panel.grid.minor.x = element_blank(),        
#                      panel.grid.major.y = element_blank(),
#                      panel.grid.minor.y = element_blank())
             p <- p + geom_boxplot(aes(fill = Cultivar, colour = CO2))
             #p <- p + scale_fill_manual(values = c("darkgrey", "lightgrey"))
             #p <- p + scale_colour_manual(values = c("blue", "red"), name = my.scale.label)
             p <- p + scale_y_continuous(limits = c(200, 1080), breaks = c(200, 400, 600, 800, 1000))
             p <- p + labs(x = "Environment, year x irrigation treatment",
                           y = expression("Yield"~"["~g~m^2~"]"))
             p
                   })

png(file = "GRDC_yield_graph_%03d.png",
    width = 800, height = 800, units = "px")
    print(GRDC.plot)
dev.off()

# facet plot
p <- ggplot(GRDC.yield, aes(x = Environment, y = value))
     p <- p + geom_boxplot(aes(fill = Cultivar, linetype = CO2))
     p <- p + labs(y = expression("Yield"~"["~g~m^2~"]"),
                   x = "Environment")
     p <- p + theme_bw()
     p <- p + facet_grid(. ~ my.Trait)
p

ggsave(file = "Yield_Environment_boxplot.pdf",
       width = 14, height = 7)

# tillering
# based on
my.boxplots$Tillers.m2..SS.dry.

# re-create the tillering graph for the GRDC report as png
GRDC.para <- c("Tillers.m2..SS.dry.")
GRDC.tiller <- df.melt[df.melt$variable %in% GRDC.para &
                       df.melt$my.Trait == "Silverstar", ]

p <- ggplot(GRDC.tiller, aes(x = Environment, y = value))
     p <- p + geom_boxplot(aes(fill = Cultivar, linetype = CO2))
     p <- p + facet_grid(Stage ~ .)
     p <- p + labs(y = expression(Tillers~m^-2),
                   x = "Environment")
     p <- p + theme_bw()
     p <- p + theme(legend.position = c(0.18, 0.88))
p

ggsave(file = "Tillers_m2_Environment_boxplot.pdf",
       width = 7, height = 14)


