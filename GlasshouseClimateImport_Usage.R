# test code for GlasshouseAllImport
library(GlasshouseClimateImport)
setwd("~/AgFace/2015/Glasshouses")

df <- GlasshouseFolderImport()

library(reshape2)
# re-melting the data
my.data.all.melt <- melt(df,
                         id.vars = "TIME")

# plot
library(ggplot2)
MyPlotFunction <- function(data) {
    my.name <- sort(unique(data$variable))
    p <- ggplot(data, aes(x = TIME, y = value))
      p <- p + geom_line()
      p <- p + labs(y = my.name)
      p <- p + theme_bw()
    return(p)
}

library(plyr)
my.plots <- dlply(my.data.all.melt,
                 .(variable),
                 function(x) MyPlotFunction(x))

pdf(file = "Glasshouse_figures.pdf", width = 9, height = 9)
print(my.plots)
dev.off()
