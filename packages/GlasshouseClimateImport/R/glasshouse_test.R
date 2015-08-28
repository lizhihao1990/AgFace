# PC2 glasshouse import testfile

setwd("~/AgFace/2015/Glasshouses/PC2/")
source("~/AgFace/R_scripts/packages/GlasshouseClimateImport/R/GlasshouseFileImport.R")
source("~/AgFace/R_scripts/packages/GlasshouseClimateImport/R/GlasshouseFolderImport.R")

df <- GlasshouseFolderImport(glasshouse = "PC2")

library(reshape2)
library(ggplot2)

df.melt <- melt(df, id.vars = "TIME")

p <- ggplot(df.melt, aes(x = TIME, y = value))
  p <- p + geom_line()
  p <- p + facet_grid(variable ~ . , scale = "free_y")
  p <- p + theme_bw()
#p
ggsave(p, file = "Glasshouse_data.pdf", width = 13, height = 27)
