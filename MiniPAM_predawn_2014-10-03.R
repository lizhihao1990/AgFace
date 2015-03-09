# MiniPAM pre-anthesis 2014

setwd("~/AgFace/2014/MiniPAM/2014-10-03_pre_dawn")

df <- LoadMiniPAM("minicom_no_settings.cap")

df$MyMarker <- NextLabel(df$Mark, "l", )

library(plyr)
library(ggplot2)

p <- ggplot(df, aes(x = PAR, y = ETR))
  p <- p + geom_point(aes(colour = Mark))
p

