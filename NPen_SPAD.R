# Analysis of N-Pen and SPAD campaign,
# Agface, 2014-08-06 ff
# Markus Löw

setwd("~/AgFace/2014/N_pen_SPAD")

library(ggplot2)

# import treatment information
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# import data
df <- read.csv("Npen_SPAD.csv")

# convert date to be a date
df$Date <- as.Date(df$Date)

# first look
with(df, table(Date, Crop))

# add CO2 treatments
df$CO2[df$Ring %in% ambient_rings_2014]  <- "aCO2"
df$CO2[df$Ring %in% elevated_rings_2014] <- "eCO2"
df$CO2 <- as.factor(df$CO2)

# add Cultivar information
df <- merge(df, PlotIDsCultivars2014,
             all.x = TRUE)
df$Cultivar <- as.character(df$Cultivar)
df$Cultivar[is.na(df$Cultivar)] <- as.character(df$PlotID[is.na(df$Cultivar)])
df$Cultivar <- as.factor(df$Cultivar)

# create a combination of PlotID and Cultivar for plotting
df$PlotCult <- interaction(df$PlotID, df$Cultivar)
df$PlotCult <- as.character(df$PlotCult)
df$PlotCult[df$Crop == "Lentil"] <- as.character(df$PlotID[df$Crop == "Lentil"])
df$PlotCult <- as.factor(df$PlotCult)

df$Ring <- as.factor(df$Ring)

# second look
with(df, table(Crop, CO2))

# Correlation between Npen and SPAD?
p <- ggplot(df, aes(x = Npen, y = SPAD))
  p <- p + geom_smooth(aes(linetype = CO2), method = "lm")
  p <- p + geom_point(aes(colour = Crop, shape = CO2))
  p <- p + theme_bw()
p
fig.Npen_SPAD_corr <- p

# linear regression analysis
MyLm <- function(data) {
     summary(lm(SPAD ~ Npen, data = data))
}

MyLm(df)
MyLm(df[df$CO2 == "aCO2", ])
MyLm(df[df$CO2 == "eCO2", ])

# flexible Plot function for Crop/instrument
PlotNPenSpad <- function(data, instrument, mycrop) {
  p <- ggplot(data[data$Crop == mycrop, ], 
              aes_string(x = "PlotCult", y = instrument))
  p <- p + geom_boxplot(aes(fill = CO2))
  p <- p + facet_grid(Crop ~ Date)
  p <- p + labs(x = "PlotID.Cultivar")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(size = rel(0.7), angle = 90))
return(p)
}

# create a list of plots
my.plots <- list(
   PlotNPenSpad(df, "Npen", "Wheat"),
   PlotNPenSpad(df, "SPAD", "Wheat")#,
#   PlotNPenSpad(df, "Npen", "Lentil"),
#   PlotNPenSpad(df, "SPAD", "Lentil")
   )

# export the plots
pdf(file = "NPen_SPAD_figures.pdf",
    width = 9, height = 9)
  print(fig.Npen_SPAD_corr)
  print(my.plots)
dev.off()

# What's going on with Boomer?
df.Boomer <- df[df$PlotID == "Boomer", ]
with(df.Boomer, table(Npen, Date, CO2))

# save workspace
save.image("Npen_Spad_workspace.RData", compress = TRUE)

