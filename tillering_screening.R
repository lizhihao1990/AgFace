# screenings

setwd("~/AgFace/2007_2013")

df <- read.csv("Agface_Plant_production_2007_to_2013.csv")

setwd("~/AgFace/Topics/Tillering_2011_2012")

df <- df[df$Year == 2011 | df$Year == 2012, ]
df <- df[df$Cultivar == "Silverstar" | df$Cultivar== "SSR T65", ]

# load libraries
require(ggplot2)
require(plyr)
require(reshape)

# load my ggplot themes
source("~/AgFace/R_scripts/MyThemes.R")

# Standard error function
stderr <- function(x) {
          sqrt(var(x[!is.na(x)]) / length(x[!is.na(x)]))
}

# Then create a wrapper to *stderr* to make it compatible with *stat_summary*
# see values shown in ?stat_summary
my.stderr <- function(x) {
             meany <- mean(x)
             ymin  <- mean(x) - stderr(x)
             ymax  <- mean(x) + stderr(x)
             # assemble the named output
             out <- c(y = meany, ymin = ymin, ymax = ymax)
             return(out)
}

# graph output sizes
my.width  <- 17
my.height <- my.width

# graph elements
CO2.label  <- expression(textstyle(CO[2]~treatment))
CO2.treats <- c(expression(textstyle(aCO[2])), 
                expression(textstyle(eCO[2])))
# create environment
df$Environment <- interaction(df$Year, df$Irrigation, drop = TRUE)
# -------------------------------------------------------
# Some Plant production figures
# -------------------------------------------------------

# Screenings Screenings...2mm....."

p <- ggplot(df[df$Stage == "DC90", ], 
            aes(x = Cultivar, y = Screenings...2mm.....))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = "Screenings (< 2 mm)")
  p <- p + facet_grid(Stage ~ Environment)
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.62, 0.8))
p

fig.Screenings <- p

ggsave(file = "Screenings_se.png", 
       width = my.width, height = my.height, 
       units = "cm")

