# Npen analysis for GRDC report Feb 2015

# Markus Löw, Jan 2015

# based on script "NPen_SPAD.R"

setwd("~/AgFace/2014/N_pen_SPAD")
load("Npen_Spad_workspace.RData")

# create indicator for irrigation
df$Irrigation <- "rainfed"
df$Irrigation[grepl("Wet", df$Cultivar) == TRUE] <- "irrigated"
df$Irrigation <- as.factor(df$Irrigation)

# get wheat data
wheat <- df[df$Crop == "Wheat", ]
wheat$my.Cultivar <- wheat$Cultivar
wheat$my.Cultivar <- gsub("Wet ", "", wheat$my.Cultivar)

# correct typo in qulity lines
wheat$my.Cultivar <- gsub("RS-4", "RS4", wheat$my.Cultivar)
wheat$my.Cultivar <- gsub("RS4", "RS4 ", wheat$my.Cultivar)
wheat$my.Cultivar <- as.factor(wheat$my.Cultivar)

# define trait groups according to GRDC project
wheat$Trait <- NA
wheat$Trait[wheat$my.Cultivar == "Gladius" | wheat$my.Cultivar == "Wyalkatchem"] <- "NUE"
wheat$Trait[wheat$my.Cultivar == "Scout" | wheat$my.Cultivar == "Yitpi"] <- "Root"
wheat$Trait[wheat$my.Cultivar == "RS4 11-1" | wheat$my.Cultivar == "RS4 11-5"] <- "Quality"
wheat$Trait <- as.factor(wheat$Trait)

library(ggplot2)
p <- ggplot(wheat[wheat$Trait != "Root", ], 
            aes(x = Date, y = Npen))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(shape = CO2, colour = my.Cultivar), 
                        position = position_dodge(width = 0.6), 
                        geom = "line")
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(shape = CO2, colour = my.Cultivar), 
                        position = position_dodge(width = 0.6))
  p <- p + facet_grid(Trait ~ CO2)
  p <- p + scale_y_continuous(limits = c(2.5, 10.5))
  p <- p + theme_bw()
  p <- p + theme(plot.title = element_text(hjust = -0.07, face = "bold"))
  p <- p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
  p <- p + labs(shape = expression(bold(CO[2])),
                linetype = "Cultivar",
                colour = "Cultivar",
                y = "Nitrogen content [%]",
                title = "A")
p
fig.non.root.time <- p
ggsave(fig.non.root.time, 
       file = "NUE_and_Quality_trait_Nperc_timecourse.png", 
       width = 8, height = 6)

# figure for root trait
p <- ggplot(wheat[wheat$Trait == "Root", ], 
            aes(x = Date, y = Npen))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(shape = CO2, colour = my.Cultivar), 
                        position = position_dodge(width = 0.6), 
                        geom = "line")
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1,
                        aes(shape = CO2, colour = my.Cultivar), 
                        position = position_dodge(width = 0.6))
  p <- p + facet_grid(Irrigation ~ CO2)
  p <- p + scale_y_continuous(limits = c(2.5, 10.5))
  p <- p + labs(shape = expression(bold(CO[2])),
                linetype = "Cultivar",
                colour = "Cultivar",
                y = "Nitrogen content [%]",
                title = "B")
  p <- p + theme_bw()
  p <- p + theme(plot.title = element_text(hjust = -0.07, face = "bold"))
  p <- p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
  
p
fig.root.time <- p
ggsave(fig.root.time, 
       file = "Root_trait_Nperc_timecourse.png", 
       width = 8, height = 6)

library(gridExtra)
png(file = "NPen_over_season.png", width = 700, height = 900)
grid.arrange(fig.non.root.time, fig.root.time, ncol = 1)
dev.off()

# stats

wheat$RingPlot <- interaction(wheat$Ring, wheat$Plot, drop = TRUE)
wheat$facDate <- as.factor(wheat$Date)
 
library(nlme)

# repeated measures approach
#  test takes the change into account that happens from day to day within each ID.

my.mod <- lme(fixed = Npen ~ CO2 * my.Cultivar * Irrigation * facDate,
              random = ~ 1 | RingPlot/facDate, 
              method = "REML",
              data = wheat[wheat$Trait == "Root", ])
anova(my.mod)

my.mod <- lme(fixed = Npen ~ CO2 * my.Cultivar * facDate,
              random = ~ 1 | RingPlot/facDate, 
              method = "REML",
              data = wheat[wheat$Trait == "NUE", ])
anova(my.mod)

t.test(Npen ~ my.Cultivar,
       data = wheat[wheat$facDate == "2014-09-10" &
                    wheat$CO2 == "aCO2" &
                    wheat$Trait == "NUE", ])

t.test(Npen ~ my.Cultivar,
       data = wheat[wheat$facDate == "2014-08-06" &
                    wheat$CO2 == "aCO2" &
                    wheat$Trait == "NUE", ])

my.mod <- lme(fixed = Npen ~ CO2 * my.Cultivar * facDate,
              random = ~ 1 | RingPlot/facDate, 
              method = "REML",
              data = wheat[wheat$Trait == "Quality", ])
anova(my.mod)
