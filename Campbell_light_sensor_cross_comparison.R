# light sensor cross-reference

the.folder <- "~/AgFace/2015/Campbell_logger/Transmissions/Light_sensor_cross_reference"
setwd(the.folder)


library(plyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

source("~/AgFace/R_scripts/import_Campbell.R")

# import first file
#df <- CampbellAllImport(logger.folder = the.folder,
#                        log.interval  = "5Min")
df <- CampbellFileImport("merged_5Min.dat")

# data cleaning
df$MLPAR_Avg[df$MLPAR_Avg < 0] <- NA
df$ML_PPFD_Avg[df$ML_PPFD_Avg < 0] <- NA
df$SYSTEM <- "SYS3"

# gap in data when sensors were moved around
# March 31, afternoon
gap.start <- "2015-03-31 14:05"
gap.end   <- "2015-03-31 15:00"

gap.time <- c(gap.start, gap.end)
gap.time <- as.POSIXct(gap.time, tz = "Australia/Melbourne")

df <- df[df$TIMESTAMP < gap.time[1] |
         df$TIMESTAMP > gap.time[2], ]
                        
df$logPAR <- log(df$PAR_Den_Avg)
df$expMLPAR <- exp(df$MLPAR_Avg)
df$tenMLPAR <- 10^df$MLPAR_Avg

df.melt <- melt(df,
                id = c("SYSTEM", "TIMESTAMP"))

light <- df.melt[df.melt$variable == "MLPAR_Avg" |
                 df.melt$variable == "PAR_Den_Avg", ]

lightexp <- df.melt[df.melt$variable == "expMLPAR" |
                 df.melt$variable == "PAR_Den_Avg", ]

p <- ggplot(light, aes(x = TIMESTAMP, y = value))
  p <- p + geom_line(aes(colour = factor(variable, 
                                         labels = c("Licor 190", "Silonex SLD-70BG2"))))
  p <- p + facet_grid(variable ~ ., scales = "free_y")
  p <- p + theme_bw()
  p <- p + labs(colour = "Sensor type",
                y = "PPFD for Licor or mV for Silonex sensor",
                x = "Time")
p
fig.timecourse <- p

p <- ggplot(df, aes(y = PAR_Den_Avg, x = MLPAR_Avg))
  p <- p + geom_point()
  p <- p + labs(x = "Silonex SLD-70BG2 (mV)",
                y = expression("PPFD ["~mu*mol~photons~m^-2~s^-1~"]"))
  p <- p + theme_bw()

p
fig.cor <- p


p <- ggplot(df[df$logPAR > -50, ], aes(y = log(PAR_Den_Avg), x = MLPAR_Avg))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_point()
  #p <- p + coord_cartesian(xlim = c(180, 400))
  p <- p + labs(x = "Silonex SLD-70BG2 (mV)",
                y = expression("log PPFD ["~mu*mol~photons~m^-2~s^-1~"]"))
  p <- p + theme_bw()
p
fig.log <- p

# linear regression
my.lm <- lm(log(PAR_Den_Avg) ~ MLPAR_Avg, data = df[df$logPAR > -50, ])
summary(my.lm)
my.coef <- coef(my.lm)
df$ML_PPFD <- exp(df$MLPAR_Avg * my.coef[[2]] + my.coef[[1]])

df.melt.cor <- melt(df,
                    id = c("TIMESTAMP", "SYSTEM"))

light.cor <- df.melt.cor[df.melt.cor$variable == "ML_PPFD" |
                         df.melt.cor$variable == "PAR_Den_Avg", ]

p <- ggplot(light.cor, aes(x = TIMESTAMP, y = value))
  p <- p + geom_line(aes(colour = factor(variable, 
                                         labels = c("Licor 190", "Silonex SLD-70BG2"))))
  p <- p + labs(x = "Time",
                y = expression("PPFD ["~mu*mol~photons~m^-2~s^-1~"]"),
                colour = "Sensor type")
  p <- p + theme_bw()
p
fig.timecourse.after <- p


# ----- assemble figures ----
my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))
names(my.figures.list) <- my.figures


# check for gg object
lapply(my.figures.list, inherits, "gg")
my.grobs <- lapply(my.figures.list, ggplotGrob)


pdf(file = "Light_sensor_comparison.pdf", width = 17, height = 12)
grid.arrange(my.grobs[[3]], my.grobs[[1]], my.grobs[[2]], my.grobs[[4]], ncol = 1)
#grid.draw(rbind(my.grobs[[1]], my.grobs[[2]], my.grobs[[3]], size = "first"))
#grid.arrange(a, b, c, d, e, ncol = 1)
dev.off()


