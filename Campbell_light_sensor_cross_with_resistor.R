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
df <- CampbellFileImport("out.dat")

resistor.date <- as.POSIXct("2015-05-01 12:00:00", tz = "Australia/Melbourne")
df <- df[df$TIMESTAMP >= resistor.date, ]

# data cleaning
df$SYS6_MLPAR_raw_Avg[df$SYS6_MLPAR_raw_Avg < 0] <- NA
df$SYS6_ML_PAR_PPFD_Avg[df$SYS6_ML_PAR_PPFD_Avg < 0] <- NA
df$SYSTEM <- "SYS6"

df$logLicorQ <- log(df$SYS6_Licor190PPFD_Avg)
df$exp_MLPAR_raw_Avg <- exp(df$SYS6_MLPAR_raw_Avg)

df.melt <- melt(df,
                id = c("SYSTEM", "TIMESTAMP"))
                
light <- df.melt[#df.melt$variable == "SYS6_MLPAR_raw_Avg" |
                 df.melt$variable == "SYS6_Licor190PPFD_Avg" |
                 df.melt$variable == "SYS6_MLPAR_raw2_Avg", ]

p <- ggplot(light, aes(x = TIMESTAMP, y = value))
  p <- p + geom_line(aes(colour = variable))
  p <- p + facet_grid(variable ~ ., scales = "free_y")
  p <- p + theme_bw()
  p <- p + labs(colour = "Sensor type",
                y = "PPFD for Licor or mV for Silonex sensor",
                x = "Time")
p
fig.timecourse <- p

high.light.days <- c("2015-04-02", "2015-04-04")
high.light.days <- as.POSIXct(high.light.days, tz = "Australia/Melbourne")


#p <- ggplot(df[df$SYS6_Licor190PPFD_Avg >= 500 &
#               df$TIMESTAMP > high.light.days[1] &
#               df$TIMESTAMP < high.light.days[2], ],
p <- ggplot(df, 
           aes(y = SYS6_Licor190PPFD_Avg, x = SYS6_MLPAR_raw2_Avg))
  p <- p + geom_smooth(method = "lm", se= TRUE)
  p <- p + geom_point()
  p <- p + labs(x = "Silonex SLD-70BG2 (mV)",
                y = expression("PPFD ["~mu*mol~photons~m^-2~s^-1~"]"))
  p <- p + theme_bw()

p
fig.cor <- p

my.lm <- lm(SYS6_Licor190PPFD_Avg ~ SYS6_MLPAR_raw2_Avg,
            data = df)
summary(my.lm)

# assemble some figures
# ----- assemble figures ----
my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))
names(my.figures.list) <- my.figures
lapply(my.figures.list, inherits, "gg")
my.grobs <- lapply(my.figures.list, ggplotGrob)

pdf(file = "Light_sensor_comparison.pdf", width = 9, height = 7)
grid.arrange(my.grobs[[2]], my.grobs[[1]], ncol = 1)
#grid.draw(rbind(my.grobs[[1]], my.grobs[[2]], my.grobs[[3]], size = "first"))
#grid.arrange(a, b, c, d, e, ncol = 1)
dev.off()


p <- ggplot(df[df$TIMESTAMP > high.light.days[1] &
               df$TIMESTAMP < high.light.days[2], ],
            aes(y = SYS6_Licor190PPFD_Avg, x = SYS6_MLPAR_raw_Avg))
  p <- p + geom_point()
  p <- p + labs(x = "Silonex SLD-70BG2 (mV)",
                y = expression("PPFD ["~mu*mol~photons~m^-2~s^-1~"]"))
  p <- p + theme_bw()

p

p <- ggplot(df[df$SYS6_MLPAR_raw_Avg >= 250 &
               df$logLicorQ > -1, ], 
              aes(y = log(SYS6_Licor190PPFD_Avg), x = SYS6_MLPAR_raw_Avg))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_point()
  #p <- p + coord_cartesian(xlim = c(180, 400))
  p <- p + labs(x = "Silonex SLD-70BG2 (mV)",
                y = expression("log PPFD ["~mu*mol~photons~m^-2~s^-1~"]"))
  p <- p + theme_bw()
p
fig.log <- p


my.lm <- lm(log(SYS6_Licor190PPFD_Avg) ~ SYS6_MLPAR_raw2_Avg, 
            data = df[df$logLicorQ > -1,] )
summary(my.lm)
my.coef <- coef(my.lm)




df$ML_PPFD <- exp(df$SYS6_MLPAR_raw_Avg * my.coef[[2]] + my.coef[[1]])

df.melt.cor <- melt(df,
                    id = c("TIMESTAMP", "SYSTEM"))

light.cor <- df.melt.cor[df.melt.cor$variable == "ML_PPFD" |
                         df.melt.cor$variable == "SYS6_Licor190PPFD_Avg", ]

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
