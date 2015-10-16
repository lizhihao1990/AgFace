# using the CampbellLogger library to import files

library(CampbellLogger)
library(doMC)
#library(doParallel)

# set up cluster for parallel computing
registerDoMC(4)

#cl <- makeCluster(4)
#registerDoParallel(cl)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

#individual import for trouble shooting
#my.folder <- "/home/loewi/AgFace/2015/Campbell_logger/logger_data"
#my.file <- "SYS2_5Min.dat"
#my.import <- paste(my.folder, my.file, sep = "/")

#x <- CampbellFileImport(my.import)
#my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#x <- read.csv(my.import, skip = 4, header = F)
#names(x) <- names(my.header)
#x$Date <- as.POSIXct(x$TIMESTAMP, tz = "GMT")

# end of individual import
df <- CampbellAllImport(log.interval = "5Min", use.parallel = TRUE, time.zone = "GMT", skip.rows = 30000)
#df <- CampbellAllImport(log.interval = "5Min", use.parallel = FALSE, logger.name = "SYS2")

df.cast <- CampbellCast(df, use.parallel = TRUE)
#df.cast[df.cast$IR_Narrow_Max == 2, ]

ephemeral.times <- CampbellSunriseSunset(df)
ephemeral.times$sunrise <- ephemeral.times$sunrise + 60*60*10
ephemeral.times$sunset <- ephemeral.times$sunset + 60*60*10

my.time.to.plot <- 96
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = -6.5, yscale_max = 45,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast, #logger = "SYS7",
             yscale_min = -5, yscale_max = 45,
             sensor.colour = TRUE, cartesian =TRUE)
my.hum.time <-144
MyRecentPlot("Hum_Avg", my.hum.time, df.cast, #logger = "SYS8",
             yscale_min = 0, yscale_max = 105,
             sensor.colour = TRUE, cartesian =TRUE)
MyRecentPlot("Temp_Avg", my.hum.time, df.cast, #logger = "SYS1",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian =TRUE)

my.par.time <- 200

MyRecentPlot("PAR_Avg", my.par.time, df.cast,
             yscale_min = 0, yscale_max = 10,
             sensor.colour = TRUE, cartesian = FALSE)
library(ggplot2)
# all PAR sensor with 10 ohm resistor
PAR10ohm <- as.POSIXct("2015-10-06 13:00:00")
PAR_data <- df.cast[df.cast$TIMESTAMP > PAR10ohm &
                    #df.cast$SYSTEM != "SYS2" &
                    df.cast$SensorID == "1",]

PAR_data$PARtype <- "SLD"
PAR_data$PARtype[PAR_data$SYSTEM == "SYS2"] <- "Li190"
PAR_data$PARtype <- as.factor(PAR_data$PARtype)

p <- ggplot(PAR_data, 
            aes(x = TIMESTAMP, y = PAR_Avg))
  p <- p + geom_line(aes(colour = SYSTEM))
  p <- p + facet_grid(PARtype ~ ., scales = "free_y")
  p <- p + theme_bw()
p

library(reshape2)

df.PAR <- df[, c("SYSTEM", "TIMESTAMP", "PAR_Avg")]
df.PAR <- df.PAR[df.PAR$TIMESTAMP > PAR10ohm, ]
df.PAR$variable <- "PAR"
df.PAR.cast <- dcast(df.PAR,
                   TIMESTAMP ~ SYSTEM + variable,
                   value.var = "PAR_Avg")


Li190 <- MyRecentPlot("PAR_Avg", my.par.time, df.cast, logger = "SYS2",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
SLD_open <- MyRecentPlot("PAR_Avg", my.par.time, df.cast, logger = "SYS4",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
SLD_filter <- MyRecentPlot("PAR_Avg", my.par.time, df.cast, logger = "SYS3",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
SLD_1ohm <- MyRecentPlot("PAR_Avg", my.par.time, df.cast, logger = "SYS1",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
SLD_2ohm <- MyRecentPlot("PAR_Avg", my.par.time, df.cast, logger = "SYS6",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
SLD_5ohm <- MyRecentPlot("PAR_Avg", my.par.time, df.cast, logger = "SYS5",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
SLD_10ohm <- MyRecentPlot("PAR_Avg", my.par.time, df.cast, logger = "SYS8",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian = TRUE)
                     
df.PAR <- df[, c("SYSTEM", "TIMESTAMP", "PAR_Avg")]
#start.PAR <- as.POSIXct("2015-09-01", tz = "GMT")
df.PAR <- df.PAR[df.PAR$TIMESTAMP > PAR10ohm, ]


library(reshape2)
library(ggplot2)
df.PAR$variable <- "PAR"
df.PAR.cast <- dcast(df.PAR,
                   TIMESTAMP ~ SYSTEM + variable,
                   value.var = "PAR_Avg")
df.PAR.long <- melt(df.PAR.cast,
                    id.vars = c("TIMESTAMP", "SYS2_PAR"))
df.PAR.long <- df.PAR.long[df.PAR.long$value > 0, ]
df.PAR.long <- df.PAR.long[!is.na(df.PAR.long$value), ]

# correlations
# between Li190 and unfiltered SLD

library(plyr)
MyCor <- function(data, x, y) {
       #name <- unique(data$variable)
       my.lm <- lm(x ~ y)
       my.intercept <- coef(my.lm)[1]
       my.slope     <- coef(my.lm)[2]
       my.r2 <- summary(my.lm)[["adj.r.squared"]]
       my.list <- data.frame(intercept = my.intercept, slope = my.slope, r2 = my.r2)
       return(my.list)
}

my.cor <- ddply(df.PAR.long,
               .(variable),
               function(z) MyCor(x = z$SYS2_PAR, y = z$value))
my.cor.round <- my.cor
my.cor.round[, 2:4] <- sapply(my.cor[, 2:4], function(x) round(x, 2))
y.pos <- 10

p <- ggplot(df.PAR.long, aes(x = SYS2_PAR, y = value))
  p <- p + geom_point(aes(colour = variable))
  p <- p + geom_smooth(aes(colour = variable), method = "lm")
  p <- p + facet_grid(variable ~ .)
  p <- p + geom_text(aes(x = 0, y = 12, label = "intercept"),
                    data = my.cor.round, size = rel(4))
  p <- p + geom_text(aes(x = 200, y = 12, label = "slope"),
                    data = my.cor.round, size = rel(4))
  p <- p + geom_text(aes(x = 400, y = 12, label = "r2"),
                    data = my.cor.round, size = rel(4))
  p <- p + geom_text(aes(x = 0, y = 10, label = intercept),
                    data = my.cor.round, size = rel(4))
  p <- p + geom_text(aes(x = 200, y = 10, label = slope),
                    data = my.cor.round, size = rel(4))
  p <- p + geom_text(aes(x = 400, y = 10, label = r2),
                    data = my.cor.round, size = rel(4))
#  p <- p + annotate("text", aes(x = 0, y = 10, label = "Hi"), data = my.cor.round)
  p <- p + theme_bw()
p

df.PAR.long.recal <- df.PAR.long
df.PAR.long.recal$value <- df.PAR.long.recal$value -50 * 200

#PAR_data$PAR_Avg[PAR_data$SYSTEM != "SYS2"] <- PAR_data$PAR_Avg[PAR_data$SYSTEM != "SYS2"] - 50 * 200

p <- ggplot(PAR_data, 
            aes(x = TIMESTAMP, y = PAR_Avg))
  p <- p + geom_line(aes(colour = SYSTEM))
  p <- p + facet_grid(PARtype ~ .)
  p <- p + theme_bw()
p


# Sap flow
my.sap.time <- 144
MyRecentPlot("Sapflow_Avg", data = df.cast, my.sap.time, 
              yscale_min = -100, yscale_max = 500, cartesian = FALSE)
MyRecentPlot("dT_Avg", data = df.cast, my.sap.time, 
              yscale_min = NA, yscale_max = NA, cartesian = FALSE)
MyRecentPlot("Qf_Avg", data = df.cast, my.sap.time, 
              yscale_min = 0.053, yscale_max = 0.06, cartesian = FALSE)
MyRecentPlot("Kshapp_Avg", data = df.cast, my.sap.time, 
              yscale_min = -100, yscale_max = 100, cartesian = FALSE)
MyKshPlot(df.cast)
# ============ old correlations =============

lm.unfiltered <- lm(SYS4_PAR ~ SYS2_PAR, 
                  data = df.PAR.cast[df.PAR.cast$SYS4_PAR > 0 &
                                     df.PAR.cast$SYS2_PAR > 0, ])
summary(lm.unfiltered)
r2.unfiltered <- summary(lm.unfiltered)[["adj.r.squared"]]
r2.unfiltered <- summary(lm.unfiltered)[["adj.r.squared"]]
r2.unfiltered <- round(r2.unfiltered, 3)
r2.txt <- paste("Adj. R2 =", r2.unfiltered, sep = " ")

p <- ggplot(df.PAR.cast[df.PAR.cast$SYS4_PAR > 0 &
                        df.PAR.cast$SYS2_PAR > 0, ], 
                        aes(x = SYS2_PAR, y = SYS4_PAR))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_point()
  p <- p + annotate(geom= "text", x = 50, y = 0.3, label = r2.txt)
  p <- p + theme_bw()
  p <- p + labs(x = "PPFD from Li190 [umol photons m-2 s-1]",
                y = "PPFD from Silonex SLD, unfiltered [mV]")
p
LI190vsunfiltered <- p

# between Li190 and filtered SLD
lm.filtered <- lm(SYS3_PAR ~ SYS2_PAR, 
                    data = df.PAR.cast[df.PAR.cast$SYS3_PAR > 0 &
                                       df.PAR.cast$SYS2_PAR > 0, ])
summary(lm.filtered)
r2.filtered <- summary(lm.filtered)[["adj.r.squared"]]
r2.filtered <- round(r2.filtered, 3)
r2.txt <- paste("Adj. R2 =", r2.filtered, sep = " ")

p <- ggplot(df.PAR.cast[df.PAR.cast$SYS3_PAR > 0 &
                        df.PAR.cast$SYS2_PAR > 0, ], 
                        aes(x = SYS2_PAR, y = SYS3_PAR))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_point()
  p <- p + annotate(geom= "text", x = 50, y = 0.3, label = r2.txt)
  p <- p + theme_bw()
  p <- p + labs(x = "PPFD from Li190 [umol photons m-2 s-1]",
                y = "PPFD from Silonex SLD, filtered [mV]")
p
LI190vsfiltered <- p


library(ggplot2)
library(grid)
library(gridExtra)
a <- ggplotGrob(Li190)
b <- ggplotGrob(SLD_open)
c <- ggplotGrob(SLD_filter)
d <- ggplotGrob(SLD_1ohm)
e <- ggplotGrob(SLD_2ohm)
f <- ggplotGrob(SLD_5ohm)
g <- ggplotGrob(SLD_10ohm)
h <- ggplotGrob(LI190vsfiltered)
i <- ggplotGrob(LI190vsunfiltered)

# arrange several plots on one page with aligned y-axes
# not sure about a 3x2 rows  and column layout
grid.draw(rbind(a, b, c, d, e, f, g, size = "first"))
grid.draw(rbind(g, h, size = "first"))

pdf(file = "PAR_sensor_comparison.pdf",
    width = 11, height = 9)
print(grid.draw(rbind(g, h, size = "first")))
print(grid.draw(rbind(a, b, c, d, e, f,  size = "first")))
dev.off()



