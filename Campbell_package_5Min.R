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
df <- CampbellAllImport(log.interval = "5Min", use.parallel = TRUE, time.zone = "GMT")
#df <- CampbellAllImport(log.interval = "5Min", use.parallel = FALSE, logger.name = "SYS2")

df.cast <- CampbellCast(df, use.parallel = TRUE)

ephemeral.times <- CampbellSunriseSunset(df)
ephemeral.times$sunrise <- ephemeral.times$sunrise + 60*60*10
ephemeral.times$sunset <- ephemeral.times$sunset + 60*60*10

my.time.to.plot <- 72
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast,
             yscale_min = -6.5, yscale_max = 45,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast,
             yscale_min = -5, yscale_max = 45,
             sensor.colour = TRUE, cartesian =TRUE)
my.hum.time <-72
MyRecentPlot("Hum_Avg", my.hum.time, df.cast, #logger = "SYS8",
             yscale_min = 0, yscale_max = 105,
             sensor.colour = TRUE, cartesian =TRUE)
MyRecentPlot("Temp_Avg", my.hum.time, df.cast, logger = "SYS1",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian =TRUE)


my.par.time <- 48

MyRecentPlot("PAR_Avg", my.par.time, df.cast,
             yscale_min = 0, yscale_max = 10,
             sensor.colour = TRUE, cartesian = FALSE)

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
start.PAR <- as.POSIXct("2015-09-01", tz = "GMT")
df.PAR <- df.PAR[df.PAR$TIMESTAMP > start.PAR, ]

library(reshape2)
library(ggplot2)
df.PAR$variable <- "PAR"
df.PAR.cast <- dcast(df.PAR,
                   TIMESTAMP ~ SYSTEM + variable,
                   value.var = "PAR_Avg")
                   
# correlations
# between Li190 and unfiltered SLD

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



