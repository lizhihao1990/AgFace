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
#my.files <- list.files(path = my.folder, "*_5Min.dat")

#library(plyr)
#my.list <- ldply(my.files,
#                 #.progress = "text",
#                 function(y) {
#                 print(y)
#                 my.import <- paste(my.folder, y, sep = "/")
#                 x <- CampbellFileImport(my.import)
#                 my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#                 names(x) <- names(my.header)
#                 x$Date <- as.POSIXct(x$TIMESTAMP, tz = "GMT")
#                 return(x)
#                 })

#my.file <- "SYS7_5Min.dat"
#my.import <- paste(my.folder, my.file, sep = "/")

#x <- CampbellFileImport(my.import)
#my.header   <- read.csv(my.import, skip = 1, nrows = 4)
#x <- read.csv(my.import, skip = 4, header = F)
#names(x) <- names(my.header)
#x$Date <- as.POSIXct(x$TIMESTAMP, tz = "GMT")

# end of individual import

df <- CampbellAllImport(log.interval = "5Min", #logger.name = "SYS8",
                        use.parallel = TRUE,
                        time.zone = "GMT", 
                        skip.rows = NA)

df <- df[df$TIMESTAMP > as.POSIXct("2015-12-01"), ]

system.time(
df.cast <- CampbellCast(df, use.parallel = TRUE)
)

# export data
#write.csv(df,
#          file = "5Min_Campbell_logger_data.csv",
#          row.names = FALSE, na = "")


# create complete timecourse
start.time <- min(df$TIMESTAMP)
end.time <- max(df$TIMESTAMP)

comp.time <- seq(from = start.time, to = end.time, by = "5 min")
comp.time <- as.data.frame(comp.time)
names(comp.time) <- "TIMESTAMP"

library(plyr)
df.comp <- ddply(df,
                 .(SYSTEM),
                 function(x) {
                 my.SYS <- unique(x$SYSTEM)
		 df.comp <- merge(comp.time, x, all.x = TRUE)
		 df.comp$mSYS <- my.SYS
		 return(df.comp)
                 })
df.comp$missing <- NA
df.comp$missing[is.na(df.comp$RECORD)] <- 1

# summary for Oct 1, 2015
#my.summary <- summary(df.comp[df.comp$Date == "2015-10-01" &
#                  !is.na(df.comp$SYSTEM), ])

#write.csv(df.comp[df.comp$Date == "2015-10-01" &
#                  is.na(df.comp$IR_Narrow_Max_4_) &
#                  !is.na(df.comp$SYSTEM), ], file = "Oct1.csv", row.names = F)
library(ggplot2)
p <- ggplot(df.comp, aes(x = TIMESTAMP, y = missing))
  p <- p + geom_line(colour = "red", size = 1.2)
  p <- p + facet_grid(mSYS ~ .)
  p <- p + labs(y = "Missing samples. Value of 1 indicates this sample is missing")
  p <- p + theme_bw()
p
ggsave(file = "Missing_5Min_data.pdf",
       width = 9, height = 7)
time.overview <- df.comp[df.comp$missing == 1, names(df.comp) %in% c("TIMESTAMP", "mSYS", "missing")]
write.csv(na.omit(time.overview), file = "Missing_5Min_data.csv",
          na = "", row.names = F)

# translate SYSTEMs to Rings
SYS.numbers <- c(1:8)
SYS.numbers <- paste("SYS", SYS.numbers, sep = "")
Ring.numbers <- c(3, 4, 6, 7, 10, 11, 15, 16)
CO2.treat  <- c("aCO2", "eCO2", "eCO2", "aCO2", "eCO2", "aCO2", "aCO2", "eCO2")
SYS.Ring <- data.frame(SYSTEM = as.factor(SYS.numbers),
                       Ring = as.factor(Ring.numbers),
                       CO2_treatment = as.factor(CO2.treat))

df.cast <- merge(df.cast, SYS.Ring)

# Sensor ID, System ID and Cultivar identification for sap flow
SYS.numbers.2 <- rep(SYS.numbers, 2)
Ring.numbers2 <- rep(Ring.numbers, 2)
SensorID.2 <- c(rep("1", 8), rep("2", 8))
# cultivar: sap flow sensor #1 always with long cable, i.e. far away plot
Cultivar.Plot <- c("Scout", "Yitpi", "Scout", "Yitpi", "Scout", "Scout", "Scout", "Yitpi", "Yitpi", "Scout", "Yitpi", "Scout", "Yitpi", "Yitpi", "Yitpi", "Scout")
Ring.Cultivar <- data.frame(Ring = as.factor(Ring.numbers2),
                            SensorID = as.factor(SensorID.2),
                            Cultivar = as.factor(Cultivar.Plot))
df.cast <- merge(df.cast, Ring.Cultivar, all.x = TRUE)

ephemeral.times <- CampbellSunriseSunset(df)
ephemeral.times$sunrise <- ephemeral.times$sunrise + 60*60*10
ephemeral.times$sunset <- ephemeral.times$sunset + 60*60*10

my.time.to.plot <- 180
MyRecentPlot("IR_Horz_Avg", my.time.to.plot, df.cast, #logger = "SYS7",
             yscale_min = -6.5, yscale_max = 55,
             sensor.colour = TRUE, cartesian = TRUE)
MyRecentPlot("IR_Narrow_Avg", my.time.to.plot, df.cast, logger = "SYS8",
             yscale_min = -5, yscale_max = 65,
             sensor.colour = TRUE, cartesian =TRUE)

my.hum.time <- 1000
MyRecentPlot("PAR_Avg", my.hum.time, df.cast, logger = "SYS2",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian =TRUE)
A <- MyRecentPlot("PAR_Avg", my.hum.time, df.cast,
             yscale_min = 0, yscale_max = 10,
             sensor.colour = TRUE, cartesian =TRUE)
A
B <- MyRecentPlot("Hum_Avg", my.hum.time, df.cast, #logger = "SYS8",
             yscale_min = 0, yscale_max = 105,
             sensor.colour = TRUE, cartesian =TRUE)
B
C <- MyRecentPlot("Temp_Avg", my.hum.time, df.cast, #logger = "SYS1",
             yscale_min = NA, yscale_max = NA,
             sensor.colour = TRUE, cartesian =TRUE)
C

# Sap flow
my.sap.time <- my.hum.time
#MyRecentPlot("Sapflow_Avg", data = df.cast, my.sap.time, 
#              sensor.colour = TRUE,
#              yscale_min = -100, yscale_max = 1, cartesian = FALSE)
D <- MyRecentPlot("dT_Avg", data = df.cast, my.sap.time, sensor.colour = TRUE,
              yscale_min = -1.1, yscale_max = 3.2, cartesian = FALSE)

# date of stem-cut
stem.cut.date <- as.POSIXct("2015-11-20 09:00:00", tz = "GMT")
put.back.up.date <- as.POSIXct("2015-11-24 16:00:00", tz = "GMT")

library(ggplot2)
D <- D + geom_vline(xintercept = as.numeric(stem.cut.date), 
               colour = "blue")
D <- D + geom_vline(xintercept = as.numeric(put.back.up.date), 
               colour = "green")
D

MyRecentPlot("Qf_Avg", data = df.cast, my.sap.time, sensor.colour = TRUE, #logger = "SYS1"
              yscale_min = 0.03, yscale_max = 0.06, cartesian = TRUE)
MyRecentPlot("Qf_Avg", data = df.cast, my.sap.time, sensor.colour = TRUE, logger = "SYS1",
              yscale_min = 0.0425, yscale_max = 0.0475, cartesian = FALSE)
MyRecentPlot("Kshapp_Avg", data = df.cast, my.sap.time, sensor.colour = TRUE,
              yscale_min = 0, yscale_max = 0.05, cartesian = TRUE)
MyKshPlot(df.cast)

# self-calculated sap flow
# sap flow = Qf / (dT * 4.168) * 3600 [g/h]
df.cast$my.sapflow <- with(df.cast, ((Qf_Avg / (dT_Avg * 4.186)) * 3600))

MyRecentPlot("my.sapflow", data = df.cast, my.sap.time, 
              sensor.colour = TRUE,
              yscale_min = 0, yscale_max = 100, cartesian = FALSE)

library(ggplot2)
p <- ggplot(df.cast[df.cast$SYSTEM == "SYS2", ],
            aes(x = PAR_Avg, y = dT_Avg))
  p <- p + geom_point()
p

# save.image(file = "5Min.RData", compress = TRUE)

# calculate VPD
# from Abtew, Melesse (2013) Evaporation and Evapotranspiration
# chapter 5.2.1
# es = 0.611 * exp((17.27 * T) / (T + 237.3))
# VPD = es * (1 - (RH/100))

df.cast$es <- with(df.cast, 0.611 * exp((17.27 * Temp_Avg) / (Temp_Avg + 237.3)))
df.cast$VPD_Avg <- with(df.cast, es * (1 - (Hum_Avg/100)))

E <- MyRecentPlot("VPD_Avg", data = df.cast, my.sap.time, 
              sensor.colour = TRUE,
              yscale_min = NA, yscale_max = NA, cartesian = FALSE)
E

# calculate sap flow Jw from dT alone
# Langensiepen et al (2014) Eq 3:
# J w,t = k(T d,t − T u,t )
k <- 0.46 # set to a constant mean value from the range of 0.37 < k < 0.55
df.cast$Jw_Langensiepen <- k * df.cast$dT_Avg

F <- MyRecentPlot("Jw_Langensiepen", data = df.cast, my.sap.time, 
              sensor.colour = TRUE,
              yscale_min = 0, yscale_max = 2, cartesian = FALSE)
F

# calculation sap flow Jw the old fashioned way (Eq. 1, Langensiepen)
# Jw = (Qh - Qv - Qr) / Cw * (Td - Tu) # [g s-1]
Cw <- 4.18 # specific heat of water, 4.18 J g-1 K-1
df.cast$Jw_trad <- with(df.cast, (Pin_Avg - Qv_Avg - Qr_Avg) / (Cw * dT_Avg))


G <- MyRecentPlot("Jw_trad", data = df.cast, my.sap.time, 
              sensor.colour = TRUE,
              yscale_min = 0, yscale_max = 2, cartesian = FALSE)
G

# get an idea of day and night-time.
df.cast$Day <- format(df.cast$TIMESTAMP, "%Y-%m-%d")
df.cast$Hour <- as.numeric(format(df.cast$TIMESTAMP, "%H"))

# crude daytime definition
df.cast$TOD <- "Nighttime"
df.cast$TOD[df.cast$Hour >= 7 & df.cast$Hour <= 19] <- "Daytime"
df.cast$TOD <- as.factor(df.cast$TOD)

p <- ggplot(df.cast[!is.na(df.cast$Jw_Langensiepen) &
                    df.cast$TOD == "Daytime" &
                    #df.cast$CO2_treatment == "aCO2" & 
                    df.cast$Jw_Langensiepen >= 0 &
                    df.cast$TIMESTAMP < stem.cut.date, ],
            aes(x = VPD_Avg, y = Jw_Langensiepen))
  p <- p + geom_point(aes(colour = CO2_treatment), alpha = 0.1)
  p <- p + geom_smooth(aes(colour = CO2_treatment), method = "lm")
  p <- p + facet_grid(. ~ Cultivar)
  p <- p + scale_x_continuous(limits = c(0, 6))
  #p <- p + coord_cartesian(ylim = c(0, 2), xlim = c(-0.2, 6))
  p <- p + theme_bw()
  p <- p + labs(y = expression("Sapflow estimated via fixed 'k', cf. Langensiepen et al. 2014 ["~g* hr^-1~"]"),
                x = "Vapour pressure deficit inside canopy, VPD [kPa]",
                colour = expression(bold("CO"[2]~"treatment")),
                title = "Sapflow to VPD daytime relationship, 5 min raw data")
p
fig.sapflow <- p

my.sunset <- ephemeral.times$sunset[1:length(ephemeral.times$sunrise) - 1] + 2*60*60
my.sunrise <- ephemeral.times$sunrise[2:length(ephemeral.times$sunrise)] + 2*60*60

my.start <- as.POSIXct("2015-11-06")
my.end <- max(df.cast$TIMESTAMP)
p <- ggplot(df.cast[df.cast$VPD_Avg > 0 &
                    df.cast$TIMESTAMP > my.start,],
                    aes( x = TIMESTAMP, y = VPD_Avg))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1, 
                        aes(colour = CO2_treatment), 
                        geom = "errorbar", alpha = 0.1)
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1, 
                        aes(colour = CO2_treatment), 
                        geom = "line")
  p <- p + annotate("rect", 
                    xmin = my.sunset, 
                    xmax = my.sunrise, 
                    ymin = -Inf, ymax = Inf, 
                    fill = "grey", alpha = 0.1)
  p <- p + coord_cartesian(xlim = c(my.start, my.end))
  p <- p + facet_grid(. ~ Cultivar)
  p <- p + theme_bw()
  p <- p + labs(y = "Vapour pressure deficit inside canopy, VPD [kPa]",
                colour = expression(bold("CO"[2]~"treatment")),
                title = "Example VPD timecourse")
p
fig.VPD <- p

p <- ggplot(df.cast[df.cast$VPD_Avg > 0 &
                    df.cast$TIMESTAMP > my.start,],
                    aes( x = TIMESTAMP, y = Temp_Avg))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1, 
                        aes(colour = CO2_treatment), 
                        geom = "errorbar", alpha = 0.1)
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1, 
                        aes(colour = CO2_treatment), 
                        geom = "line")
  p <- p + annotate("rect", 
                    xmin = my.sunset, 
                    xmax = my.sunrise, 
                    ymin = -Inf, ymax = Inf, 
                    fill = "grey", alpha = 0.1)
  p <- p + coord_cartesian(xlim = c(my.start, my.end))
  p <- p + facet_grid(. ~ Cultivar)
  p <- p + theme_bw()
  p <- p + labs(y = "Temperature inside canopy [°C]",
                colour = expression(bold("CO"[2]~"treatment")),
                title = "Example temperature timecourse")
p
fig.Temp <- p

# Grab maximum sap flow per day
library(plyr)
Jw.daily.max <- ddply(df.cast,
                      .(Day, SYSTEM, SensorID, Ring, CO2_treatment, Cultivar),
                      summarise,
                      Jw.max = max(Jw_Langensiepen))
Jw.daily.max$Day <- as.POSIXct(Jw.daily.max$Day, tz = "GMT")

Jw.decline <- ddply(Jw.daily.max[Jw.daily.max$Day > as.POSIXct("2015-10-20"), ],
                    .(CO2_treatment, Ring, Cultivar),
                    function(x) {
                    my.lm <- lm(Jw.max ~ Day, data = x)
                    my.coef <- coef(my.lm)
                   return(data.frame(Intercept = my.coef[1],
                                     Slope = my.coef[2]))
                    #return(summary(my.lm))
                #    return(my.coef)
                    }
                    )
Jw.decline$fin <- with(Jw.decline, (Intercept * -1)/Slope) 
Jw.decline$fin <- as.POSIXct(Jw.decline$fin, origin = "1970-01-01")

p <- ggplot(Jw.daily.max[Jw.daily.max$Day > as.POSIXct("2015-10-20"), ], 
            aes(x = Day, y = Jw.max))
  p <- p + geom_hline(yintercept = 0, alpha = 0.3)
  p <- p + geom_abline(data = Jw.decline, aes(intercept = Intercept, slope = Slope))
  p <- p + geom_point(data = Jw.decline, y = 0, aes(x = fin))
  p <- p + geom_vline(xintercept = as.numeric(stem.cut.date), colour = "blue")
  p <- p + geom_vline(xintercept = as.numeric(put.back.up.date), colour = "green")

  p <- p + geom_smooth(aes(colour = CO2_treatment), method = "lm")
  p <- p + stat_summary(fun.data = "mean_sdl", aes(colour = CO2_treatment))
  p <- p + facet_grid(Ring ~ Cultivar)
  p <- p + expand_limits(x = as.POSIXct("2015-12-04"))
  p <- p + coord_cartesian(xlim = c(as.POSIXct("2015-10-20"), as.POSIXct("2015-12-06")))
  p <- p + theme_classic()
  p <- p + labs(y = "Maximum daily sapflow Jw [g hr-1]")
p
fig.decline <- p

#ggsave(file = "Decline_of_sapflow.pdf",
#       width = 11, height = 9)

p <- ggplot(df.cast[df.cast$SensorID %in% c("1", "2") &
                    !is.na(df.cast$Jw_Langensiepen), ], 
                    aes(x = VPD_Avg, y = Jw_Langensiepen))
  p <- p + geom_point(alpha = 0.1)
  p <- p + geom_smooth(method = "lm", aes(colour = CO2_treatment, linetype = Cultivar))
#  p <- p + geom_smooth(aes(colour = Cultivar, linetype = CO2_treatment),
#                       method = "nls",
#                       formula = y ~ a * b^((x - c)^2), 
#                       start = c(a = 1, b = 0.01, c = 2.5))
  p <- p + coord_cartesian(ylim = c(0, 2), xlim = c(-0.2, 5))
  p <- p + facet_grid(. ~ Ring)
  p <- p + labs(y = expression("Sapflow estimated via fixed 'k', cf. Langensiepen et al. 2014 ["~g* hr^-1~"]"),
                x = "Vapour pressure deficit, VPD [kPa]")
  p <- p + theme_bw()
p
fig.VPD.resp <- p

# light dependence
df.cast$Stem.intact <- "Stem ok"
df.cast$Stem.intact[df.cast$TIMESTAMP > stem.cut.date] <- "Stem cut"
df.cast$Stem.intact <- as.factor(df.cast$Stem.intact)

# merge the PAR data with the other data to ensure each SensorID has access to PAR data.
to.keep <- c("TIMESTAMP", "SYSTEM", "Ring", "PAR_Avg")
PAR <- df.cast[, names(df.cast) %in% to.keep]
names(PAR) <- gsub("PAR_Avg", "myPAR", names(PAR))

df.castPAR <- merge(df.cast, PAR)

# light dependence of Jw before and after stem cutting                    
pardep <- df.castPAR[!is.na(df.castPAR$Jw_Langensiepen) &
                    df.castPAR$myPAR > 0.05, ]
pardep <- pardep[!is.na(pardep$TIMESTAMP), ]
p <- ggplot(pardep, aes(x = myPAR, y = Jw_Langensiepen))
  p <- p + geom_point(alpha = 0.15)
  p <- p + geom_smooth(method = "lm", aes(colour = Cultivar))
#  p <- p + geom_smooth(aes(colour = Cultivar, linetype = CO2_treatment),
#                       method = "nls",
#                       formula = y ~ a * b^((x - c)^2), 
#                       start = c(a = 1, b = 0.01, c = 2.5))
 # p <- p + coord_cartesian(ylim = c(0, 2), xlim = c(-0.2, 5))
  p <- p + facet_grid(Stem.intact ~ Ring, scales = "free_x")
  p <- p + labs(y = expression("Sapflow estimated via fixed 'k', cf. Langensiepen et al. 2014 ["~g* hr^-1~"]"),
                x = expression("Photosynthetic active radiation PAR ["~mu*mol~m^-2*s^-1~"] or mV"))
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(size = rel(0.65), angle = 0))
p
fig.PAR.resp <- p

pdf(file = "VPD_Temp_and_sapflow.pdf",
    width = 11, height = 9)
#png("Rplot%03d.png", width = 1024, height = 768)
print(fig.Temp)
print(fig.VPD)
print(fig.sapflow)
print(fig.decline)
print(fig.VPD.resp)
print(fig.PAR.resp)
dev.off()

ggsave(fig.PAR.resp, file = "Light_response_of_Jw_pre-post_cut.pdf",
       width = 9, height = 7)

