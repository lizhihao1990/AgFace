#ML3_soil_moisture_analysis

# after HH2_workflow_pre_season_calibration.R
# after import via HH2_ML3_import.R

library(ggplot2)
p <- ggplot(ML3.precal, aes(x = Sample, y = Percent_Vol))
  p <- p + geom_point()
  p <- p + theme_bw()
p

PR2.precal$Instrument <- "PR2"
PR2.precal.top <- PR2.precal[PR2.precal$Depth == 100, ]
PR2.precal.top$Week <- NULL
PR2.precal.top$Weekname <- NULL

ML3.precal$Instrument <- "ML3"
ML3.precal$Depth <- 100
ML3.precal$Day <- format(ML3.precal$Time, "%Y-%m-%d")

precal <- rbind(PR2.precal.top, ML3.precal)
precal$Vol_Error <- NULL
precal$mV_Error <- NULL
precal$Time <- NULL
precal$Device <- NULL
precal$Experiment <- NA
precal$Experiment[precal$Sample < 13] <- "Wet sump"
precal$Experiment[precal$Sample >= 13] <- "NFace"
precal$Experiment <- as.factor(precal$Experiment)

library(reshape2)
precal.melt <- melt(precal,
                    id.vars = c("Experiment", "Sample", "Instrument", "Day", "Depth"))

precal.cast <- dcast(precal.melt,
                     Experiment + Day + Sample + Depth ~ Instrument + variable)

p <- ggplot(precal.cast, aes(x = PR2_Percent_Vol, y = ML3_Percent_Vol))
  p <- p + geom_point(aes(colour = Experiment))
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
  p <- p + labs(x = "PR2 profile probe volumetric soil moisture [%]",
                y = "ML3 TDR volumetric soil moisture [%]")
p

# grab the gravimetric data
grav <- read.csv("../Gravimetric_data/2015-03-03_NFACE_calibration_grav_water.csv")
grav$X <- NULL
grav$Depth <- NULL
names(grav) <- gsub("Wet\\.Sumps", "Experiment", names(grav))
grav$Experiment <- gsub("Wet Sumps", "Wet sump", grav$Experiment)
grav$Experiment <- as.factor(grav$Experiment)
names(grav) <- gsub("Depth\\.*", "Depth", names(grav))
grav$Depth <- gsub( " 10-20", "10-20", grav$Depth)

# recode depth to 100  200  300  400  600 1000 mm
grav$Depth <- gsub("0-10", "100", grav$Depth)
grav$Depth <- gsub("10-20", "200", grav$Depth)
grav$Depth <- gsub("20-30", "300", grav$Depth)
grav$Depth <- gsub("30-40", "400", grav$Depth)
grav$Depth <- gsub("40-60", "600", grav$Depth)
grav$Depth <- gsub("61000", "1000", grav$Depth)
grav$Depth <- as.numeric(grav$Depth)
#grav$Depth <- as.factor(grav$Depth)
grav$Tube <- as.numeric(as.character(gsub("Tube ", "", grav$Tube)))
grav$Sample <- grav$Tube
grav$Tube <- NULL
grav$Day <- as.Date("2015-03-04", tz = "Australia/Melbourne")

#calculate bulk density
# diameter of corer
core.dia <- 24 # mm

# length of cores
grav$core.length <- NA
grav$core.length[grav$Depth == 100] <- 100
grav$core.length[grav$Depth == 200] <- 100
grav$core.length[grav$Depth == 300] <- 100
grav$core.length[grav$Depth == 400] <- 100
grav$core.length[grav$Depth == 600] <- 200
grav$core.length[grav$Depth == 1000] <- 400

grav$BulkDens <- with(grav,
                    Dry.Wt / (pi * ((core.dia/10)/2)^2 * (core.length/10)))

p <-ggplot(grav, aes(x = BulkDens))
  p <- p + geom_histogram()
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
p

#volumetric soil water content is bulk density multiplied by gravimetric water content
grav$volSWC <- with(grav, BulkDens * grav_water)

gravWC <- grav[, c("Sample", "Depth", "Day", "volSWC", "grav_water")]
gravWC.100 <- gravWC[gravWC$Depth == 100, ]
# merge gravimetric soil water content with precal.cast
precal.cast.noexp <- precal.cast[, names(precal.cast) != "Experiment"]
precal.SWC <- merge(precal.cast.noexp, gravWC.100,
                    all = TRUE)
precal.SWC$Sample <- as.factor(precal.SWC$Sample)

p <- ggplot(precal.SWC[!is.na(precal.SWC$volSWC), ],
             aes(y = PR2_Percent_Vol, x = grav_water))
  p <- p + geom_abline(slope = 1, intercept = 0, colour = "blue")
  p <- p + geom_point(aes(colour = Sample))
  p <- p + scale_x_continuous(limits = c(0, 22))
  p <- p + scale_y_continuous(limits = c(0, 22))
  p <- p + labs(y = "PR2 profile probe soil moisture [%]",
                x = "Gravimetric water content, soil core [%]")
  p <- p + theme_bw()
p
A <- p

p <- ggplot(precal.SWC[!is.na(precal.SWC$volSWC), ],
             aes(y = ML3_Percent_Vol, x =grav_water))
  p <- p + geom_abline(slope = 1, intercept = 0, colour = "blue")
  p <- p + geom_point(aes(colour = Sample))
  p <- p + scale_x_continuous(limits = c(0, 22))
  p <- p + scale_y_continuous(limits = c(0, 22))
  p <- p + labs(y = "ML3 profile probe soil moisture [%]",
                x = "Gravimetric water content, soil core [%]")
  p <- p + theme_bw()
p
B <- p

write.table(precal.SWC,
            file = "Soil_water_calibration.csv",
            row.names = F, sep = ",", na = "")

p <- ggplot(precal.SWC[!is.na(precal.SWC$volSWC), ], 
            aes(x = PR2_Percent_Vol, y = ML3_Percent_Vol))
  p <- p + geom_point(aes(colour = Sample))
  p <- p + geom_abline(slope = 1, intercept = 0, colour = "blue")
  p <- p + scale_x_continuous(limits = c(0, 9))
  p <- p + scale_y_continuous(limits = c(0, 9))
  p <- p + labs(x = "PR2 profile probe soil moisture [%]",
                y = "ML3 TDR soil moisture [%]")
  p <- p + theme_bw()
p

C <- p

a <- ggplotGrob(A)
b <- ggplotGrob(B)
c <- ggplotGrob(C)

library(gridExtra)
#pdf(file = "myfile.pdf", width = 19, height = 17)
grid.draw(rbind(a, b, c, size = "first"))
#grid.arrange(a, b, c, d, e, ncol = 1)
#dev.off()
