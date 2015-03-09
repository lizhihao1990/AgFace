setwd("~/AgFace/2014/HH2_Soil_moisture")

load("PR2_soil_moisture.RData")

# import calibration data set (R. Argall, 2014-12-01)

calib <- read.csv("./calibration_data/2014-12-01_soil_moisture_calibration_harvest.csv")

# recode depth to 100  200  300  400  600 1000 mm
calib$Depth <- gsub(" 0-10", "100", calib$Depth)
calib$Depth <- gsub("0-10", "100", calib$Depth)
calib$Depth <- gsub(" 10-20", "200", calib$Depth)
calib$Depth <- gsub("20-30", "300", calib$Depth)
calib$Depth <- gsub("30-40", "400", calib$Depth)
calib$Depth <- gsub("40-60", "600", calib$Depth)
calib$Depth <- gsub("61000", "1000", calib$Depth)
calib$Depth <- as.numeric(calib$Depth)
#calib$Depth <- factor(calib$Depth, levels = c(100, 200, 300, 400, 600, 1000))

# sensor parameters
a0 <- 1.6
a1 <- 8.4


# overview plots
library(ggplot2)

p <- ggplot(calib[calib$Cultivar != "" &
                  calib$Tube_treatment != "dry", ], aes(y = Depth, x = GravWater))
  p <- p + geom_path(aes(colour = Tube_treatment))
  p <- p + facet_grid(Cultivar ~ Ring)
  p <- p + scale_y_reverse()
  p <- p + theme_bw()
  p <- p + labs(y = "Depth [mm]",
                x = "Gravimetric soil moisture [%]",
                colour = "Plot irrigation",
                title = "Soil moisture calibration data per ring, Russel, Dec 2, 2014")
p
fig.profile <- p

p <- ggplot(calib[calib$Cultivar != "" &
                  calib$Tube_treatment != "dry", ], 
            aes(x = Cultivar, y = FreshWt))
  p <- p + geom_boxplot(aes(fill = CO2))
  p <- p + facet_grid(Depth ~ Tube_treatment, scale = "free_y")
  p <- p + theme_bw()
  p <- p + labs(y = "Soil fresh weight [g]")
p
fig.boxplot.FreshWt <- p

p <- ggplot(calib[calib$Cultivar != "" &
                  calib$Tube_treatment != "dry", ], 
            aes(x = Cultivar, y = DryWt))
  p <- p + geom_boxplot(aes(fill = CO2))
  p <- p + facet_grid(Depth ~ Tube_treatment, scale = "free_y")
  p <- p + theme_bw()
  p <- p + labs(y = "Soil dry weight [g]")
p
fig.boxplot.DryWt <- p

p <- ggplot(calib[calib$Cultivar != "" &
                  calib$Tube_treatment != "dry", ], 
            aes(x = Cultivar, y = GravWater))
  p <- p + geom_boxplot(aes(fill = CO2))
  p <- p + facet_grid(Depth ~ Tube_treatment)
  p <- p + theme_bw()
    p <- p + labs(y = "Gravimetric water content [%]")
p
fig.boxplot.GravWaterCont <- p

p <- ggplot(calib[calib$Cultivar != "" &
                  calib$Tube_treatment != "dry", ], 
            aes(x = Cultivar, y = GravWater))
  #p <- p + geom_boxplot(aes(fill = CO2))
  p <- p + stat_summary(fun.data = "mean_sdl", mult = 1, aes(colour = CO2, shape = Tube_treatment))
  p <- p + facet_grid(Depth ~ Ring)
  p <- p + theme_bw()
    p <- p + labs(y = "Gravimetric water content [%]",
                  shape = "Plot irrigation",
                  title = "Soil moisture per ring, plot, and depth (n = 1)")
p
fig.boxplot.GravWaterContInd <- p


# merge gravimetric soil moisture calibration with last soil moisture readings
sm.Dec <- sm[sm$Time > as.POSIXct("2014-12-01", tz = "Australia/Melbourne"), ]

to.keep <- c("Crop", "Cultivar", "Ring", "PlotID", "Sample", "Tube_treatment", "Depth", "DryWt", "FreshWt", "GravWater")

calib.strip <- calib[, names(calib) %in% to.keep]

calib.strip$PlotID[calib.strip$PlotID == "" ] <- NA
calib.strip$Crop[calib.strip$Crop == "" ] <- NA

sm.calib <- merge(sm.Dec, calib.strip, all.x = TRUE)
#sm.calib <- sm.calib[sm.calib$Crop != "Lentil", ]

#xxx <- merge(sm.Dec[sm.Dec$Sample == 1, ], calib.strip[calib.strip$Sample == 1, ])

p <- ggplot(sm.calib, aes(x = GravWater, y = mV))
  p <- p + geom_point(aes(colour = Depth))
  p <- p + geom_smooth(method = "lm")
  #p <- p + facet_grid(Cultivar ~ Tube_treatment)
  p <- p + theme_bw()
  p <- p + labs(x = "Gravimetric water content [%]",
                y = "Profile probe reading [mV]",
                colour = "Depth [mm]")
p

# Convert the Profile Probe measurements into √ε using its sensor calibration equation
# sqrt(E) <- a0 + a1 * sm.calib$Percent_Vol
sm.calib$V <- sm.calib$mV/1000

sm.calib$sqE <- with(sm.calib, 
                     1.125 - 5.53 * V + 67.17 * V^2 - 234.42 * 
                     V^3 + 413.56 * V^4 - 356.68 * V^5 + 121.53 * V^6)

# diameter of corer
core.dia <- 42 # mm

# length of cores
sm.calib$core.length <- NA
sm.calib$core.length[sm.calib$Depth == 100] <- 100
sm.calib$core.length[sm.calib$Depth == 200] <- 100
sm.calib$core.length[sm.calib$Depth == 300] <- 100
sm.calib$core.length[sm.calib$Depth == 400] <- 100
sm.calib$core.length[sm.calib$Depth == 600] <- 200
sm.calib$core.length[sm.calib$Depth == 1000] <- 400

# volumetric water content theta = (mass.wet - mass.dry) / (density of water pw * Volume.wet)

# bulk density" dry weight / volume"
# different bulk densities at different depths
# then bulk multiplied with gravimetric to get to volumetric water
# to get bulk density in g/cm3
sm.calib$BulkDens <- with(sm.calib,
                    DryWt / (pi * ((core.dia/10)/2)^2 * (core.length/10)))

library(plyr)
bulk.dens.median <- ddply(sm.calib,
                          .(Depth),
                          summarise,
                          my.n = length(BulkDens),
                          my.mean = mean(BulkDens, na.rm = TRUE),
                          my.median = median(BulkDens, na.rm = TRUE),
                          my.sdup = my.mean + sd(BulkDens, na.rm = TRUE),
                          my.sddown = my.mean - sd(BulkDens, na.rm = TRUE))

# are there differences in bulk density due to depth?
my.aov <- aov(BulkDens ~ Depth, data = sm.calib)
summary(my.aov)

# After phone conversation with Roger, Gary, Russel, Sam, Glenn, me:
# remove some data due to "impossible" bulk densities.
# Rules for exclusion of data:
# bulk densities > 2 g cm3 get discarded
# bulk densities < 1 g cm3 get discarded

# histogram of Bulk densities
p <- ggplot(sm.calib, aes(x = BulkDens)) 
  p <- p + geom_histogram()
  # p <- p + geom_density(colour = "blue")
  p <- p + geom_vline(aes(xintercept = my.median), 
                      colour = "red", 
                      data = bulk.dens.median)
  p <- p + geom_vline(aes(xintercept = my.sdup), 
                      colour = "grey", 
                      data = bulk.dens.median)
  p <- p + geom_vline(aes(xintercept = my.sddown), 
                      colour = "grey", 
                      data = bulk.dens.median)
  p <- p + geom_vline(aes(xintercept = 2), colour = "blue")
  p <- p + geom_vline(aes(xintercept = 1), colour = "blue")
  p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
  p <- p + labs(x = expression("Bulk Density"~"["*g~cm^3*"]"),
                title = "Bulk density histograms, red line indicates median, grey lines sd")
p
fig.BulkDensityHistogram <- p
ggsave(file = "Bulk_densities_per_depth.pdf",
       width = 9, height = 7)

water.density.rho <- 997.7735 # kg/m3 at 22°C
# water.density.rho <- water.density.rho * 1000

# volumetric soil water content [m3 m^-3]
sm.calib$volSWC <- with(sm.calib, (FreshWt/1000 - DryWt/1000) / (water.density.rho * (pi * ((core.dia/1000)/2)^2 * (core.length/1000))))

# Roger Armstrong: volumetric soil water content is bulk density multiplied by gravimetric water content
sm.calib$volSWCRoger <- with(sm.calib, BulkDens * GravWater)
p <- ggplot(sm.calib, aes(x = volSWC, y = volSWCRoger/100))
  p <- p + geom_smooth(method = "lm", se = FALSE)
  p <- p + geom_point()
  p <- p + theme_bw()
  p <- p + labs(x = expression(over((Freshwt-DryWt) , (rho%.%Volume))),
                y = expression(over(DryWt, Volume)%.%gravimetric~water~content))
p
fig.CalcComparison <- p

# data rejection for bulk density and for sqrt(e)
# bulk densities > 2 g cm3 get discarded
# bulk densities < 1.2 g cm3 get discarded
# sqE > 6 get discarded

sm.calib$BulkDens[sm.calib$BulkDens < 0.9 | sm.calib$BulkDens > 2] <- NA
sm.calib$sqE[sm.calib$sqE > 7.5] <- NA

p <- ggplot(sm.calib,
            aes(x = volSWC, y = sqE))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_smooth(method = "lm", se = TRUE)
  #p <- p + facet_grid(Cultivar ~ Tube_treatment)
  p <- p + theme_bw()
  p <- p + labs(x = "Volumetric water content [%], based on soil cores",
                y = expression(sqrt(epsilon)),
                colour = "Irrigation")
p

# linear regression
my.lm <- lm(sqE ~ volSWC, data = sm.calib)
summary(my.lm)
my.a0 <- coef(my.lm)[1]
my.a1 <- coef(my.lm)[2]
my.r2 <- round(summary(my.lm)$r.squared, 3)
my.sum <- summary(my.lm)
my.p.value <- pf(my.sum$fstatistic[1], my.sum$fstatistic[2], my.sum$fstatistic[3],
     lower.tail = FALSE)

intercept_slope <- paste("Slope:", round(my.a1, 3), 
                         "Intercept:", round(my.a0, 3), sep = " ")

p <- ggplot(sm.calib,
            aes(x = volSWC, y = V))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_smooth(method = "lm")
  #p <- p + facet_grid(Cultivar ~ Tube_treatment)
  p <- p + annotate("text",
                    x = 0.15, y = 1,
                    label = intercept_slope)
  p <- p + annotate("text",
                    x = 0.15, y = 0.9,
                    label = paste("R^2*':'~", my.r2, "~p-value<=", signif(my.p.value, 1), 
                    sep = ""),  parse = TRUE)
  p <- p + theme_bw()
  p <- p + labs(x = "Volumetric soil water content [%], from soil cores",
                y = "Profile probe output [V]",
                colour = "Irrigation")
p
fig.calib <- p

# conversion mV to sqrt(epsilon)
p <- ggplot(sm.calib,
            aes(x = mV, y = sqE))
  p <- p + geom_point(aes(colour = Tube_treatment))
  #p <- p + geom_smooth(method = "lm")
  #p <- p + facet_grid(Cultivar ~ Tube_treatment)
  p <- p + theme_bw()
  p <- p + labs(x = "Profile probe output [mV]",
                y = expression("Refractive index"~sqrt(epsilon)),
                colour = "Irrigation")
p
fig.mVtoSqE <- p

# calculate theta [m3 m-3]
sm.calib$SWC <- with(sm.calib, ((1.125 - 5.53 * V + 67.17 * V^2 - 234.42 * 
                V^3 + 413.56 * V^4 - 356.68 * V^5 + 121.53 * V^6) - my.a0) / my.a1)

p <- ggplot(sm.calib,
            aes(x = SWC, y = sqE))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_smooth(method = "lm")
  #p <- p + facet_grid(Cultivar ~ Tube_treatment)
  p <- p + theme_bw()
  p <- p + labs(x = "SWC [m3 m-3]",
                y = expression(sqrt(epsilon)),
                colour = "Irrigation")
p

p <- ggplot(sm.calib,
            aes(x = volSWC, y = SWC))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_abline(aes(intercept = 0, slope = 1), colour = "grey")
  p <- p + geom_smooth(method = "lm")
  #p <- p + facet_grid(Cultivar ~ Tube_treatment)
  p <- p + theme_bw()
  p <- p + coord_equal(ratio = 1, xlim = c(0, 1.2001), ylim = c(0, 1.2001))
  p <- p + labs(x = expression("volumetric SWC"~"["*m^3~m^-3*"]"~"from soil cores"),
                y = expression("volumetric SWC"~"["*m^3~m^-3*"]"~"from self-calibrated profile probes"),
                colour = "Irrigation")
p

fig.SWCcores_vs_SWC_self_calibrated <- p
ggsave(file = "Soil_moisture_from_soil_cores_vs_self-calibrated.pdf",
       width = 9, height = 9)
       
my.lm.volSWCvsSWC <- lm(SWC ~ volSWC,
                        data = sm.calib)
summary(my.lm.volSWCvsSWC)


p <- ggplot(sm.calib[is.na(sm.calib$Ring) &
                     !is.na(sm.calib$Percent_Vol), ],
            aes(x = Percent_Vol, y = volSWC*100))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_smooth(method = "lm")
  #p <- p + geom_smooth(method = "lm", data = sm.calib[sm.calib$Percent_Vol <= 10, ])
  # p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
  p <- p + coord_equal(ratio=1, ylim = c(0, 65), xlim = c(0, 65))
  p <- p + labs(x = "Soil moisture from factory default profile probe [%]",
                y = "Soil moisture recalibrated [%]",
                colour = "Irrigation",
                title = "Reference tubes only")
p

p <- ggplot(sm.calib[is.na(sm.calib$Ring) &
                     !is.na(sm.calib$Percent_Vol), ],
            aes(x = Percent_Vol, y = SWC*100))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_smooth(method = "lm")
  #p <- p + geom_smooth(method = "lm", data = sm.calib[sm.calib$Percent_Vol <= 10, ])
  # p <- p + facet_grid(Depth ~ .)
  p <- p + theme_bw()
  p <- p + coord_equal(ratio=1)#, ylim = c(0, 65), xlim = c(0, 65))
  p <- p + labs(x = "Soil moisture from factory default profile probe [%]",
                y = "Soil moisture recalibrated [%]",
                colour = "Irrigation",
                title = "Reference tubes only")
p
fig.comparison.calib.references <- p

p <- ggplot(sm.calib[!is.na(sm.calib$Ring), ],
            aes(x = Percent_Vol, y = SWC*100))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_abline(aes(intercept = 0, slope = 1), colour = "grey")
  #p <- p + geom_smooth(method = "lm", data = sm.calib[sm.calib$Percent_Vol <= 10, ])
  #p <- p + facet_grid(. ~ Tube_treatment)
  p <- p + theme_bw()
  p <- p + coord_equal(ratio=1, ylim = c(0, 100), xlim = c(0, 100))
  p <- p + labs(x = "Soil moisture from factory default profile probe [%]",
                y = "Soil moisture recalibrated [%]",
                colour = "Irrigation",
                title = "Data from reference profiles not included")
p
fig.comparison.calib.all <- p

my.lm.swc.com <- lm(SWC*100 ~ Percent_Vol, 
                    data = sm.calib[!is.na(sm.calib$Ring), ])
summary(my.lm.swc.com)


# compile output file
pdf(file = "Soil_moisture_calibration_Dec_2014.pdf",
    width = 9, height = 7)
print(fig.profile)
print(fig.boxplot.FreshWt)
print(fig.boxplot.DryWt)
print(fig.boxplot.GravWaterCont)
print(fig.boxplot.GravWaterContInd)
print(fig.BulkDensityHistogram)
print(fig.calib)
print(fig.mVtoSqE)
dev.off()

# compile output file for Roger regarding bulk densities
pdf(file = "Bulk_densities_et_al.pdf",
    width = 9, height = 7)
print(fig.BulkDensityHistogram)    
print(fig.CalcComparison)
dev.off()

# regression output comparing profie probe data and self-calibrated data.
pdf(file = "Correlation_factory_default_vs_self_calibrated.pdf",
    width = 9, height = 9)
print(fig.comparison.calib.all)
#print(fig.comparison.calib.references)
dev.off()

# comparison soil-cores versus profile probes for each individual probe.
p <- ggplot(sm.calib[!is.na(sm.calib$Ring) &
                     sm.calib$Crop == "Wheat", ],
            aes(y = SWC, x = volSWC))
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_abline(aes(intercept = 0, slope = 1), colour = "grey")
  #p <- p + geom_smooth(method = "lm", data = sm.calib[sm.calib$Percent_Vol <= 10, ])
  p <- p + facet_wrap(~ Ring * Sample, nrow = 8)
  p <- p + theme_bw()
  #p <- p + coord_equal(ratio=1)
  p <- p + labs(x = expression("Volumetric soil moisture from soil cores ["*m^3~m^-3*"]"),
                y = expression("Volumetric soil moisture from self-calibrated profile probes ["*m^3~m^-3*"]"),
                colour = "Irrigation")#,
                #title = "Comparison soil cores vs profile probe data per each profile probe.\nWheat data only. Each graph represents one profile probe indicated\nby ring and tube number.")
 p <- p + theme(legend.position=c(.96, .95))
p

fig.individual.tube.correlation <- p

ggsave(file = "volumetric_soil_moisture_probe_vs_core_comparison_for_each_probe.pdf",
       width = 10, height = 10)
       
# calculate averages per and water treatment depth
get.rid <- names(sm.calib)[!names(sm.calib) %in% c("Vol_Error", "mV_Error")]
sm.calib.strip <- sm.calib[, get.rid]

library(reshape2)
sm.calib.melt <- melt(sm.calib.strip,
                     id.vars = names(sm.calib.strip)[1:12])

sm.calib.cast <- dcast(sm.calib.melt,
                       Crop + Ring + Tube_treatment + Depth ~ variable,
                       fun.aggregate = mean, na.rm = TRUE)
# mean regressions
p <- ggplot(sm.calib.cast, aes(x = volSWC, y = sqE))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + theme_bw()
  #p <- p + facet_grid(Depth ~ .)
  p <- p + labs(x = expression("volumetric SWC"~"["*m^3~m^-3*"]"~"from soil cores"),
                y = expression("refractive index"~sqrt(epsilon)),
                colour = "Irrigation")
p
fig.calib.mean <- p

my.lm.mean <- lm(sqE ~ volSWC, data = sm.calib.cast)
summary(my.lm.mean)

# mean regressions per depth
p <- ggplot(sm.calib.cast, aes(x = volSWC, y = sqE))
  p <- p + geom_smooth(method = "lm")
  p <- p + geom_point(aes(colour = Tube_treatment))
  p <- p + theme_bw()
  p <- p + facet_grid(Depth ~ .)
  p <- p + labs(x = expression("volumetric SWC"~"["*m^3~m^-3*"]"~"from soil cores"),
                y = expression("refractive index"~sqrt(epsilon)),
                colour = "Irrigation")
p
fig.calib.mean.depth <- p

# lm per depth
# library(broom)
my.lm.depth <- ddply(sm.calib.cast, 
                     .(Depth),
                     function(x){
                     my.lm.mean <- lm(sqE ~ volSWC, data = x)
                     #summary(my.lm.mean)
                     # my.output <- tidy(my.lm.mean)
                     my.a0 <- coef(my.lm.mean)[1]
		     my.a1 <- coef(my.lm.mean)[2]
		     my.summary <- summary(my.lm.mean)
                     my.p.value <- pf(my.summary$fstatistic[1], 
                                      my.summary$fstatistic[2], 
                                      my.summary$fstatistic[3],
                                      lower.tail = FALSE)
                     my.r2 <- summary(my.lm.mean)$r.squared
                     out <- data.frame(Intercept = my.a0,
                                       Slope = my.a1,
                                       pvalue = my.p.value,
                                       R2 = my.r2)
                     return(out)
})

#my.lm.depth$term <- gsub("\\(Intercept\\)", "Intercept", my.lm.depth$term)
#my.lm.depth$term <- gsub("volSWC", "Slope", my.lm.depth$term)
#my.lm.depth$term <- as.factor(my.lm.depth$term)

#my.limits <- aes(ymax = estimate + std.error, 
#                 ymin = estimate - std.error)

#p <- ggplot(my.lm.depth, aes(x = Depth, y = estimate))
#  p <- p + geom_point()
#  p <- p + geom_errorbar(my.limits)
#  p <- p + facet_grid(term ~ ., scales = "free_y")
#  p <- p + theme_bw()
#  p <- p + labs(x = "Depth [mm]",
#                y = "Estimate for intercept or slope with standard error",
#                title = "Slopes and intercept for profile probe calibration per depth")
#p 

# calibration coefficient table
calib.coef <- my.lm.depth

pdf(file = "Soil_moisture_calibration_per_mean_water_treatment.pdf",
    width = 9, height = 7)
print(fig.calib.mean)
print(fig.calib.mean.depth)
dev.off()

save.image("HH2_calibration_workspace.RData", compress = TRUE)
