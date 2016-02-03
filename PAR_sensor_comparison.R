# follow-up script to Campbell package 5Min script
# now outdated (Nov 2015) Light sensor comprison already done elsewhere.

# light sensor cross-check
Li190 <- PAR[PAR$SYSTEM == "SYS2", ]
names(Li190) <- gsub("myPAR", "Li190", names(PAR))
Li190$SYSTEM <- NULL
Li190$Ring <- NULL
noSYS2 <- PAR[PAR$SYSTEM != "SYS2", ]
PAR.Li <- merge(Li190, noSYS2)
PAR.similar.date <- as.POSIXct("2015-10-06 13:00:00")
PAR.Li <- PAR.Li[!is.na(PAR.Li$Li190) &
                 !is.na(PAR.Li$myPAR) &
                 PAR.Li$TIMESTAMP > PAR.similar.date, ]
PAR.Li <- PAR.Li[!is.na(PAR.Li$SYSTEM), ]

p <- ggplot(PAR.Li[PAR.Li$Li190 > 0 &
                   PAR.Li$myPAR > 0.05 &
                   !is.na(PAR.Li$SYSTEM), ], 
            aes( x= Li190, y = myPAR))
  p <- p + geom_point(alpha = 0.15)
  p <- p + geom_abline(aes(intercept = 0, slope = 12/2500, colour = "red"), name = "slope 12/2500", show_guide = TRUE)
  p <- p + geom_smooth(method = "lm", aes(colour = "blue"), show_guide = FALSE)
  p <- p + scale_colour_manual("Lines", values = c("red", "blue"),
                                        labels = c("linear model", 
                                                 expression("fixed Slope "~frac(12~"mV", 2500~mu*mol^-2*s^-1)==frac(1~mV, 208.3~mu*mol^-2*s^-1))))
  p <- p + facet_grid(. ~ SYSTEM)
  p <- p + labs(x = expression("Photosynthetic active radiation measured by Licor Q190 sensor installed in SYS2"~"["~mu*mol^-2*s^-1~"]"),
                y = "Light as measured by Silonex SLD sensor [mV]")
  p <- p + guides(colour = guide_legend(override.aes = list(linetype = 1, fill = NA, shape = c(1, 1))))
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(size = rel(0.65), angle = 0),
                 legend.position = "bottom")
p
lightsensor.cor <- p

ggsave(lightsensor.cor, file = "Light_sensor_comparison_Li190_vs_Silonex_SLD.pdf",
       width = 9, height = 7)

# curve fit with Type V exponential function
# Y = ab^((X-c)^2)
# from raw data: a (peak) = 1, b = 0.01, c = 2.5

# function for testing regressions
FitTypeV <- function(x, y) {
		nlsfit <- try(nls( y ~ a * b^((x - c)^2),
		              start = c(a = 1, b = 0.01, c = 2.5)))
		if (inherits(nlsfit, "try-error")) {
		    coefs <- c(NA, NA, NA)
		    r2 <- NA
		} 
		else {coefs <- coef(nlsfit)
		      r2 <- 1 - (var(residuals(nlsfit))/var(y))
		}
		out <- c(coefs, r2)
		names(out) <- c("A", "B", "C", "R2")
		return(out)
}

FitTypeVvalues <- function(x, y) {
		nlsfit <- try(nls( y ~ a * b^((x - c)^2),
		              start = c(a = 0.6, b = 0.6, c = 4.5)))
		if (inherits(nlsfit, "try-error")) {
		    out <- data.frame(NA)
		} 
		else {coefs <- coef(nlsfit)
		      r2 <- 1 - (var(residuals(nlsfit))/var(y))
		      out <- fitted.values(nlsfit)
		}
		return(out)
}

require(plyr)
my.fit <- ddply(df.cast[df.cast$SensorID %in% c("2") &
                        df.cast$SYSTEM == "SYS1" &
                        !is.na(df.cast$Jw_Langensiepen), ],
                .(SYSTEM, SensorID),
                function(x) {
                FitTypeV(x = x$VPD_Avg, y = x$Jw_Langensiepen)
                })

my.fitv <- ddply(df.cast[df.cast$SensorID %in% c("2") &
                        df.cast$SYSTEM == "SYS1" &
                        !is.na(df.cast$Jw_Langensiepen), ],
                .(SYSTEM, SensorID),
                function(x) {
                values <- FitTypeVvalues(x = x$VPD_Avg, y = x$Jw_Langensiepen)
                my.df <- data.frame(VPD_Avg = x$VPD_Avg, 
                                    Jw_Langensiepen = values)
                return(my.df)
                })
#p <- ggplot(df.cast[df.cast$SensorID %in% c("2") &
#                        df.cast$SYSTEM == "SYS1" &
#                        !is.na(df.cast$Jw_Langensiepen), ],
#                        aes(x = VPD_Avg, y = Jw_Langensiepen))
#p <- p + geom_point(alpha = 0.1)
#p <- p + geom_line(data = my.fitv, colour = "blue")
#p


# output example timecourse
pdf(file = "Timecourse.pdf", width = 11, height = 8)
print(D + labs(title = "Temperature difference in sap dT °C") )
print(F + labs(title = "Estimated sap flow based on k = 0.46 [g hr-1], Langensiepen et al. 2014"))
print(A + labs(title = "PPFD alternative from SLD uncorrected"))
print(B + labs(title = "in-canopy relative humidity rH%"))
print(C + labs(title = "in-canopy temperature °C"))
print(E + labs(title = "in-canopy VPD kPa"))
dev.off()

# all PAR sensor with 10 ohm resistor
PAR10ohm <- 
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
