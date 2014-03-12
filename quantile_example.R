# Quantile regression example

library(quantreg)

# create some data with a peak in the middle
df <- data.frame(x = rnorm(100, mean = 50),
                 y = rnorm(100, mean = 35, sd = 3))

plot(y ~ x, data = df)

# modify the data, so that we have a peak in the middle
my.length <- length(df$y[df$x > 49 & df$x < 51])
df$y[df$x > 49 & df$x < 51] <- df$y[df$x > 49 & df$x < 51] * rnorm(my.length, mean = 1.5, sd = 0.2)
plot(y ~ x, data = df)



# quadratic Y = a + bX + cX2
# a = 6, b = 0.2, c = 50.37
nls.fit <- nls(y ~ a + b * x + c*x^2,
           data = df,
           #weights = weights,
           start = list(a = -2, b = 1, c = 48),
           control = nls.control(maxiter = 700,
                                 warnOnly = TRUE,
                                 minFactor = 6.10352e-09))

predict_range <- data.frame(x = seq(min(df$x), max(df$x), length = 100))

# calculate for each x-range value the corresponding y-range
my.mean.fit <- within(predict_range, y <- predict(nls.fit, newdata = predict_range))

lines(y ~ x, data = my.mean.fit, col = "blue", lwd = 2)

# using quantreg
my.model <- nlrq(y ~ a + b * x + c * x^2,
            data  = df,
            start = list(a = -2, b = 1, c = 48),
            tau   = .95)

my.upper95.fit <- within(predict_range, y <- predict(my.model, newdata = predict_range))

lines(y ~ x, data = my.upper95.fit, col = "red", lwd = 2)

# hyperbola
# example from light response
# Pre-dawn measurement analysis Oct 17, 2013
# rapid light response
setwd("~/AgFace/2013/MiniPAM/2013-10-17_predawn")
# load helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")
# call loading script
source("~/AgFace/R_scripts/MiniPAM_minicom_import.R")

# import data
df <- LoadMiniPAM("minicom.cap")
# get rid of failed measurements with no leaf
df <- df[!df$F == 0 & !df$Fm. == 0, ]
# Meta data
# from lab notebook:
# wrong settings for sample 65
# getting rid of it
df <- df[-65, ]

# create a plot
plot(ETR ~ PAR, data = df)

# fit hyperbola
nls.fit <- nls(ETR ~ (a * PAR) / (b + PAR),
           data = df,
           start = list(a = 10, b = 2))

my.range <- data.frame(PAR = seq(0, 600, length = 200))

# calculate for each x-range value the corresponding y-range
my.range$ETR <- predict(nls.fit, newdata = my.range)

lines(ETR ~ PAR, data = my.range, col = "blue", lwd = 2)

# using quantreg
my.model <- nlrq(ETR ~ (a * PAR) / (b + PAR),
            data  = df,
            weights = my_weights,
            start = list(a = 10, b = 2),
            tau   = .95)

my.range <- data.frame(PAR = seq(0, 600, length = 200))
my.upper95.fit <- within(my.range, ETR <- predict(my.model, newdata = my.range))

lines(ETR ~ PAR, data = my.upper95.fit, col = "red", lwd = 2)

p <- ggplot(df, aes(x = PAR, y = ETR))
        p <- p + geom_point()
        p <- p + geom_line(data = my.upper95.fit, colour = "blue")
        p <- p + theme_bw()
        p <- p + labs(y = expression("ETR ["~mu*mol~m^-1~s^-2~"]"),
                      x = expression("PPFD ["~mu*mol~m^-1~s^-2~"]"))
p


