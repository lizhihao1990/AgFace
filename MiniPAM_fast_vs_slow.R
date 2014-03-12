# comparison fast - slow light response

setwd("~/AgFace/2013/MiniPAM/2013-11-06_slow_fast_light_response")

# call loading script
source("~/AgFace/R_scripts/MiniPAM_minicom_import.R")

library(ggplot2)
library(plyr)
library(reshape)

# import data
fastslow <- LoadMiniPAM("minicom.cap")

fastslow$Mark <- NextLabel(fastslow$Mark, "l")
fastslow$Mark <- as.factor(fastslow$Mark)

fastslow$Type <- NA
fastslow$Type[fastslow$Mark == "A"] <- "fast response"
fastslow$Type[fastslow$Mark == "B"] <- "slow response"

nls.fit <- ddply(fastslow,
                 .(Type),
           function(x) {
           nls.fit <- nls(ETR ~ (a * PAR) / (b + PAR),
                       data = x,
                       start = c(a = 10, b = 2),
                       control = nls.control(maxiter = 700,
                                    warnOnly = FALSE,
                                    minFactor = 6.10352e-09))
            
            my.a <- coef(nls.fit)[1]
            my.b <- coef(nls.fit)[2]
            my.output <- data.frame(a = my.a, b = my.b)   
            return(my.output)
})

# upper asymptote "a"
# half-maximum reached at "b"

x <- seq(from = 0, to = 1000, by = 1)
y <- (45 * x) / (175 + x)

df <- data.frame(x = x, y = y)
plot(df)


p <- ggplot(fastslow, aes(x = PAR, y = ETR))
     p <- p + geom_smooth(aes(colour = Type), se = F,
                 method  = "nls", 
                 formula = y ~ (a * x) / (b + x),
                 start   = c(a = 10, b = 2),
                 control = nls.control(maxiter = 700,
                             warnOnly = FALSE,
                             minFactor = 6.10352e-09))
     p <- p + geom_point(aes(colour = Type))
     p <- p + geom_text(x = 500, y = 20, label = print(nls.fit),
                        size = rel(5))
     p <- p + geom_vline(aes(xintercept = b, colour = Type), 
                         linetype = "dashed", data = nls.fit)
     p <- p + geom_hline(aes(yintercept = a, colour = Type), 
                         linetype = "dotted", data = nls.fit)
     p <- p + labs(x = expression(PPFD~"["~mu*mol~m^-2*s^-1~"]"),
                   y = expression("ETR ["~mu*mol~m^-1~s^-2~"]"))
     p <- p + theme_bw()
     p <- p + theme(legend.position = c(0.8, 0.2),
                    legend.key = element_blank(),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank())
p
ggsave(file = "Slow_vs_fast_light_response.pdf", width = 7, height = 7)

fast.ETR <- fastslow$ETR[fastslow$Mark == "A"]
slow.ETR <- fastslow$ETR[fastslow$Mark == "B"]

my.ETR <- data.frame(fast_ETR = fast.ETR, slow_ETR = slow.ETR)

my.lm <- lm(slow_ETR ~ fast_ETR, data = my.ETR)
summary(my.lm)
my.adj.rsquared <- format(summary(my.lm)$adj.r.squared, digits = 3)
my.text <- paste("R2 = ", my.adj.rsquared)

p <- ggplot(my.ETR, aes(x = fast_ETR, y = slow_ETR))
        p <- p + geom_smooth(method = "lm")
        p <- p + geom_point()
        p <- p + annotate(x = 8, y = 30, label = my.text, geom = "text")
        p <- p + labs(x = "Fast ETR light response",
                      y = "Slow ETR light response")
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(0.8, 0.2),
                       legend.key = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank())
p

ggsave(file = "Slow_vs_fast_regression.pdf", width = 7, height = 7)

# import data from glasshouse test
fastslow2 <- LoadMiniPAM("../2013-11-14_slow_fast_light_response_glasshouse/minicom.cap")

fastslow2$Mark <- NextLabel(fastslow2$Mark, "l")

# one measurement without double Fv/Fm
fastslow2 <- fastslow2[!(fastslow2$Mark == "D" & fastslow2$Yield == 0.653), ]
fastslow2$Mark <- NextLabel(fastslow2$Mark, "l")
fastslow2$Mark <- as.factor(fastslow2$Mark)


fastslow2$Type <- NA
fastslow2$Type[fastslow2$Mark == "A" | fastslow2$Mark == "D"] <- "fast response"
fastslow2$Type[fastslow2$Mark == "B" | fastslow2$Mark == "C"] <- "slow response"

fastslow2$Location <- "Creswick"
fastslow$Location  <- "Horsham"

fastslow <- rbind(fastslow, fastslow2)
fastslow$Location <- as.factor(fastslow$Location)
fastslow$Type     <- as.factor(fastslow$Type)

# check Fv/Fm
p <- ggplot(fastslow[fastslow$PAR < 2, ], aes(x = Type, y = Yield))
  p <- p + geom_point(aes(colour = Location))
p

nls.fit <- ddply(fastslow,
                 .(Type, Location),
           function(x) {
           nls.fit <- try(nls(ETR ~ (a * PAR) / (b + PAR),
                       data = x,
                       start = c(a = 10, b = 2),
                       control = nls.control(maxiter = 700,
                                    warnOnly = FALSE,
                                    minFactor = 6.10352e-09)))
            if (inherits(nls.fit, "try-error")) {
                
                 my.a <- NA
                 my.b <- NA
            }
            else {
            my.a <- coef(nls.fit)[1]
            my.b <- coef(nls.fit)[2]}
            my.output <- data.frame(a = my.a, b = my.b)   
            return(my.output)
})


p <- ggplot(fastslow, aes(x = PAR, y = ETR))
     p <- p + geom_smooth(aes(colour = Type), se = F,
                 method  = "nls", 
                 formula = y ~ (a * x) / (b + x),
                 start   = c(a = 10, b = 2),
                 control = nls.control(maxiter = 700,
                             warnOnly = FALSE,
                             minFactor = 6.10352e-09))
     p <- p + geom_point(aes(colour = Type))
     p <- p + geom_text(x = 500, y = 20, label = print(nls.fit),
                        size = rel(5))
     p <- p + geom_vline(aes(xintercept = b, colour = Type), 
                         linetype = "dashed", data = nls.fit)
     p <- p + geom_hline(aes(yintercept = a, colour = Type), 
                         linetype = "dotted", data = nls.fit)
     p <- p + labs(x = expression(PPFD~"["~mu*mol~m^-2*s^-1~"]"),
                   y = expression("ETR ["~mu*mol~m^-1~s^-2~"]"))
     p <- p + facet_grid(. ~ Location)
     p <- p + theme_bw()
     p <- p + theme(legend.position = c(0.8, 0.2),
                    legend.key = element_blank(),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank())
p
