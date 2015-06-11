# analyse respiration for both measurement dates in 2014

source("~/AgFace/R_scripts/Night-time_respiration_Oct_2014.R")
source("~/AgFace/R_scripts/Night-time_respiration_Oct_22_2014.R")
source("~/AgFace/R_scripts/theme_white.R")

# add date information to the data frames
nightres.2014.10.02$Date <- "2014-10-02"
nightres.2014.10.22$Date <- "2014-10-22"

# combine both data frames
df <- rbind(nightres.2014.10.02, nightres.2014.10.22)
df$Date <- as.Date(df$Date)

# calculat respiration from Photo
df$Res <- df$Photo * (-1)

# load libraries
library(ggplot2)
library(nlme)
library(plyr)

p <- ggplot(df, aes(x = Cultivar, y = Res))
  p <- p + geom_boxplot(aes(fill = CO2_treatment))
  p <- p + facet_grid(Date ~ Treatment)
  p <- p + labs(y = expression("Respiration rate ("~mu*mol*CO[2]*m^-2*s^-1~")"))
  p <- p + theme_white()
p
ggsave("Respiration_October_2014.pdf",
       width = 9, height = 7)
# statistical tests

# heat plots in comparison to root plots
MyLme <- function(data) {
   my.lme <- lme(Res ~ Cultivar * CO2_treatment * Treatment,
              random = ~ 1 | Ring,
              data = data,
              na.action = na.omit)
   return(anova(my.lme))
}
dlply(df,
      .(Date),
      function(x) MyLme(x))
# "Treatment" has an effect on Res on Oct 2 only.

dlply(df[df$Treatment != "wet", ],
      .(Date),
      function(x) MyLme(x))

# when comparing heat vs root (inside vs outside the chambers)
# both dates indicate a Treatment effect

MyLmeNoCult <- function(data) {
   my.lme <- lme(Res ~ CO2_treatment * Treatment,
              random = ~ 1 | Ring,
              data = data,
              na.action = na.omit)
   return(anova(my.lme))
}

dlply(df,
      .(Date, Cultivar),
      function(x) MyLmeNoCult(x))
      
MyLmeNoTreatment <- function(data) {
   my.lme <- lme(Res ~ CO2_treatment,
              random = ~ 1 | Ring,
              data = data,
              na.action = na.omit)
   return(anova(my.lme))
}

dlply(df,
      .(Date, Cultivar, Treatment),
      function(x) MyLmeNoTreatment(x))

MyLmeCultonly <- function(data) {
   my.lme <- lme(Res ~ Cultivar,
              random = ~ 1 | Ring,
              data = data,
              na.action = na.omit)
   return(anova(my.lme))
}

dlply(df,
      .(Date, CO2_treatment, Treatment),
      function(x) MyLmeCultonly(x))
      
# Results
# Treatment affects Respiration on both dates
# Differences due to eCO2 rare, but Scout Oct 2 wet and Yitpi Oct 22 root show such a difference
# differences between traits rare but when found under eCO2 only.
