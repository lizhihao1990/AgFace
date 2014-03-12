## Maryse Bourgault, January 2014

##Quick look at homogeneity (or lack therof) of variances by factor
boxplot(PeaData$yield.tha ~ PeaData$Cultivar)
boxplot(PeaData$yield.tha ~ PeaData$CO2.level)
boxplot(PeaData$yield.tha ~ PeaData$Irrigation)
boxplot(PeaData$yield.tha ~ PeaData$Year)
 
## No random effects; homogenous variance (LM to look at diagnostic plots)
yield.lm1<-lm(yield.tha ~ 1 + Year + CO2.level + Year:CO2.level +
                Irrigation + Irrigation:CO2.level + Irrigation:CO2.level:Year +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                data=PeaData, na.action="na.omit")
plot(yield.lm1)
 
## No random effects; homogenous variance (GLS – baseline to test variance structures)
yield.gls1<-gls(yield.tha ~ 1 + Year + CO2.level + Year:CO2.level +
                Irrigation + Irrigation:CO2.level + Irrigation:CO2.level:Year +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                method="REML", data=PeaData, na.action="na.omit")
plot(yield.gls1, select=c(1))
AIC(yield.gls1)
 
## Variance proportional to yield value
yield.gls2<-gls(yield.tha ~ 1 + Year + CO2.level + Year:CO2.level + Irrigation + Irrigation:CO2.level +
                Irrigation:Year +Irrigation:Year:CO2.level + Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                weights=varFixed(~yield.tha), method="REML", data=PeaData, na.action="na.omit")
plot(yield.gls2, select=c(1))
AIC(yield.gls2)
 
## Without Year
yield.gls3<-gls(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                weights=varFixed(~yield.tha), method="REML", data=PeaData, na.action="na.omit")
plot(yield.gls3, select=c(1))
AIC(yield.gls3)
 
## Year as random factor
yield.lme1<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="REML", data=PeaData, na.action="na.omit")
plot(yield.lme1, select=c(1))
anova(yield.gls3, yield.lme1)
 
## Year as random factor with lower level RingID grouping
yield.lme2<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                random = ~1|Year/RingID,
                weights=varFixed(~yield.tha), method="REML", data=PeaData, na.action="na.omit")
anova(yield.lme1, yield.lme2)
 
## Year as random factor with lower level RingID and HalfRing
yield.lme3<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                random = ~1|Year/RingID/HalfRingID,
                weights=varFixed(~yield.tha), method="REML", data=PeaData, na.action="na.omit")
anova(yield.lme1, yield.lme3)
anova(yield.lme2, yield.lme3)
 
## Testing fixed terms
## Looking at p values in best fit
summary(yield.lme1)
 
## Recalculate with ML (?)
yield.lme4<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation + Cultivar:Irrigation:CO2.level,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="ML", data=PeaData, na.action="na.omit")
summary(yield.lme4)
 
## Drop last interaction term
yield.lme5<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level +
                Cultivar + Cultivar:CO2.level + Cultivar:Irrigation,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="ML", data=PeaData, na.action="na.omit")
anova(yield.lme4, yield.lme5)
 
## Drop irrigation by cultivar
yield.lme6<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level + Cultivar + Cultivar:CO2.level,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="ML", data=PeaData, na.action="na.omit")
anova(yield.lme5, yield.lme6)
anova(yield.lme4, yield.lme6)
 
## Drop irrigation by CO2 level
yield.lme7<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Cultivar + Cultivar:CO2.level + Cultivar:Irrigation,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="ML", data=PeaData, na.action="na.omit")
anova(yield.lme5, yield.lme7)
anova(yield.lme4, yield.lme7)
 
## Drop Cultivar by CO2 level
yield.lme8<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Irrigation:CO2.level +
                Cultivar + Cultivar:Irrigation,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="ML", data=PeaData, na.action="na.omit")
anova(yield.lme4, yield.lme8)
summary(yield.lme8)
 
## Dropping all interactions
yield.lme9<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Cultivar,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="ML", data=PeaData, na.action="na.omit")
anova(yield.lme4, yield.lme9)
anova(yield.lme5, yield.lme9)
anova(yield.lme6, yield.lme9)
anova(yield.lme7, yield.lme9)
summary(yield.lme9)
 
## Refit with REML
yield.lmeFinal<-lme(yield.tha ~ 1 + CO2.level + Irrigation + Cultivar,
                random = ~1|Year,
                weights=varFixed(~yield.tha), method="REML", data=PeaData, na.action="na.omit", contrasts=)
summary(yield.lmeFinal)
