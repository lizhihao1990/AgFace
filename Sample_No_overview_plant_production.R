# sample number overview

load("~/AgFace/2007_2013/Agface_Plant_production_2007_to_2013.RData")
dim(Agface)
Scout_Yitpi <- Agface[Agface$Cultivar == "Scout" |
                      Agface$Cultivar == "Yitpi", ]

dim(Scout_Yitpi) # 912 samples

# costs per sample: $55 includion delta15N
# costs per sample: $30 for %N and No3-

dim(Scout_Yitpi)[1] * 55
dim(Scout_Yitpi)[1] * 30

with(Agface, table(Cultivar, Year))
with(Agface, table(Cultivar, Year, TrialID))

Scout_Yitpi$Environment <- with(Scout_Yitpi, 
                                interaction(Year, TrialID, TOS, CO2, Irrigation, Ntreat, 
                                            drop = TRUE))
# How many unique environments
length(sort(unique(Scout_Yitpi$Environment)))

with(Scout_Yitpi, table(Environment, Cultivar))
with(Scout_Yitpi, table(Year, Cultivar))

