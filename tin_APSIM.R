# analyse APSIm files

library(APSIM) # to import APSIM result files

setwd("~/AgFace/2011_2012/Apsim_soil_moisture/RUE_at_0.94/Daily")

# load sample information from Silverstar/tin SB workspace
load("~/AgFace/Plant_Production/Silverstar_tin/Tin_SB_treatments.RData")

df <- loadApsim(".", fill = TRUE)

# organise treatments and plot IDs
# extracting from Title

out <- sapply(strsplit(df$Title, " "), '[', 2)
df$Year <- sapply(strsplit(out, "_"), '[', 1)
df$Year <- as.factor(df$Year)

df$RingPlot <- sapply(strsplit(out, "_"), '[', 2)

# in front of each letter, add an "_"
# 9U becomes 9_U
df$RingPlot <- gsub("([[:alpha:]])", "_\\1", df$RingPlot)

# extract Ring and Plot Information
df$RingID <- sapply(strsplit(df$RingPlot, "_"), '[', 1)
df$RingID <- as.factor(df$RingID)
df$PlotID <- sapply(strsplit(df$RingPlot, "_"), '[', 2)
df$PlotID <- as.factor(df$PlotID)

# get treatment information
tin <- tin.SB.treat[tin.SB.treat$my.Trait == "Silverstar", ]
tin$Stage <- NULL
tin$Sample.date <- NULL
tin$Sowing.date <- NULL
tin$RingTrt <- NULL

# merge treatment
aps <- merge(tin, df)
aps <- unique(aps)

aps$Date <- as.Date(aps$Date, 
                    format = "%d/%m/%Y", 
                    tz = "Australia/Melbourne")
# order
aps <- aps[with(aps, order(Date, RingID, PlotID)), ]

# get rid of "()" in the names of the data frame
names(aps) <- gsub("\\(", "_", names(aps))
names(aps) <- gsub("\\)", "", names(aps))

# in the physical world, soil moisture was sampled between 0 and 120 cm depth
# in apsim, soil water content is modelled for 10 layers 0 - 180 cm
# only using the layers 1 to 7 

aps$swc.apsim <- aps$sw_1 + aps$sw_2 + aps$sw_3 + aps$sw_4 + aps$sw_5 + aps$sw_6 + aps$sw_7

# write.table(aps, file = "out.csv", row.names = F, sep = ",")

library(ggplot2)
p <- ggplot(aps, aes(x = Date, y = Biomass))
  p <- p + geom_line(aes(colour = RingID))
  p <- p + facet_grid(Cultivar ~ CO2)
p

p <- ggplot(aps, aes(x = Date, y = swc.apsim))
  p <- p + geom_point(aes(colour = RingID))
  p <- p + facet_grid(Cultivar ~ CO2)
p

p <- ggplot(aps, aes(x = Date, y = esw))
  p <- p + geom_point(aes(colour = RingID))
  p <- p + facet_grid(Cultivar ~ CO2)
p

# import principal dates
stage.dates <- read.csv("~/AgFace/2011_2012/Principal_dates/Principal_dates_2011_2012.csv")
stage.dates$Date <- as.Date(stage.dates$Date)
stage.dates <- stage.dates[stage.dates$Cultivar != "SB 062", ]
stage.dates$Cultivar[stage.dates$Date == as.Date("2012-08-15")] <- "Silverstar"
stage.dates <- stage.dates[stage.dates$Date != as.Date("2012-10-18"), ]
stage.dates$Cultivar[stage.dates$Cultivar == "all"] <- NA
stage.dates$Date[stage.dates$Date == as.Date("2012-05-30")] <- as.Date("2012-05-31")
#stage.dates$Cultivar[stage.dates$Cultivar == "remaining"] <- NA


stage.dates.Silver <- stage.dates[is.na(stage.dates$Cultivar), ]
stage.dates.SSRT65 <- stage.dates[is.na(stage.dates$Cultivar), ]

stage.dates.Silver$Cultivar <- "Silverstar"
stage.dates.SSRT65$Cultivar <- "SSR T65"

stage.dates.Cult <- stage.dates[!is.na(stage.dates$Cultivar), ]

stage.dates <- rbind(stage.dates.Silver, stage.dates.SSRT65, stage.dates.Cult)
stage.dates$Cultivar <- as.factor(stage.dates$Cultivar)
aps.principal.dates <- merge(aps, stage.dates)

aps.principal.dates[aps.principal.dates$RingID == "1" & aps.principal.dates$PlotID == "C", ]

save(aps.principal.dates, 
     file = "Apsim_soil_water_biomass_principal_dates_2011_2012.RData",
     compress = TRUE)
write.table(aps.principal.dates, file = "APSIM_principal_dates.csv",
            sep = ",", row.names = F)
            
#summary
p <- ggplot(aps.principal.dates, aes(x = esw, y = swc.apsim))
  p <- p + geom_point()
p

library(plyr)
aps.summary <- ddply(aps.principal.dates,
                    .(Year, Cultivar, Irrigation, Event),
                    summarise,
                    mean.esw = mean(esw, na.rm = TRUE),
                    sd.esw = sd(esw))

write.csv(aps.summary, file = "mean_esw.csv", row.names = F)
