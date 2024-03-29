# import Carbohydrates 2009 - 2011 file

require(plyr)
require(ggplot2)

setwd("~/AgFace/Topics/Carbohydrates_DC65_2009_2010_2011_2012")

source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

df <- read.csv("../../2009/Water_soluble_carbs/WSC_raw_2009_2010_2011.csv")

# reverting the factor-conversion for sample.ID
df$sample.ID <- as.character(df$sample.ID)

# correct some typos and inconsistencies
df$sample.ID <- gsub("DC 65", "DC65", df$sample.ID)
df$sample.ID <- gsub("Silvestar", "Silverstar", df$sample.ID)
df$sample.ID <- gsub(" - DC65", " DC65", df$sample.ID)
df$sample.ID <- gsub("Stems", "Stem", df$sample.ID)
df$sample.ID <- gsub("SSRT 65", "SSR T65", df$sample.ID)
df$plot      <- gsub("P ", "P", df$plot)

# confirmed with Michael, unit is mg/mg
# changing names and values accordingly
names(df) <- gsub("carbohydrate.concentration", "Conc..mg.mg.", names(df))
df$Conc..mg.mg. <- df$Conc..mg.mg. / 1000

names(df) <- gsub("ring", "Ring", names(df))
names(df) <- gsub("plot", "Plot", names(df))

# get rid of samples without ring information
df <- df[!is.na(df$Ring), ]

# have to split the sample.ID into Cultivar, Organ, Stage, and year
df$Cultivar   <- sapply(strsplit(df$sample.ID, " -"), "[", 1)
df$OrganStage <- sapply(strsplit(df$sample.ID, " -"), "[", 2)
df$Organ      <- sapply(strsplit(df$OrganStage, " "), "[", 2)
df$Stage      <- sapply(strsplit(df$OrganStage, " "), "[", 3)
df$OrganStage <- NULL
df$Year       <- sapply(strsplit(df$sample.ID, " -"), "[", 3)
df$Year       <- gsub(" ", "", df$Year)


# re-code some columns
df$Cultivar <- as.factor(df$Cultivar)
df$Organ    <- as.factor(df$Organ)
df$Stage    <- as.factor(df$Stage)
df$Year     <- as.factor(df$Year)
df$Plot     <- as.factor(df$Plot)

# add CO2 information
# doing it individually per year
df$CO2 <- NA
df$CO2[df$Year == "2009" & df$Ring %in% ambient_rings_2009]  <- "aCO2"
df$CO2[df$Year == "2009" & df$Ring %in% elevated_rings_2009] <- "eCO2"
df$CO2[df$Year == "2010" & df$Ring %in% ambient_rings_2010]  <- "aCO2"
df$CO2[df$Year == "2010" & df$Ring %in% elevated_rings_2010] <- "eCO2"
df$CO2[df$Year == "2011" & df$Ring %in% ambient_rings_2011]  <- "aCO2"
df$CO2[df$Year == "2011" & df$Ring %in% elevated_rings_2011] <- "eCO2"
df$CO2 <- as.factor(df$CO2)

# correct the cultivar information:
# email from Michael, 09/12/13 15:25:37

#That's right: "low" should be the same as SB003, and "high" same as SB062 (or SB063 - I think at one stage we were also unclear which one it was). There should not be a "SB062 low" (only "SB low" would be correct), but that's just a misnomer.

df$Cultivar <- gsub("SB003 low",  "SB003", df$Cultivar)
df$Cultivar <- gsub("SB062 HIGH", "SB062", df$Cultivar)
# List of tillering-related cultivars
tin_cultivars <- c("SB003", "SB062", "Silverstar", "SSR T65")

# Ring number CO2-treatment mismatch for Yitpi in 2010
# well, for now we only need 2011 samples
Carb.DC65.2011.tin <- df[df$Year == "2011" &
                         df$Cultivar %in% tin_cultivars, ]

Carb.DC65.2011.tin$Year <- as.numeric(as.character(Carb.DC65.2011.tin$Year))

# add Irrigation treatment
# have to find a way to vectorise the function
my.Irrigation <- ddply(Carb.DC65.2011.tin,
                      .(Year, Ring, Plot),
                      function(x) 
                      .PlotIsIrrigated(x$Plot, x$Ring, x$Year))
my.Irrigation$Irrigation <- as.factor(my.Irrigation$Irrigation)

Carb.DC65.2011.tin <- merge(Carb.DC65.2011.tin, my.Irrigation)

# adding position of the plot within the ring (east/west)
Carb.DC65.2011.tin$RingPos <- NA
Carb.DC65.2011.tin$RingPos[Carb.DC65.2011.tin$Plot %in% common_plot_names_east] <- "E"
Carb.DC65.2011.tin$RingPos[Carb.DC65.2011.tin$Plot %in% common_plot_names_west] <- "W"
Carb.DC65.2011.tin$RingPos <- as.factor(Carb.DC65.2011.tin$RingPos)

# create "Environments" - expecting four Environments
Carb.DC65.2011.tin$Environment <- interaction(Carb.DC65.2011.tin$Year, 
                                              Carb.DC65.2011.tin$Irrigation,
                                              drop = TRUE)

# create halfringID for nesting
Carb.DC65.2011.tin$my.HalfringID <- interaction(Carb.DC65.2011.tin$Year,
                                                Carb.DC65.2011.tin$Ring,
                                                Carb.DC65.2011.tin$RingPos)


p <- ggplot(Carb.DC65.2011.tin, aes(x = Irrigation, y = Conc..mg.mg.))
        p <- p + geom_boxplot(aes(linetype = CO2, fill = Cultivar))
        p <- p + facet_grid(Stage ~ Organ)
        p <- p + labs(title = "DC65 2011, Carbohydrates, n = 4",
                          y = "Carbohydrate concentration [mg / mg]")
        p <- p + theme_bw()
p
ggsave(file = "2011_DC65_Carbohydrates.pdf", width = 7, height = 7)

write.table(Carb.DC65.2011.tin,
            file = "Carbohydrates_DC65_2011.csv", sep = ",", row.names = FALSE)
save(Carb.DC65.2011.tin, file = "DC65_Carbohydrates_2011.RData", compress = TRUE)
