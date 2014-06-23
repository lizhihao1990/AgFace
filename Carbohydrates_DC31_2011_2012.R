# Import carbohydrate data for 2011 and 2012

# load libraries
require(xlsx)
require(ggplot2)
require(plyr)
require(nlme)

setwd("~/AgFace/Topics/Carbohydrates_DC31_2011_2012")

# run the AgFace helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# filenames of the DC31 carbohydrate files
the_file_a <- "~/AgFace/2011/Carbohydrates/Carbohydrates_2011_DC31.xlsx"
the_file_b <- "~/AgFace/2012/Carbohydrates/Carbohydrates_2012_DC31.xlsx"

# function to import multiple sheets via xlsx
my.import <- function (the_file, sheets) {
        for (i in 1:sheets)
        {
          if (i == 1) {
            # during the first run
            my.data  <- read.xlsx(file = the_file, sheetIndex = 1)
            my.names <- names(my.data)
            print(head(my.data))
          } 
          if ( i > 1) {
            cur.file <- read.xlsx(file = the_file, sheetIndex = i)
            names(cur.file) <- my.names
            print(head(cur.file))
            # using rbind to append the data
            my.data <- rbind(my.data, cur.file)                  
            }
        }
        return(my.data)
}

# import all sheets from the DC31 files
DC31_2011 <- my.import(the_file_a, 4)
DC31_2012 <- my.import(the_file_b, 4)

# add CO2 information
DC31_2011$CO2 <- NA
DC31_2011$CO2[DC31_2011$Ring %in% ambient_rings_2011] <- "aCO2"
DC31_2011$CO2[DC31_2011$Ring %in% elevated_rings_2011] <- "eCO2"

DC31_2012$CO2 <- NA
DC31_2012$CO2[DC31_2012$Ring %in% ambient_rings_2012] <- "aCO2"
DC31_2012$CO2[DC31_2012$Ring %in% elevated_rings_2012] <- "eCO2"

# identified typos in the data files provided by Michael (french interns)
# Ring 3, Plot F, 2012, Sb062 should be plot P!
DC31_2012$Plot[DC31_2012$Plot == "F" &
               DC31_2012$Ring == 3 &
               DC31_2012$Year == 2012] <- as.factor("P")

# combine 2011 and 2012 DC31 data
DC31_both <- rbind(DC31_2011, DC31_2012)
DC31_both$Year <- as.factor(DC31_both$Year)
DC31_both$CO2 <- as.factor(DC31_both$CO2)

# adding Stage information - to match exisiting harvest data, assigning DC30 here
DC31_both$Stage <- as.factor("DC30")

# adding Organ information - to match other data sets
DC31_both$Organ <- as.factor("Stem")

# adding position of the plot within the ring (east/west)
DC31_both$RingPos <- NA
DC31_both$RingPos[DC31_both$Plot %in% common_plot_names_east] <- "E"
DC31_both$RingPos[DC31_both$Plot %in% common_plot_names_west] <- "W"
DC31_both$RingPos <- as.factor(DC31_both$RingPos)

# rename some cultivars to match the existing terminology
# correct the cultivar information:
# email from Michael, 09/12/13 15:25:37

#That's right: "low" should be the same as SB003, and "high" same as SB062 (or SB063 - I think at one stage we were also unclear which one it was). There should not be a "SB062 low" (only "SB low" would be correct), but that's just a misnomer.

#DC31_both$Cultivar <- gsub("SB062", "SB062 high", DC31_both$Cultivar)
#DC31_both$Cultivar <- gsub("SB003", "SB062 low",  DC31_both$Cultivar)
#DC31_both$Cultivar <- as.factor(DC31_both$Cultivar)

# have to find a way to vectorise the function
my.Irrigation <- ddply(DC31_both,
                      .(Year, Ring, Plot),
                      function(x) 
                      .PlotIsIrrigated(x$Plot, x$Ring, x$Year))
my.Irrigation$Irrigation <- as.factor(my.Irrigation$Irrigation)

#DC31_both$Irrigation <- unlist(my.Irrigation)
#DC31_both$Irrigation <- as.factor(DC31_both$Irrigation)

DC31_both <- merge(DC31_both, my.Irrigation)

# create "Environments" - expecting four Environments
DC31_both$Environment <- interaction(DC31_both$Year, DC31_both$Irrigation, drop = TRUE)

# create halfringID for nesting
DC31_both$my.HalfringID <- interaction(DC31_both$Year, DC31_both$Ring, DC31_both$RingPos)


# some overview graph
p <- ggplot(DC31_both, aes(x = Environment, y = Conc..mg.mg.))
  p <- p + geom_boxplot(aes(fill = Cultivar, linetype = CO2))
  p <- p + facet_grid(Stage ~ .)
  p <- p + theme_bw()
  p <- p + labs(title = "At stage DC 31, Stem",
                y = "Carbohydrates [mg/mg]")
p
ggsave(file = "Carbohydrates_2011_2012_DC31_stems.pdf", width = 7, height = 7)

# stats
lme.out <- anova(lme(Conc..mg.mg. ~ CO2 * Cultivar * Environment,
                      random = ~ 1 | my.HalfringID/Cultivar,
                      data = DC31_both,
                      na.action = na.omit))
lme.out

to_keep <- c("Cultivar", "CO2", "Year", "Ring", "RingPos", "my.HalfringID", "Plot", "Environment", "Stage", "Irrigation", "Organ", "Conc..mg.mg.")
names_to_keep <- names(DC31_both) %in% to_keep
CarbohydratesDC31 <- DC31_both[, names_to_keep]
# save the object for later use
save(CarbohydratesDC31, file = "CarbohydratesDC31_2011_2012.RData", compress = TRUE)

