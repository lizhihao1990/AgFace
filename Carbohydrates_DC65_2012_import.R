# Carbohydrates DC65 2012

require(xlsx)
require(plyr)
require(ggplot2)

setwd("~/AgFace/Topics/Carbohydrates_DC65_2009_2010_2011_2012")

# run the AgFace helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# filenames of the DC31 carbohydrate files
the_file_a <- "~/AgFace/2012/Carbohydrates/Carbohydrates_2012-Silv.SB062.SSRT65.SB003.xls"


# function to import multiple sheets via xlsx
my.import <- function (the_file, sheets) {
        for (i in 1:sheets)
        {
          if (i == 1) {
            # during the first run
            my.data <- read.xlsx(file = the_file, sheetIndex = 1)
            my.data <- my.data[, c(1:11)]
            my.names <- names(my.data)
            print(head(my.data))
          } 
          if ( i > 1) {
            cur.file <- read.xlsx(file = the_file, sheetIndex = i)
            cur.file <- cur.file[, c(1:11)]
            names(cur.file) <- my.names
            print(head(cur.file))
            # using rbind to append the data
            my.data <- rbind(my.data, cur.file)                  
            }
        }
        return(my.data)
}

# import all sheets from the DC65 files
DC65_2012 <- my.import(the_file_a, 4)

# Get rid of the useless "Average" column
DC65_2012$Average <- NULL

# Some negative concentrations were excluded from the average in the data sheet.
# doing the same here
DC65_2012$Conc..mg.mg.[DC65_2012$Conc..mg.mg. < 0] <- NA

# rename the "Part" column
# gsub("Part", "Organ", names(DC65_2012))
names(DC65_2012)[which(names(DC65_2012) == "Part")] <- "Organ"

p <- ggplot(DC65_2012, aes(x = as.factor(Ring), y = Conc..mg.mg.))
  p <- p + geom_point(aes(shape = Cultivar, colour = Plot), 
                      position = position_dodge(width = 0.8))
  p <- p + facet_grid(Organ ~ Year, scale = "free_y")
  p <- p + labs(title = "Comparison of replicate analysis per ring.\nData of same shape should be close together irrespective of colour.")
  p <- p + theme_bw()
p

ggsave(file = "Carbohydrates_DC65_2012_replicates.pdf", width = 9, height = 7)

# calculate the mean properly without empty rows
DC65_2012b <- ddply(DC65_2012,
                    .(Cultivar, Year, Ring, Plot, Organ),
                    summarise,
                    Conc..mg.mg. = mean(Conc..mg.mg., na.rm = TRUE))

DC65_2012 <- DC65_2012b

# add CO2 information
DC65_2012$CO2 <- NA
DC65_2012$CO2[DC65_2012$Ring %in% ambient_rings_2012]  <- "aCO2"
DC65_2012$CO2[DC65_2012$Ring %in% elevated_rings_2012] <- "eCO2"
DC65_2012$CO2 <- as.factor(DC65_2012$CO2)
# add irrigation information
# have to find a way to vectorise the function
my.Irrigation <- ddply(DC65_2012,
                      .(Year, Ring, Plot),
                      function(x) 
                      .PlotIsIrrigated(x$Plot, x$Ring, x$Year))
my.Irrigation$Irrigation <- as.factor(my.Irrigation$Irrigation)

DC65_2012 <- merge(DC65_2012, my.Irrigation)

# add Stage information
DC65_2012$Stage <- as.factor("DC65")

# adding position of the plot within the ring (east/west)
DC65_2012$RingPos <- NA
DC65_2012$RingPos[DC65_2012$Plot %in% common_plot_names_east] <- "E"
DC65_2012$RingPos[DC65_2012$Plot %in% common_plot_names_west] <- "W"
DC65_2012$RingPos <- as.factor(DC65_2012$RingPos)

# create "Environments" - expecting four Environments
DC65_2012$Environment <- interaction(DC65_2012$Year, DC65_2012$Irrigation, drop = TRUE)

# create halfringID for nesting
DC65_2012$my.HalfringID <- interaction(DC65_2012$Year, DC65_2012$Ring, DC65_2012$RingPos)

# adjust Cultivar information to match the names in other data sets
# DC65_2012$Cultivar <- gsub("SB062", "SB062 high", DC65_2012$Cultivar)

used_rings <- sort(unique(interaction(DC65_2012$Ring, 
                                      DC65_2012$CO2)))
used_rings <- paste(used_rings, collapse = " ")
used_rings <- paste("Rings and their CO2 treatment: ", used_rings)

# graph
p <- ggplot(DC65_2012, aes(x = Irrigation, y = Conc..mg.mg.))
        p <- p + geom_boxplot(aes(fill = Cultivar, linetype = CO2))
        p <- p + labs(title = "DC65 2012, Carbohydrates, n = 2",
                      y     = "Carbohydrate concentration [mg/mg]")
        #p <- p + annotate("text", x = 0.5, y = 0.45, label = used_rings, hjust = 0)
        p <- p + facet_grid(Stage ~ Organ)
        p <- p + theme_bw()
p
ggsave(file = "2012_DC65_Carbohydrates.pdf", width = 7, height = 7)

save(DC65_2012, file = "DC65_Carbohydrates_2012.RData", compress = TRUE)

