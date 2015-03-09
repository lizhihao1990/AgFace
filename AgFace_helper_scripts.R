# AgFACE helper R scripts

# my commonly used, handy functions for many AgFACE data analysis purposes 

# Author: Markus Löw, June 2013

# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE generic plot names and position in halfring
# Plots A - L are West
# PLots M - X are East
common_plot_names_west <- c(1:4, LETTERS[1:12])
common_plot_names_east <- c(5:8, LETTERS[13:24])
# +++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE CO2 treatments in 2009 based on ring numbers
ambient_rings_2009  <- c(1, 4, 7, 8, 11, 12, 14, 16)
elevated_rings_2009 <- c(2, 3, 5, 6, 9, 10, 13, 15)
# +++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE CO2 treatments in 2010 based on ring numbers
ambient_rings_2010  <- c(1, 8, 12, 14)
elevated_rings_2010 <- c(3, 5, 9, 13)
# +++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE CO2 treatments in 2011 based on ring numbers
ambient_rings_2011  <- c(4, 7, 11, 16)
elevated_rings_2011 <- c(2, 6, 10, 15)
# +++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE Irrigation treatments in 2011 based on ring and plot numbers
supplement_loc_2011 <- data.frame(Year = rep(2011, 8),
                                  Ring = c(ambient_rings_2011, elevated_rings_2011),
                                  supp = c("west", "east", "east", "east",
                                           "east", "east", "west", "east"))
# +++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE CO2 treatments in 2012 based on ring numbers
ambient_rings_2012  <- c(1, 8, 12, 14)
elevated_rings_2012 <- c(3, 5, 9, 13)
# +++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++
# PlotIDs 2012
# Plot Ids are ring specific!

 


# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE Irrigation treatments in 2012 based on ring and plot numbers
supplement_loc_2012 <- data.frame(Year = rep(2012, 8),
                                  Ring = c(ambient_rings_2012, elevated_rings_2012),
                                  supp = c("west", "east", "west", "west",
                                           "west", "west", "west", "west"))
# +++++++++++++++++++++++++++++++++++++++++++++

supplement_locations <- rbind(supplement_loc_2011, supplement_loc_2012)

# +++++++++++++++++++++++++++++++++++++++++++++
# AgFACE CO2 treatments in 2013 based on ring numbers
ambient_rings_2013  <- c(4, 7, 11, 16)
elevated_rings_2013 <- c(2, 6, 10, 15)
# +++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++
# AgFACE CO2 treatments in 2014 based on ring numbers
# with lentil rings
ambient_rings_2014  <- c(1, 3, 7, 8, 11, 12, 14, 15)
elevated_rings_2014 <- c(2, 4, 5, 6, 9, 10, 13, 16)
# +++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++
# PlotIDs and Cultivar 2014
# applies to wheat only
PlotIDs2014 <- c("W1", "W2", "R1", "R2", "N1", "N2", "Q1", "Q2")
Cultivars2014 <- c("Wet Scout", "Wet Yitpi", "Scout", "Yitpi", "Gladius", 
                    "Wyalkatchem", "RS411-1", "RS-411-5")
PlotIDsCultivars2014 <- data.frame(PlotID = PlotIDs2014, 
                                   Cultivar = Cultivars2014)
# +++++++++++++++++++++++++++++++++++++++++++++++++++


# +++++++++++++++++++++++++++++++++++++++++++++++++++
# PlotIDs and Cultivar 2009 to 2013
# applies to wheat only
PlotRingCult <- read.csv("~/AgFace/Samples/Sample_list_Sept_2014/Sample_list_plotIDs_2009_2013.csv")
PlotRingCult$sample. <- NULL
PlotRingCult$Stage <- NULL
PlotRingCult$Head <- NULL
PlotRingCult$leaf <- NULL
PlotRingCult$Stem <- NULL
PlotRingCult$Grain <- NULL
PlotRingCult$Comments <- NULL

names(PlotRingCult) <- gsub("Variety", "Cultivar", names(PlotRingCult))

PlotRingCult$TOS <- gsub(" ", "", PlotRingCult$TOS)
PlotRingCult$TOS <- as.factor(PlotRingCult$TOS)

PlotRingCult$RingID <- as.numeric(as.character(PlotRingCult$RingID))

PlotRingCult$CO2 <- gsub("No CO2", "aCO2", PlotRingCult$CO2)
PlotRingCult$CO2 <- gsub("^CO2", "eCO2", PlotRingCult$CO2)
PlotRingCult$CO2 <- as.factor(PlotRingCult$CO2)

PlotRingCult$Irrigation <- gsub("No irrig", "rainfed", PlotRingCult$Irrigation)
PlotRingCult$Irrigation <- gsub("Irrig", "supp", PlotRingCult$Irrigation)
PlotRingCult$Irrigation <- gsub("Rain$", "rainfed", PlotRingCult$Irrigation)
PlotRingCult$Irrigation <- gsub("Sup$", "supp", PlotRingCult$Irrigation)
PlotRingCult$Irrigation <- gsub("Suppl", "supp", PlotRingCult$Irrigation)
PlotRingCult$Irrigation <- as.factor(PlotRingCult$Irrigation)
PlotRingCult <- unique(PlotRingCult)

# +++++++++++++++++++++++++++++++++++++++++++++++++++


.PlotIsIrrigated <- function(PlotName, Ring, Year) {
              Year <- as.numeric(as.character(Year))
              if (PlotName %in% common_plot_names_east) {
                 Plotloc <- "east"
              } else {
                 Plotloc <- "west"
              }
              
              if (Plotloc == (supplement_locations$supp[
                              supplement_locations$Year == Year &
                              supplement_locations$Ring == Ring])) {
                 is.irrigated <- "Supp"}
              else {
                 is.irrigated <- "Rain"}
              names(is.irrigated) <- "Irrigation"
              return(is.irrigated)
}


# +++++++++++++++++++++++++++++++++++++++++++++
# capitalise the data frame entries
# elegant solution with example function from ?toupper
.simpleCap <- function(x) {
    s <- strsplit(as.character(x), " ")
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "")
}
# +++++++++++++++++++++++++++++++++++++++++++++


# +++++++++++++++++++++++++++++++++++++++++++++
# Create plots for multiple parameters from a melted dataframe
# i.e. loop over all parameters
# allows to specify the variablenames to be used as axis label, treatment colours, xaxis, yaxis, and two facet parameters
# includes a workaround for pasing strings from a function to facet_grid
# example usage:
#my_plots <- dlply(df.melt,
#                  .(variable),
#                  function(x) {
#                  MyMultPlot(x, x$variable, "Date", "value", "CO2_treatment", "Genotype", ".")})

# create plot function suitable for dlply
MyMultPlot <- function(dataframe, 
                         label, 
                         xaxis, 
                         yaxis, 
                         treatment_sep, 
                         facet_var_a,
                         facet_var_b) {

  require(ggplot2)
  
  axis_label <- unique(label)
 
  # workaround for specifying facets from strings in a function from Hadley Wickham:
  # http://r.789695.n4.nabble.com/ggplot2-proper-use-of-facet-grid-inside-a-function-td906018.html
  # facets <- facet_grid(paste(facet_var_a, "~ ."))
  #facets <- facet_grid(paste(facet_var_a, facet_var_b, sep = " ~ "))
  facets <- facet_grid(paste(facet_var_a, facet_var_b, sep = " ~ "), scales = "free_y")
  
  p <- ggplot(dataframe, aes_string(x = xaxis, y = yaxis))
       p <- p + stat_summary(aes_string(colour = treatment_sep),
                             fun.data = mean_sdl, mult = 1)
       p <- p + stat_summary(aes_string(colour = treatment_sep),
                             fun.data = mean_sdl, mult = 1, geom = "line")
       p <- p + ylab(axis_label)
       # p <- p + facet_wrap(facet_var ~ .)
       p <- p + facets
       # p <- p + facet_grid(scales = "free_y")
       p <- p + theme_bw()
       
  return(p)
  
}
# +++++++++++++++++++++++++++++++++++++++++++++

# multiplot for Plant Production data
# modified MyMultplot function
# added shape_sep to allow for symbols
MyMultPlotPlantProd <- function(dataframe, 
                         label, 
                         xaxis, 
                         yaxis, 
                         treatment_sep,
                         shape_sep, 
                         facet_var_a,
                         facet_var_b) {

  require(ggplot2)
  
  axis_label <- unique(label)
  missing_data <- length(dataframe$value[is.na(dataframe$value)])

  # workaround for specifying facets from strings in a function from Hadley Wickham:
  # http://r.789695.n4.nabble.com/ggplot2-proper-use-of-facet-grid-inside-a-function-td906018.html
  # facets <- facet_grid(paste(facet_var_a, "~ .")) 
  facets <- facet_grid(paste(facet_var_a, facet_var_b, sep = " ~ "))
  
  if (nrow(dataframe) != missing_data ) {
  p <- ggplot(dataframe, aes_string(x = xaxis, y = yaxis))
       p <- p + stat_summary(aes_string(colour = treatment_sep,
                                        shape  = shape_sep),
                             fun.data = mean_sdl, mult = 1)
       p <- p + stat_summary(aes_string(colour = treatment_sep,
                                        shape  = shape_sep),
                             fun.data = mean_sdl, mult = 1, geom = "line")
       p <- p + ylab(axis_label)
       # p <- p + facet_wrap(facet_var ~ .)
       p <- p + facets
       p <- p + theme_bw()
       p <- p + theme(axis.text.x = element_text(size = rel(0.8), 
                                                angle = 90, 
                                                vjust = 0.5))
       
  return(p) } else {
  # create a dummy plot for parameters with 0 samples
  p <- ggplot()
        p <- p + geom_text(data = NULL, aes(x = 1, y = 1), label = "missing data")
        p <- p + ylab(axis_label)
        p <- p + theme_bw()
  return(p)
  }
}
# +++++++++++++++++++++++++++++++++++++++++++++ 
