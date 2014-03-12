# Analysis of indoor spectra absorptivity / transmission test

library(ggplot2)
library(reshape)

setwd("~/AgFace/2013/Spectra/2013-11-26_lab")

# load spectras
source("~/AgFace/R_scripts/Spectra_import.R")

spectra <- my.data


# import file description
desc <- read.csv("file_description.csv", skip = 2)

# merge the data frames
spectra$Name <- gsub(".asd", "", spectra$Name)

spectra <- merge(spectra, desc, 
                 by.x = "Name", 
                 by.y = "File")

sworkflow <- c("TA00001", "TA00002", "TA00003", "TA00004")

spectra$Workflow <- NA
spectra$Workflow[spectra$Name %in% sworkflow] <- "Standard"
spectra$Workflow[!spectra$Name %in% sworkflow] <- "Opposite"

spectra$Mode <- NA
spectra$Mode[grep("Trans", as.character(spectra$Description))] <- "Transmission"
spectra$Mode[grep("Refl", as.character(spectra$Description))]  <- "Reflectance"
spectra$Mode[grep("White", as.character(spectra$Description))] <- "White reference"
spectra$Mode[grep("Black", as.character(spectra$Description))] <- "Black reference"

spectra[, c("Name", "Workflow", "Mode")] <- lapply(spectra[, c("Name", "Workflow", "Mode")], as.factor)

# Standard lead clip workflow
# files 3 and 4

p <- ggplot(spectra,
            aes(x = Wavelength, y = Signal))
  p <- p + geom_line(aes(colour = Mode))
  p <- p + facet_grid(Workflow ~ .)
  p <- p + theme_bw()
p
raw_spectra <- p 

# Correlation plot
trans <- spectra[spectra$Mode == "Transmission", ]

trans.melt <- melt(trans,
                   id = c("Name", "Description", "Workflow", "Mode", "Wavelength"))
                   
trans.mean <- cast(trans.melt,
                   Wavelength ~ Workflow,
                   fun = mean)

#trans.mean <- aggregate(trans, 
#                        by = list(wavelength = trans$Wavelength,
#                                  workflow = trans$Workflow, 
#                                  mode = trans$Mode), 
#                        FUN = mean)



#trans.mean.plot <- merge(trans.mean[trans.mean$workflow == "Standard", ],
#                         trans.mean[trans.mean$workflow == "Opposite", ]

#plot(trans.mean$Signal[trans.mean$workflow == "Opposite"] ~ 
#     trans.mean$Signal[trans.mean$workflow == "Standard"])

p <- ggplot(trans.mean, aes(x = Standard, y = Opposite))
  p <- p + geom_point()
  p <- p + theme_bw()
  p <- p + labs(x = "Standard transmission measurement",
                y = "Opposite transmission measurement")
p
correlation_plot <- p

# Absorptivity
#  average the multiple opposite transmission measurements merge back with the standard data
av.opp <- spectra[spectra$Workflow == "Opposite",]
av.opp$Name <- NULL
av.opp.melt <- melt(av.opp,
                    id = c("Description", "Workflow", "Mode", "Wavelength"))
av.opp.mean <- cast(av.opp.melt, fun = mean)
av.opp.mean$Name <- NA

# merge standard data with averages opposite data
x <- rbind(spectra[spectra$Workflow == "Standard", ], 
           av.opp.mean[, c("Name", "Wavelength", "Signal", "Description", "Workflow", "Mode")])


# get rid of the Modes that are not needed any more
x <- x[x$Mode == "Reflectance" | x$Mode == "Transmission", ]


x.melt <- melt(x,
          id = c("Name", "Wavelength", "Description", "Workflow", "Mode"))

x.cast <- cast(x.melt,
               Wavelength ~ Workflow + Mode)

x.cast$Abs_standard <- x.cast$Standard_Reflectance - x.cast$Standard_Transmission
x.cast$Abs_opposite <- x.cast$Standard_Reflectance - x.cast$Opposite_Transmission


p <- ggplot(x.cast, aes(x = Wavelength, y = Abs_standard))
        p <- p + geom_line()
        p <- p + geom_line(aes(y = Abs_opposite), colour = "red")
        p <- p + labs(y = "Absorptivity  (Red = opposite, black = standard)")
        p <- p +theme_bw()
p
full_range <- p

p <- ggplot(x.cast, aes(x = Wavelength, y = Abs_standard))
        p <- p + geom_line()
        p <- p + geom_line(aes(y = Abs_opposite), colour = "red")
        p <- p + labs(y = "Absorptivity  (Red = opposite, black = standard)")
        p <- p + coord_cartesian(xlim = c(350, 750), ylim = c(750, -750))
        p <- p + theme_bw()
p
limited_range <- p

p <- ggplot(x.cast, aes(x = Wavelength, y = Abs_standard))
        p <- p + geom_line()
        p <- p + geom_line(aes(y = Abs_opposite), colour = "red")
        p <- p + labs(y = "Absorptivity  (Red = opposite, black = standard)")
        p <- p + coord_cartesian(xlim = c(350, 750), ylim = c(50, -50))
        p <- p + theme_bw()
p
very_limited_range <- p

# Equation from Glenn Wed, 27 Nov 2013 11:47:18

#Thinking of the pathway of light through the leaf I come up with the following: 

#T = Transmission once through leaf 
#R = Reflectance (measured with black background) 
#A= Absorptance from one pass through leaf 
#T' = Transmission second pass through leaf (measured with white background) 
#A' = Absorptance, second pass through leaf 

#T = R - A 
#T' = T - A' 
#T' = T - A         ;assume A' = A 
#T' = (R - A) - A 
#T' = R - 2A 
#T' - R = -2A 
#A = -(T' - R)/2 

#So, T' and R are measured, leaving only A to calculate.  Try this formula and see what happens.  I am assuming there aren't any non-linear relationships (like A' = A) but I think there aren't since A, R and T are proportions of total light at any particular point in the measurement path. 

#Glenn 


x.cast$Abs2 <- -(x.cast$Standard_Reflectance - x.cast$Standard_Transmission)/2
x.cast$Abs3 <- -(x.cast$Standard_Transmission/2 ) + x.cast$Standard_Reflectance
# calculate the mean Absorptivity
mean_abs <- mean(x.cast$Abs3[x.cast$Wavelength <= 700 &
                             x.cast$Wavelength >= 400])

my.text <- paste("Mean Absorptivity is", round(mean_abs, 2))

p <- ggplot(x.cast, aes(x = Wavelength, y = Abs_standard * -1))
  p <- p + geom_line()
  p <- p + geom_line(aes(y = Abs2), colour = "red")
  p <- p + geom_line(aes(y = Abs3), colour = "green")
  p <- p + coord_cartesian(xlim = c(350, 750),  ylim = c(-20, 750))
  #p <- p + annotate("text", x = 450, y = 25, label = my.text)
  p <- p + annotate("text", x = 600, y = 35, label = "(R - T')*(-1)", colour = "black")
  p <- p + annotate("text", x = 500, y = 50, label = "-(T' - R)/2", colour = "red")
    p <- p + annotate("text", x = 470, y = 420, label = "-(T'/2) + R", colour = "green")
  p <- p + labs(y = "Absorptivity. Standard method in black, -(T' - R)/2 in red, -(T'/2) + R in green")
  p <- p + theme_bw()
p

ggsave(file = "Three_Abs_calculations.pdf", width = 7, height = 7)
