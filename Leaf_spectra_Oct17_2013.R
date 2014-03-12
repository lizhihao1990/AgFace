# Spectral analysis
# leaf level, Oct 17, a few hours after pre-dawn chlorophyll fluorescence measurements

# set working directory
setwd("~/AgFace/2013/Spectra/2013-10-17_leaves/converted")

# load spectra import script
source("~/AgFace/R_scripts/Spectra_import.R")

library(scales)
library(ggplot2)
library(reshape)

spectra <- my.data

# create treatment columns
spectra$Mode <- NA
spectra$Ring <- NA
spectra$Heat_treat <- NA

# First set of measurements is 275 (transmission) and 276 (reflectance) Ring 2 outside
small <- spectra[spectra$Name == "X2013.10.1700275.asd" | 
                 spectra$Name == "X2013.10.1700276.asd", ]

small$Mode <- NA
small$Mode[small$Name == "X2013.10.1700275.asd"] <- "Transmission"
small$Mode[small$Name == "X2013.10.1700276.asd"] <- "Reflectance"
small$Mode <- as.factor(small$Mode)

Trans <- small[small$Mode == "Transmission", ]
names(Trans)[2] <- "Trans"

Refl  <- small[small$Mode == "Reflectance",]

trans_refl_abs <- Trans
trans_refl_abs$Refl <- Refl$Refl
trans_refl_abs$N_Trans <- rescale(trans_refl_abs$Trans, to = c(0, 1))
trans_refl_abs$N_Refl  <- rescale(trans_refl_abs$Refl,  to = c(0, 1))

# Abs with normalisation
trans_refl_abs$N_Abs <- trans_refl_abs$N_Refl - trans_refl_abs$N_Trans

# Abs without normalisation
trans_refl_abs$Abs <- trans_refl_abs$Refl - trans_refl_abs$Trans


tra.melt <- melt(trans_refl_abs, id = c("Name", "Mode", "Wavelength"))

p <- ggplot(tra.melt[tra.melt$variable == "N_Refl" |
                     tra.melt$variable == "N_Trans" |
                     tra.melt$variable == "N_Abs", ], 
            aes(x = Wavelength, y = value))
        p <- p + geom_line(aes(colour = variable))
        p <- p + theme_bw()
        p <- p + labs(title = "Normalised Refl and Trans spectra plus resulting Abs")
        
p
ggsave(file = "Normalised_spectra_trans_refl.pdf", width = 7, height = 7)
full_norm_spectra <- p

p <- ggplot(tra.melt[tra.melt$variable == "Refl" |
                     tra.melt$variable == "Trans" |
                     tra.melt$variable == "Abs", ], 
            aes(x = Wavelength, y = value))
        p <- p + geom_line(aes(colour = variable))
        p <- p + theme_bw()
        p <- p + labs(title = "Original Refl and Trans spectra plus resulting Abs")
        
p
ggsave(file = "Original_spectra_trans_refl.pdf", width = 7, height = 7)
full_orig_spectra <- p


p <- ggplot(tra.melt[tra.melt$variable == "N_Refl" |
                     tra.melt$variable == "N_Trans" |
                     tra.melt$variable == "N_Abs", ], 
            aes(x = Wavelength, y = value))
        p <- p + geom_line(aes(colour = variable))
        p <- p + coord_cartesian(xlim = c(380, 720))
        p <- p + theme_bw()
        p <- p + labs(title = "Normalised Refl and Trans spectra plus resulting Abs")
        
p
ggsave(file = "Normalised_spectra_trans_refl_400-700.pdf", width = 7, height = 7)
norm_spectra_400t0700 <- p

p <- ggplot(tra.melt[tra.melt$variable == "Refl" |
                     tra.melt$variable == "Trans" |
                     tra.melt$variable == "Abs", ], 
            aes(x = Wavelength, y = value))
        p <- p + geom_line(aes(colour = variable))
        p <- p + coord_cartesian(xlim = c(380, 720))
        p <- p + theme_bw()
        p <- p + labs(title = "Original Refl and Trans spectra plus resulting Abs")
        
p
ggsave(file = "Original_spectra_trans_refl_400-700.pdf", width = 7, height = 7)
org_spectra_400t0700 <- p


p <- ggplot(tra.melt[tra.melt$variable == "Abs", ], 
            aes(x = Wavelength, y = value))
        p <- p + geom_line()
        p <- p + coord_cartesian(xlim = c(380, 720), ylim = c(0, -0.2))
        p <- p + theme_bw()
        p <- p + labs(title = "Absorptivity")
        
p
ggsave(file = "Absorptivity_400to700nm.pdf", width = 7, height = 7)
org_400to700_spectra <- p

p <- ggplot(tra.melt[tra.melt$variable == "N_Abs", ], 
            aes(x = Wavelength, y = 1 - value))
        p <- p + geom_line()
        p <- p + coord_cartesian(xlim = c(380, 720))
        p <- p + geom_hline(yintercept = 0.84, linetype = "dashed", colour = "red")
        p <- p + theme_bw()
p
norm_400to700_spectra_reversed <- p

mean_N_Abs <- mean(tra.melt$value[tra.melt$variable == "N_Abs" & 
                   (tra.melt$Wavelength >= 400 | tra.melt$Wavelength <= 700)])

# Force all data to the range from 0 to 1
# spectra$Norm_Ref <- rescale(my.data$Reflectance,  to = c(0, 1))

# calculate Absorptivity
# my.data$Abs <- my.data$Norm_Ref - my.data$Norm_Tra

p <- ggplot(spectra, aes(x = Wavelength, y = Refl))
  p <- p + geom_line(aes(colour = Name))
  p <- p + theme_bw()
p

p <- ggplot(my.data, aes(x = Wavelength, y = Refl))
 p <- p + geom_line(aes(colour = Name))
 p <- p + scale_x_continuous(limits = c(380, 720))
 #p <- p + scale_y_continuous(limits = c(0, 0.2))
 p <- p + theme_bw()
p
