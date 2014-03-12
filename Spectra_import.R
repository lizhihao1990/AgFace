# import test spectra

#setwd("~/AgFace/2013/Spectra")

#df <- read.csv("loPcday200185.asd.txt", skip = 40, sep = "\t")

#plot(loPcday200185.asd ~ Wavelength, data = df)

my.files <- list.files(pattern = ".txt")

for (i in 1:(length(my.files)))
        {
        if (i == 1) {
                my.data <- data.frame(Wavelength = NA, 
                                       Signal = NA, 
                                       Name = NA)
                }
        print(my.files[i])
        cur.file <- read.csv(file = my.files[i], 
                             skip = 40, sep = "\t")
        cur.file$Name <- names(cur.file)[2]
        names(cur.file)[2] <- "Signal"
        my.data <- rbind(my.data, cur.file)
}

my.data$Name <- as.factor(my.data$Name)
my.data <- my.data[!is.na(my.data$Name), ]

