# Analysis of night-time respiration
# Oct 2, 2014

library(plyr)
library(reshape2)
library(ggplot2)

setwd("/home/loewi/AgFace/2014/Night_time_respiration/2014-10-02")

# import the data files, one file per instrument
PSC0161 <- read.csv("2014-10-02_nighttime_respiration_0161.csv",
                    skip = 9)
PSC1256 <- read.csv("2014-10-02_nighttime_respiration_1256.csv",
                    skip = 12, sep = "\t")
PSC1375 <- read.csv("2014-10-02_nighttime_respiration_1375.csv",
                    skip = 14, sep = "\t")

# Putting the instrument name in each data frame
PSC0161$Instrument <- "PSC0161"
PSC1256$Instrument <- "PSC1256"
PSC1375$Instrument <- "PSC1375"

# PSC1375 (Irga3) has leaf chamber fluorometer installed but no fluorescence was measured during night time respiration.
# Getting rid of those columns
fluoro.paras <- c("Fo", "Fm", "Fo.", "Fm.",  "Fs", "Fv.Fm", "Fv..Fm.", "PhiPS2", "Adark", "RedAbs", "BlueAbs", "X.Blue", "LeafAbs", "PhiCO2", "qP", "qN", "NPQ", "ParIn.Fs", "PS2.1", "ETR")
PSC1375.cleaned <- PSC1375[, !(names(PSC1375) %in% fluoro.paras)]

# merge data from instrument PSC1256 and PSC1375 (Irga2 and Irga3)
nightres <- rbind(PSC1256, PSC1375.cleaned)

# adding a comment column
nightres$Comment <- NA

# instrument PSC0161 (irga from Roger Armstrong) needs some housekeeping due to an older software version
# LPL version 4 can only handle two prompts, treatment was put in comments

# columns with different names compared to newer LPL versions
names(PSC0161) <- gsub("Time", "FTime", names(PSC0161))
PSC0161$HHMMSS <- NA
PSC0161$StableF <- NA
# Identify comments, in this case, in column "Ring"
# get the row numbers with comments in them
is.comment <- grep(":", PSC0161$Ring)
my.comments <- PSC0161$Ring[is.comment]

# repeat each comment three times
comment.rep <- as.data.frame(sapply(my.comments, FUN = function(x) rep(x, 3)))
comment.rep <- t(comment.rep)
comment.rep <- as.data.frame(comment.rep)
comment.rep$Treatment <- row.names(comment.rep)
comment.rep$Treatment <- gsub("V", "", comment.rep$Treatment)
comment.rep$Treatment <- as.numeric(as.character(comment.rep$Treatment))
comment.rep[, 1:3] <- lapply(comment.rep[, 1:3], as.character)

comment.plyr <- ddply(comment.rep,
                     .(Treatment),
                     function(x) {
                          V1 <- x$V1
                          V2 <- x$V2
                          V3 <- x$V3
                         rbind(V1, V2, V3)
                })

names(comment.plyr) <- c("Remark_ID", "Comment")

# get rid of the comments in PSC0161 data
PSC0161.clean <- PSC0161[-is.comment, ]

# add comments as Treatment descriptors
PSC0161.clean$Comment <- comment.plyr$Comment
PSC0161.clean$Treatment <- NA

# now merging this instrument with the existing data
nightres <- rbind(nightres, PSC0161.clean)

# convert the Treatments to text
nightres$Treatment <- as.character(nightres$Treatment)

# moving the treatment column into the comments
nightres$Comment[nightres$Instrument != "PSC0161"] <- nightres$Treatment[nightres$Instrument != "PSC0161"]

# get rid of excessive spaces
nightres$Comment <- gsub("  +", "", nightres$Comment)
nightres$Ring    <- gsub("  +", "", nightres$Ring)

# process cultivar names
nightres$Cultivar <- gsub("  +", "", nightres$Cultivar)

# all Cultivar names start with capital letter - function from ?chartr
.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

nightres$Cultivar <- sapply(nightres$Cultivar, function(x) .simpleCap(x))
nightres$Cultivar <- gsub("Scoutt", "Scout", nightres$Cultivar)
nightres$Cultivar <- gsub("Scouy", "Scout", nightres$Cultivar)
nightres$Cultivar <- gsub("Yitpy", "Yitpi", nightres$Cultivar)

nightres$Comment <- gsub("rot", "root", nightres$Comment)
nightres$Comment <- gsub("rooty", "root", nightres$Comment)
nightres$Comment <- gsub("rooyt", "root", nightres$Comment)
nightres$Comment <- gsub("laf", "leaf", nightres$Comment)
nightres$Comment <- gsub("[[:digit:]]+\\:[[:digit:]]+\\:[[:digit:]]+", "", 
                              nightres$Comment)
nightres$Comment <- gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2", 
                              nightres$Comment)

# create the treatment information
nightres$Treatment <- NA
nightres$Treatment[grep("wet",  nightres$Comment, ignore.case = TRUE)] <- "wet"
nightres$Treatment[grep("root", nightres$Comment, ignore.case = TRUE)] <- "root"
nightres$Treatment[grep("heat", nightres$Comment, ignore.case = TRUE)] <- "heat"

# create an index for leaf numbers
nightres$Leaf <- NA
nightres$Leaf[grep("1", nightres$Comment)] <- 1
nightres$Leaf[grep("2", nightres$Comment)] <- 2

# getting rid of known "bad" measurements
# IRGA2, PSC1256, logs, 1:3 and 15 and 32 are bad
nightres2 <- nightres[-c(1:3, 16, 32), ]

# getting rid of logs based on comments
# Ring 4, Scout wet, wet 1 wet 2
nightres2$bad.data <- "good"
nightres2$bad.data[nightres2$Cultivar == "Scout" &
                  nightres2$Ring == 4 &
                  (nightres2$Comment == "wet 1" |
                   nightres2$Comment == "wet 2")] <- "bad"
nightres2$bad.data[nightres2$Cultivar == "Scout" &
                  nightres2$Ring == 4 &
                  nightres2$Comment == "heat 1" ] <- "bad"

nightres3 <- nightres2[nightres2$bad.data == "good", ]

nightres3$bad.data[nightres3$Photo > 5] <- "bad"
nightres4 <- nightres3[nightres3$bad.data == "good", ]

nightres4$bad.data <- NULL

# keep the original data
nightres.orig <- nightres
nightres <- nightres4


# figures per leaf
p <- ggplot(nightres, aes(x = Cultivar, y = Photo))
     p <- p + geom_boxplot(aes(colour = Leaf, fill = Treatment))
     p <- p + facet_grid(. ~ Ring)
p


# get a handle on the columns that are not numeric or integer
my.factors    <- sapply(nightres, is.factor)

nightres.nofactor <- nightres[, my.factors == FALSE]

# create factor levels for Ring, Cultivar, and treatment
nightres.nofactor$Ring      <- as.factor(nightres.nofactor$Ring)
nightres.nofactor$Cultivar  <- as.factor(nightres.nofactor$Cultivar)
nightres.nofactor$Treatment <- as.factor(nightres.nofactor$Treatment)

my.characters <- sapply(nightres.nofactor, is.character)
nightres.nocharacter <- nightres.nofactor[, my.characters == FALSE]

# average the two leaves per ring and treatment
nightres.melt <- melt(nightres.nocharacter,
                      id.vars = c("Ring", "Cultivar", "Treatment", "Leaf"))

# leaf-level aggregation
nightres.cast <- dcast(nightres.melt,
                       Ring + Cultivar + Treatment + Leaf ~ variable,
                       fun = mean)

# second level of aggregation
nightres.melt2 <- melt(nightres.cast,
                      id.vars = c("Ring", "Cultivar", "Treatment", "Leaf"))

nightres.cast <- dcast(nightres.melt2,
                       Ring + Cultivar + Treatment ~ variable,
                       fun = mean)

# load Agface helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# add the CO2 treatment
nightres.cast$CO2_treatment <- NA
nightres.cast$CO2_treatment[nightres.cast$Ring %in% ambient_rings_2014]  <- "aCO2"
nightres.cast$CO2_treatment[nightres.cast$Ring %in% elevated_rings_2014] <- "eCO2"
nightres.cast$CO2_treatment <- as.factor(nightres.cast$CO2_treatment)

with(nightres.cast, table(CO2_treatment, Cultivar, Treatment))

# figures for night time respiration
p <- ggplot(nightres.cast, aes(x = Cultivar, y = Photo * -1))
     p <- p + geom_boxplot(aes(colour = CO2_treatment))
     p <- p + facet_grid(. ~ Treatment)
     p <- p + labs(y = expression("Respiration rate at night"~"["~mu*mol~CO[2]~m^-2*s^-1~"]"))
     p <- p + theme_bw()
p

ggsave(file = "2014-10-02_Nighttime_respiration.pdf",
       width = 9, height = 7)
       
my.aov.nested <- aov(Photo ~ CO2_treatment * Cultivar * Treatment + Error(Ring), 
              data = nightres.cast)
summary(my.aov.nested)

my.aov <- aov(Photo ~ CO2_treatment * Cultivar * Treatment, 
              data = nightres.cast)
summary(my.aov)

my.aov.noheat <- aov(Photo ~ CO2_treatment * Cultivar * Treatment, 
                     data = nightres.cast[nightres.cast$Treatment != "heat", ])
summary(my.aov.noheat)

my.aov.nowet <- aov(Photo ~ CO2_treatment * Cultivar * Treatment, 
                     data = nightres.cast[nightres.cast$Treatment != "wet", ])
summary(my.aov.nowet)

my.aov.heat <- aov(Photo ~ CO2_treatment * Cultivar, 
                     data = nightres.cast[nightres.cast$Treatment == "heat", ])
summary(my.aov.heat)

my.aov.root <- aov(Photo ~ CO2_treatment * Cultivar, 
                     data = nightres.cast[nightres.cast$Treatment == "root", ])
summary(my.aov.root)

my.aov.wet <- aov(Photo ~ CO2_treatment * Cultivar, 
                     data = nightres.cast[nightres.cast$Treatment == "wet", ])
summary(my.aov.wet)

my.aov.scout.root <- aov(Photo ~ CO2_treatment, 
                     data = nightres.cast[nightres.cast$Treatment == "root" &
                                          nightres.cast$Cultivar == "Scout", ])
summary(my.aov.scout.root)

my.aov.yitpi.root <- aov(Photo ~ CO2_treatment, 
                     data = nightres.cast[nightres.cast$Treatment == "root" &
                                          nightres.cast$Cultivar == "Yitpi", ])
summary(my.aov.yitpi.root)

my.aov.scout.wet <- aov(Photo ~ CO2_treatment, 
                     data = nightres.cast[nightres.cast$Treatment == "wet" &
                                          nightres.cast$Cultivar == "Scout", ])
summary(my.aov.scout.wet)

my.aov.yitpi.wet <- aov(Photo ~ CO2_treatment, 
                     data = nightres.cast[nightres.cast$Treatment == "wet" &
                                          nightres.cast$Cultivar == "Yitpi", ])
summary(my.aov.yitpi.wet)

nightres.2014.10.02 <- nightres.cast

