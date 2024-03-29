# import data from 2007, 2008 and 2009

# load packages
require(xlsx)
require(reshape)

# set working directory
setwd("~/AgFace/Plant_Production")

# load AgFACE helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# import data sets - one per year
df2007 <- read.xlsx("../2007/FACE_2007_Plant_production_summary_27Jun13.xls", 
                    sheetName = "Final")
# removed duplicate header lines from file

df2008 <- read.xlsx("../2008/FACE_2008_PBC_Plant_production_summary_27Jun13.xls", 
                    sheetName = "Final")
# removed duplicate header lines from file

df2009 <- read.xlsx("../2009/FACE_2009_PBC_Plant_Production_Summary_27Jun13.xls", 
                    sheetName = "Final")
# removed duplicate header lines from file

# import takes quite a long time
# creating a save-point after import
save.image("Plant_Production_data_00.RData")
# load("Plant_Production_data_00.RData")
# read.xlsx2 to speed up the import process, but creates problems with some columns


# checking the column names
# creating a data frame with the names
cnames2007 <- names(df2007)
cnames2008 <- names(df2008)
cnames2009 <- names(df2009)

# getting rid of empty columns
# rather radical, just looking for columns with names that start with "NA." after import
noname2007 <- grep("^NA.", cnames2007)
noname2008 <- grep("^NA.", cnames2008)
noname2009 <- grep("^NA.", cnames2009)

# delete the columns that are not part of noname2007
# getting rid of empty columns
# rather radical, just looking for columns with names that start with "NA." after import
df2007 <- df2007[, -noname2007]
df2008 <- df2008[, -noname2008]
df2009 <- df2009[, -noname2009]

# now the dataframes should have the same amount of columns!
my.columns <- c(ncol(df2007), ncol(df2008), ncol(df2009))
names(my.columns) <- c("2007", "2008", "2009")

# Different number of columns!! 2009 has more than any other year!

# comparing the names of the columns of the three data frames
different.names <- which(!(names(df2007) %in% names(df2008)))

names(df2007)[different.names]
names(df2008)[different.names]
# ==> different spelling of names in different years

# same test for 2007 in comparison to 2009
different.names <- which(!(names(df2007) %in% names(df2009)))

names(df2007)[different.names]
names(df2009)[different.names]
# ==> again different spelling of names


# get rid of completely empty cases in the descriptors
# not needed anymore
#df2007.orig <- df2007
#df2007 <- df2007[complete.cases(df2007[, 1:19]),]
#df2008.orig <- df2008
#df2008 <- df2008[complete.cases(df2008[, 1:19]),]
#df2009.orig <- df2009
#df2009 <- df2009[complete.cases(df2009[, 1:19]),]


# double check the deletion of the empty sample - should not change the distribution
#summary(df2007.orig$NUE..yield.N.uptake.)
#summary(df2007$NUE..yield.N.uptake.)

# Additionally:
# discrepancies between Horsham and Walpeup data:
# important parameter seeds/floret is missing from all Horsham files
# Glenn: it is part of the "growth" and "summary" sheets in the Horsham files, but not part of the "Final" sheet that is used here.
# Seeds/floret uses information from previous samplings, i.e. not easy to re-calculate
# have to identify the corresponding sample from a different grwoth stage to do tha calculation. The Seeds/Floret data is stored in column GN in Excel for Walpeup, after column NUE yield uptake.
# Equation is Seeds/floret = columnFL/columnDJ19 - both columns only available for DC90
# the equation translates to Seeds/Floret = Grains per head quadrat/ Spikelets per head
# Spikelets/head only available for DC65
# Grains/Head only available for DC90

# Doing this per imported year, not in the final data set as we are adding an additional column later.

# How to identify a sample between grwoth stages?
# Same samples have the same interaction of 
# TrialID x Year x TOS x RingID x HalfRingID x PlotTrt x Cultivar
# 

CalcSeedFlorets <- function(data) {
                data$myID <- interaction(data$TrialID,
                                         data$Year,
                                         data$TOS,
                                         data$RingID,
                                         data$HalfRingID,
                                         data$PlotTrt,
                                         data$Cultivar,
                                         drop = TRUE)
                
                # select the parameters to keep
                paras.to.keep <- c("myID", 
                                   "Stage",
                                   "Grains.head..quadrat.",
                                   "Spikelets.head")
                
                # cutting down the data
                sdata <- data[, names(data) %in% paras.to.keep]
               
                dc65 <- sdata[sdata$Stage == "DC65",]
                names(dc65) <- gsub("^", "dc65.", names(dc65))
                
                dc90 <- sdata[sdata$Stage == "DC90",]
                names(dc90) <- gsub("^", "dc90.", names(dc90))
                 
                
                out <- merge(dc65, dc90, by.x = "dc65.myID",
                                         by.y = "dc90.myID")
               
                out$Seeds.Floret <- out$dc90.Grains.head..quadrat. / 
                                    out$dc65.Spikelets.head

                # get rid of ballast
                out <- out[, c("dc65.myID", "dc90.Stage", "Seeds.Floret")]
                names(out) <- c("myID", "Stage", "Seeds.Floret")
                
                odata <- merge(data, out,
                               by.x = c("myID", "Stage"),
                               by.y = c("myID", "Stage"),
                               all.x = TRUE)
                odata$myID <- NULL
                return(odata)
}

df2007 <- CalcSeedFlorets(df2007)
df2008 <- CalcSeedFlorets(df2008)
df2009 <- CalcSeedFlorets(df2009)

# just to have a look how it looks like on disk
write.table(df2008,
            file = "Floret_outcome.csv",
            sep = ",",
            row.names = F)


# which are the additional columns in the 2009 data?
different.names <- which(!(names(df2009) %in% names(df2007)))
names(df2009)[different.names]

# have to move the NDVI.colume. column in 2009 to the end to match the other years
# badly hardcoded - will fail every time when the number of columns changes!!
df2009 <- df2009[, c(1:193, 195, 194)]

# ==> seems to be "NDVI.volume."
names(df2009)[names(df2009) == "NDVI.volume."]
grep("NDVI", names(df2009))
names(df2007)[grep("NDVI", names(df2007))]
names(df2008)[grep("NDVI", names(df2008))]
names(df2009)[grep("NDVI", names(df2009))]

# ==> seems like "NDVI.volume." is an additional column, not a new name for an existing column.

# common names for all data frames
# using the names from 2007 for now.
names(df2007)[161]
names(df2008)[161]
names(df2009)[161]
# rename accordingly
names(df2008) <- names(df2007)
names(df2009) <- c(names(df2007), "NDVI.volume.")

# add the additional column to 2007 and 2008
df2007$NDVI.volume. <- as.numeric(NA)
df2008$NDVI.volume. <- as.numeric(NA)


# merging data frames, as they have common names
df <- rbind(df2007, df2008, df2009)

# put the Stage-column back into its original location (was moved around during merging of the Seeds.Floret data)
# goes between "Cultivar" and "TrialName"
df <- df[, c(2:16, 1, 17:ncol(df))]


write.table(df,
            file = "merged_data_2007-2009.csv",
            sep = ",",
            row.names = FALSE)

# save the data
save.image("Plant_Production_data_01.RData")
# load("Plant_Production_data_01.RData")

#convert Year to factor
df$Year <- as.factor(df$Year)

# convert dates (following advice from ?read.xlsx)
# To convert an Excel datetime colum to POSIXct, do something like:
#   as.POSIXct((x-25569)*86400, tz="GMT", origin="1970-01-01")
# seems to be unnecessary, all dates imported correctly - for now...

# why are there NA's in the "CO2" column?
df[is.na(df$CO2), ]
# ==> seem to be empty rows. No worries.
# throwing those away...
df <- df[!is.na(df$CO2), ]

# some dramas with numeric columns that come out as character
col.num.character <- sapply(df[, 1:ncol(df)], is.character)
col.num.character[col.num.character == TRUE]

#manually checked these columns - all contain numeric elements
#df$Marc.Nicolas.Sample.Fresh.Plant.No.
#df$Sub.sample.Head.Wt..g.
#df$X.C.straw 
#df$Straw.C.N
#df$Remainder.Sample.Grain.Wt..g.

# Straw.C.N contains "Div/0" errors from excel. Those get imported with an error message. Replacing the error message with "NA"
imp_errors <- grep("jav", df$Straw.C.N)
df$Straw.C.N[imp_errors] <- NA

# then convert to numeric
df$Straw.C.N <- as.numeric(as.character(df$Straw.C.N))

# the other columns can be converted directly
df$Marc.Nicolas.Sample.Fresh.Plant.No. <- as.numeric(
                as.character(df$Marc.Nicolas.Sample.Fresh.Plant.No.))
df$Sub.sample.Head.Wt..g. <- as.numeric(
               as.character(df$Sub.sample.Head.Wt..g.))
df$X.C.straw  <- as.numeric(
               as.character(df$X.C.straw))
df$Remainder.Sample.Grain.Wt..g. <- as.numeric(
               as.character(df$Remainder.Sample.Grain.Wt..g.))

# treat other columns like "Trmt.Order" and "Plot.Order" etc. as descriptive factors, not numeric
df$Trmt.Order <- as.factor(as.character(df$Trmt.Order))
df$Plot.Order <- as.factor(as.character(df$Plot.Order))
df$RingID     <- as.factor(as.character(df$RingID))

# find out which colums are defined as logical, as date, or as factor
# these columns will mess up the later "melt" statement.
# logical first

# keep a copy of the full data
df.full <- df

col.num.logical <- sapply(df[, 1:ncol(df)], is.logical)

# get an index of column that are not logical
col.num <- which(col.num.logical == FALSE)
df <- df[, col.num] # keep only the non-logical columns

# get an index of column that are not factor, but keeping the descriptive columns
col.num.factor  <- sapply(df[, 1:ncol(df)], is.factor)
col.num <- which(col.num.factor == FALSE)
df.new <- df[, col.num] # keep only the non-logical columns

# test df.new for data columns. data columns can not be tested easily
date.columns <- c("Sample.Date", "Sowing.date", "Emergence.date")

# now df and df.new both contian date columns - getting rid of them in df.new
df.new <- df.new[, !names(df.new) %in% date.columns]


df <- cbind(df[, 1:20], df.new) # combine with the descriptive columns
rm(df.new)

# now we only have numeric and integer columns left in the data frame

# confirmed wrong 'Cultivar' information regarding Janz
# was detected during processing "yitpi" samples
# there are 32 Janz samples in Yitpi rows
# renaming those cases
df$Cultivar[(df$PlotTrt == "YitpiN+" |
             df$PlotTrt == "YitpiN0") & 
             df$Cultivar == "Janz"] <- as.factor("Yitpi")

# correct factor levels in "Sampling" to upper case
# capitalise using function from helper script
df$Sampling <- .simpleCap(df$Sampling)
df$Sampling <- as.factor(df$Sampling)

# An incompletely labelled sample was identified. Some values are duplicates of values in the previous row, missing database ID
# getting rid of this case
# now obsolete, as delat with in the original file before import
df[is.na(df$CO2.TOS.Irr.Cultivar.DATABASE), ]
df <- df[!is.na(df$CO2.TOS.Irr.Cultivar.DATABASE), ]

# put the data in long format
# data in "long" format
df.melt <- melt(df,
                id = names(df[1:20]))

# create some graphs
my_plots <- dlply(df.melt,
                  .(variable),
                  function(x) {
                  MyMultPlotPlantProd(x, x$variable, "Sample.Date", "value", "CO2", "Irrigation", "Cultivar", "Stage")})

#pdf(file = "All_graphs.pdf")
#        print(my_plots)
#dev.off()

# save the data
save.image("Plant_Production_data_02.RData")
# load("Plant_Production_data_02.RData")
# 
# Follow-up-script for Yitpi N0/N+ analysis run "Yitpi_N_experiment_2007_2009.R":
# Follow-up script for Horsham-Walpeup Environment comparison

