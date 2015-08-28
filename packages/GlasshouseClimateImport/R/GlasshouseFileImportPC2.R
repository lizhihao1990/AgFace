#' Function to import DAT files from Creswick Glasshouses
#'
#' Author: Markus Löw
#' Date: May 2015
#' Imports and processes a DAT file.
#' @param file Filename of the DAT file as character
#' @glasshouse Mandatory characterstring, either "PC2" or "teaching" 
#' @return returns a data frame containing the data of the specified DAT file
#' @examples
#' GlasshouseFileImport("myfile.DAT")

GlasshouseFileImport <- function (data, glasshouse) {

stopifnot(glasshouse %in% c("PC2", "teaching"))

if (glasshouse == "PC2") {
    sensor.number <- 11}
if (glasshouse == "teaching") {
    sensor.number <- 5}

# +++++++++++++++++++++++++
# import the file
df <- readLines(data)

# +++++++++++++++++++++++++
# extracting the individual elements of the DAT file
# grab the date
the.date <- df[1]

# identify lines that start with a hh:mm pattern via regular expression matching
time.found <- grep("^[[:digit:]]*:[[:digit:]]*", df)

# get first time entry, this is the first row with 
# the first data entry is the next row
time.found.first <- time.found[1]
data.start <- time.found.first + 1

# index of rows that are data
# not header, not time

# how many rows in the data file?
my.length <- length(df)

# Vector of headers
header.rows <- seq(1:time.found.first -1)
# have to kick out the rows that are a timestamp
header.rows <- header.rows[header.rows %in% time.found == FALSE]

data.rows <- seq(1:my.length)
# have to kick out the rows that are a timestamp
data.rows <- data.rows[data.rows %in% time.found == FALSE]
data.rows <- data.rows[data.rows >= data.start]
# +++++++++++++++++++++++++

# +++++++++++++++++++++++++
# get all timestamps
my.time <- df[time.found]

# Process time stamps
my.time <- paste(the.date, my.time, sep = " ")
my.datetime <- as.POSIXct(my.time, format = "%A %d %B %Y %H:%M")
# +++++++++++++++++++++++++

# +++++++++++++++++++++++++
# get the actual data
# using the information we gathered before to distinguish data from date and time
#my.data <- df[data.rows]
#my.data <- read.table("01012014.DAT", header = FALSE, skip = data.start, sep = ",")
my.data <- read.fwf(data, widths = c(3, 15, 17, 15), skip = data.start-1)
# replace multiple spaces to single spaces
#my.data <- gsub(" +", ",", my.data)

# strip the leading " "
#my.data <- gsub("^,", "", my.data)
# strip the trailing " "
#my.data <- gsub(",$", "", my.data)

# convert to data frame
#my.data <- as.data.frame(my.data)

#remove the rows with the timestamps
# find ":" in the data
my.time <- grep(":", my.data$V1)
my.data <- my.data[-my.time, ]

# add timestamp to the data 
# five entries per timestamp
my.datetime.rep <- unlist(lapply(my.datetime, function(x) rep(x, sensor.number)))
my.datetime.rep <- as.POSIXct(my.datetime.rep, origin = "1970-01-01")

# add the timestamp information to the data
my.data$TIME <- my.datetime.rep

# reorder the data frame
my.data <- my.data[, c("TIME", "V1", "V2", "V3", "V4")]

# re-factor V1
my.data$V1 <- as.numeric(as.character(my.data$V1))
# +++++++++++++++++++++++++



# identify sensors
if (glasshouse == "teaching") {
sensor.names <- c("TempHum01", "PAR01", "Temp", "TempHum02", "PAR02")
sensor.name.table <- data.frame(SensorID = 1:sensor.number,
                                SensorName = sensor.names)

} 
if (glasshouse == "PC2") { 
sensor.names <- c("TempHum01", "PAR01", "TempHum02", "PAR02", "TempHum03", "PAR03", "CO21", "CO22", "TempHum04", "TempHum05", "TempHum06") 
sensor.name.table <- data.frame(SensorID = 1:sensor.number,
                                SensorName = sensor.names)
}

my.data <- merge(my.data, sensor.name.table,
                  by.x = "V1",
                  by.y = "SensorID")

# +++++++++++++++++++++++++
# put each sensor in its own column
# extract V4 only as a new data frame, delete V4 from original data frame
# give the paramter V4 a unique ID in V1
# put my.data and my.V4 back together

# have to grab the V4 parameter
my.V4 <- my.data[, c("TIME", "SensorName", "V1", "V2", "V4")]

# get rid of samples were V4 is NA
my.V4 <- my.V4[!is.na(my.V4$V4), ]

# rename the SensorID
# V4 is temperature
my.V4$SensorName <- gsub("TempHum", "Temperature", my.V4$SensorName)
# inventing a new sensor ID
max.ID <- max(my.V4$V1)
my.V4$V1 <- my.V4$V1 + max.ID

# now renaming V4 to V3 in preparation of rbinding the data frames together
names(my.V4) <- gsub("V4", "V3", names(my.V4))

#removing the V4 column from the original dataframe
V4column.no <- which(names(my.data) == "V4")
my.data.noV4 <- my.data[, -V4column.no]
# renaming the TempHum as this data frame only contains Humidity
my.data.noV4$SensorName <- gsub("TempHum", "Humidity", my.data.noV4$SensorName)

# put the data frames back together
my.data.all <- rbind(my.data.noV4, my.V4)
# +++++++++++++++++++++++++

# putting the data in wide format
# require(reshape2) # will be loaded on when package is loaded

# drop V1 from the data, now redundant
V1column.no <- which(names(my.data.all) == "V1")
my.data.all.noV1 <- my.data.all[, -V1column.no]

my.data.all.cast <- reshape2::dcast(my.data.all.noV1,
                    value.var = "V3",
                    TIME ~ SensorName)
return(my.data.all.cast)

}
