#' Import function for Delta-T Devices HH2 Soil moisture files
#'
#' @description Imports a file created by the DeltaT handheld soil mositure reader (HH2). Works for profile probes (\code{PR2}) or TDRs (\code{ML3}) files. Sensor type must be specified at import. Re-organises the data of the HH2 file to be compatible with a data frame.
#' @param filename The name of the file to be imported. Character string.
#' @param sensor.type Either \code{"PR2"} for soil mositure profile probe or \code{"ML3"} for TDR. Other values are not allowed.
#' @return Returns a data frame with the elements Time, Sample, Plot, Device, Depth, Percent_Vol, Vol_Error, mV, mV_Error.

HH2Import <- function(filename, sensor.type) {
   # filename: character string, path and filename of the HH2 file
   # sensor.type: character string, either PR2 or ML3
   
   allowed.sensor.types <- c("PR2", "ML3")
   stopifnot(sensor.type %in% allowed.sensor.types)
   
   # import the raw file   
   rawfile <- readLines(filename)
   
   # determine the row with the start of the data
   # searching for the character string "Time,Sample,Plot"
   header.string <- "Time,Sample,Plot,"
   header.line   <- grep(header.string, rawfile)
   
   if (sensor.type == "PR2") {
   # Column names are not specific, adding sensor depth information to the name
   # grab the line that holds the sensor depth information
   # dynamic, just in case the depth will change in future instalments
   sensor.depth.line <- grep("Sensor Depth", rawfile)
   sensor.depth <- gsub(" ", "", rawfile[sensor.depth.line])
   sensor.depth <- gsub(",,*", ",", sensor.depth)
   sensor.depth <- strsplit(sensor.depth, ",")
   sensor.depth <- sensor.depth[[1]][2:length(sensor.depth[[1]])]
   }
   
   # header is in line "header_line", have to skip all lines before "header_line"
   # therefore, lines to skip are all lines up to "header.line - 1"
   skip.line <- header.line - 1
   
   # remove blanks and special characters from "header_line",
   # split the raw line into a vector to match a comma separated file
   my.header <- gsub(" ", "_", rawfile[header.line])
   my.header <- gsub("%", "Percent", my.header)
   my.header <- strsplit(my.header, ",")
   
   if (sensor.type == "PR2") {
   # add the depth information to the relevant column names
   # there are four columns per depth
   sensor.depth <- rep(sensor.depth, each = 4)
   }
   # column names to alter; "Error" refers to both, "% vol" and "mV"!
   # adding the depth information at the end of the name.
   if (sensor.type == "PR2") {
   non.specific.names <- rep(c("Percent_Vol", "Vol_Error", "mV", "mV_Error"), 4)
   specific.names <- paste(non.specific.names, sensor.depth, sep = "_")
   } else {
   specific.names <- rep(c("Percent_Vol", "Vol_Error", "mV", "mV_Error"), 1)
   }
   
   # import the HH2 file as csv file
   imported <- read.csv(filename, skip = skip.line)
   
   # replace the header with the special characters replaced
   names(imported) <- c(names(imported)[1:4], specific.names)

   # format the "Time" column
   imported$Time <- as.POSIXct(imported$Time, format = "%d/%m/%Y %H:%M")
   
   # split the data frame per sensor depth
   # based on column names
   my.descriptors        <- c("Time", "Sample", "Plot", "Device")
   my.descriptor.columns <- which(names(imported) %in% my.descriptors)
   
   if (sensor.type == "PR2") {
   # split the data frame per each unique depth
   # returns a list of data frames, one for each depth
   # each data frame has depth information
   my.frames <- lapply(unique(sensor.depth), function(x) {
        # creates a search pattern based on sensor depth information
        # use the search pattern result to select vectors from the data frame
        # add depth information to resulting data frames
        # remove depth information from vector names, as it's now redundant
        selector   <- paste(x, "$", sep = "")
        my.columns <- grep(selector, names(imported))
        my.frame   <- imported[, c(my.descriptor.columns, my.columns)]
        my.frame$Depth <- as.numeric(as.character(x))
        # now that there is depth information for each sample, 
        # removing the depth from the names again
        # search pattern for "_" followed by repeated numbers at the end of the character string
        names(my.frame) <- gsub("_[0-9]*$", "", names(my.frame))
        return(my.frame)
   })
   
   # put the data frames from the list back together into one data frame
   imported <- do.call(rbind, my.frames)
   }
   
   # potential to do:
   # there is an empty column in the csv file on the right end
   # removing this column, but only when it is empty - just in case
   # leaving it alone for now, who knows if this column serves a purpose in the future
   
   # re-order column order in data frame
   if (sensor.type == "PR2") {
   imported <- imported[, c("Time", "Sample", "Plot", "Device", "Depth", "Percent_Vol", "Vol_Error", "mV", "mV_Error")]
   } else {
   imported <- imported[, c("Time", "Sample", "Plot", "Device", "Percent_Vol", "Vol_Error", "mV", "mV_Error")]
   }
   
   # return the imported data frame
   return(imported)
}

