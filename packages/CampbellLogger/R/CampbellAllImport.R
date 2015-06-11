#' Import several Campbell Science AgFace data logger *.dat files from a Folder
#'
#' @description
#' Uses \code{CampbellFileImport} to import all *.dat files from a folder. Files are selected based on their log-interval that is part of their file name. Allowed intervals are "5Min", "15Min", "Hourly", "Daily".
#'
#' @param logger.folder Full path to the location of the *.dat files. Defaults to "~/AgFace/2015/Campbell_logger/logger_data"
#' @param log.interval Select the files based on the common log interval that is specified in the filename. Allowed values are \code{"5Min", "15Min", "Hourly", "Daily"}. Only one time interval is allowed.
#' @param logger.name Select files from one logger only. \code{logger.name} has to match the System name as given in the file name. Allowed values are \code{"SYS1", "SYS2", ..., "SYS8"}

CampbellAllImport <- function(logger.folder   = "~/AgFace/2015/Campbell_logger/logger_data",
                              log.interval    = "5Min",
                              logger.name     = NA) {
 ## function import all files from logger.folder that match the requestes log.interval
 ## returns data frame of all files
 
 # keep track of current folder
  my.folder <- getwd()
 # switch to logger folder
  setwd(logger.folder)
  
  # determine log intervall
  pos.log.intervals <- c("5Min", "15Min", "Hourly", "Daily")
  
  if (log.interval %in% pos.log.intervals) {
     message("Processing requested log.interval")

     # get a list of file names
     my.files.list <- list.files(pattern = "\\.dat$")
     
     # the letters "5Min" are found in the "15Min" as well. Workaround
     # to distinquish 5Min from 15 Min
     if (log.interval == "5Min") {
         log.interval = "_5Min"
     }
     
     # check the filenames against the requested logger interval
     found.files <- grep(log.interval, list.files(), value = TRUE)
    
     # import all files. Returns a named list of data frames 
     my.list <- lapply(found.files, CampbellFileImport) 
     names(my.list) <- found.files
     
     # process each data frame, to have a column with System Name
     # get first four characters of last column to determine System Name
     # then, remove the System Name from the parameter/column names
     GetSystemName <- function(data) {
         last.col.name <- names(data)[length(names(data))]
         system.name <- substr(last.col.name, 1, 4)
         data$SYSTEM <- system.name
         system.name.underscore <- paste0(system.name, "_", "")
         names(data) <- gsub(system.name.underscore, "", names(data))
         return(data)
     }
     
     data.with.system.name.column <- lapply(my.list, GetSystemName)
    
    # check if the individual data frames have a Battery Voltage column
    CheckVoltCol <- function(data) {
       my.names <- names(data)
       if ("Batt_volt_Min" %in% my.names) {
       # do nothing
       return(data)
       } else {
        # get SYSTEMID info
        my.SYS <- unique(data$SYSTEM)
        #my.Bat.name <- paste(my.SYS, "Batt_volt_Min", sep = "_")
        my.Bat.name <- "Batt_volt_Min"
        data$myBat <- NA
        names(data) <- gsub("myBat", my.Bat.name, names(data))
        return(data)
       }
    }
 
     data.with.voltage <- lapply(data.with.system.name.column, CheckVoltCol)

     # switch back to original working folder
     setwd(my.folder)
     
     # merge all data frames
     merged.data <- do.call("rbind", data.with.voltage)
     
     # move System name column to the front
     system.name.colnum <- which(names(merged.data) == "SYSTEM")
     not.system.name    <- which(names(merged.data) != "SYSTEM")
     merged.data <- merged.data[, c(system.name.colnum, not.system.name)]
     
     # define SYSTEM as a factor
     merged.data$SYSTEM <- as.factor(merged.data$SYSTEM)
     
     # sort by System then by TIMESTAMP
     merged.data <- merged.data[with(merged.data, order(SYSTEM, TIMESTAMP)), ]
     
     return(merged.data)                        
    
  } else {
     error.text <- "Log.interval not found. Check spelling.\nAllowed log.intervals are:"
     error.text <- paste(error.text, paste0(pos.log.intervals, collapse = ", "), sep = " ")
     message(error.text)
     return(NULL)
  }
}