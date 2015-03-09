# import all Campbell logger files
# based on timing intervalls

# Analysis of Campbell sensors

CampbellFileImport <- function(file, time.zone = "Australia/Melbourne") {
  # imports a single Campbell file as per given filename
  # converts the TIMESTAMP to POSIXct, given the provided time zone
  # returns a data frame
  
  my.header   <- read.csv(file, skip = 1)
  my.header   <- names(my.header)
  my.descript <- read.csv(file, skip = 3)
  my.descript <- names(my.descript)

  df <- read.csv(file, skip = 4, na.strings = "NAN")
  names(df) <- my.header
  names(df) <- gsub("\\.", "_", names(df))
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = time.zone)
  return(df)
}

# list of our sensor names without their system ID number
sensor.names <- c("RawCh_SGA2_1_Avg", "RawAh_SGA2_1_Avg", "RawBh_SGA2_1_Avg", "RawHeater_SGA2_1_Avg", "RawCh_SGA2_2_Avg", "RawAh_SGA2_2_Avg", "RawBh_SGA2_2_Avg", "RawHeater_SGA2_2_Avg", "Sapflow_SGA2_1_Avg", "Kshapp_SGA2_1_Avg", "dT_SGA2_1_Avg", "Pin_SGA2_1_Avg", "Qv_SGA2_1_Avg", "Qr_SGA2_1_Avg", "Qf_SGA2_1_Avg", "Sapflow_SGA2_2_Avg", "Kshapp_SGA2_2_Avg", "dT_SGA2_2_Avg",
"Pin_SGA2_2_Avg", "Qv_SGA2_2_Avg", "Qr_SGA2_2_Avg", "Qf_SGA2_2_Avg", "IR_Narrow_Min_1_", "IR_Narrow_Min_2_", 
"IR_Narrow_Min_3_", "IR_Narrow_Min_4_", "IR_Narrow_Min_5_", 
"IR_Narrow_Min_6_", "IR_Narrow_Min_7_", "IR_Narrow_Min_8_", 
"IR_Narrow_Max_1_", "IR_Narrow_Max_2_", "IR_Narrow_Max_3_",
"IR_Narrow_Max_4_", "IR_Narrow_Max_5_", "IR_Narrow_Max_6_",
"IR_Narrow_Max_7_", "IR_Narrow_Max_8_", "IR_Narrow_Avg_1_",
"IR_Narrow_Avg_2_", "IR_Narrow_Avg_3_", "IR_Narrow_Avg_4_",
"IR_Narrow_Avg_5_", "IR_Narrow_Avg_6_", "IR_Narrow_Avg_7_",
"IR_Narrow_Avg_8_", "IR_Narrow_Std_1_", "IR_Narrow_Std_2_",
"IR_Narrow_Std_3_", "IR_Narrow_Std_4_", "IR_Narrow_Std_5_",
"IR_Narrow_Std_6_", "IR_Narrow_Std_7_", "IR_Narrow_Std_8_",
"IR_Horz_Min_1_", "IR_Horz_Min_2_", "IR_Horz_Max_1_",
"IR_Horz_Max_2_", "IR_Horz_Avg_1_", "IR_Horz_Avg_2_",
"IR_Horz_Std_1_", "IR_Horz_Std_2_")

GetSensorID <- function(sensor.name) {
   if (grepl("SGA2_", sensor.name) == TRUE) {
      my.split.1 <- "SGA2_"  
      my.split.2 <- "_"
      }
   if (grepl("IR_", sensor.name) == TRUE) {
      my.split.1 <- ""
   }
   # split the sensor.name using the gathered my.split.x information
   splitted <- strsplit(sensor.name, split = my.split.1)
   first.bit  <- unlist(lapply(splitted, "[", 1))
   second.bit <- unlist(lapply(splitted, "[", 2))

   # split the second bit again to get to the sensor id
   sensor.id <- unlist(strsplit(second.bit, split = "_"))
   sensor.id <- unlist(lapply(strsplit(second.bit, split = my.split.2), "[", 1))
   sensor.desc <- unlist(lapply(strsplit(second.bit, split = my.split.2), "[", 2))
   
   # assemble sensor name
   sensor.name.no.id <- paste(first.bit, sensor.desc, sep = "_")
   sensor.name.no.id <- gsub("__", "_", sensor.name.no.id)
   
   # create output data frame
   out <- data.frame(SensorID = sensor.id,
                     SensorName = sensor.name.no.id)
   return(out)
}

# test code
#out <- GetSensorID("Kshapp_SGA2_1_Avg")
#my.out <- lapply(sensor.names[1:15], GetSensorID)
#do.call(rbind, my.out)

CampbellAllImport <- function(logger.folder   = "~/AgFace/2014/Campbell_logger/logger_data",
                              log.interval    = "5Min",
                              logger.name     = NA) {
 ## function import all files from logger.folder that match the requestes log.interval
 ## returns data frame of all files
  setwd(logger.folder)
  
  # determine log intervall
  pos.log.intervals <- c("5Min", "15Min", "Hourly", "Daily") #, 
  
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
     
     # merge all data frames
     merged.data <- do.call("rbind", data.with.system.name.column)
     
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

# example code
# df <- CampbellAllImport(log.interval = "5Min")
