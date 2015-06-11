# import all Campbell logger files
# based on timing intervalls

# Analysis of Campbell sensors

CampbellFileImport <- function(file, time.zone = "Australia/Melbourne", checkduplicates = TRUE) {
  # require(readr) # faster import, but problems whith coding "NA" 
  # imports a single Campbell file as per given filename
  # converts the TIMESTAMP to POSIXct, given the provided time zone
  # returns a data frame
  
  my.header   <- read.csv(file, skip = 1, nrows = 4)
  my.header   <- names(my.header)
  my.descript <- read.csv(file, skip = 3, nrows = 4)
  my.descript <- names(my.descript)

  df <- read.csv(file, skip = 4, na.strings = "NAN")
  #readr version: 
  #df <- read_csv(file, skip = 4, na = "NAN", progress = TRUE)
  names(df) <- my.header
  names(df) <- gsub("\\.", "_", names(df))
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = time.zone)
  
  # get rid of duplicate samples
  # df.orig <- df
  df$RECORD <- 0
  if (isTRUE(checkduplicates) == TRUE) {
	  df <- unique(df)
  }
  return(df)
}

# list of our sensor names without their system ID number
sensor.names <- c("Batt_volt_Min", "RawCh_SGA2_1_Avg", "RawAh_SGA2_1_Avg", "RawBh_SGA2_1_Avg", "RawHeater_SGA2_1_Avg", "RawCh_SGA2_2_Avg", "RawAh_SGA2_2_Avg", "RawBh_SGA2_2_Avg", "RawHeater_SGA2_2_Avg", "Sapflow_SGA2_1_Avg", "Kshapp_SGA2_1_Avg", "dT_SGA2_1_Avg", "Pin_SGA2_1_Avg", "Qv_SGA2_1_Avg", "Qr_SGA2_1_Avg", "Qf_SGA2_1_Avg", "Sapflow_SGA2_2_Avg", "Kshapp_SGA2_2_Avg", "dT_SGA2_2_Avg",
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
"IR_Horz_Std_1_", "IR_Horz_Std_2_", "Soil_Avg_1_", "Soil_Avg_2_", "Soil_Avg_3_", "Soil_Avg_4_", "Soil_Avg_5_",
"PAR_Avg", "Temp_Avg_1_", "Temp_Avg_2_", "Hum_Avg_1_", "Hum_Avg_2_")

GetSensorID <- function(sensor.name) {
 # Sap flow sensors SGA2
 stopifnot(length(unique(sensor.name)) == 1 )
 sensor.name <- unique(sensor.name)
 sensor.name <- as.character(sensor.name)
 
 if (grepl("Batt_volt_Min", sensor.name) == TRUE) {
   
   out <- data.frame(FullName = sensor.name,
                     SensorID = "1",
                     SensorName = "Batt_volt_Min")
   return(out)
   }
   
   if (grepl("PAR_Avg", sensor.name) == TRUE) {
   
   out <- data.frame(FullName = sensor.name,
                     SensorID = "1",
                     SensorName = "PAR_Avg")
   return(out)
   }
   
   if (grepl("SGA2_", sensor.name) == TRUE) {
   
      my.split.1 <- "SGA2_"  
      my.split.2 <- "_"
      
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
   out <- data.frame(FullName = sensor.name,
                     SensorID = sensor.id,
                     SensorName = sensor.name.no.id)
   return(out)
      }
      
 # IR sensors
   if (grepl("IR_", sensor.name) == TRUE) {
      if(grepl("Narrow", sensor.name) == TRUE) {
      my.split.1 <- "IR_Narrow_"
      my.split.2 <- "_"
      } 
      if(grepl("Horz", sensor.name) == TRUE) {
      my.split.1 <- "IR_Horz_"
      my.split.2 <- "_"
      }
      # split the sensor.name using the gathered my.split.x information
      splitted <- strsplit(sensor.name, split = my.split.1)
      #first.bit  <- unlist(lapply(splitted, "[", 1))
      first.bit <- my.split.1
      second.bit <- unlist(lapply(splitted, "[", 2))
      # split the second bit again to get to the sensor id
      sensor.id <- unlist(strsplit(second.bit, split = "_"))
      sensor.id <- unlist(lapply(strsplit(second.bit, split = my.split.2), "[", 2))
      sensor.desc <- unlist(lapply(strsplit(second.bit, split = my.split.2), "[", 1))
      # assemble sensor name
      sensor.name.no.id <- paste(first.bit, sensor.desc, sep = "_")
      sensor.name.no.id <- gsub("__", "_", sensor.name.no.id)
   
      # create output data frame
      out <- data.frame(FullName = sensor.name,
                        SensorID = sensor.id,
                        SensorName = sensor.name.no.id)
   return(out)
   }
   # Soil moisture
   if (grepl("Soil_", sensor.name) == TRUE) {
      my.split.1 <- "Soil_"
      my.split.2 <- "_"
       
      # split the sensor.name using the gathered my.split.x information
      splitted <- strsplit(sensor.name, split = my.split.1)
      #first.bit  <- unlist(lapply(splitted, "[", 1))
      first.bit <- my.split.1
      second.bit <- unlist(lapply(splitted, "[", 2))
      # split the second bit again to get to the sensor id
      sensor.id <- unlist(strsplit(second.bit, split = "_"))
      sensor.id <- unlist(lapply(strsplit(second.bit, split = my.split.2), "[", 2))
      sensor.desc <- unlist(lapply(strsplit(second.bit, split = my.split.2), "[", 1))
      # assemble sensor name
      sensor.name.no.id <- paste(first.bit, sensor.desc, sep = "_")
      sensor.name.no.id <- gsub("__", "_", sensor.name.no.id)
   
      # create output data frame
      out <- data.frame(FullName = sensor.name,
                      SensorID = sensor.id,
                      SensorName = sensor.name.no.id)
   return(out)
   }
   if (grepl("RECORD", sensor.name) == TRUE) {
   out <- data.frame(FullName = "RECORD",
                     SensorID = "1",
                     SensorName = "RECORD")
   return(out)
   }
}

# test code
#out <- GetSensorID("Kshapp_SGA2_1_Avg")
#GetSensorID("IR_Narrow_Std_1_")
#GetSensorID("Soil_Avg_2_")
#GetSensorID("Batt_volt_Min")
#GetSensorID("RECORD")
#my.out <- lapply(sensor.names, GetSensorID)
#do.call(rbind, my.out)

CampbellAllImport <- function(logger.folder   = "~/AgFace/2015/Campbell_logger/logger_data",
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

# example code
# df <- CampbellAllImport(log.interval = "5Min")

# Function to plot the last x hours
MyRecentPlot <- function(para, hours, data, logger = NA, yscale_min = NA, yscale_max = NA, cartesian = TRUE, sensor.colour = FALSE) {
    # function to plot a specific parameter for the last x hours
    
    # determine if all logger data should be used or only one specific logger
    if (is.na(logger) == TRUE){
        # do nothing
    } else {
       # subset data to use the named system only
       print(logger)
       data <- data[data$SYSTEM == logger, ]
    }
    
    # filter data to only include the valid SensorIDs vor this Sensor type. 
    # This is to reduce the amount of SensorIDs in the figure legends
    #data <- data[, names(data) %in% c("SYSTEM", "TIMESTAMP", "SensorID", para)]
    
    # determine valid and unique SensorIDs for the chosen parameter
    # get SensorIDs for non-NA entrys in the para column
    # only works if there is a SensorID available
    if ("SensorID" %in% names(data) ) {
    my.nas <- is.na(data[, which(names(data) == para)])
    used.IDs <- unique(data$SensorID[my.nas == FALSE])

    data <- data[data$SensorID %in% used.IDs, ]
    }
    # determine time frame to display
    my.time    <- hours * 60 * 60 # conversion from hours to seconds
    last.time  <- max(data$TIMESTAMP, na.rm = TRUE)
    start.time <- last.time - my.time
    
    my.data <- data[data$TIMESTAMP > start.time &
                    data$TIMESTAMP <= last.time, ]
    
    my.para <- which(names(data) == para)
    
    # get rid of Infinite data that mess up the determination of the scales
    my.infinites <- is.infinite(my.data[, my.para])
    my.data.clean <- my.data[which(my.infinites == FALSE), ]
    
    # calculate min and max values for the scaling
    if (is.na(yscale_min) == TRUE) {
	    my.max <- max(my.data.clean[, my.para], na.rm = TRUE)
	    my.min <- min(my.data.clean[, my.para], na.rm = TRUE)
	    } else {
	    my.max <- yscale_max
	    my.min <- yscale_min
	    }
    
    # put the figure together
    p <- ggplot(data, aes_string(x = "TIMESTAMP", y = para))
      p <- p + annotate("rect", 
          xmin = ephemeral.times$sunset[1:length(ephemeral.times$sunrise) - 1], 
          xmax = ephemeral.times$sunrise[2:length(ephemeral.times$sunrise)], 
          ymin = my.min, ymax = my.max,
          fill = "grey", alpha = 0.1)
      
      if (isTRUE(sensor.colour) == FALSE) {
      p <- p + geom_line()
      } else {
      p <- p + geom_line(aes(colour = SensorID))
      }
      if (isTRUE(cartesian)) {
      p <- p + coord_cartesian(xlim = c(start.time, last.time),
                               ylim = c(my.min, my.max))
      } else {
      p <- p + coord_cartesian(xlim = c(start.time, last.time))
      p <- p + scale_y_continuous(limits = c(my.min, my.max))
      }
      p <- p + labs(y = para)
      p <- p + facet_grid(SYSTEM ~ ., scales = "free_y")
      p <- p + theme_bw()
    return(p)
}
# example
# MyRecentPlot("Qr_SGA2_1_Avg", my.time.to.plot, df, yscale_min = NA, yscale_max = NA)

# figure for Ksh
MyKshPlot <- function(data, date = Sys.time(), para = "Kshapp_Avg", ylim = c(0, 1)) {
     require(ggplot2)
     
    # filter data to only include the valid SensorIDs vor this Sensor type. 
    # This is to reduce the amount of SensorIDs in the figure legends
        
    # determine valid and unique SensorIDs for the chosen parameter
    # get SensorIDs for non-NA entrys in the para column
    if ("SensorID" %in% names(data) ) {
        my.nas <- is.na(data[, which(names(data) == para)])
        used.IDs <- unique(data$SensorID[my.nas == FALSE])

        data <- data[data$SensorID %in% used.IDs, ]
     }
     
     # assemble start and end time, based on given POSIXct time
     my.start <- paste(as.Date(date), "04:00:00", sep = " ")
     my.start <- as.POSIXct(my.start)
     my.end   <- paste(as.Date(date), "06:00:00", sep = " ")
     my.end   <- as.POSIXct(my.end)
     
     # create the figure
     p <- ggplot(data, aes(x = TIMESTAMP, y = Kshapp_Avg))
       p <- p + geom_line(aes(colour = SensorID))
       p <- p + facet_grid(SYSTEM ~ .)
       p <- p + theme_bw()
       p <- p + coord_cartesian(xlim = c(my.start, my.end),
                                ylim = ylim)
     return(p)
}
