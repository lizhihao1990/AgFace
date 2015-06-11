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

#' Identify the SensorID associated with a specific sensor
#'
#' @param sensor.name Fullname of the Sensor from the header of the dat file. Has to be part of the list \code{sensor.names}
#' @return Data frame with Fullname, SensorId, and Sensor name 

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
