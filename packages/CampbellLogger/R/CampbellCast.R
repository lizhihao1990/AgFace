#' Function to re-organise Campbell data into a format suitable for creating figures per sensor type
#'
#' @description Merges sensor data from multiple sensors of the same type into one column. Identifies each value using SensorID created from the Sensor number, SYSTEM name and TIMESTAMP.
#' @param data Data frame with AgFace Campbell logger data that was imported by either \code{\link{CampbellAllImport}}.
#' @param use.parallel Logical. Option to enable parallel processing for the file import. Parallel-computing has to be configured before this option can be used. Defaults to FALSE. Unfortunately, it does not speed up the \code{joining} part of the data reorganisation.
#' @return data frame with values of all sensors of a given type in the same column

CampbellCast <- function(data, use.parallel = FALSE) {
	# require(plyr)     # will be loaded when package is loaded
	# require(reshape2) # will be loaded when package is loaded
	
	# determine the character-type parameters
	# remove them before reshaping to ensure all values are numeric
	my.characters <- sapply(data[, 1:ncol(data)], is.character)
	col.num <- which(my.characters == FALSE)
	to.remove <- names(my.characters)[which(my.characters == TRUE)]
	
	if (length(to.remove > 0)) {
		msg.text <- "Removing the following parameters, as they are of class character:"
		msg.text <- paste(msg.text, to.remove, sep = " ")
        	message(msg.text)
		data <- data[, col.num]
	}
		
	# melt data into long format based on SYSTEM and TIMESTAMP
	df.melt <- reshape2::melt(data, id.vars = c("SYSTEM", "TIMESTAMP"))

	# remove RECORD id from the file, as there can be duplicates	
	df.melt <- df.melt[df.melt$variable != "RECORD", ]

        # using plyr to process SensorIDs
        message("Create SensorIDs")
	my.names <- plyr::ddply(df.melt,
		        plyr::.(SYSTEM, variable),
		        .parallel = use.parallel,
		        .paropts = list(.export = c("x", "GetSensorID"),
                                        .packages = c("CampbellLogger")),
		       function(x) GetSensorID(x$variable))
	my.names$SensorID <- as.factor(as.character(my.names$SensorID))
	
	# merge the sensor names and the melted data
	# message("Merging")
	
       # df.melt.merge <- merge(df.melt, my.names,
       #	                by.x = c("SYSTEM", "variable"),
       #	                by.y = c("SYSTEM", "variable"))
        
        # using join from plyr package instead
        # fast than using "merge"
        message("Joining data, be patient.")
        #print(system.time(
        df.melt.merge <- plyr::join(df.melt, my.names,
                              by = c("SYSTEM", "variable"),
                              match = "first")
        #))
        # message("Get rid of duplicates")
	df.melt.merge <- unique(df.melt.merge)
	df.melt <- df.melt.merge

	df.melt$variable <- NULL
	df.melt$FullName <- NULL
        #print(names(df.melt))
	# cast the merged data into wide format
	data.out <- reshape2::dcast(df.melt,
		      SYSTEM + TIMESTAMP + SensorID ~ SensorName)
        return(data.out)
}
