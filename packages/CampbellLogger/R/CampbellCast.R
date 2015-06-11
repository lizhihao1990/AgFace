# function to re-organise Campbell data
# returns data frame with values of all sensors of a given type in the same column
# identifies each value using SensorID crated from the Sensor number, SYSTEM name and TIMESTAMP

CampbellCast <- function(data) {
	require(plyr) # will be loaded when package is loaded
	# require(reshape2) # will be loaded when package is loaded
	
	# melt data into long format based on SYSTEM and TIMESTAMP
	df.melt <- reshape2::melt(data, id.vars = c("SYSTEM", "TIMESTAMP"))

	# remove RECORD id from the file, as there can be duplicates	
	df.melt <- df.melt[df.melt$variable != "RECORD", ]

        # using plyr to process SensorIDs
	my.names <- ddply(df.melt,
		        .(SYSTEM, variable),
		       function(x) GetSensorID(x$variable))
	my.names$SensorID <- as.factor(as.character(my.names$SensorID))
	
	# merge the sensor names and the melted data
	df.melt.merge <- merge(df.melt, my.names,
		               by.x = c("SYSTEM", "variable"),
		               by.y = c("SYSTEM", "variable"))

	df.melt.merge <- unique(df.melt.merge)
	df.melt <- df.melt.merge

	df.melt$variable <- NULL
	df.melt$FullName <- NULL

	# cast the merged data into wide format
	data.out <- reshape2::dcast(df.melt,
		      SYSTEM + TIMESTAMP + SensorID ~ SensorName)
        return(data.out)
}
