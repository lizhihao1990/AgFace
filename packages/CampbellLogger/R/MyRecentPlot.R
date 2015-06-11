#' Visualise recent data measured by an AgFace Campbell Scientific logger over a selected Time Period
#'
#' @description Visualise a selected parameter from a Campbell logger file for a given time window starting from the latest timestamp in the data frame. All sensors that measure this parameter on each logger system will be shown. Sensors are identified by their SensorID and logger system ID.
#' @param para Name of the parameter to be visualised. See \code{sensor.names} for a list of parameters. Works with non-missing data only.
#' @param hours Number of recent hours over which to visualise data. Starts from last timestamp in the data frame.
#' @param data name of the data frame.
#' @param logger Only visualise data from the specified logger system. Defaults to NA to use data from all logger systems.
#' @param yscale_min Numeric. Lower limit of the y-axis. Defaults to NA to visualise the full range of values.
#' @param yscale_max Numeric. Upper limit of the y-axis. Defaults to NA to visualise the full range of values.
#' @param cartesian Logical. Discard data outside of the specified y-axis range. Defaults to TRUE.
#' @param sensor.colour Logical. Identify each SensorID per logger system with a unique colour. Defaults to TRUE.
#' @param ephemeral.time Logical. If true, an object named \code{ephemeral.times}. Default is \code{TRUE}. The object ephemeral.times contains sunrise and sunset information as provided by the function \code{CampbellSunriseSunset}.

MyRecentPlot <- function(para, hours, data, logger = NA, yscale_min = NA, yscale_max = NA, cartesian = TRUE, sensor.colour = FALSE, ephemeral.time = TRUE) {
    # require(ggplot2) # will be loaded when the package is loaded
    
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
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "TIMESTAMP", y = para))
    
    if (isTRUE(ephemeral.time)) {
    p <- p + ggplot2::annotate("rect", 
          xmin = ephemeral.times$sunset[1:length(ephemeral.times$sunrise) - 1], 
          xmax = ephemeral.times$sunrise[2:length(ephemeral.times$sunrise)], 
          ymin = my.min, ymax = my.max,
          fill = "grey", alpha = 0.1)
     }
      
      if (isTRUE(sensor.colour) == FALSE) {
      p <- p + ggplot2::geom_line()
      } else {
      p <- p + ggplot2::geom_line(ggplot2::aes(colour = SensorID))
      }
      if (isTRUE(cartesian)) {
      p <- p + ggplot2::coord_cartesian(xlim = c(start.time, last.time),
                               ylim = c(my.min, my.max))
      } else {
      p <- p + ggplot2::coord_cartesian(xlim = c(start.time, last.time))
      p <- p + ggplot2::scale_y_continuous(limits = c(my.min, my.max))
      }
      p <- p + ggplot2::labs(y = para)
      p <- p + ggplot2::facet_grid(SYSTEM ~ ., scales = "free_y")
      p <- p + ggplot2::theme_bw()
    return(p)
}

