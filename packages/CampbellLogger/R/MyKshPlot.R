#' Visualise the Sap flow paramter Ksh for a given date
#'
#' @description Version of MyRecentPlot with changes specific to visualising the thermal conductance constant \code{KSh} for a given spa flow gauge installation. Visualises Ksh over the time from 4:00 to 6:00 hrs for the given date with a y-axis range from 0 to 1.
#' @param data Data frame name with the data
#' @param date Character string in POSIXct format (YYYY-mm-dd). Defaults to the current date.
#' @param para The parameter to visualise. Defaults to "Kshapp_Avg".
#' @param ylim y-axis range as vector. Defautls to \code{c(0, 1)}
#' @return Returns a ggplot object.

MyKshPlot <- function(data, date = Sys.time(), para = "Kshapp_Avg", ylim = c(0, 1)) {
     #require(ggplot2) # will be loaded when the whle package is loaded
     
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
     p <- ggplot2::ggplot(data, ggplot2::aes(x = TIMESTAMP, y = Kshapp_Avg))
       p <- p + ggplot2::geom_line(aes(colour = SensorID))
       p <- p + facet_grid(SYSTEM ~ .)
       p <- p + ggplot2::theme_bw()
       p <- p + ggplot2::coord_cartesian(xlim = c(my.start, my.end),
                                ylim = ylim)
     return(p)
}
