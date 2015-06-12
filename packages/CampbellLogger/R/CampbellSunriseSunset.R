#' Calculate sunrise and sunset times based on location and date for Campell *.dat files
#'
#' @description Calculates sunrise and sunset for each unique day from the \code{TIMESTAMP} information of the logger file. Uses function \code{sunriseset} from package \pkg{maptools}
#' @param data Data frame with a \code{TIMESTAMP} entry
#' @param location Location of the measurement for which to calculate sunrise and sunset. Given in matrix format containing x and y coordinates. Defaults to \code{Agface.loc}.
#' @param DayLigthSaving Logical. If \code{TRUE} one hour is subtracted from the calculated sunrise and sunset times.
#' @return Returns a data frame with Date and sunrise/sunset information for each \code{TIMESTAMP}.
#' @seealso \code{\link{maptools}}

CampbellSunriseSunset <- function(data, location = Agface.loc, DayLightSaving = FALSE) {
#require(maptools) # provides functions "sun-methods" # will be loaded when package is loaded
# Agface field site position
# -36.751367, 142.114477

# Creswick position
# -37.423003, 143.900472
Sys.setenv(TZ='Australia/Melbourne')

# provide Agface locations
# Agface.loc <- matrix(c(142.114477, -36.751367), nrow = 1)
# Creswick.loc <- matrix(c(143.900472, -37.423003), nrow = 1)

# one hour expressed in seconds
my.hour <- 60 * 60

# function to correct for Daylight Saving
NoDaylightSaving <- function(my.time) {
  my.hour <- 60 * 60
  out.time <- my.time - my.hour
}

CalcSunriseSunset <- function(date, spatial.loc = location) {
   # calculate sunrise and sunset for a location
   # requires maptools
   # require(maptools)
   spatial.loc <- sp::SpatialPoints(spatial.loc,
                                proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
   my.sunrise <- maptools::sunriset(spatial.loc, 
                          date, 
                          direction = "sunrise", 
                          POSIXct.out = TRUE)
   my.sunset  <- maptools::sunriset(spatial.loc, 
                          date, 
                          direction = "sunset",  
                          POSIXct.out = TRUE)
   # strip decimal information
   my.sunrise <- my.sunrise$time
   my.sunset  <- my.sunset$time
   
   sunrise <- my.sunrise
   sunset  <- my.sunset
   
   out <- data.frame(sunrise = sunrise,
                     sunset  = sunset)
     
   return(out)
}

# calculate the sunrise and sunset times for each day in the data frame
my.calendar.days <- format(data$TIMESTAMP, "%Y-%m-%d")
my.calendar.days <- as.POSIXct(unique(my.calendar.days))

my.calendar.days <- data.frame(Date = my.calendar.days)

ephemeral.times <- plyr::ddply(my.calendar.days,
                        plyr::.(Date),
                        .progress = "text",
                        function(x) CalcSunriseSunset(x$Date, 
                                     spatial.loc = location))

# correct for daylight saving if needed
if (isTRUE(DayLightSaving) == TRUE) {
# not needed any more after 
	ephemeral.times$sunrise <- ephemeral.times$sunrise - my.hour
	ephemeral.times$sunset  <- ephemeral.times$sunset  - my.hour
}

return(ephemeral.times)
}
