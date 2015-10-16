#' import a Campbell logger *.dat file
#' @param file filename of the dat file
#' @param time.zone Time zone, defaults to "Australia/Melbourne"
#' @param checkduplicates Check for and remove duplicates from the file. Logical, defaults to TRUE.
#' @param skip.rows Number of rows of the start of the file to discard. This si to speed up data import by reducing number of samples. Numeric, defaults to NA.
#' @return data frame with imported *.dat file

CampbellFileImport <- function(file, 
                               time.zone = "Australia/Melbourne", 
                               checkduplicates = TRUE,
                               skip.rows = NA) {
  # require(readr) # faster import, but problems whith coding "NA" 
  # imports a single Campbell file as per given filename
  # converts the TIMESTAMP to POSIXct, given the provided time zone
  # returns a data frame
  
  my.header   <- read.csv(file, skip = 1, nrows = 4)
  my.header   <- names(my.header)
  my.descript <- read.csv(file, skip = 3, nrows = 4)
  my.descript <- names(my.descript)

# only import last rows as defined by parameter lrows
  # number of rows to skip for the actual file import
  to.skip <- 4
  if (!is.na(skip.rows)) {
     base.skip <- 4
     to.skip <- base.skip + skip.rows
  }

  df <- read.csv(file, skip = to.skip, na.strings = "NAN")

  names(df) <- my.header
  names(df) <- gsub("\\.", "_", names(df))
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = time.zone)
  
  # get rid of rows without TIMESTAMP - not sure why these exist
  df <- df[!is.na(df$TIMESTAMP), ]
  
  # get rid of duplicate samples
  df$RECORD <- 0
  if (isTRUE(checkduplicates) == TRUE) {
	  df <- unique(df)
  }
  
  return(df)
}
