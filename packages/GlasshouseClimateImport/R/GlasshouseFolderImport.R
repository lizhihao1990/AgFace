#' An import function for all DAT glasshouse climate files from a folder
#'
#' Author: Markus Löw
#' Date: May 2015

#' Imports and processes all Glasshouse DAT files from a folder. Uses \code{\link{GlasshouseFileImport}}.
#' @param folder foldername of the DAT files. Defaults to current folder.
#' @param glasshouse Mandatory argument. Either "PC2" or "teaching" to specify which glasshouse the data come from.
#' @return One data frame containing the data from all DAT files in the \code{foldername}


# function to import all DAT files from a folder
GlasshouseFolderImport <- function(folder = ".", glasshouse) {
 
 stopifnot(glasshouse %in% c("PC2", "teaching"))
 
 if (folder != ".") {
 # Get info on current folder
 cur.folder <- getwd()
 message("Switching to folder: ", folder) 
 
 # switch folder 
 setwd(folder)
 }
 # get a list of file names
 my.files.list <- list.files(pattern = "\\.DAT$")

 my.list <- lapply(my.files.list, function(x){GlasshouseFileImport(data = x, glasshouse = glasshouse)}) 
 names(my.list) <- my.files.list
 
 # merge all data frames
 merged.data <- do.call("rbind", my.list)
 
 # sort data based on date
 merged.data <- merged.data[with(merged.data, order(TIME)), ]
 
 
 # go back to previous folder
 if (folder != ".") {
 setwd(cur.folder)
 }
 
 return(merged.data)
}
