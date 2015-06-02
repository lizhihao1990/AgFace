#' An import function for all DAT glasshouse climate files from a folder
#'
#' Author: Markus Löw
#' Date: May 2015
#' Imports and processes all Glasshouse DAT files from a folder.
#' @param foldername of the DAT files. Defaults to current folder.
#' @return One data frame containing the data from all DAT files in the foldername
#' @examples 
#' GlasshouseFolderImport()
#' GlasshouseFolderImport("C:/Mydata/Climate")


# function to import all DAT files from a folder
GlasshouseFolderImport <- function(folder = ".") {
 
 if (folder != ".") {
 # Get info on current folder
 cur.folder <- getwd()
 message("Switching to folder: ", folder) 
 
 # switch folder 
 setwd(folder)
 }
 # get a list of file names
 my.files.list <- list.files(pattern = "\\.DAT$")

 my.list <- lapply(my.files.list, GlasshouseFileImport) 
 names(my.list) <- my.files.list
 
 # merge all data frames
 merged.data <- do.call("rbind", my.list)
 
 
 # go back to previous folder
 if (folder != ".") {
 setwd(cur.folder)
 }
 
 return(merged.data)
}
