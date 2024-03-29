# import an Excel sheet into R

# The recommended way is still to save the sheet as .csv (comma separated values)  # file and then import the csv file via 
# df <- read.csv("filename.csv", header = TRUE)

# Import Excel sheet directly via package "xlsx". 
# Depends on java to import the file.

# install the package if it does not exist yet
install.packages("xlsx", dep = TRUE)

# load the package
require(xlsx)

# set the working directory
setwd("C:/Path/to/my/files/")

# importing the actual sheet from the specified Excel file from the directory that # was specified above into a data frame
# you have to specify the name of the sheet, or give the number of the sheet
# see ?xlsx
df <- read.xlsx("filename.xlsx", 
                sheetName = "Final")

# import can be very slow for large files,
# as Excel does not specify the type (numeric, integer, date, factor, ...) 
# of each column and R has to do the guesswork.
# Enjoy!
