# Import data in folder "~/AgFace/2010/Data_files_for_Markus" from Sabine

# set working directory
setwd("~/AgFace/2010/Data_files_for_Markus")

# load packages
require(gnumeric)
require(xlsx)


# import data from file "Zebu-Janz     dry weight-width.xlsx"
file_to_import <- "Zebu-Janz_dry_weight-width.xlsx"
dry_weight_width <- read.xlsx(file_to_import, 
                              1)
                              
df <- read.gnumeric.sheet(file = "Zebu-Janz_dry_weight-width.ods", 
                          head = TRUE, 
                          sheet.name = "Sheet1")

