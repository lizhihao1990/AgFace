# package building
library(devtools)
library(roxygen2)

devtools::create("path/to/package/pkgname")
# got to package folder (folder with the DESCRIPTION file)
setwd("/home/loewi/AgFace/R_scripts/packages/GlasshouseClimateImport")
setwd("/home/loewi/AgFace/R_scripts/packages/CampbellLogger/")
devtools::document()
devtools::check()
devtools::load_all()
devtools::build()
# do last step as root
devtools::install()
