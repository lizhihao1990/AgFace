GlasshouseClimateImport
=======================

R-package to import, and merge Creswick glasshouse *.dat files

See 

	help(package = GlasshouseClimateImport) 

for details on the functions provided by this package.

### Installation
To enable installations of packages straight from github use:
```{r}
install.packages("devtools", dep = TRUE)
```

Then
```{r}
install_github("MarkusLoew/AgFace/packages/GlasshouseClimateImport")
```

### Example session

```{r}
# test code for package GlasshouseClimateImport

# load library
library(GlasshouseClimateImport)

# set the working directory to the folder with the files
# setwd("~/AgFace/2015/Glasshouses")

# import and merge all DAT files from the folder
df <- GlasshouseFolderImport()
        
# to visualise the data 
library(reshape2)
# re-melting the data
my.data.all.melt <- melt(df,
                         id.vars = "TIME")

# create figures for all parameters
library(ggplot2)
MyPlotFunction <- function(data) {
    my.name <- sort(unique(data$variable))
    p <- ggplot(data, aes(x = TIME, y = value))
    p <- p + geom_line()
    p <- p + labs(y = my.name)
    p <- p + theme_bw()
    return(p)
}

library(plyr)
my.plots <- dlply(my.data.all.melt,
                .(variable),
	         function(x) MyPlotFunction(x))

# put all figures into a pdf file
pdf(file = "Glasshouse_figures.pdf", width = 9, height = 9)
print(my.plots)
dev.off()
```
