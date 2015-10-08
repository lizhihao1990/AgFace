HH2
==============

R-package to import DeltaT Moisture Meter files
http://www.delta-t.co.uk/product-display.asp?id=721&div=Soil%20Science&cat=Meters%20and%20Loggers

See 

	help(package = HH2) 

for details on the functions provided by this package.

### Installation

Installation straight from github via

```{r}
install_github("MarkusLoew/AgFace/packages/HH2")
```

Installation under Windows might require the installation of Rtools. There will be a prompt for it if needed.

### Example usage

```{r}
# using the HH2 library to import files

library(HH2)

df <- HH2Import("PR2_2015-09-06.csv", sensor.type = "PR2")

```
