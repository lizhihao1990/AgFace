# How to fit an ellipse to xy data
#
# using "conicfit" library
# conicfit: Algorithms for Fitting Circles, Ellipses and Conics Based on the Work by Prof. Nikolai Chernov
# Circular and Linear Regression
# Fitting Circles and Lines by Least Squares
# Nikolai Chernov
# CRC Press 2010
# Print ISBN: 978-1-4398-3590-6, eBook ISBN: 978-1-4398-3591-3
# http://www.crcnetbase.com.ezp.lib.unimelb.edu.au/isbn/9781439835913
# or
# http://cseweb.ucsd.edu/~mdailey/Face-Coord/ellipse-specific-fitting.pdf
# http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/PILU1/demo.html

# Markus Löw, February 2016

# check for or install conicfit package
if (isTRUE("conicfit" %in% installed.packages() == FALSE)) {
  message("Installing conicfit package")
  install.packages("conicfit")
}

# load conicfit
library(conicfit)

# create some data for an xy scatterplot
x <- rep(1:5, 2)
y <- c(1, 2.5, 3.8, 5, 6, 0.5, 0.8, 1, 2.6, 5.5)

# put data in a data frame
df <- data.frame(x = x, y = y)

# create a figure of the data
plot(y ~ x, data = df,
     xlim=c(0, 6),
     ylim=c(-1, 6))

# the ellipse-fitting algorithm requires a matrix as input
# does not like data frames
mat <- as.matrix(df)

# fit the ellipse to the data
# following the example in the helpfile for ?EllipseDirectFit
ellipDirect <- EllipseDirectFit(mat)

# function returns five algebraic parameters (a to f) that describe an ellipse
# ellipse equation is ax^2 + bxy + cy^2 +dx + ey + f = 0

# algebraic parameters need to be converted to geometric parameters
ellipDirectG <- AtoG(ellipDirect)$Par

# from the five parameters that describe the ellipse, calculate the corresponding ellipse coordinates
# returns by default 50 pairs of points along the calculated ellipse
# see ?calculateEllipse
# the five parameters describe the center point x, center point y, axis a, axis b, and tilt angle
xyDirect <- calculateEllipse(x = ellipDirectG[1], 
                             y = ellipDirectG[2], 
                             a = ellipDirectG[3], 
                             b = ellipDirectG[4], 
                             angle = 180 / pi * ellipDirectG[5])

# add a second layer to the plot that was created previously
par(new = TRUE)
lines(xyDirect[,1],
      xyDirect[,2],
      col='red')

# other method: ellipse fit by Taubin
# Taubin is less robust for "bad" data
ellipTaubin <- EllipseFitByTaubin(mat)
ellipTaubinG <- AtoG(ellipTaubin)$ParG
xyTaubin <- calculateEllipse(ellipTaubinG[1], 
                             ellipTaubinG[2], 
                             ellipTaubinG[3], 
                             ellipTaubinG[4], 
                             180/pi*ellipTaubinG[5])

# add the Taubin ellipse to the plot
par(new = TRUE)
lines(xyTaubin[,1],
     xyTaubin[,2],
     col='green')
