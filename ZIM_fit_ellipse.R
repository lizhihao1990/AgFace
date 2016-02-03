# test case for fitting an ellipse to ZIM sensor data

library(conicfit)
library(ggplot2)

# green leaf 3, August 8
test.data <- ZIM.hour[ZIM.hour$Date == as.Date("2015-08-08") &
                      ZIM.hour$Subject == "Green leaf 3", ]

xy <- test.data[, c("temp.hour", "turgor.hour")]
xy.mat <- as.matrix(na.omit(xy))

ellipDirect <- EllipseDirectFit(xy.mat)

ellipDirectG <- AtoG(ellipDirect)$Par
xyDirect <- calculateEllipse(ellipDirectG[1], 
                            ellipDirectG[2], 
                            ellipDirectG[3], 
                            ellipDirectG[4], 
                            180/pi * ellipDirectG[5])

p <- ggplot(xy)                            
  p <- p + geom_point(aes(x = temp.hour, y = turgor.hour))
  p <- p + geom_path(data = as.data.frame(xyDirect), aes(x = V1, y = V2), colour = "red")
  p <- p + theme_my
  p <- p + labs(y = "Mean hourly turgor, rescaled to daily min and max value [dimensionless]",
                x = expression("Temperature ["~phantom()*degree~C~"]"),
                colour = "Hour of day")
p
