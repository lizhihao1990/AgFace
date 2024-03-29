# Import and process daily weather data

# load lubridate library
library(lubridate)

# set working directory
setwd("~/AgFace/Weather")

# load workspace
load("Daily_weather_2011_2012.RData")

df <- daily_weather_2011_2012

# create a year-indicator
df$Year <- format(df$Date, "%Y")
df$Year <- as.factor(df$Year)
df$DOY <- format(df$Date, "%j")
# common year
# shift the year 2011 up by one year
df$commondate <- df$Date


df$commondate[df$Year == "2011"] <- df$commondate[df$Year == "2011"] +years(1)

#par(mar=c(1,5,1,6))

pdf(file = "Weather_2011_2012.pdf",
    width = 11, height = 7)
par(mar=c(5,5,1,6))
plot(Ave.AirTemp..degC. ~ commondate, 
     data = df[df$Year == "2011", ],
     type = "l",
     ylab = "Daily mean air temperature [°C]",
     xlab = "Date in 2011 or 2012")
lines(Ave.AirTemp..degC. ~ commondate, 
     data = df[df$Year == "2012", ],
     type = "l",
     lty = "dashed")
# seasonal indicator
arrows(as.Date("2012-05-25"), 33, as.Date("2012-12-5"), 33, 
       length = 0, lwd = 2) # 2011
arrows(as.Date("2012-05-30"), 34, as.Date("2012-12-5"), 34, 
       length = 0, lty = "dashed", lwd = 2) # 2012

legend(as.Date("2012-12-08"), 34, c("2011", "2012"),
	bty = "n", # draw no box
	lty = c(1, 2), # gives the legend appropriate symbols (lines
	cex = 0.8
	)

# temperature df
meantemp <- df[, c("Year", "DOY", "commondate", "Ave.AirTemp..degC.")]
meanyear <- merge(meantemp[meantemp$Year == "2011",],
                  meantemp[meantemp$Year == "2012",],
                  by = c("DOY"))
meanyear$AveTDiff <- meanyear$Ave.AirTemp..degC..x - meanyear$Ave.AirTemp..degC..y

# draw reference line at 20°C
abline(a = 20, b = 0, col ="red", lty = "dashed")
lines(AveTDiff  + 20 ~ commondate.x, 
     data = meanyear,
     type = "l",
     col = "red",
     lwd = 1)

# rainfall df
rain <- df[, c("Year", "commondate", "RainTot..mm.")]

rainyear <- data.frame(commondate = rain$commondate[rain$Year == "2012"],
                       x.2011 = c(rain$RainTot..mm.[rain$Year == "2011"], NA),
                       x.2012 = rain$RainTot..mm.[rain$Year == "2012"])

# correct for additional day in 2012
after.gap <- rainyear$x.2011[rainyear$commondate > as.Date("2012-02-28")]
rainyear$x.2011[rainyear$commondate == as.Date("2012-02-29")] <- NA
rainyear$x.2011[rainyear$commondate > as.Date("2012-02-29")] <- after.gap[1:306]

par(new = TRUE)
barplot(as.matrix(t(rainyear[, 2:3]), 
        names.arg = as.character(rainyear$commondate)),
        axes = FALSE,
        #col = c("black", "white"),
        add = FALSE)
axis(side = 4)
mtext("Rainfall [mm]", side = 4, padj = 4)
dev.off()
