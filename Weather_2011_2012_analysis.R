# daily weather data analysis 2011 and 2012

# uses existing weather data workspace from script "Weather_data_2011_2012.R

setwd("~/AgFace/Weather/2011_2012")

load("../../Topics/Met_Irri/Meteo_Irrigation_info.RData")
load("../Daily_weather_2011_2012.RData")

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

df <- daily_weather_2011_2012

df$Year <- format(df$Date, "%Y")
df$DOY <- as.numeric(as.character(format(df$Date, "%j")))

# principal_dates 2011 and 2012
# sowing/harvest dates are the same for rainfed and supplemental water treatments
principal.dates <- read.csv("../../2011_2012/Principal_dates/Principal_dates.csv")
principal.dates$Date <- as.POSIXct(principal.dates$Date, tz = "Australia/Melbourne")
principal.dates$Year <- as.numeric(as.character(format(principal.dates$Date, "%Y")))
principal.dates$DOY <- as.numeric(as.character(format(principal.dates$Date, "%j")))

# for this excercise, we only keep 'tin' related cultivars
principal.dates.tin <- principal.dates[principal.dates$Cultivar != "SB 062", ]
principal.dates.tin <- principal.dates.tin[!(principal.dates.tin$Event == "DC65" &
                                         principal.dates.tin$Cultivar == "remaining"), ]

principal.dates.tin$Event <- factor(principal.dates.tin$Event,
                                    levels = c("Sowing", "DC31", "DC65", "Harvest"))



# Water for different periods
mi <- Met.Irri[Met.Irri$Year == 2011 | Met.Irri$Year == 2012, ]
mi <- mi[mi$Species == "Wheat", ]

# calculate irrigation amount for time between sowing and DC31
CalcIrri <- function(data, start.date, end.date) {
   #require(plyr)   
   my.data <- data[data$Irrigation.dates >= start.date &
                   data$Irrigation.dates <= end.date &
                   !is.na(data$Irrigation.dates) &
                   data$Irrigation.treatment == "Supplemental (Wheat)", ]
   Irrigation_amount <- sum(my.data$Irrigation.amts..mm., na.rm = TRUE)
   return(Irrigation_amount)
}

# from 2011 sowing to DC31
Irri.2011.sowing.DC31 <- CalcIrri(mi, 
               start.date = principal.dates.tin$Date[1],
               end.date = principal.dates.tin$Date[5])
# from 2011 sowing to DC65
Irri.2011.sowing.DC65 <- CalcIrri(mi, 
         start.date = principal.dates.tin$Date[1],
         end.date = principal.dates.tin$Date[6])
# from 2011 sowing to Harvest
Irri.2011.sowing.harvest <- CalcIrri(mi, 
         start.date = principal.dates.tin$Date[1],
         end.date = principal.dates.tin$Date[2])
# from 2011 DC65 to Harvest
Irri.2011.DC65.harvest <- CalcIrri(mi, 
         start.date = principal.dates.tin$Date[6],
         end.date = principal.dates.tin$Date[2])

# from 2012 sowing to DC31
Irri.2012.sowing.DC31.SSRT65 <- CalcIrri(mi, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[7])
Irri.2012.sowing.DC31.all <- CalcIrri(mi, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[8])
Irri.2012.sowing.DC65.all <- CalcIrri(mi, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[10])
Irri.2012.sowing.harvest.all <- CalcIrri(mi, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[4])
# from 2012 DC65 to Harvest
Irri.2012.DC65.harvest <- CalcIrri(mi, 
         start.date = principal.dates.tin$Date[10],
         end.date = principal.dates.tin$Date[4])

# assemble Irigation information
my.Irrigation <- ls()[grep("^Irri", ls())]
my.Irrigation.list <- llply(my.Irrigation,
              function(x) get(x))
names(my.Irrigation.list) <- my.Irrigation
irrigation.per.period <-as.data.frame(my.Irrigation.list)
irrigation.per.period <- t(irrigation.per.period)
irrigation.per.period <- as.data.frame(irrigation.per.period)
names(irrigation.per.period) <- "Irrigation_received"

#sdepths <- strsplit(unique.depths, split = "-")
#ulimit <- as.numeric(unlist(lapply(sdepths, "[", 1)))

# calculate rainfall per period
CalcRain <- function(data, start.date, end.date) {
   data$Date <- as.POSIXct(data$Date, tz = "Australia/Melbourne")
   my.data <- data[data$Date >= start.date &
                   data$Date <= end.date, ]
   my.rain <- sum(my.data$RainTot..mm., na.rm = TRUE)
}

# for the whole year
Whole.Rain.2011.whole.year <- CalcRain(df, 
               start.date = as.POSIXct("2011-01-01", tz = "Australia/Melbourne"),
               end.date = as.POSIXct("2011-12-31", tz = "Australia/Melbourne"))
Whole.Rain.2012.whole.year <- CalcRain(df, 
               start.date = as.POSIXct("2012-01-01", tz = "Australia/Melbourne"),
               end.date = as.POSIXct("2012-12-31", tz = "Australia/Melbourne"))


# from 2011 sowing to DC31
Rain.2011.sowing.DC31 <- CalcRain(df, 
               start.date = principal.dates.tin$Date[1],
               end.date = principal.dates.tin$Date[5])
# from 2011 sowing to DC65
Rain.2011.sowing.DC65 <- CalcRain(df, 
         start.date = principal.dates.tin$Date[1],
         end.date = principal.dates.tin$Date[6])
# from 2011 sowing to Harvest
Rain.2011.sowing.harvest <- CalcRain(df, 
         start.date = principal.dates.tin$Date[1],
         end.date = principal.dates.tin$Date[2])
# from 2011 DC65 to Harvest
Rain.2011.DC65.harvest <- CalcRain(df, 
         start.date = principal.dates.tin$Date[6],
         end.date = principal.dates.tin$Date[2])

# from 2012 sowing to DC31
Rain.2012.sowing.DC31.SSRT65 <- CalcRain(df, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[7])
Rain.2012.sowing.DC31.all <- CalcRain(df, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[8])
Rain.2012.sowing.DC65.all <- CalcRain(df, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[10])
Rain.2012.sowing.harvest.all <- CalcRain(df, 
               start.date = principal.dates.tin$Date[3],
               end.date = principal.dates.tin$Date[4])
# from 2012 DC65 to Harvest
Rain.2012.DC65.harvest <- CalcRain(df, 
         start.date = principal.dates.tin$Date[10],
         end.date = principal.dates.tin$Date[4])

# assemble Rainfall information
my.Rain <- ls()[grep("^Rain", ls())]
my.Rainfall.list <- llply(my.Rain,
              function(x) get(x))
names(my.Rainfall.list) <- my.Rain
rainfall.per.period <-as.data.frame(my.Rainfall.list)
rainfall.per.period <- t(rainfall.per.period)
rainfall.per.period <- as.data.frame(rainfall.per.period)
names(rainfall.per.period) <- "Rainfall_received"

# combine rainfall and irrigation
water.per.period <- cbind(rainfall.per.period, irrigation.per.period)
water.per.period$Water_received <- water.per.period$Rainfall_received + 
                                   water.per.period$Irrigation_received
rownames(water.per.period) <- gsub("Rain\\.", "", rownames(water.per.period))
water.per.period$Period <- rownames(water.per.period)
water.per.period$Period <- sub("\\.", "_", water.per.period$Period)
water.per.period$Period <- sub("\\.", "_to_", water.per.period$Period)
water.per.period <- water.per.period[, c("Period", "Rainfall_received", "Irrigation_received", "Water_received")]

write.table(water.per.period,
            file = "Water_received_per_period_tin.csv",
            sep = ",", row.names = FALSE)

# create irrigation-date data frame
my.irri <- mi[, c("Year", "Irrigation.treatment", "Irrigation.amts..mm.", "Irrigation.dates")]
my.irri$Irrigation <- NA
my.irri$Irrigation[grep("Rainfed", my.irri$Irrigation.treatment)] <- "rainfed"
my.irri$Irrigation[grep("Supp", my.irri$Irrigation.treatment)] <- "supp"
my.irri$Irrigation <- as.factor(my.irri$Irrigation)

my.irri$DOY <- as.numeric(as.character(format(my.irri$Irrigation.dates, "%j")))

# add DC90 to harvest events in principal.dates
principal.dates$DCEvent <- principal.dates$Event
principal.dates$DCEvent <- gsub("Harvest", "DC90\nHarvest", principal.dates$DCEvent)
principal.dates$DCEvent <- gsub("DC65", "DC65\nAnthesis", principal.dates$DCEvent)
principal.dates$DCEvent <- gsub("DC31", "DC31\nElongation", principal.dates$DCEvent)
principal.dates$DCEvent <- factor(principal.dates$DCEvent,
                            levels = c("Sowing", 
                                       "DC31\nElongation", 
                                       "DC65\nAnthesis", 
                                       "DC90\nHarvest"))

# figures
p <- ggplot(df, aes(x = DOY, y = RainTot..mm.))
  p <- p + geom_bar(stat = "identity")
  p <- p + geom_point(data = my.irri, 
                     aes(x = DOY, y = Irrigation.amts..mm.), 
                     colour = "black", size = 0.9, shape = 6)
  p <- p + geom_vline(data = principal.dates.tin, 
                      aes(xintercept = DOY), colour = "grey", 
                      show_guide = TRUE)
  p <- p + geom_text(data = principal.dates[principal.dates$Year == "2011",],
                     aes(x = DOY, y = 78, label = DCEvent), hjust = 1, size = 3.8)
  p <- p + facet_grid(Year ~ .)
  p <- p + labs(y = "Rainfall, irrigation [mm]",
                x = "Day of year")
  p <- p + theme_bw()
  p <- p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 #legend.position = c(0.3, 0.8),
                 legend.position = "none",
                 axis.text.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.y = element_text(vjust = 1),
                 strip.background = element_rect(fill = "white"),
                 plot.margin = unit(c(1, 1, 0, 0.5), "lines")) # third number is bottom
p

fig.rainfall <- p

# Temperature
df.no.outliers <- df

# get rid of temperature outliers
df.no.outliers$Min.AirTemp..degC.[df.no.outliers$Min.AirTemp..degC. < -39] <- NA
df.no.outliers$Max.AirTemp..degC.[df.no.outliers$Max.AirTemp..degC. > 60] <- NA

p <- ggplot(df.no.outliers, aes(x = DOY, y = Ave.AirTemp..degC.))
  p <- p + geom_vline(data = principal.dates.tin, 
                      aes(xintercept = DOY),
                      show_guide = FALSE, colour = "grey")
  p <- p + geom_ribbon(aes(ymin = Min.AirTemp..degC., 
                           ymax = Max.AirTemp..degC.), fill = "grey")
  p <- p + geom_line(colour = "black")

  #p <- p + scale_colour_discrete(labels = c("minimum", "average", "maximum"))
  p <- p + geom_hline(yintercept = c(-2, 34), linetype = "dashed", 
                      colour = "grey", alpha = 0.66) # heat shock
  p <- p + coord_cartesian(ylim = c(-5, 45))
  #p <- p + scale_y_continuous(limits = c(-10, 60))
  p <- p + facet_grid(Year ~ .)
  p <- p + theme_bw()
  p <- p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.title.y = element_text(vjust = 1),
                 strip.background = element_rect(fill = "white"),
                 plot.margin = unit(c(0, 1, 0.5, 0.5), "lines"))
  p <- p + labs(y = "Air temperature [°C]",
                x = "Day of year",
                colour = "Temperature (daily)")
p
fig.temperature.ribbon <- p

#p <- p + annotation_custom(grob = textGrob("Hi"), 
#                  xmin = 3, xmax = 6, ymin = 2, ymax = 8)
#p

library(gridExtra)
g <- arrangeGrob(p, 
           sub = textGrob("B", x = 0.01, y = 15.5, hjust = 0, vjust=0, 
                          gp = gpar(fontface = "bold", fontsize = 18)))
g

fig.temperature.ribbon <- g
               

df.melt <- melt(df,
                id.vars = c("Date", "Year", "DOY"))

p <- ggplot(df.melt[df.melt$variable == "Max.AirTemp..degC." |
                    df.melt$variable == "Ave.AirTemp..degC." |
                    df.melt$variable == "Min.AirTemp..degC.", ],
            aes(x = DOY, y = value) )
  p <- p + geom_vline(data = principal.dates.tin, 
                      aes(xintercept = DOY, fill = Event), colour = "grey",
                      show_guide = FALSE)
  p <- p + geom_line(aes(colour = variable))
  p <- p + scale_colour_discrete(labels = c("minimum", "average", "maximum"))
  p <- p + geom_hline(yintercept = c(-2, 34), linetype = "dashed", 
                      colour = "grey", alpha = 0.66) # heat shock
  p <- p + coord_cartesian(ylim = c(-10, 60))
  p <- p + facet_grid(Year ~ .)
  p <- p + theme_bw()
  p <- p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.title.y = element_text(vjust = 1),
                 legend.position = c(0.5, 0.33),
                 legend.background = element_blank(),
                 strip.background = element_rect(fill = "white"),
                 plot.margin = unit(c(0, 1, 0.5, 0.5), "lines")) # third number is bottom)
  p <- p + labs(y = "Air temperature [°C]",
                x = "Day of year",
                colour = "Temperature (daily)")
p

fig.temperature.lines <- p

# assembling the combined rainfall and temperature figure
a <- ggplotGrob(fig.rainfall)
# adding panel indicators
a <- gtable_add_grob(a, 
            grobTree(textGrob("A", x=-0.042, hjust=0)), 
            t=1, l=4, clip = "off") # clip has to be off or has to be set to inherit

b <- ggplotGrob(fig.temperature.ribbon)
b <- gtable_add_grob(b, 
            grobTree(textGrob("B", x=-0.042, hjust=0)), 
            t=1, l=4, clip = "off") # clip has to be off or has to be set to inherit

c <- ggplotGrob(fig.temperature.lines)


pdf(file = "Rainfall_and_temperature_2011_2012.pdf", width = 13, height = 8)
grid.draw(rbind(a, b, size = "first"))
#grid.arrange(a, b, c, d, e, ncol = 1)
dev.off()
