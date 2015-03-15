# Irrigation characterisation for Tillering analysis 2011/2012

library(reshape)
library(plyr)
library(ggplot2)

source("~/AgFace/R_scripts/Met_Irrigation_import.R")

setwd("~/AgFace/Topics/Met_Irri")

# import daily weather data 2011 and 2012
load("~/AgFace/Weather/Daily_weather_2011_2012.RData")
daily_weather_2011_2012$Date <- as.POSIXct(daily_weather_2011_2012$Date, 
                                           tz = "Australia/Melbourne")
daily_weather_2011_2012$Year <- as.numeric(format(daily_weather_2011_2012$Date, "%Y"))

# use a simpler name
df <- daily_weather_2011_2012

# limit data to 2011 & 2012, Horsham and Wheat
df <- df[(df$Year == 2011 | df$Year == 2012) & 
          df$Location == "Horsham" & 
          df$Species == "Wheat", ]

# import Stage-dates
stage_dates <- read.csv("~/AgFace/2012/Stage_dates.csv")

stage_dates$DC90_Date <- as.character(stage_dates$DC90_Date)
stage_dates$DC65_Date <- as.character(stage_dates$DC65_Date)

# missing stage dates for DC90 all cultivars in 2012
# missing stage date for DC65 2011 cultivar SB062
# using corresponding dates from the other year for now
stage_dates$DC90_Date[1:4] <- rep(as.character("05/12/2012"), 4)
stage_dates$DC90_Date[9:12] <- rep(as.character("05/12/2012"), 4)
# using other SB cultivar
stage_dates$DC65_Date[stage_dates$Cultivar == "SB062" & stage_dates$Year == 2011] <- "17/10/2011"

# convert to date
stage_dates[, 4:6] <- lapply(stage_dates[, 4:6], 
                             function(x) as.POSIXct(as.character(x), 
                                         format = "%d/%m/%Y", 
                                         tz = "Australia/Melbourne"))
stage_dates.melt <- melt(stage_dates,
                         id = c("Cultivar", "Irrigation", "Year"))

df$Irrigation <- gsub("Supp", "Sup", df$Irrigation)
df$Irrigation <- as.factor(df$Irrigation)

# create a table with season start and end dates
start_end_season <- df[!is.na(df$Sowing.date) &
                       !is.na(df$Harvest.date) &
                       df$Location == "Horsham" &
                       df$Species == "Wheat", ]

# create environments
df$Environment <- interaction(df$Year, df$Irrigation)

Env.char <- ddply(df,
                  .(Environment, Irrigation),
                  summarise,
                  Rainfall = sum(In.season.Rainfall..mm., na.rm = TRUE),
                  Irrigation_added = sum(Irrigation.amts..mm., na.rm = TRUE))

Env.char$Water_received <- Env.char$Rainfall + Env.char$Irrigation_added

# In-crop water reported by Mitchell et al. 2012 Crop & Pasture Science
# Table 1, summarising pre-and post anthesis
In_crop_rainfall.pre <- c(193, 354, 414, 185, 62, 23, 104, 152, 77, 104, 152, 77, 104, 104, 54, 61, 107)

# Zeros repsent the cases without separate pre-post anthesis rainfall. Might be better to have those as NA...
In_crop_rainfall.post <- c(27, 0, 0, 120, 58, 31, 13, 0, 16, 17, 0, 16, 17, 17, 107, 71, 10)

Mitchell.In_crop.rainfall <- In_crop_rainfall.pre + In_crop_rainfall.post
Mitchell.mean <- mean(Mitchell.In_crop.rainfall)
Mitchell.sd   <- sd(Mitchell.In_crop.rainfall)

# Mitchell et al. (2013) JExpBot
# irrigated treatment received 171 mm of rainfall during season
Mitchell2013 <- 171

Horsham.annu.rainfall.2011 <- 507
Horsham.annu.rainfall.2012 <- 287

p <- ggplot(Env.char, aes(x = Environment, y = Water_received))
  p <- p + geom_bar(stat = "identity", fill = "#003366")
  p <- p + geom_text(aes(y = Water_received + 8, label = Water_received))
#  p <- p + geom_hline(yintercept = Mitchell.mean, colour = "red")
#  p <- p + geom_hline(yintercept = Mitchell.mean - Mitchell.sd, 
#                      colour = "grey", linetype = "dashed")
#  p <- p + geom_hline(yintercept = Mitchell.mean + Mitchell.sd, 
#                      colour = "grey", linetype = "dashed")
#  p <- p + geom_hline(yintercept = Mitchell2013, 
#                      colour = "green", linetype = "solid")
#  p <- p + annotate("text", y = 325, x = 0.5, 
#                    label = "Mitchell et al. (2012): 167 mm, red", 
#                    colour = "red", hjust = 0, size = 3)
#  p <- p + annotate("text", y = 310, x = 0.5, 
#                    label = "Mitchell et al. (2012): SD, grey", 
#                    colour = "grey", hjust = 0, size = 3)
#  p <- p + annotate("text", y = 295, x = 0.5, 
#                    label = "Mitchell et al. (2013): 171 mm, green", 
#                    colour = "green", hjust = 0, size = 3)
#  p <- p + annotate("text", y = 280, x = 0.5, 
#                    label = "Average annual rainfall for wheat worldwide:\n275 to 700 mm ", 
#                    colour = "black", hjust = 0, size = 4)
#  p <- p + annotate("text", y = 265, x = 0.5, 
#                    label = "Long-term annual rainfall for Horsham: ~450 mm ", 
#                    colour = "black", hjust = 0, size = 3)
#  p <- p + annotate("text", y = 250, x = 0.5, 
#                    label = "Annual rainfall Horsham 2011: 507 mm BOM shows 552 mm", 
#                    colour = "black", hjust = 0, size = 3)
#  p <- p + annotate("text", y = 235, x = 0.5, 
#                    label = "Annual rainfall Horsham 2012: 287 mm, no BOM data yet", 
#                    colour = "black", hjust = 0, size = 3)
# size = theme_get()$text[["size"]])
  p <- p + labs(y = "Water received during season [mm]")
  p <- p + theme_bw()
  p <- p + theme(panel.grid.major.x = element_blank(),
                 panel.grid.major.y = element_blank())
p

ggsave(file = "Environment_rainfall.pdf",
       width = 7, height = 5)

# calculate amount of water received up until each stage date
stage_water <- ddply(stage_dates.melt,
                     .(Cultivar, Irrigation, Year, variable, value),
               function(x) {
               current_date <- unique(x$value)
               current_year <- unique(x$Year)
               current_Irri <- unique(x$Irrigation)
               current_sowing <- start_end_season$Sowing.date[start_end_season$Year == current_year & start_end_season$Irrigation == current_Irri]
               print(current_sowing)
               current_harvest <- start_end_season$Harvest.date[start_end_season$Year == current_year & start_end_season$Irrigation == current_Irri]
               #print(current_harvest)
               irri <- df[df$Irrigation.dates <= current_date &
                          df$Year == current_year &
                          df$Irrigation == current_Irri, ]
               rain <- daily_weather_2011_2012[daily_weather_2011_2012$Date <= current_date &
                                               daily_weather_2011_2012$Date >= current_sowing &
                                               daily_weather_2011_2012$Date <= current_harvest &
                                               daily_weather_2011_2012$Year == current_year, ]
               
               water_received <- sum(rain$RainTot..mm., na.rm = TRUE) + 
                                 sum(irri$Irrigation.amts..mm., na.rm = TRUE)
               })

stage_water$variable <- gsub("_Date", "", stage_water$variable)
stage_water$variable <- as.factor(stage_water$variable)

# export the water amount per stage
save(stage_water, file = "Water_received_per_Stage_2011_2012.RData", compress = TRUE)
