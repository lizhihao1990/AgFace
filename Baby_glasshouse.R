# Visualise the CO2 concentrationover time in the baby glasshouse

library(ggplot2)

setwd("~/AgFace/2013/Baby_Glasshouse")

# Days the test was conducted
Day01 <- "2013-09-04"
Day02 <- "2013-09-05"
Day03 <- "2013-09-06"
Day04 <- "2013-09-09"
Day05 <- "2013-09-10"
Day06 <- "2013-09-17"

df01 <- read.csv("2013_09_04_Glasshouse_log_01.csv",
               header = TRUE,
               skip = 14)
df01$Day <- Day01
df01$Date  <- paste(df01$Day, df01$HHMMSS, sep = " ")

df01$Date <- as.POSIXct(as.character(df01$Date), tz = "Australia/Melbourne")

# Importing second file from the first day
df02 <- read.csv("2013_09_04_Glasshouse_log_02.csv",
               header = TRUE,
               skip = 14)
df02$Day  <- Day01
df02$Date <- paste(df02$Day, df02$HHMMSS, sep = " ")
df02$Date <- as.POSIXct(as.character(df02$Date), tz = "Australia/Melbourne")

# import file from second day of testing
df03 <- read.csv("Glasshouse_log_2013-09-05.csv", skip = 14)
df03$Day <- Day02
df03$Date  <- paste(df03$Day, df03$HHMMSS, sep = " ")
df03$Date <- as.POSIXct(as.character(df03$Date), tz = "Australia/Melbourne")

# import file from third day of testing
df04 <- read.csv("Glasshouse_log_2013_09_06.csv", skip = 14)
df04$Day <- Day03
df04$Date  <- paste(df04$Day, df04$HHMMSS, sep = " ")
df04$Date <- as.POSIXct(as.character(df04$Date), tz = "Australia/Melbourne")

# import file from fourth day of testing
df05 <- read.csv("Glasshouse_log_2013_09_09.csv", skip = 14)
df05$Day <- Day04
df05$Date  <- paste(df05$Day, df05$HHMMSS, sep = " ")
df05$Date <- as.POSIXct(as.character(df05$Date), tz = "Australia/Melbourne")

# import file from fifth day of testing
df06 <- read.csv("Glasshouse_log_2013-09-10.csv", skip = 14)
df06$Day <- Day05
df06$Date  <- paste(df06$Day, df06$HHMMSS, sep = " ")
df06$Date <- as.POSIXct(as.character(df06$Date), tz = "Australia/Melbourne")

# import file from fifth day of testing
df07 <- read.csv("Glasshouse_log_2013_09_17.csv", skip = 14)
df07$Day <- Day06
df07$Date  <- paste(df07$Day, df07$HHMMSS, sep = " ")
df07$Date <- as.POSIXct(as.character(df07$Date), tz = "Australia/Melbourne")

# put them all together
df <- rbind(df01, df02, df03, df04, df05, df06, df07)

# Experimental remarks
remarks <- read.csv("Logbook.csv", header = TRUE)
remarks$Date <- as.POSIXct(remarks$Date, tz = "Australia/Melbourne")


# after all data is imported
df$CO2S[df$CO2S < 10] <- NA

my.mean <- mean(df$CO2S, na.rm = TRUE)
my.sd   <- sd(df$CO2S, na.rm = TRUE)

my.mean.first <- mean(df$CO2S[df$Date < remarks$Date[2]], na.rm = TRUE)
my.sd.first   <-   sd(df$CO2S[df$Date < remarks$Date[2]], na.rm = TRUE)

my.mean.second <- mean(df$CO2S[df$Date > remarks$Date[2] &
                               df$Date < remarks$Date[3]], na.rm = TRUE)
my.mean.second <-   sd(df$CO2S[df$Date > remarks$Date[2] &
                               df$Date < remarks$Date[3]], na.rm = TRUE)

my.mean.day2 <- mean(df$CO2S[df$Date > remarks$Date[3] &
                      df$Date > remarks$Date[4] ], na.rm = TRUE)
my.sd.day2 <-   sd(df$CO2S[df$Date > remarks$Date[6] &
                   df$Date > remarks$Date[6]], na.rm = TRUE)

my.mean.day3 <- mean(df$CO2S[df$Date > remarks$Date[6] & df$Date < remarks$Date[6]], na.rm = TRUE)
my.sd.day3 <-   sd(df$CO2S[df$Date > remarks$Date[6] & df$Date < remarks$Date[6]], na.rm = TRUE)

my.label <- paste("Mean value for Day3", round(my.mean.day3, 1), sep = " ")
my.sd.label <- paste("SD of", round(my.sd.day3, 1), sep = " ")

my.mean.day4 <- mean(df$CO2S[df$Date > remarks$Date[7] &
                             df$Date < remarks$Date[11]], na.rm = TRUE)
my.sd.day4 <-   sd(df$CO2S[df$Date > remarks$Date[7] &
                           df$Date < remarks$Date[11]], na.rm = TRUE)

my.mean.day4.afternoon <- mean(df$CO2S[df$Date > remarks$Date[10] ], na.rm = TRUE)
my.sd.day4.afternoon <-   sd(df$CO2S[df$Date > remarks$Date[10] ], na.rm = TRUE)

my.mean.day4.morning <- mean(df$CO2S[df$Date < remarks$Date[10] &
                                df$Date > remarks$Date[7]], na.rm = TRUE)
my.sd.day4.morning <-   sd(df$CO2S[df$Date < remarks$Date[10] &
                                     df$Date > remarks$Date[7] ], na.rm = TRUE)

my.mean.day5 <- mean(df$CO2S[df$Date > remarks$Date[11] &
                             df$Date < remarks$Date[12]], na.rm = TRUE)
my.sd.day5 <-   sd(df$CO2S[df$Date > remarks$Date[11] &
                           df$Date < remarks$Date[12]], na.rm = TRUE)


my.mean.day6 <- mean(df$CO2S[df$Date > remarks$Date[12] ], na.rm = TRUE)
my.sd.day6 <-     sd(df$CO2S[df$Date > remarks$Date[12] ], na.rm = TRUE)


my.label <- paste("Mean value for Day 6", round(my.mean.day6, 1), sep = " ")
my.sd.label <- paste("SD of", round(my.sd.day6, 1), sep = " ")


p <- ggplot(df, aes(x = Date, y = CO2S))
        p <- p + geom_line()
        # p <- p + geom_smooth()
#        p <- p + geom_hline(aes(yintercept = my.mean.first), 
#                                colour = "red")
#        p <- p + geom_hline(aes(yintercept = (my.mean.first + my.sd)), 
#                                colour = "green", 
#                                linetype = "dotted")
#        p <- p + geom_hline(aes(yintercept = (my.mean - my.sd)), 
#                                colour = "green", 
#                                linetype = "dotted")
        p <- p + geom_hline(aes(yintercept = my.mean.day2), 
                                colour = "red")
        p <- p + geom_hline(aes(yintercept = (my.mean.day2 + my.sd.day2)), 
                                colour = "green", 
                                linetype = "dotted")
        p <- p + geom_hline(aes(yintercept = (my.mean.day2 - my.sd.day2)), 
                                colour = "green", 
                                linetype = "dotted")                        
        p <- p + labs(y = expression(CO[2]~concentration~"["~mu*mol~mol^-1~"]"),
                      x = "Date and time")
        p <- p + theme_bw()
p

ggsave(file = "CO2_concentration_glasshouse_2013-09-04.pdf",
       width = 9, height = 9)

# Day 2 only
p <- ggplot(df[df$Date > remarks$Date[3], ], aes(x = Date, y = CO2S))
        p <- p + geom_line()
        p <- p + geom_smooth()
        p <- p + geom_hline(aes(yintercept = my.mean.day2), 
                                colour = "red")
        p <- p + geom_hline(aes(yintercept = (my.mean.day2 + my.sd.day2)), 
                                colour = "green", 
                                linetype = "dotted")
        p <- p + geom_hline(aes(yintercept = (my.mean.day2 - my.sd.day2)), 
                                colour = "green", 
                                linetype = "dotted")                        
                                
                                
        p <- p + labs(y = expression(CO[2]~concentration~"["~mu*mol~mol^-1~"]"),
                      x = "Time on Sept 5, 2013")
        p <- p + theme_bw()
p

ggsave(file = "CO2_concentration_glasshouse_2013-09-05.pdf",
       width = 9, height = 9)
       
# Day 3 only
p <- ggplot(df[df$Date > remarks$Date[6] & df$Date < remarks$Date[8], ], aes(x = Date, y = CO2S))
        p <- p + geom_line()
        p <- p + geom_smooth()
        p <- p + geom_hline(aes(yintercept = my.mean.day2), 
                                colour = "red")
        p <- p + geom_hline(aes(yintercept = (my.mean.day2 + my.sd.day2)), 
                                colour = "green", 
                                linetype = "dotted")
        p <- p + geom_hline(aes(yintercept = (my.mean.day2 - my.sd.day2)), 
                                colour = "green", 
                                linetype = "dotted")                        
                                
                                
        p <- p + labs(y = expression(CO[2]~concentration~"["~mu*mol~mol^-1~"]"),
                      x = "Time on Sept 6, 2013")
        p <- p + theme_bw()
p

ggsave(file = "CO2_concentration_glasshouse_2013-09-06.pdf",
       width = 9, height = 9)
       
# Day 4 only
p <- ggplot(df[df$Date > remarks$Date[8] &
               df$Date < remarks$Date[11], ], aes(x = Date, y = CO2S))
        p <- p + geom_line()                 
        p <- p + labs(y = expression(CO[2]~concentration~"["~mu*mol~mol^-1~"]"),
                      x = "Time on Sept 9, 2013")
        p <- p + theme_bw()
p


ggsave(file = "CO2_concentration_glasshouse_2013-09-09.pdf",
       width = 9, height = 9)

# Day 5 only
p <- ggplot(df[df$Date > remarks$Date[11] &
               df$Date < remarks$Date[12], ], aes(x = Date, y = CO2S))
        p <- p + geom_line()
        p <- p + geom_hline(aes(yintercept = my.mean.day5), 
                                colour = "red")
        p <- p + geom_hline(aes(yintercept = (my.mean.day5 + my.sd.day5)), 
                                colour = "green", 
                                linetype = "dotted")
        p <- p + geom_hline(aes(yintercept = (my.mean.day5 - my.sd.day5)), 
                                colour = "green", 
                                linetype = "dotted")     
        p <- p + labs(y = expression(CO[2]~concentration~"["~mu*mol~mol^-1~"]"),
                      x = "Time on Sept 10, 2013")

        p <- p + annotate("text",
                          label = my.label,
                          x = as.POSIXct("2013-09-10 10:00:00"), 
                          y = 650, 
                          colour = "red",
                          hjust = 0)

        p <- p + annotate("text",
                          x = as.POSIXct("2013-09-10 10:00:00"), 
                          y = 635, 
                          label = my.sd.label, 
                          colour = "red",
                          hjust = 0)
        p <- p + theme_bw()
p


ggsave(file = "CO2_concentration_glasshouse_2013-09-10.pdf",
       width = 9, height = 9)
       
# Day 6 only
p <- ggplot(df[df$Date > remarks$Date[12], ], aes(x = Date, y = CO2S))
        p <- p + geom_line()
        p <- p + geom_hline(aes(yintercept = my.mean.day6), 
                                colour = "red")
        p <- p + geom_hline(aes(yintercept = (my.mean.day6 + my.sd.day6)), 
                                colour = "green", 
                                linetype = "dotted")
        p <- p + geom_hline(aes(yintercept = (my.mean.day6 - my.sd.day6)), 
                                colour = "green", 
                                linetype = "dotted")     
        p <- p + labs(y = expression(CO[2]~concentration~"["~mu*mol~mol^-1~"]"),
                      x = "Time on Sept 17, 2013")

        p <- p + annotate("text",
                          label = my.label,
                          x = as.POSIXct("2013-09-17 10:00:00"), 
                          y = 825, 
                          colour = "red",
                          hjust = 0)

        p <- p + annotate("text",
                          x = as.POSIXct("2013-09-17 10:00:00"), 
                          y = 800, 
                          label = my.sd.label, 
                          colour = "red",
                          hjust = 0)
        p <- p + theme_bw()
p

ggsave(file = "CO2_concentration_glasshouse_2013-09-17.pdf",
       width = 9, height = 9)
