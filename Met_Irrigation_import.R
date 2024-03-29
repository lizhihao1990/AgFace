# Import Met & Irrigation data

setwd("~/AgFace/Topics/Met_Irri")

df <- read.csv("../../2007_2012/Met&Irr_data_for_distribution_131212.csv",
               sep = "\t")

names(df) <- gsub("X", "Remarks", names(df))

# convert all date-columns into dates
df[, c(grep("date", names(df)))] <- lapply(df[, c(grep("date", names(df)))], 
              function (x) {
              as.POSIXct(as.character(x), 
              format = "%d/%m/%Y", tz = "Australia/Melbourne") 
})

# add species information
df$Species <- NA
df$Species[grep("entil", df$Irrigation.treatment)] <- "Lentil"
df$Species[grep("Pea",   df$Irrigation.treatment)]   <- "Pea_Canola"
df$Species[grep("Wheat", df$Irrigation.treatment)] <- "Wheat"
df$Species[is.na(df$Species)] <- "Wheat"

df$Species <- as.factor(df$Species)

# Separate TOS and Irrigation treatment
df$Irrigation <- NA

# can't use strsplit due to inconsistent naming conventions
# doing it manually via pattern-search
df$Irrigation[grep("Rain", df$Irrigation.treatment)] <- "Rain"
df$Irrigation[grep("Supp", df$Irrigation.treatment)] <- "Supp"

# Walpeup does not have an irrigation treatment, only "Rain"
df$Irrigation[df$Location == "Walpeup"] <- "Rain"

df$TOS[grep("TOS1", df$Irrigation.treatment)] <- "TOS1"
df$TOS[grep("TOS2", df$Irrigation.treatment)] <- "TOS2"

# Horsham in 2010, 2011, 2012 only has one time of sowing, that is "TOS1"
df$TOS[is.na(df$TOS)] <- "TOS1"

# convert Irrigation and TOS to factor
df[, c("Irrigation", "TOS")] <- lapply(df[, c("Irrigation", "TOS")],
             function(x) {
             as.factor(as.character(x))
             })

write.table(df,
            file = "Irrigation_rainfall_data_2007_2012.csv",
            sep = ",", row.names = FALSE)

Met.Irri <- df
rm(df)
save.image("Meteo_Irrigation_info.RData", compress = TRUE)
