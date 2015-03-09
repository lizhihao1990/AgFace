# read minicom.cap file
LoadMiniPAM <- function (x) {
        df <- read.csv(x)

        # weird dates
        weird.dates <- grep("<", df$D.M.Y)
        
        if (length(weird.dates >= 1)) {
        print("weird.date")
                df <- df[-weird.dates, ]}

        # second round
        weird.dates <- grep(":", df$D.M.Y)

        if (length(weird.dates >= 1)) {
                print("more weird dates")
                df <- df[-weird.dates, ]}

        # set date
        df$Date <- paste(df$D.M.Y, df$H.M.S, sep = " ")
        df$Date <- as.POSIXct(as.character(df$Date), 
                             format = "%d/%m/%y %H:%M:%S", 
                             tz = "Australia/Melbourne")

        # remove the space behind the Marker                     
        df$Mark <- gsub(" ", "", df$Mark)
        return(df)
}

# NextLabel function
# the initial Fv/Fm measurements have the label "l".
# They should have the same label as the rapid light response they belong to.

# get the next label for each "l" label
NextLabel <- function(marker, badlabel, position = 1) {
        my.bad  <- which(marker == badlabel)
        marker[my.bad] <- marker[my.bad + position]
        return(marker)
}
