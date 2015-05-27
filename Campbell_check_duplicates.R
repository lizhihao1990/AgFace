# with(df[1:1000, ], table(TIMESTAMP, RECORD))
setwd("~/AgFace/2015/Campbell_logger/Transmissions/data_duplication_simple_program")

library(plyr)
source("~/AgFace/R_scripts/import_Campbell.R")

df <- CampbellFileImport("CR1000_1_SYS8_5Min.dat", checkduplicates = FALSE)

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

df$my.rownames <- rownames(df)

out <- ddply(df,
            .(TIMESTAMP),
            summarise,
            my.n = length(RECORD),
            my.records = paste(RECORD, collapse = "_"),
            my.rownames = paste(as.numeric(my.rownames) + 5, collapse = "_")#,
            #my.values = paste(SYS8_IR_Horz_Std_2_, collapse = "_")
            )

out.single <- out[out$my.n != 1, ]
nrow(out.single)
#write.table(out.single, file = "duplicate_dates.csv",
#            row.names = F, sep = ",")

# search for gaps
# create a perect 5 min timecourse from first sample to last

first.time <- df$TIMESTAMP[1]
second.time <- df$TIMESTAMP[2]
last.time <- df$TIMESTAMP[length(df$TIMESTAMP)]

time.seq <- difftime(second.time, first.time)

my.seq <- seq(from = first.time, to = last.time, by = time.seq)

df.length <- length(df$TIMESTAMP)
my.seq.length <- length(my.seq)

# gaps
my.seq.length - df.length

gap.found <- my.seq.length - df.length

if (gap.found > 0) {
my.seq <- data.frame(FullTime = my.seq)
df.full.time <- merge(my.seq, df,
                      by.x = "FullTime",
                      by.y = "TIMESTAMP",
                      all = TRUE)
df.full.time$FullTime[is.na(df.full.time$RECORD)]
}

