# checking for duplicates

library(CampbellLogger)

setwd("~/AgFace/2015/Campbell_logger/logger_data")

df <- CampbellAllImport(log.interval = "Daily",
                        checkduplicates = FALSE)

# df$my.rownames <- rownames(df)

out <- ddply(df,
            .(SYSTEM, TIMESTAMP),
            summarise,
            my.n = length(RECORD),
            my.records = paste(RECORD, collapse = "_")#,
            #my.rownames = paste(as.numeric(my.rownames) + 5, collapse = "_")#,
            #my.values = paste(SYS8_IR_Horz_Std_2_, collapse = "_")
            )

out.single <- out[out$my.n != 1, ]
nrow(out.single)
