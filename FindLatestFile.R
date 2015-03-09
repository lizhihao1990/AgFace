# find newest file in a folder

df <- file.info(list.files())

# df is a dataframe with 10 variables
# one variable is 'mtime' the file modification POSIX data,

latest.row <- which.max(df$mtime)

latest.file <- rownames(df[latest.row, ])
