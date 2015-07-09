# Passing columns of a dataframe to a function without quotes
# how to creata an "data =" argument for a function
# http://www.r-bloggers.com/passing-columns-of-a-dataframe-to-a-function-without-quotes/

someFunction <- function(y, data) {
  arguments <- as.list(match.call())
  y = eval(arguments$y, data)
  sum(y)
}

data(iris)
someFunction(Sepal.Width, iris)

# convert character to object name
MyEswPlot <- function(data, param.base) {
 # function that mathces two vectors from a data frame
 # the common base name of both parameters is given as argument to the function
 # the two actual vector names are created via paste and used via "as.name()"
 # during evaluation within the data frame
 arguments <- as.list(match.call())
 param.base <- arguments$param.base
 # print(param.base)
 param.mean <- paste(param.base, "_mean", sep = "")
 param.sd   <- paste(param.base, "_sd", sep = "")
 param.mean.eval <- eval(as.name(param.mean), data)
 print(summary(param.mean.eval))
}

MyEswPlot(df.mean, NUE..yield.N.uptake.)
matches NUE..yield.N.uptake._mean and NUE..yield.N.uptake._sd within df.mean
