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
