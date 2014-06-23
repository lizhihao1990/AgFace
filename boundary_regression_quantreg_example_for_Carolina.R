
# load iris example data
data(iris)

# create a simple xy plot
plot(Sepal.Length ~ Petal.Width, data = iris)

# load library "quantreg"
# install via
# install.packages("quantreg", dep = TRUE)

library(quantreg)

# calculating a simple linear regression
linear <- lm(Sepal.Length ~ Petal.Width, data = iris)

# putting the regression line in the graph
abline(linear)

# calculate the upper bound at 0.95 quantile
mod <- rq(Sepal.Length ~ Petal.Width, data = iris, tau = .95)

# use the quantreg-model to predict where the line should be
# first provide a meaningful x-range that should match the x-range of the data
# we want 100 points to be predicted in this range
predict_range <- data.frame(Petal.Width = seq(0, 3, length = 100))

# calculate for each x-range value the corresponding y-range
pDF <- within(predict_range, Sepal.Length <- predict(mod, newdata = predict_range))

# put this line on the graph
lines(Sepal.Length ~ Petal.Width, data = pDF, col = "red", lwd = 2)

# Now with a hyperbola
hyp <- nls(Sepal.Length ~ (a * Petal.Width) / (b + Petal.Width),
           data = iris,
           start = list(a = 5, b = 15))

mod2 <- nlrq(Sepal.Length ~ (a * Petal.Width) / (b + Petal.Width),
           data = iris,
           start = list(a = 5, b = 15),
           tau = .95)

# calculate for each x-range value the corresponding y-range
pDF2 <- within(predict_range, Sepal.Length <- predict(mod2, newdata = predict_range))

# put the hyperbola line on the graph
lines(Sepal.Length ~ Petal.Width, data = pDF2, col = "blue", lwd = 2)

library(ggplot2)
p <- ggplot(pDF2, aes(x = Petal.Width, y = Sepal.Length))
        p <- p + geom_point()
p
