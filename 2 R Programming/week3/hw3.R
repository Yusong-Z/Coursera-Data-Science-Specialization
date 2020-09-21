library(datasets)

# question 1
data(iris)

s <- split(iris, iris$Species)
lapply(s, function(x) { colMeans(x[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]) } )

# question 3
data(mtcars)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
with(mtcars, tapply(mpg, cyl, mean))

# question 4
vec <- sapply(split(mtcars$hp, mtcars$cyl), mean)
vec[3] - vec[1]

# question 5
debug(ls)
ls()