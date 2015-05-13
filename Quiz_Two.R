set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f1 <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f1, layout = c(1, 2)) ## Plot with 2 panels

library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)

library(lattice)
library(datasets)
data(airquality)
xyplot(Ozone ~ Wind | factor(Month), data = airquality)

##ggplot questions

#Q7
library(datasets)
data(airquality)

airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

#Q9
library(ggplot2)
g <- ggplot(movies, aes(votes, rating))
print(g)
p<-g+geom_point()
p

#Q10
qplot(votes, rating, data = movies)
qplot(votes, rating, data = movies) + geom_smooth()