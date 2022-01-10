# Chapter 2 Code

# Random Variables --------------------------------------------
set.seed(1)

rnorm(10)           # Random number generation
rnorm(10, 50, 5)

dnorm(0)            # Density values: p(x) = P(X = x) for disc or f(x) for cont

pnorm(0)            # Distribution value: F(x) = P(X <= x)

qnorm(0.5)          # Quantile values: P(X <= ?) = value 



# Plotting ----------------------------------------------------
data(cars)
attach(cars)

hist(dist)

plot(speed, dist)

plot(speed, dist, xlab = "Speed of car", ylab = "Stopping distance", 
     main = "My car plot")

plot(speed, dist, type = "l")

plot(speed, dist, type = "l", col = "red")

plot(speed, dist)
fit <- lm(dist ~ speed)
abline(fit)

# Note: many people (myself included) prefer plotting with the 'ggplot2' package.


# KNN ---------------------------------------------------------

# Chapter 2 Exercise 7
x <- matrix(c(0, 2, 0, 0, -1, 1, 0,
              3, 0, 1, 1, 0, 1, 0,
              0, 0, 3, 2, 1, 1, 0),
            nrow = 7, ncol = 3)

dist(x)



# Using the 'class' library
library(class)

set.seed(115)

n <- 100
x1 <- runif(n, 1, 10)
x2 <- runif(n, 1, 10)
y <- ifelse(x1^2 + x2^2 > 8^2, "Yes", "No")

dat <- data.frame(x1, x2, y)


plot(dat$x1, dat$x2, pch = c(3, 16)[dat$y]) # str(dat$y)?

?knn
knn(train = dat[, c(1,2)], 
    test = dat[, c(1,2)], 
    cl = dat[, 3], 
    k = 10)

preds <- knn(dat[, c(1,2)], dat[, c(1,2)], dat[, 3], k = 10)

preds == dat$y
sum(preds == dat$y)

browseURL("https://github.com/tylerlewiscook/UCO-Data-Mining-Stat-Learning/blob/master/knn.png")






