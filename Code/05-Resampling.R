# Chapter 5 Code

# For loops --------------------------------------------------------------------

x <-  seq(1:10)
y <-  NULL

for(i in 1:length(x)){
  y[i] <- 2*x[i]
}
y



# Functions --------------------------------------------------------------------

find.mean <- function(x){
  the.mean <- sum(x)/length(x)
  return(the.mean)
}

set.seed(1)
z <- rnorm(100)
find.mean(z)
mean(z)



# Cross-Validation -------------------------------------------------------------

library(boot)
data(cars)
attach(cars)
set.seed(2)

fit <- glm(dist ~ speed + I(speed^2))

cv.loocv <- cv.glm(cars, fit)
cv.loocv$delta[1]

cv.k5 <- cv.glm(cars, fit, K = 5)
cv.k5$delta[1]

cv.k10 <- cv.glm(cars, fit, K = 10)
cv.k10$delta[1]

cv.error <- NULL
for(i in 1:3){
  fit <- glm(dist ~ poly(speed, i, raw = TRUE))
  cv.error[i] <- cv.glm(cars, fit)$delta[1]
}
which.min(cv.error)

detach(cars)



# Bootstrap --------------------------------------------------------------------

set.seed(3)
dat <- rnorm(100, 50, 5)

bstrap <- NULL
for(i in 1:1000){
  bstrap[i] <- mean(sample(dat, 100, replace = TRUE))
}

head(bstrap)
mean(bstrap)
sd(bstrap)

# Note: more functionality in 'boot' package
