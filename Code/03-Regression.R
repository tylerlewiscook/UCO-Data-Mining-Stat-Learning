# Chapter 3 Code

# Regression --------------------------------------------

# Simple linear
data(cars)
attach(cars)

fit <- lm(dist ~ speed)
summary(fit)

fit$coefficients
fit$residuals
confint(fit)

plot(speed, dist)
abline(fit)

par(mfrow=c(2,2))	#Splits display screen into 2x2 grid
plot(fit)
detach(cars)



# Multiple linear
wine <- read.csv(file="Data/wine.csv")   #Check your file path

plot(wine)

head(wine)

cor(wine)	


fit1 <- lm(quality ~ volatile.acidity + citric.acid + residual.sugar + pH + density + alcohol, data = wine)
summary(fit1) #Note: we are ignoring that quality is an ordinal variable

fit2 <- lm(quality ~ ., data = wine)	#Automatically uses all variables
summary(fit2)

fit3 <- lm(quality ~ alcohol + pH + volatile.acidity, data = wine)
summary(fit3)


#Interactions
fit4 <- lm(quality ~ alcohol + pH + volatile.acidity + pH:volatile.acidity, data = wine)
summary(fit4)

fit4v2 <- lm(quality ~ alcohol + pH*volatile.acidity, data = wine)
summary(fit4v2)


#Nonlinear terms
fit5 <- lm(quality ~ alcohol + I(alcohol^2), data = wine)
summary(fit5)

fit5v2 <- lm(quality ~ poly(alcohol, 2), data = wine)
summary(fit5v2)

fit5v3 <- lm(quality ~ poly(alcohol, 2, raw = TRUE), data = wine)
summary(fit5v3)


#Predictions
newwine <- data.frame(volatile.acidity = 0.8, alcohol = 10, pH = 3.65)
predict(fit3, newwine)
predict(fit3, newwine, interval = "confidence")
predict(fit3, newwine, interval = "prediction")


#Qualitative predictors
wine$vineyard <- as.factor(c(rep("A", 532), rep("B", 532), rep("C", 532)))
fit6 <- lm(quality ~ alcohol + pH + vineyard, data = wine)
summary(fit6)
contrasts(wine$vineyard)