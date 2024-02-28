# Chapter 7 Code

# Step functions ---------------------------------------------------

data(airquality)
airquality <- na.omit(airquality)

pairs(airquality)

plot(airquality$Temp, airquality$Ozone)

table(cut(airquality$Temp ,3)) 

stepfit1 <- lm(Ozone ~ cut(Temp, 3), data = airquality)
coef(stepfit1)
steppred1 <- predict(stepfit1)
lines(airquality$Temp, steppred1)

stepdat1 <- data.frame(Temp = airquality$Temp, steppred1)
stepdat1 <- stepdat1[order(stepdat1$Temp), ]
plot(airquality$Temp, airquality$Ozone)
lines(stepdat1$Temp, stepdat1$steppred1)



# Splines ----------------------------------------------------------
# https://pclambert.net/interactivegraphs/spline_eg/spline_eg

library(splines)

plot(airquality$Temp, airquality$Ozone)

fit1 <- lm(Ozone ~ bs(Temp, knots = c(70, 85)), data = airquality)
summary(fit1)
pred1 <- predict(fit1)
dat1 <- data.frame(Temp = airquality$Temp, pred1)
dat1 <- dat1[order(dat1$Temp), ]
lines(dat1$Temp, dat1$pred1, type = "l", lwd = 2)

fit2 <- lm(Ozone ~ bs(Temp, df = 8), data = airquality)
pred2 <- predict(fit2)
dat2 <- data.frame(Temp = airquality$Temp, pred2)
dat2 <- dat2[order(dat2$Temp), ]
lines(dat2$Temp, dat2$pred2, type = "l", col = "red", lwd = 2)
attr(bs(airquality$Temp, df = 8), "knots")

fit3 <- lm(Ozone ~ ns(Temp, df = 5), data = airquality)
pred3 <- predict(fit3)
dat3 <- data.frame(Temp = airquality$Temp, pred3)
dat3 <- dat3[order(dat3$Temp), ]
lines(dat3$Temp, dat3$pred3, type = "l", col = "green", lwd = 2)

fit4 <- smooth.spline(airquality$Temp, airquality$Ozone, cv = TRUE) 
pred4 <- predict(fit4)
lines(pred4$x, pred4$y, col = "blue", lwd = 2)



library(boot)

cvfit1 <- glm(Ozone ~ ns(Temp, df = 5), data = airquality)
cvfit1loocv <- cv.glm(airquality, cvfit1)
cvfit1loocv$delta[1]

cvfit2 <- glm(Ozone ~ ns(Temp, df = 6), data = airquality)
cvfit2loocv <- cv.glm(airquality, cvfit2)
cvfit2loocv$delta[1]



# Local regression --------------------------------------------------

plot(airquality$Temp, airquality$Ozone)

locfit1 <- loess(airquality$Ozone ~ airquality$Temp, span = 1)
predloc1 <- predict(locfit1)
locdat1 <- data.frame(Temp = airquality$Temp, predloc1)
locdat1 <- locdat1[order(locdat1$Temp), ]
lines(locdat1$Temp, locdat1$predloc1, lwd = 2)

locfit2 <- loess(airquality$Ozone ~ airquality$Temp, span = 0.5)
predloc2 <- predict(locfit2)
locdat2 <- data.frame(Temp = airquality$Temp, predloc2)
locdat2 <- locdat2[order(locdat2$Temp), ]
lines(locdat2$Temp, locdat2$predloc2, col = "blue", lwd = 2)

locfit3 <- loess(airquality$Ozone ~ airquality$Temp, span = 0.5, degree = 1)
predloc3 <- predict(locfit3)
locdat3 <- data.frame(Temp = airquality$Temp, predloc3)
locdat3 <- locdat3[order(locdat3$Temp), ]
lines(locdat3$Temp, locdat3$predloc3, col = "red", lwd = 2)


# GAM ---------------------------------------------------------------

library(gam)
par(mfrow = c(3,2))

gam1 <- gam(Ozone ~ s(Wind, 5) + s(Temp, 5), data = airquality)
plot(gam1)

gam2 <- gam(Ozone ~ s(Wind, 3) + s(Temp, 3), data = airquality)
plot(gam2)

gam3 <- gam(Ozone ~ s(Wind, 3) + lo(Temp, span = 0.25), data = airquality)
plot(gam3)

gam4 <- lm(Ozone ~ bs(Wind, 3) + ns(Temp, 3) + poly(Solar.R, 2), data = airquality)
summary(gam4)
