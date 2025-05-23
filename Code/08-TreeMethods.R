# Chapter 8 Code

wine <- read.csv(file = "wine.csv")

set.seed(9876)
train <- sample(1:dim(wine)[1], 1200)

trainWine <- wine[train, ]
testWine <- wine[-train, ]



# Trees ------------------------------------------------------------------------

library(tree)

fitTree <- tree(quality ~ ., data = wine, subset = train)
summary(fitTree)
plot(fitTree)
text(fitTree, cex = 0.7)

predTree <- predict(fitTree, testWine)
mean((testWine$quality - predTree)^2)

cvTree <- cv.tree(fitTree)
cvTree
plot(cvTree$size, cvTree$dev, type = "b")
which(cvTree$dev == min(cvTree$dev))
cvTree$size[1]

prunedTree <- prune.tree(fitTree, best = 8, method = "deviance")
plot(prunedTree)
text(prunedTree, cex = 0.7)



# Random forests ---------------------------------------------------------------

library(randomForest)

bag1 <- randomForest(quality ~ ., data = wine, mtry = 10, importance = TRUE, 
                     subset = train)
bag1
plot(bag1)
predbag1 <- predict(bag1, testWine)
mean((testWine$quality - predbag1)^2)

bag2 <- randomForest(quality ~ ., data = wine, mtry = 10, importance = TRUE, 
                     subset = train, ntree = 100)
plot(bag2)
predbag2 <- predict(bag2, testWine)
mean((testWine$quality - predbag2)^2)

rf1 <- randomForest(quality ~ ., data = wine, importance = TRUE, 
                    subset = train)
rf1
plot(rf1)
importance(rf1)
varImpPlot(rf1)
predrf1 <- predict(rf1, testWine)
mean((testWine$quality - predrf1)^2)


library(plotmo)

plotmo(rf1, degree1 = FALSE, degree2 = c("alcohol", "sulphates"))



# Boosting ---------------------------------------------------------------------

library(gbm)
set.seed(54321)

boost1 <- gbm(quality ~ ., data = trainWine, distribution = "gaussian", 
              cv.folds = 5)
summary(boost1)
plot(boost1, i = "alcohol")
gbm.perf(boost1)
predBoost1 <- predict(boost1, testWine)
mean((testWine$quality - predBoost1)^2)

boost2 <- gbm(quality ~ ., data = trainWine, distribution = "gaussian", 
              interaction.depth = 2, shrinkage = 0.2, n.trees = 500, cv.folds = 5)
summary(boost2)
gbm.perf(boost2)
best.iter <- gbm.perf(boost2)
predBoost2 <- predict(boost2, testWine, n.trees = best.iter)
mean((testWine$quality - predBoost2)^2)



# BART -------------------------------------------------------------------------

library(BART)

trainWine.x <- trainWine[, 1:10]
trainWine.y <- trainWine[, 11]
testWine.x <- testWine[, 1:10]
testWine.y <- testWine[, 11]

bart <- gbart(trainWine.x, trainWine.y, x.test = testWine.x)
predBart <- bart$yhat.test.mean
mean((testWine.y - predBart)^2)
