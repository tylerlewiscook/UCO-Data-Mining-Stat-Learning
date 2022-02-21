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

prunedTree <- prune.tree(fitTree, best = 10)
plot(prunedTree)
text(prunedTree, cex = 0.7)



# Random forests ---------------------------------------------------------------

library(randomForest)

bag1 <- randomForest(quality ~ ., data = wine, mtry = 10, importance = TRUE, 
                     subset = train)
bag1
predbag1 <- predict(bag1, testWine)
mean((testWine$quality - predbag1)^2)

bag2 <- randomForest(quality ~ ., data = wine, mtry = 10, importance = TRUE, 
                     subset = train, ntree = 100)
bag2
predbag2 <- predict(bag2, testWine)
mean((testWine$quality - predbag2)^2)

rf1 <- randomForest(quality ~ ., data = wine, mtry = 3, importance = TRUE, 
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
              n.trees = 1000, interaction.depth = 4)
summary(boost1)
plot(boost1, i = "alcohol")
gbm.perf(boost1)
predBoost1 <- predict(boost1, testWine)
mean((testWine$quality - predBoost1)^2)

boost2 <- gbm(quality ~ ., data = trainWine, distribution = "gaussian", 
              interaction.depth = 4, cv.folds = 10)
summary(boost2)
gbm.perf(boost2, method = "cv")
best.iter2 <- gbm.perf(boost2)
predBoost2 <- predict(boost2, testWine, n.trees = best.iter2)
mean((testWine$quality - predBoost2)^2)

boost3 <- gbm(quality ~ ., data = trainWine, distribution = "gaussian", 
              interaction.depth = 4, shrinkage = 0.5, cv.folds = 10)
summary(boost3)
best.iter3 <- gbm.perf(boost3, method = "cv")
predBoost3 <- predict(boost3, testWine, n.trees = best.iter3)
mean((testWine$quality - predBoost3)^2)



# BART -------------------------------------------------------------------------

library(BART)

trainWine.x <- trainWine[, 1:10]
trainWine.y <- trainWine[, 11]
testWine.x <- testWine[, 1:10]
testWine.y <- testWine[, 11]

bart <- gbart(trainWine.x, trainWine.y, x.test = testWine.x)
predBart <- bart$yhat.test.mean
mean((testWine.y - predBart)^2)
