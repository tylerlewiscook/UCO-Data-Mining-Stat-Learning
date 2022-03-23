# Chapter 9 Code

leuk <- read.csv(file="Data/leukemia.csv", stringsAsFactors = TRUE)

# SVM -----------------------------------------------------------------

library(e1071)
set.seed(1)

plot(leuk$gene7, leuk$gene1, col = leuk$tumor)
datdim2 <- leuk[, c(1, 7, 11)]

# Support vector classifiers
svmFit1 <- svm(tumor ~ ., data = datdim2, kernel = "linear", cost = 2, 
               scale = TRUE)
plot(svmFit1, datdim2)
summary(svmFit1)

svmFit2 <- svm(tumor ~ ., data = datdim2, kernel = "linear", cost = 0.1)
plot(svmFit2, datdim2)
summary(svmFit2)

svmCV <- tune(svm, tumor ~ ., data = datdim2, kernel = "linear", 
              ranges = list(cost = c(0.01, 0.1, 1, 5)))
svmBest <- svmCV$best.model
summary(svmBest)

svmPred <- predict(svmBest, datdim2)
table(svmPred, datdim2$tumor)

svmFull <- svm(tumor ~ ., data = leuk, kernel = "linear")
plot(svmFull, leuk, gene1 ~ gene7)
fullPred <- predict(svmFull, leuk)
table(fullPred, leuk$tumor)

# Radial basis kernel
svmRad1 <- svm(tumor ~ ., data = datdim2, kernel = "radial")
plot(svmRad1, datdim2)
summary(svmRad1)
radPred1 <- predict(svmRad1, datdim2)
table(radPred1, datdim2$tumor)


svmRad2 <- svm(tumor ~ ., data = datdim2, kernel = "radial", cost = 5)
plot(svmRad2, datdim2)
radPred2 <- predict(svmRad2, datdim2)
table(radPred2, datdim2$tumor)


svmRad3 <- svm(tumor ~ ., data = datdim2, kernel = "radial", gamma = 1, 
               cost = 5)
plot(svmRad3, datdim2)
radPred3 <- predict(svmRad3, datdim2)
table(radPred3, datdim2$tumor)


svmRad4 <- svm(tumor ~ ., data = datdim2, kernel = "radial", gamma = 0.25, 
               cost = 5)
plot(svmRad4, datdim2)
radPred4 <- predict(svmRad4, datdim2)
table(radPred4, datdim2$tumor)

radCV <- tune(svm, tumor ~ ., data = datdim2, kernel = "radial", 
           ranges = list(cost = c(0.1, 1, 5), gamma = c(0.25, 0.5, 1, 5)))

summary(radCV)
bestRad <- radCV$best.model
plot(bestRad, datdim2)
radPred5 <- predict(bestRad, datdim2)
table(radPred5, datdim2$tumor)


radCVFull <- tune(svm, tumor ~ ., data = leuk, kernel = "radial", 
              ranges = list(cost = c(0.1, 1, 5), gamma = c(0.25, 0.5, 1, 5)))

summary(radCVFull)
bestRadFull <- radCVFull$best.model
plot(bestRadFull, leuk, gene1 ~ gene7)
radPred6 <- predict(bestRadFull, leuk)
table(radPred6, leuk$tumor)


# Polynomial kernel
polyCV <- tune(svm, tumor ~ ., dat = leuk, kernel = "polynomial",
            ranges = list(cost = c(0.1, 1, 5), degree = c(2, 3, 4)))

summary(polyCV)
bestPoly <- polyCV$best.model
summary(bestPoly)
plot(bestPoly, leuk, gene1 ~ gene7)
polyPred <- predict(bestPoly, leuk)
table(polyPred, leuk$tumor)


