# Chapter 4 Code

leukemia <- read.csv(file = "leukemia.csv", stringsAsFactors = TRUE)
# leukemia <- read.csv(file = "https://raw.githubusercontent.com/tylerlewiscook/UCO-Data-Mining-Stat-Learning/refs/heads/main/Data/leukemia.csv", stringsAsFactors = TRUE)

# Logistic regression ----------------------------------------------------------

fit1 <- glm(tumor ~ ., data = leukemia, family = binomial)
summary(fit1)

contrasts(leukemia$tumor) # dummy is 1 for AML, so probs are for AML over ALL

predict(fit1, leukemia)
probs1 <- predict(fit1, leukemia, type = "response")
head(probs1)

pred1 <-  rep("ALL", 72)
pred1[probs1 > 0.5] <- "AML" # if prob > 0.5 predict AML


table(pred1, leukemia$tumor) # confusion matrix
pred1 == leukemia$tumor
mean(pred1 == leukemia$tumor)

library(pROC)
plot.roc(tumor ~ probs1, asp = NA, data = leukemia)



# Discriminant analysis --------------------------------------------------------

# LDA
library(MASS)
fit2 <- lda(tumor ~ ., data = leukemia)
summary(fit2)
fit2

pred2 <- predict(fit2, leukemia)
table(pred2$class, leukemia$tumor)
mean(pred2$class == leukemia$tumor) # same as logistic

# QDA
fit3 <- qda(tumor ~ ., data = leukemia)
summary(fit3)
fit3

pred3 <- predict(fit3, leukemia)
table(pred3$class, leukemia$tumor)
mean(pred3$class == leukemia$tumor) # better

plot.roc(tumor ~ pred3$posterior[,1], asp = NA, col = "blue", add = TRUE, data = leukemia)



# Naive Bayes ------------------------------------------------------------------

library(e1071)
fit4 <- naiveBayes(tumor ~ ., data = leukemia)
fit4

pred4 <- predict(fit4, leukemia)
table(pred4, leukemia$tumor)
mean(pred4 == leukemia$tumor) # same as LDA

predict(fit4, leukemia, type = "raw")

# Training and test data -------------------------------------------------------

dim(leukemia)
set.seed(1)
train <- sample(1:72, 58)

train_leukemia <- leukemia[train, ]
test_leukemia <- leukemia[-train, ]

# Logistic
fit5 <- glm(tumor ~ ., data = train_leukemia, family = binomial)
probs5 <- predict(fit5, test_leukemia, type = "response")
pred5 <-  rep("ALL", 14)
pred5[probs5 > 0.5] <- "AML"
table(pred5, test_leukemia$tumor)

# QDA
fit6 <- qda(tumor ~ ., data = train_leukemia)
pred6 <- predict(fit6, test_leukemia)
table(pred6$class, test_leukemia$tumor)

# KNN
library(class)

train_leukemia_x <- train_leukemia[, 1:10]
train_leukemia_y <- train_leukemia[, 11]
test_leukemia_x <- test_leukemia[, 1:10]
test_leukemia_y <- test_leukemia[, 11]

fit7 <- knn(train_leukemia_x, test_leukemia_x, train_leukemia_y, k = 5)
table(fit7, test_leukemia_y)

# Naive Bayes
fit8 <- naiveBayes(tumor ~ ., data = leukemia, subset = train)
pred8 <- predict(fit8, test_leukemia)
table(pred8, test_leukemia$tumor)
