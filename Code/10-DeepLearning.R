# Chapter 10 Code

library(keras)
park <- read.csv(file = "Data/park.csv")
park <- park[, -1]

# Outline ----------------------------------------------------------------------

#   Define model structure (keras_model_sequential()...)
#   Details for fitting algorithm (compile)
#   Fit model (fit)



# Regression -------------------------------------------------------------------

set.seed(1)
train <- sample(1:dim(park)[1], 0.75*dim(park)[1])
x <- model.matrix(total_UPDRS ~ . -1, park)
x <- scale(x)
y <- park$total_UPDRS

# Lasso baseline
library(glmnet)
fit.l <- cv.glmnet(x[train, ], y[train], alpha = 1)
bestlam.l <- fit.l$lambda.min
pred.lasso <- predict(fit.l, s = bestlam.l, newx = x[-train, ])
mean((pred.lasso - y[-train])^2)

# NN with one hidden layer and dropout
modnn <- keras_model_sequential() %>% 
  layer_dense(units = 25, activation = "relu", input_shape = ncol(x)) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 1)

modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop() 
)

history <- modnn %>% fit(
  x[train, ], 
  y[train], 
  epochs = 100, 
  batch_size = 100
)

pred.nn <- predict(modnn, x[-train, ])
mean((pred.nn - y[-train])^2)
summary(modnn)

# NN with two hidden layers and regularization
modnn2 <- keras_model_sequential() %>% 
  layer_dense(units = 50, 
              activation = "relu",
              kernel_regularizer = regularizer_l1(l = 0.1),
              input_shape = ncol(x)) %>% 
  layer_dense(units = 50,
              activation = "relu",
              kernel_regularizer = regularizer_l1(l = 0.1)) %>% 
  layer_dense(units = 1)

modnn2 %>%  compile(loss = "mse",
                    optimizer = optimizer_adam(),
                    metrics = list("mean_absolute_error")
)

history <- modnn2 %>% 
  fit(
    x[train, ],
    y[train],
    epochs = 50,
    batch_size = 50,
    validation_data = list(x[-train, ], y[-train])
  )

pred.nn2 <- predict(modnn2, x[-train, ])
mean((pred.nn2 - y[-train])^2)
summary(modnn2)



# Classification (MNIST) -------------------------------------------------------
# Code is mostly taken from the textbook

# Function for displaying digits taken from:
# https://www.r-bloggers.com/build-your-own-neural-network-classifier-in-r/
displayDigit <- function(X){ 
  m <- matrix(unlist(X),nrow = 28,byrow = T)  
  m <- t(apply(m, 2, rev))  
  image(m,col=grey.colors(255))
} 

mnist <- dataset_mnist()
x_train <- mnist$train$x
g_train <- mnist$train$y
x_test <- mnist$test$x
g_test <- mnist$test$y
dim(x_train)
dim(x_test)

displayDigit(x_train[1, ])
g_train[1]
displayDigit(x_train[2, ])
g_train[2]
displayDigit(x_test[1,])
g_test[1]

x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
y_train <- to_categorical(g_train, 10)
y_test <- to_categorical(g_test, 10)

x_train <- x_train / 255
x_test <- x_test / 255

modelnn <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax")

summary(modelnn)

modelnn %>% compile(loss = "categorical_crossentropy",
                    optimizer = optimizer_rmsprop(), 
                    metrics = c("accuracy")
)

history <- modelnn %>%
  fit(x_train, 
      y_train, 
      epochs = 30, 
      batch_size = 128,
      validation_split = 0.2
  )

# Accuracy calculation from textbook:
accuracy <- function(pred, truth)
  mean(drop(as.numeric(pred)) == drop(truth))
modelnn %>% predict(x_test) %>% k_argmax() %>% accuracy(g_test)

# What if we want to make a confusion matrix?
pred.mnist <- predict(modelnn, x_test)
dim(pred.mnist)
head(pred.mninst)
head(y_test)
new.pred.mnist <- apply(pred.mnist, 1, which.max)
pred.digit <- new.pred.mnist - 1
table(pred.digit, g_test)

# Pretrained CNN ---------------------------------------------------------------
# Code taken from textbook

img_dir <- "book_images"
image_names <- list.files(img_dir)
num_images <- length(image_names)
x2 <- array(dim = c(num_images, 224, 224, 3))
for (i in 1:num_images) {
  img_path <- paste(img_dir, image_names[i], sep = "/")
  img <- image_load(img_path, target_size = c(224, 224))
  x2[i,,, ] <- image_to_array(img)
}
x2 <- imagenet_preprocess_input(x2)

model <- application_resnet50(weights = "imagenet")
summary(model)

pred.img <- model %>% predict(x2) %>%
  imagenet_decode_predictions(top = 3)
names(pred.img) <- image_names
print(pred.img)

