#install.packages("keras")
library(keras)

fashion_mnist = dataset_fashion_mnist()
View(fashion_mnist)
View(fashion_mnist$train$y)
hist(fashion_mnist$train$y)
unique(fashion_mnist$train$y)
table(fashion_mnist$train$y)

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test





