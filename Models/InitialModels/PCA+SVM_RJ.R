# This script uses the following libraries:
#install.packages("readr")
#install.packages("e1071")

# Clear workspace
rm(list=ls())

# Clear console
cat("\014")
date()
########################################################################################

library(e1071)
library(readr)

# Load training data set
fullTrain <- read_csv("..\\..\\data\\train.csv")
totalRows <- nrow(fullTrain) # or use some other smaller value for testing purposes

# Split to Training and Testing Sets on known outcomes
set.seed(1)
trainRows <- sample(1:totalRows, 35000)
testRows <- setdiff(1:totalRows, trainRows)

myTrain = fullTrain[trainRows, ]
myTest = fullTrain[testRows, ]

# Store off the digit values, and run PCA on remaining features
labels = as.factor(myTrain[,1])
pca <- prcomp(myTrain[, -1], scale=FALSE)
#summary(pca)
#percentVariance <- pca$sdev^2/sum(pca$sdev^2)

#for (featureCount in seq(25, 150, 5))
#{

# 50 features give best overall results.
  featureCount <- 50
  
# Train an SVM model with the given set of features
  model <- svm(pca$x[,1:featureCount], 
               labels, kernel = "radial", probability=FALSE, gamma=0.03, cost=10);
  
  
  # Test model on test data
  test.pca <- predict(pca, newdata = myTest[, -1])
  test.pred <- predict(model, newdata = test.pca[, 1:featureCount])
  
  # Test the accuracy
  actual <- myTest[, 1]
  table(pred = test.pred, true = actual)
  sum(actual == test.pred) / length(actual)
  
#    print(c(featureCount, sum(actual == test.pred) / length(actual)))
#}


##########################################

# For Kaggle prediction, train on the full set  
  labels <- as.factor(fullTrain[,1])
  pca <- prcomp(fullTrain[, -1], scale=FALSE)
  featureCount <- 50 
  model <- svm(pca$x[,1:featureCount], 
               labels, kernel = "radial", probability=FALSE, gamma=.03, cost=10);
  
# Generate predictions on the Kaggle Set

test = read_csv("..\\..\\data\\test.csv")

# Run PCA on the test data, then use features to predict based on model
test.pca <- predict(pca, newdata = test)
test.predict <- predict(model, newdata = test.pca[, 1:featureCount])

# Build the prediection data frame, and save to disk
predictions=data.frame(ImageId=1:nrow(test), Label=levels(labels)[test.predict])
write_csv(predictions,"predictions_svm_rj.csv")

#head(predictions)

##########################################

# For Ensemble prediction, train

train <- read_csv("..\\..\\data\\\\Holdout\\data_train_minus_holdout.csv")
test <- read_csv("..\\..\\data\\Holdout\\data_holdout_test.csv")

labels <- as.factor(train[,2])
pca <- prcomp(train[, 3:786], scale=FALSE)
featureCount <- 50 
model <- svm(pca$x[,1:featureCount], 
             labels, kernel = "radial", probability=FALSE, gamma=.03, cost=10);
test.pca <- predict(pca, newdata = test)
test.predict <- predict(model, newdata = test.pca[, 1:featureCount])

# Build the prediection data frame, and save to disk
predictions=data.frame(ImageId=1:nrow(test), Label=levels(labels)[test.predict])
write_csv(predictions,"..\\..\\data\\\\Holdout\\ensemble_predictions_svm_rj.csv")
