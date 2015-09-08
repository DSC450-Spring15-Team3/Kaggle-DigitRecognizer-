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
set.seed(0)
trainRows <- sample(1:totalRows, 35000)
testRows <- setdiff(1:totalRows, trainRows)

myTrain = fullTrain[trainRows, ]
myTest = fullTrain[testRows, ]

# Store off the digit values, and run PCA on remaining features
labels = as.factor(myTrain[,1])
pca <- prcomp(myTrain[, -1], scale=FALSE)
#summary(pca)

#percentVariance <- pca$sdev^2/sum(pca$sdev^2)
#sum(percentVariance[1:100]) # 91.5%
#sum(percentVariance[1:150]) # 94.9%
#sum(percentVariance[1:200]) # 96.7%
#sum(percentVariance[1:350]) # 99.2%

#for (featureCount in seq(25, 150, 5))
#{

  # 55 Gage slightly higher accuracy on linear kernel, but sticking with 50 due to simplicity.
  featureCount <- 50 #  0.979
  
  model <- svm(pca$x[,1:featureCount], 
               labels, kernel = "radial", probability=TRUE, gamma=.03, cost=.1);
  
  
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
  labels = as.factor(fullTrain[,1])
  pca <- prcomp(fullTrain[, -1], scale=FALSE)
  featureCount <- 50 #  0.979
  model <- svm(pca$x[,1:featureCount], 
               labels, kernel = "radial", probability=TRUE, gamma=.030, c=10);
  
# Generate predictions on the Kaggle Set

test = read_csv("..\\..\\data\\test.csv")

test.pca <- predict(pca, newdata = test)
test.predict <- predict(model, newdata = test.pca[, 1:featureCount])

predictions=data.frame(ImageId=1:nrow(test), Label=levels(labels)[test.predict])

head(predictions)
write_csv(predictions,"predictions.csv")


