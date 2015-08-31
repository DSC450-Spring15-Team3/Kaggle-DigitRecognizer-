install.packages("randomForest")
install.packages("readr")

########################################################################################
# Clear plots
if(!is.null(dev.list()["RStudioGD"]))
{
  dev.off(dev.list()["RStudioGD"]);
}
# Clear workspace
rm(list=ls())
# Clear console
cat("\014")
date()
########################################################################################

# Creates a simple random forest benchmark

library(randomForest)
library(readr)

# Load data sets
train = read_csv("..\\..\\data\\train.csv")
test = read_csv("..\\..\\data\\test.csv")

# Remove columns from the training set where the all values in a given column are 0
features=colMeans(train)>0
train=train[,features]
fulltrain = train
fulltrainlabels = as.factor(fulltrain[1:nrow(fulltrain),1])
fulltrain = fulltrain[1:nrow(fulltrain),-1]

# Remove corresponding columns from the test set
features=features[-1]
test=test[,features]

# Save the new data files
#write_csv(train,"newtrain.csv")
#write_csv(test,"newtest.csv")

# Obtain a sample of training set
set.seed(0)
numTrain = nrow(train) # or use some other smaller value for testing purposes
numTrees = 200
rows=sample(1:nrow(train), numTrain)
labels = as.factor(train[rows,1])
truevalues=train[rows,1]
train = train[rows,-1]

# Convert all values to binary
minlevel=0
train=train>minlevel
fulltrain=fulltrain>minlevel
test=test>minlevel

# Build model on sample of training set, and evaluate based on full training set
#rf2=randomForest(train, labels, xtest=fulltrain, ntree=numTrees)
#evalpredictions=data.frame(ImageId=1:nrow(fulltrain), Label=levels(fulltrainlabels)[rf2$test$predicted])
#head(evalpredictions)
#mean(evalpredictions[,2]==fulltrainlabels)

# Build model on sample of training set
rf=randomForest(train, labels, xtest=test, ntree=numTrees)

# Make predictions on the official test set
predictions=data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
head(predictions)
write_csv(predictions,"predictions.csv")
