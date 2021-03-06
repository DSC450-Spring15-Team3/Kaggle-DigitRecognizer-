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
print("---------------------------------------------------------------------")
print(date())

########################################################################################

# DATASCI 450
# Team #3
# Kaggle "Digit Recognizer" competition


# Initialize global parameters
print('Intializing.........')
set.seed(0)
numTrain = 4000 #Use 42000 for full set, or something smaller for testing
numTrees = 50

do_singularityRemoval = FALSE  #WARNING... Singularity removal takes a long time
   minClumpSize = 10
do_columnRemoval = TRUE
   minNumColumnEntries = 20 #20
do_binarize = TRUE  
   minPixelLevel = 10
do_saveModifiedDataFiles = FALSE
do_crossValidation = TRUE           # Should turn this off when doing final predictions to save time
do_produceFinalPredictions = FALSE    # Should turn this off when doing cross-validation to save time
do_plotAnImage = FALSE # Plot the p'th image
  p = 1

# Load libraries
library(randomForest)
library(readr)
library(raster)
library(igraph)

# Define functions #########################################################
remove_singularities=function(dataset,minClumpSize) {
  # Function to remove isolated points
  m=nrow(dataset)
  n=ncol(dataset)
  for (i in 1:m){
    img = matrix(unlist(dataset[i,]), nrow = 28, ncol = 28, byrow = TRUE) 
    r = raster(img)
    rc = clump(r, directions = 8) 
    #remove clump observations with frequency smaller than minClumpSize
    clumps = data.frame(freq(rc))
    clumps = clumps[clumps$count >= minClumpSize, ] 
    clumps = as.vector(clumps$value) 
    rc[! rc %in% clumps]=NA
    if (i/50==floor(i/50)) { #print progress after every n records
      print(sprintf("   progress: %d/%d",i,m))
      #plot(rc,col="black",legend=FALSE)
    }
    rc[is.na(rc)] = 0
    rc[rc>0] = 1
    dataset[i,]=dataset[i,]*as.vector(rc)
  }
  dataset=dataset
}

binarize_data=function(dataset,minPixelLevel) {
  # Function to binarize data
  # Set all value > minPixelLevel to 1. Set everything else to 0
  dataset=dataset>minPixelLevel
}
##########################################################################

# Load data sets
print('Loading data................')
train = read_csv("train.csv")
test = read_csv("test.csv")
origtrain = train

# Separate the labels from the data, and save a full copy of the dataset
fulltrain=train
fulltrainlabels = as.factor(fulltrain[1:nrow(fulltrain),1])
fulltrain = fulltrain[-1]
train=train[-1]

if (do_plotAnImage) {
  ##### Plot the p'th image from the training set  
  # Extract the image matrix
  img = matrix(unlist(train[p,]), nrow = 28, ncol = 28, byrow = TRUE) 
  # Convert to raster
  r=raster(img)
  r[r == 0] = NA 
  # Print the label
  print(as.integer(as.vector(fulltrainlabels[p])))
  #plot as all black
  plot(r,col="black",legend=FALSE)
}

if (do_singularityRemoval) {
  # Erase pixels which are mostly isloated
  print('Removing singularities................ (takes a long time)')
  train=remove_singularities(train,minClumpSize)
  fulltrain=train
  if (do_produceFinalPredictions) {
    test=remove_singularities(test,minClumpSize) #Save time. Only do this if needed.
  } 
}

if (do_columnRemoval) {
  # Remove columns from the training set when the number of non-zero entries is <= minNumColumnEntries
  print('Removing extra columns................')
  features=colSums(train!=0)>minNumColumnEntries
  train=train[,features]
  fulltrain=train
  if (do_produceFinalPredictions) {
    test=test[,features] #Save time. Only do this if needed.
  }
}

if (do_binarize) {
  # Convert all values to 1's and O's
  print('Binarizing data................')
  train=binarize_data(train,minPixelLevel)
  fulltrain=train
  if (do_produceFinalPredictions) {
    test=binarize_data(test,minPixelLevel) #Save time. Only do this if needed.
  }
}

# Make sure they are all still in data.frame format
train=data.frame(train)
test=data.frame(test)
fulltrain=data.frame(fulltrain)

if (do_saveModifiedDataFiles) {
  # Save the new processed data files
  print('Saving modified data files................')
  label=as.integer(as.vector(fulltrainlabels))
  newtrain=data.frame(cbind(label,fulltrain))
  newtest=data.frame(test)
  write_csv(newtrain,"newtrain.csv")
  write_csv(newtest,"newtest.csv")
}

# Obtain a sample of training set
rows=sample(1:nrow(train), numTrain)
train=train[rows,]
labels = fulltrainlabels[rows]

if (do_crossValidation) {
  # Build model on sample of training set, and evaluate based on full training set
  print('Performing cross-validation................')
  rf2=randomForest(train, labels, xtest=fulltrain, ntree=numTrees)
  evalpredictions=data.frame(ImageId=1:nrow(fulltrain), Label=levels(fulltrainlabels)[rf2$test$predicted])
  head(evalpredictions)
  # Print accurary against full training set:
  results=mean(evalpredictions[,2]==fulltrainlabels)
  print(sprintf("    Cross validation results against full data set: %f",results))
}

if (do_produceFinalPredictions) {
  # Build model on sample of training set, and make predictions on the official test set
  print('Generating final predictions................')
  rf=randomForest(train, labels, xtest=test, ntree=numTrees)
  predictions=data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
  head(predictions)
  write_csv(predictions,"predictions.csv")
}
print('Done!')
print(date())
print("---------------------------------------------------------------------")
