# Damon LaPoint
# DSC 450 Team 3
# Create 90% Training Set and 10% Holdout for testing of Ensemble

setwd("~/Personal/School/Data Science 450 Summer 2015/Cap Stone")
data <- read.csv("train.csv")


#####################################
#create a test data set to evaluate using equal distribution of lables

samples <- 4200
data.test <- data.frame()

set.seed(50)
for(i in 0:9)
{
  data.sub <- subset(data, label == i)
  data.test <- rbind(data.test, data.sub[sample( 1:nrow(data.sub),round(samples * nrow(data[which(data$label == i),]) / nrow(data)),replace = FALSE),])
}

# remove test rows from training set
test.rows <- row.names(data.test)
data.train <- data[!rownames(data) %in% test.rows,]

#spot check label distribution
table(data.test$label) 
table(data.train$label)

#output new training set and holdout for ensemble testing
write.csv(data.test, "data_holdout_test.csv", row.names = FALSE)
write.csv(data.train, "data_train_minus_holdout.csv", row.names = FALSE)
