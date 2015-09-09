# Damon LaPoint
# DSC 450 HW5 Lecture 6
# Part 3 Ensemble

# Clear workspace
rm(list=ls())
# Clear console
cat("\014")
date()

setwd("~/Personal/School/Data Science 450 Summer 2015/Cap Stone")

# load data sets for test and predictions
test <- read.csv("data_holdout_test.csv")

#Damon LaPoint's model using feature engineering
pred_dl1 <- read.csv("prediction_holdout_dl_lr.csv")
#RJ's SVM Model
pred_dl2 <- read.csv("predictions_holdout_svm_rj.csv")
#Sandeep Pridshetti's model using Average Random Forest
pred_sp1 <- read.csv("predictions_RandomForrestAvg_holdout_Sandeep.csv")
#Marc Lauzon's model using greater than 0 features binarized and Random Forrest
pred_ml1 <- read.csv("predictions_from_holdback_testing_Marc_Lauzon.csv")

#update names
n <- names(pred_sp1)
names(pred_dl1) <- n
names(pred_dl2) <- n

# test accuracy of each data set 
mean(pred_dl1$Label == test$label)
#[1] 0.9642857
mean(pred_dl2$Label == test$label)
#[1] 0.9830952
mean(pred_sp1$Label == test$label)
#[1] 0.9654762
mean(pred_ml1$Label == test$label)
#[1] 0.9552381


vote.dl1 <- c()
vote.dl2 <- c()
vote.sp1 <- c()
vote.ml1 <- c()



###############
# Loop:  Takes forever to run, use Guess to run one-off tests.


range <- 5
i <- 0
f <- 0
g <- 0
h <- 0

pred.iter <- data.frame()
pred.total <- data.frame()

for(i in 0:range)
{
  w.sp1 <- i
  #print(w.sp1)

  for(f in 0:range)
  {
    w.dl1 <- f
    #print(w.dl1)
    for(g in 0:range)
    {
      w.ml1 <- g
      
      for(h in 3:range)
      {
        w.dl2 <- h
        
        for(j in 1:nrow(pred_dl1))
        {
          if(w.sp1>0){
            vote.sp1[1:w.sp1] <- pred_sp1[j, 2]}
          else{
            vote.sp1 <- c()}
          
          if(w.dl1>0){
            vote.dl1[1:w.dl1] <- pred_dl1[j, 2]}
          else{
            vote.dl1 <- c()}
          
          if(w.ml1>0){
           vote.ml1[1:w.ml1] <- pred_ml1[j, 2]}
          else{
            vote.ml1 <- c()}
          
          if(w.dl2>0){
            vote.dl2[1:w.dl2] <- pred_dl2[j, 2]}
          else{
            vote.dl2 <- c()
          }     
          
          vote.tot <- c(vote.dl1, vote.dl2, vote.sp1, vote.ml1 )
          
          vote.table <- table(vote.tot)
          
          iter <- c(j, as.numeric(names(which.max(vote.table))))
          pred.iter <- rbind(pred.iter, iter )
                  
          
        }
        names(pred.iter) <- n
        # score for pred.iter
        score.iter <- mean(pred.iter$Label == test$label)
        pred.total <- rbind(pred.total, c(score.iter, w.sp1, w.dl1, w.ml1, w.dl2) )
        pred.iter <- data.frame()
      }
    }
  }

}

names(pred.total) <- c("accuracy", "w.sp1", "w.dl1", "w.ml1", "w.rj1")



#################
#  Guess rather than Loop
##################

w.sp1 <- 1
w.dl1 <- 2
w.ml1 <- 1
w.dl2 <- 2
# clear every time!
pred.iter <- data.frame()

for(j in 1:nrow(pred_dl1))
{
  if(w.sp1>0){
    vote.sp1[1:w.sp1] <- pred_sp1[j, 2]}
  else{
    vote.sp1 <- c()}
  
  if(w.dl1>0){
    vote.dl1[1:w.dl1] <- pred_dl1[j, 2]}
  else{
    vote.dl1 <- c()}
  
  if(w.ml1>0){
    vote.ml1[1:w.ml1] <- pred_ml1[j, 2]}
  else{
    vote.ml1 <- c()}
  
  if(w.dl2>0){
    vote.dl2[1:w.dl2] <- pred_dl2[j, 2]}
  else{
    vote.dl2 <- c()
  }     
  
  vote.tot <- c(vote.dl1, vote.dl2, vote.sp1, vote.ml1 )
  
  vote.table <- table(vote.tot)
  
  iter <- c(j, as.numeric(names(which.max(vote.table))))
  pred.iter <- rbind(pred.iter, iter )
  
  
}
names(pred.iter) <- n
# score for pred.iter
score.iter <- mean(pred.iter$Label == test$label)
pred.total <- rbind(pred.total, c(score.iter, w.sp1, w.dl1, w.ml1, w.dl2) )

##################################################################
# Get Best scores by weight and output:

max.accuracy <- max(pred.total$accuracy)

pred.total[pred.total$accuracy==max(pred.total$accuracy) ,]

write.csv(pred.total, "prediction_weights_final.csv")


