# Damon LaPoint
# DSC 450 
# Team 3 Group Project

# Create extra data with squishing and skewing

setwd("~/Personal/School/Data Science 450 Summer 2015/Cap Stone")

# Clear workspace
rm(list=ls())
# Clear console
cat("\014")
date()

# use holdout as basis for supplement
data <- read.csv("data_train_minus_holdout.csv")

################################
# Matrix for 28 x 28
########################################
# determine column and row for each pixel
pos <- 1

rc.matrics <- data.frame()
for(pos in 1:784)
{
  x <- c()
  # assign row and columns
  if ((pos ) %% 28 == 0)
  { r <- floor((pos ) / 28) 
    c <- 28
  }
  else 
  { r <- floor((pos ) / 28) + 1
    c <- (pos) %% 28
  }  
  x <- c( pos, r, c )
  #print(x)
  rc.matrics <- rbind(rc.matrics, x )
}

names(rc.matrics) <- c('data_column_num', 'row_num', 'col_num')

################################
## Define Functions
################################

# Squish Data

squish_image_data=function(dataset,col_remove){
  i = 1
  p <- 1
  squish <- c()
  beg <- c()
  nd <- c()
  moveleft <- ceiling(length(col_remove) / 2)
  moveright <- floor(length(col_remove)/2)
  for(i in 1:28)
  {
    # depending on odd or even remove half from beginning, repeat for number of removals
    for(p in 1:moveleft)
    {
      fill <- rc.matrics[which(rc.matrics$row_num %in% i & rc.matrics$col_num == 1),1]
      beg <- c(beg, fill)
    }
    mid <- rc.matrics[which(rc.matrics$row_num %in% i & !rc.matrics$col_num %in% col_remove),1]
    # remove from end if more than 1
    # depending on odd or even remove half from beginning, repeat for number of removals
    for(p in 1:moveright)
    {
      fill <- rc.matrics[which(rc.matrics$row_num %in% i & rc.matrics$col_num == 28),1]
      nd <- c(nd, fill)
    } 
    squish <- c(squish, unlist(beg), unlist(mid), unlist(nd))
    beg <- c()
    nd <- c()
  }
  dataset = dataset[,squish]
}

# Skew Data

skew_image_data=function(dataset, amount) {
  
  sections <- abs(amount*2) + 1
  section.length <- floor( 28 / sections)
  section.mod <- 28 %% sections
  i <- 1
  p <- 1
  skew <- c()
  beg <- c()
  nd <- c()
  r <- 1
  rows <- c()
  rows.tot <- c()
  
  for(i in 1:sections)
  {
    # odd <- i %% 2 > 0 determine odd sections and make them one bigger
    if(section.mod > 0 & i %% 2 > 0)
    {
      e <- r + (section.length)
      rows <- c(r:e)
      rows.tot <- c(rows.tot, rows)
      section.mod <- section.mod - 1
    }
    else
    {
      e <- r + (section.length - 1)
      rows <- c(r:e)
      rows.tot <- c(rows.tot, rows)
    }
    
    #account for postive and negative skewing.
    if(amount > 0 )
    {
      sec.amount <- (amount + 1) - i
    }
    else if(amount == 0 )
    {print("error")}
    else
    {
      sec.amount <- i + (amount - 1)
    }
    # for each row in section (r:e), determine which columns should be filled with 0
    for(p in r:e)
    {
      if(sec.amount>0)
      {
        #loop for sec.amount
        for(z in 1:sec.amount)
        {
          fill <- rc.matrics[which(rc.matrics$row_num %in% p & rc.matrics$col_num == 1),1]
          beg <- c(beg, fill)
        }
        
        #calculate columns for this row:
        moveright <- 28 - sec.amount
        c <- c(1:moveright)
        mid <- rc.matrics[which(rc.matrics$row_num %in% p & rc.matrics$col_num %in% c),1]
        # remove from end if more than 1
        skew <- c(skew, unlist(beg), unlist(mid)) #, unlist(nd)
      }
      else if(sec.amount == 0)
      {
        # no movement
        mid <- rc.matrics[which(rc.matrics$row_num %in% p ),1]
        skew <- c(skew,  unlist(mid)) 
      }
      else
      {
        #loop for sec.amount to fill in end
        for(z in sec.amount:-1)
        {
          fill <- rc.matrics[which(rc.matrics$row_num %in% p & rc.matrics$col_num == 28),1]
          nd <- c(nd, fill)
        }
        
        #calculate columns for this row:
        moveleft <- 1 + sec.amount * (-1)
        c <- c(moveleft:28)
        mid <- rc.matrics[which(rc.matrics$row_num %in% p & rc.matrics$col_num %in% c),1]
        # add middle and end fill
        skew <- c(skew, unlist(mid), unlist(nd))
      }
      fill <- c()
      beg <- c()
      nd <- c()
    }
    r <- e + 1
  }
  dataset = dataset[,skew]
}

###############################
## Create Data Samples
## sup1 = 12.5 % to squish some

samples <- nrow(data) / 8

supplement.1 <- data.frame()

set.seed(51)
for(i in 0:9)
{
  data.sub <- subset(data, label == i)
  supplement.1 <- rbind(supplement.1, data.sub[sample( 1:nrow(data.sub),round(samples * nrow(data[which(data$label == i),]) / nrow(data)),replace = FALSE),])
}

# remove test rows from training set
sup.1.rows <- row.names(supplement.1)
data.rem <- data[!rownames(data) %in% sup.1.rows,]

data.names <- names(data.rem)


#remove labels
sup.1.label <- supplement.1$label
supplement.1$label <- c()

# sku data by -3
supplement.1.skew <- skew_image_data(supplement.1, -3)
supplement.1.skew$label <- sup.1.label
names(supplement.1.skew) <- c(data.names[2:785], data.names[1])

########################################

supplement.2 <- data.frame()
set.seed(52)
for(i in 0:9)
{
  data.sub <- subset(data.rem, label == i)
  supplement.2 <- rbind(supplement.2, data.sub[sample( 1:nrow(data.sub),round(samples * nrow(data.rem[which(data.rem$label == i),]) / nrow(data.rem)),replace = FALSE),])
}

# remove test rows from training set
sup.2.rows <- row.names(supplement.2)
data.rem <- data.rem[!rownames(data.rem) %in% sup.2.rows,]

#remove labels
sup.2.label <- supplement.2$label
supplement.2$label <- c()

# sku data by 4
#skew_image_data(supplement.2,-3)
supplement.2.skew <- skew_image_data(supplement.2, 4)
supplement.2.skew$label <- sup.2.label
names(supplement.2.skew) <- c(data.names[2:785], data.names[1])

###########################################
supplement.3 <- data.frame()
set.seed(53)
for(i in 0:9)
{
  data.sub <- subset(data.rem, label == i)
  supplement.3 <- rbind(supplement.3, data.sub[sample( 1:nrow(data.sub),round(samples * nrow(data.rem[which(data.rem$label == i),]) / nrow(data.rem)),replace = FALSE),])
}

# remove test rows from training set
sup.3.rows <- row.names(supplement.3)
data.rem <- data.rem[!rownames(data.rem) %in% sup.3.rows,]

#remove labels
sup.3.label <- supplement.3$label
supplement.3$label <- c()

# quish data for columns 8,12,16,20
#skew_image_data(supplement.2,-3)
supplement.3.skew <- squish_image_data(supplement.3, c(8, 12, 16, 20))
supplement.3.skew$label <- sup.3.label
names(supplement.3.skew) <- c(data.names[2:785], data.names[1])

###########################################
supplement.4 <- data.frame()
set.seed(54)
for(i in 0:9)
{
  data.sub <- subset(data.rem, label == i)
  supplement.4 <- rbind(supplement.4, data.sub[sample( 1:nrow(data.sub),round(samples * nrow(data.rem[which(data.rem$label == i),]) / nrow(data.rem)),replace = FALSE),])
}

# remove test rows from training set
sup.4.rows <- row.names(supplement.4)
data.rem <- data.rem[!rownames(data.rem) %in% sup.4.rows,]

#remove labels
sup.4.label <- supplement.4$label
supplement.4$label <- c()

# quish data for columns 8,12,16,20
#skew_image_data(supplement.2,-3)
supplement.4.skew <- squish_image_data(supplement.4, c(9,14,19))
supplement.4.skew$label <- sup.4.label
names(supplement.4.skew) <- c(data.names[2:785], data.names[1])

##########################################
supplement.5 <- data.frame()
set.seed(55)
for(i in 0:9)
{
  data.sub <- subset(data.rem, label == i)
  supplement.5 <- rbind(supplement.5, data.sub[sample( 1:nrow(data.sub),round(samples * nrow(data.rem[which(data.rem$label == i),]) / nrow(data.rem)),replace = FALSE),])
}

# remove test rows from training set
sup.5.rows <- row.names(supplement.5)
data.rem <- data.rem[!rownames(data.rem) %in% sup.5.rows,]

#remove labels
sup.5.label <- supplement.5$label
supplement.5$label <- c()

# quish data for columns 8,12,16,20
#skew_image_data(supplement.2,-3)
supplement.5.skew <- squish_image_data(supplement.5, c(7,11,14,18,21))
supplement.5.skew$label <- sup.5.label
names(supplement.5.skew) <- c(data.names[2:785], data.names[1])

########################################

supplement.6 <- data.frame()
set.seed(56)
for(i in 0:9)
{
  data.sub <- subset(data.rem, label == i)
  supplement.6 <- rbind(supplement.6, data.sub[sample( 1:nrow(data.sub),round(samples * nrow(data.rem[which(data.rem$label == i),]) / nrow(data.rem)),replace = FALSE),])
}

# remove test rows from training set
sup.6.rows <- row.names(supplement.6)
data.rem <- data.rem[!rownames(data.rem) %in% sup.6.rows,]

#remove labels
sup.6.label <- supplement.6$label
supplement.6$label <- c()

# sku data by 2

supplement.6.skew <- skew_image_data(supplement.6, 2)
supplement.6.skew$label <- sup.6.label
names(supplement.6.skew) <- c(data.names[2:785], data.names[1])

###########################################
# combine supplemental data
data.sup <- rbind(supplement.1.skew, supplement.2.skew, supplement.3.skew, supplement.4.skew) #, supplement.5.skew, supplement.6.skew)

write.csv(data.sup, "data_train_supplement.csv", row.names = FALSE)

##########################################
## Test Images
###########################################

for(i in 1:4)
{
  # assiign row to visualize
  r <-2000 #i #5500
  # display lable for row
  print(i)
  print(sup.6.label[r])
  
  #convert to 28 by 28 matrix for row = r
  image.row <- matrix(unlist(supplement.2[r,1:784]), nrow = 28, ncol = 28, byrow = TRUE)
  #plot matrix with max value as darkest
  image(t(image.row[28:1,]), axes = TRUE, col = grey(seq(1, 0, length = max(image.row))))
  
  #convert to 28 by 28 matrix for row = r
  image.row <- matrix(unlist(supplement.2.skew[r,1:784]), nrow = 28, ncol = 28, byrow = TRUE)
  #plot matrix with max value as darkest
  image(t(image.row[28:1,]), axes = TRUE, col = grey(seq(1, 0, length = max(image.row))))
}