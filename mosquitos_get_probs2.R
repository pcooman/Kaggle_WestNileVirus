# For each location, the dots show the date of each record we have in the data.
# For the training data, this shows us when/where measurements were actually taken.
# But for the test data, we see a very regular set of locations/dates.  
# (This is because the test data has been filled in with many rows that don't always correspond to real measurements. 
# We need to make predictions for all of these rows, but only the ones corresponding to real measurements will be 
# used in scoring.)

### This adds in past temp and precip and whether average weekly temperature surpassed some threshold (22deg C) as in paper

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(grid)
library(reshape2)
library(randomForest)
library(ggmap)
library(geosphere)
library(maxLik)

data_dir <- "input/"
train <- read_csv(file.path(data_dir, "train.csv"))
train_full <- read_csv(file.path(data_dir, "train_Pet.csv"))

# sort according to Date, Trap, Species and NumMosquitos, to ensure proper summing of Musquito counts --> OK!
train <- arrange(train, Date,Trap,Species,desc(NumMosquitos))

train$Species <- as.factor(train$Species)
train$Date <- as.factor(train$Date)
train$Trap <- as.factor(train$Trap)



myfunction <- function(x) {
  Num_tot <- sum(x$NumMosquitos)
  Num_pos <- sum(x$NumMosquitos * x$WnvPresent)
  Num_neg <- Num_tot - Num_pos
  
  Prob <- -1
  
  Prob <- (Num_pos/Num_tot)/50
  Prob
}

x_split <- split(train,list(train$Date,train$Trap,train$Species,train$Block,train$Street,train$Latitude,train$Longitude,train$AddressAccuracy),drop = T, sep ="*")
train_probs <- lapply(x_split,myfunction)
train_probs <- cbind(read.table(text = names(train_probs), sep = "*", colClasses = "character"),unlist(train_probs))
names(train_probs) <- c("Date","Trap","Species","Block","Street","Latitude","Longitude","AddressAccuracy","Prob")
train_probs <- arrange(train_probs, Date,Trap,Species)

x <- subset(train,select = c("Date","Trap","Species","NumMosquitos","WnvPresent"))
train_totals <- lapply(split(x$NumMosquitos,list(x$Date,x$Trap,x$Species),drop = T, sep ="*"),sum, na.rm=T)
train_totals <- cbind(read.table(text = names(train_totals), sep = "*", colClasses = "character"),unlist(train_totals))
names(train_totals) <- c("Date","Trap","Species","TotalNumMosquitos")
train_totals <- arrange(train_totals, Date,Trap,Species)

train_MLE <- merge(train_probs, train_totals, by = c("Date","Trap","Species"))
train_MLE <- data.frame(train_MLE)
write_csv(train_MLE,file.path(data_dir, "train_MLE2.csv"))
