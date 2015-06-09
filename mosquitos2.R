# For each location, the dots show the date of each record we have in the data.
# For the training data, this shows us when/where measurements were actually taken.
# But for the test data, we see a very regular set of locations/dates.  
# (This is because the test data has been filled in with many rows that don't always correspond to real measurements. 
# We need to make predictions for all of these rows, but only the ones corresponding to real measurements will be 
# used in scoring.)

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(grid)
library(reshape2)

data_dir <- "input/"
train <- read_csv(file.path(data_dir, "train.csv"))
test <- read_csv(file.path(data_dir, "test.csv"))
spray <- read_csv(file.path(data_dir, "spray.csv"))
weather <- read_csv(file.path(data_dir, "weather.csv"))
sample <- read_csv(file.path(data_dir, "SampleSubmission.csv"))
train$Usage <- "Train"
test$Usage <- "Test"

# sort according to Date, Trap, Species and NumMosquitos, to ensure proper summing of Musquito counts --> OK!
train <- arrange(train, Date,Trap,Species,desc(NumMosquitos))

train$Species <- as.factor(train$Species)
train$Date <- as.factor(train$Date)
train$Trap <- as.factor(train$Trap)


train2 <- split(train, train$Species, drop=T)
train3 <- unsplit(train2,train$Species)

train2 <- split(train$NumMosquitos, list(train$Date,train$Trap,train$Species), drop = T)
train3 <- unsplit(train2, list(train$Date,train$Trap,train$Species))
train3 <- unsplit(lapply(split(train$NumMosquitos, list(train$Date,train$Trap,train$Species), drop = T),sum),list(train$Date,train$Trap,train$Species))


train2 <- lapply(split(train$NumMosquitos, list(train$Date,train$Trap,train$Species), drop = T),sum)

train3 <- read.table(text = names(train2), sep = ".", colClasses = "character")

train <- arrange(train, Date,Trap,Species,desc(NumMosquitos))

train$Species <- as.factor(train$Species)
train$Date <- as.factor(train$Date)
train$Trap <- as.factor(train$Trap)

Num_by_DateTrapSpecies <- lapply(split(train$NumMosquitos, list(train$Date,train$Trap,train$Species), drop = T),sum)
train_summed <- cbind(read.table(text = names(Num_by_DateTrapSpecies), sep = ".", colClasses = "character"),unlist(Num_by_DateTrapSpecies))



df <- data.frame(matrix(unlist(train2), nrow=8475, byrow=T))

require(stats); require(graphics)
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)

### Calculate 'z-scores' by group (standardize to mean zero, variance one)
z <- unsplit(lapply(split(x, g), sum), g)


train$Species <- as.factor(train$Species)
train$Date <- as.factor(train$Date)
train$Trap <- as.factor(train$Trap)
train2 <- split(train$NumMosquitos, c(train$Date,train$Trap,train$Species))