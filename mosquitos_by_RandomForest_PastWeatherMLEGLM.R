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
library(ggmap)
library(geosphere)
library(glmnet)

data_dir <- "input/"
train_MLE <- read.csv(file.path(data_dir, "train_MLE.csv"))
train <- read.csv(file.path(data_dir, "train_full.csv"))

test <- read.csv(file.path(data_dir, "test.csv"))
# spray <- read_csv(file.path(data_dir, "spray.csv"))
weather <- read_csv(file.path(data_dir, "weather_tidy.csv"))
sample <- read.csv(file.path(data_dir, "SampleSubmission.csv"))

# Add columns for year month and day
train$Year <- year(ymd(train$Date))
train$Month <- month(ymd(train$Date))
train$Day <- day(ymd(train$Date))

test$Year <- year(ymd(test$Date))
test$Month <- month(ymd(test$Date))
test$Day <- day(ymd(test$Date))

# Add integer latitude/longitude columns
train$Latitude_int <- as.integer(train$Latitude)
train$Longitude_int <- as.integer(train$Longitude)
test$Latitude_int <- as.integer(test$Latitude)
test$Longitude_int <- as.integer(test$Longitude)

test$Id <- NULL
test$Address <- NULL
test$AddressNumberAndStreet <- NULL

# Merge with weather data
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)
train <- merge(train,weather_merged, by="Date")
test <- merge(test,weather_merged, by="Date")
train$Date <- NULL
test$Date <- NULL

# Convert categorical data to numbers
  # Replace Species by numbers
train$Species <- as.factor(train$Species)
Species_levels <- levels(train$Species)
train$Species <- match(train$Species, Species_levels, nomatch = 8)
test$Species <- as.factor(test$Species)
test$Species <- match(test$Species, Species_levels, nomatch = 8)

# Replace Street by numbers
train$Street <- as.factor(train$Street)
test$Street <- as.factor(test$Street)
Street_levels <- unique(c(levels(train$Street),levels(test$Street)))
train$Street <- match(train$Street, Street_levels)
test$Street <- match(test$Street, Street_levels)

# Replace Trap by numbers
train$Trap <- as.factor(train$Trap)
test$Trap <- as.factor(test$Trap)
Trap_levels <- unique(c(levels(train$Trap),levels(test$Trap)))
train$Trap <- match(train$Trap, Trap_levels)
test$Trap <- match(test$Trap, Trap_levels)

# Train random forest on Number of Mosquitos
set.seed(1)

labels1 <- train$TotalNumMosquitos
labels2 <- train$Prob

train$TotalNumMosquitos <- NULL
train$Prob <- NULL
train$Year.x <- NULL
train$Year.y <- NULL
test$Year.x <- NULL
test$Year.y <- NULL


train <- as.matrix(train)
test <- as.matrix(test)

fit1 <- glmnet(train, labels1)
# Predict Random forest
TotalNumMosquitos_pred <- predict(fit1, newx = test,type = "response")

# Train random forest on Probability of WNV
set.seed(1)

fit2 <- glmnet(train, labels2)
# Predict Random forest
Prob_pred <- predict(fit2, test)

Prob_pred[Prob_pred > 1] <-1
Prob_pred[Prob_pred < 0] <-0

# Set all other species to zero
Prob_pred[test[,1] == 8] <- 0

submission <- data.frame(Id = 1:nrow(test), WnvPresent = 1 - (1-Prob_pred)^TotalNumMosquitos_pred)
#submission$WnvPresent[submission$WnvPresent >= 0.5] <- 1
#submission$WnvPresent[submission$WnvPresent < 0.5] <- 0

write.csv(submission, file = "Mosquito_GLMnet_in_R_PastWeatherMLE.csv", row.names=FALSE)