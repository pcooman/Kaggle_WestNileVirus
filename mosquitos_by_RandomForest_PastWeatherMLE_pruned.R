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

data_dir <- "input/"
train <- read.csv(file.path(data_dir, "train_full.csv"))
test <- read.csv(file.path(data_dir, "test.csv"))
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
train <- merge(train,weather, by="Date")
test <- merge(test,weather, by="Date")
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

train$Trap <- NULL
train$Block <- NULL
train$Street <- NULL
train$AddressAccuracy <- NULL
train$Latitude_int <- NULL
train$Longitude_int <- NULL
train$Day <- NULL
train$Tavg_Prev2Week.x <- NULL
train$Tavg_Prev2Week.y <- NULL
train$Tavg_Prev3Week.x <- NULL
train$Tavg_Prev3Week.y <- NULL
train$Precip_Prev2Week.x <- NULL
train$Precip_Prev2Week.y <- NULL
train$Precip_Prev3Week.x <- NULL
train$Precip_Prev3Week.y <- NULL

test$Trap <- NULL
test$Block <- NULL
test$Street <- NULL
test$AddressAccuracy <- NULL
test$Latitude_int <- NULL
test$Longitude_int <- NULL
test$Day <- NULL
test$Tavg_Prev2Week.x <- NULL
test$Tavg_Prev2Week.y <- NULL
test$Tavg_Prev3Week.x <- NULL
test$Tavg_Prev3Week.y <- NULL
test$Precip_Prev2Week.x <- NULL
test$Precip_Prev2Week.y <- NULL
test$Precip_Prev3Week.x <- NULL
test$Precip_Prev3Week.y <- NULL





#tuneRF(train, labels1, mtryStart = 6, ntreeTry=50, stepFactor=2, improve=0.05,
#       trace=TRUE, plot=TRUE, doBest=FALSE)

rf1 <- randomForest(train, labels1, ntree=1000, importance=TRUE, nodesize = 5)
imp <- importance(rf1, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

windows()
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance: Total number of mosquitos\n") +
  theme(plot.title=element_text(size=18))
print(p)

# Predict Random forest
TotalNumMosquitos_pred <- predict(rf1, test)

# Train random forest on Probability of WNV
set.seed(1)

rf2 <- randomForest(train, labels2, ntree=1000, importance=TRUE, nodesize = 5)
imp <- importance(rf2, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

windows()
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
print(p)

# Predict Random forest
Prob_pred <- predict(rf2, test)
Prob_pred[Prob_pred > 1] <-1
Prob_pred[Prob_pred < 0] <-0

# Set all other species to zero
Prob_pred[test$Species == 8] <- 0

submission <- data.frame(Id = 1:nrow(test), WnvPresent = 1 - (1-Prob_pred)^TotalNumMosquitos_pred)
#submission$WnvPresent[submission$WnvPresent >= 0.5] <- 1
#submission$WnvPresent[submission$WnvPresent < 0.5] <- 0

# handle unspecified culex species
#for (i in seq(from = 8, to = nrow(submission), by = 8)) {
#  submission$WnvPresent[i] <- 0 # mean(submission$WnvPresent[(i-7):(i-1)])
#}

write.csv(submission, file = "Mosquito_random_forest_in_R_PastWeatherMLE_pruned.csv", row.names=FALSE)