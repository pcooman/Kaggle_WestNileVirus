# For each location, the dots show the date of each record we have in the data.
# For the training data, this shows us when/where measurements were actually taken.
# But for the test data, we see a very regular set of locations/dates.  
# (This is because the test data has been filled in with many rows that don't always correspond to real measurements. 
# We need to make predictions for all of these rows, but only the ones corresponding to real measurements will be 
# used in scoring.)

### This version replaces missing values by means

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
train <- read_csv(file.path(data_dir, "train.csv"))
test <- read_csv(file.path(data_dir, "test.csv"))
spray <- read_csv(file.path(data_dir, "spray.csv"))
weather <- read_csv(file.path(data_dir, "weather.csv"))
sample <- read_csv(file.path(data_dir, "SampleSubmission.csv"))
#train$Usage <- "Train"
#test$Usage <- "Test"

# Get labels
labels <- train$WnvPresent

# Not using codesum for this benchmark
weather$CodeSum <- NULL

# Fix missing WetBulb data in weather
# NAs at index: 849 2411 2413 2416
Index_missing <- which(is.na(weather$WetBulb))
for (i in Index_missing) {
  Index_other <- which(weather$Station == 3-weather$Station[i] & weather$Date == weather$Date[i])   # same date, different station
  weather$WetBulb[i] <- weather$WetBulb[Index_other]
}
# weather$WetBulb <- NULL           # contains NAs (for both station 1 and 2)

# Fix missing Tavg by averaging Tmax and Tmin
Index_missing <- which(grepl("M", weather$Tavg))
for (i in Index_missing) {
  weather$Tavg[i] <- (weather$Tmax[i]+weather$Tmin[i])/2
}

# Fix missing Depart by taking value from other station
Index_missing <- which(grepl("M", weather$Depart))
for (i in Index_missing) {
  Index_other <- which(weather$Station == 3-weather$Station[i] & weather$Date == weather$Date[i])   # same date, different station
  weather$Depart[i] <- weather$Depart[Index_other]
}

# Fix missing Heat by taking value from other station
Index_missing <- which(grepl("M", weather$Heat))
for (i in Index_missing) {
  Index_other <- which(weather$Station == 3-weather$Station[i] & weather$Date == weather$Date[i])   # same date, different station
  weather$Heat[i] <- weather$Heat[Index_other]
}

# Fix missing Cool by taking value from other station
Index_missing <- which(grepl("M", weather$Cool))
for (i in Index_missing) {
  Index_other <- which(weather$Station == 3-weather$Station[i] & weather$Date == weather$Date[i])   # same date, different station
  weather$Cool[i] <- weather$Cool[Index_other]
}

# Fix missing PrecipTotal by replacing with the monthly average
weather$PrecipTotal <- as.numeric(weather$PrecipTotal)
weather$Month <- month(ymd(weather$Date))
Precip_by_month <- split(weather$PrecipTotal,weather$Month)
Precip_avg_by_month <- sapply(Precip_by_month,mean,na.rm=T)
Index_missing <- which(is.na(weather$PrecipTotal))
for (i in Index_missing) {
  Index <- weather$Month[i]
  weather$PrecipTotal[i] <- Precip_avg_by_month[Index-4]
}
weather$Month <- NULL

# Fix missing AverageSpeed by taking value from other station
Index_missing <- which(grepl("M", weather$AvgSpeed))
for (i in Index_missing) {
  Index_other <- which(weather$Station == 3-weather$Station[i] & weather$Date == weather$Date[i])   # same date, different station
  weather$AvgSpeed[i] <- weather$AvgSpeed[Index_other]
}

# Split station 1 and 2 and join horizontally
weather_station1 <- subset(weather,subset = (weather$Station==1))
weather_station2 <- subset(weather,subset = (weather$Station==2))
weather_merged <- merge(weather_station1, weather_station2, by = "Date")

# Drop any column with missing values
logical_M <- grepl("M", weather_merged)
for (i in length(logical_M):1) {if (logical_M[i] == TRUE) {weather_merged[,i]<-NULL}}
logical_ <- grepl("-", weather_merged)
for (i in length(logical_):1) {if (logical_[i] == TRUE) {weather_merged[,i]<-NULL}}
logical_T1 <- grepl("T", weather_merged)
for (i in length(logical_T1):1) {if (logical_T1[i] == TRUE) {weather_merged[,i]<-NULL}}
logical_T2 <- grepl(" T", weather_merged)
for (i in length(logical_T2):1) {if (logical_T2[i] == TRUE) {weather_merged[,i]<-NULL}}
logical_T3 <- grepl("  T", weather_merged)
for (i in length(logical_T3):1) {if (logical_T3[i] == TRUE) {weather_merged[,i]<-NULL}}

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

# drop address columns
train$Address <- NULL
train$AddressNumberAndStreet <- NULL
train$WnvPresent <- NULL
train$NumMosquitos <- NULL

test$Id <- NULL
test$Address <- NULL
test$AddressNumberAndStreet <- NULL

# Merge with weather data
train <- merge(train,weather_merged, by="Date")
test <- merge(test,weather_merged, by="Date")
train$Date <- NULL
test$Date <- NULL

# Convert categorical data to numbers
  # Replace Species by numbers
train$Species <- as.factor(train$Species)
train$Species <- match(train$Species, levels(train$Species), nomatch = 8)
test$Species <- match(test$Species, train$Species, nomatch = 8)

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

# Train random forest
set.seed(1)

rf <- randomForest(train, labels, ntree=1000, importance=TRUE, nodesize = 1)
imp <- importance(rf, type=1)
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
Wnv_pred <- predict(rf, test)
#Wnv_pred[Wnv_pred_rf<0.15] <- 0
#Wnv_pred[Wnv_pred_rf>0.15] <- 1

Wnv_pred[Wnv_pred > 1] <- 1
Wnv_pred[Wnv_pred <= 0] <- 0

submission <- data.frame(Id = 1:nrow(test), WnvPresent = Wnv_pred)

write.csv(submission, file = "Mosquito_random_forest_in_R_FilterSpeciesMonth.csv", row.names=FALSE)