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
library(randomForest)
library(ggmap)
library(geosphere)

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

#train$Species <- as.factor(train$Species)
#train$Date <- as.factor(train$Date)
#train$Trap <- as.factor(train$Trap)

#Num_by_all <- lapply(split(train$NumMosquitos, list(train$Date,train$Trap,train$Species,train$Block,train$Street,train$AddressNumberAndStreet,train$Latitude,train$Longitude), drop = T,sep="*"),sum)
#train_Num <- cbind(read.table(text = names(Num_by_all), sep = "*", colClasses = "character"),unlist(Num_by_all))
#WnvP_by_all <- lapply(split(train$WnvPresent, list(train$Date,train$Trap,train$Species), drop = T,sep="*"),max)
#train_WnvP <- cbind(read.table(text = names(WnvP_by_all), sep = "*", colClasses = "character"),unlist(WnvP_by_all))

#train_merged <- merge(train_Num,train_WnvP, by = c("V1","V2","V3"))
#names(train_merged) <- c("Date","Trap","Species","Block","Street","AddressNumberAndStreet","Latitude","Longitude","NumMosquitos","WnvPresent")

train_merged <- train

# Replace months by numbers
train_merged$Year <- year(ymd(train_merged$Date))
train_merged$Month <- month(ymd(train_merged$Date))
train_merged$Day <- day(ymd(train_merged$Date))

test$Year <- year(ymd(test$Date))
test$Month <- month(ymd(test$Date))
test$Day <- day(ymd(test$Date))

# Replace Species by numbers
train_merged$Species <- as.factor(train_merged$Species)
train_merged$Species <- match(train_merged$Species, levels(train_merged$Species), nomatch = 8)

test$Species <- match(test$Species, levels(train_merged$Species), nomatch = 8)

# Add a column indicating the trap location was sprayed that day

# Add a column with the nearest weather station
train_merged$Longitude <- as.numeric(train_merged$Longitude)
train_merged$Latitude <- as.numeric(train_merged$Latitude)

test$Longitude <- as.numeric(test$Longitude)
test$Latitude <- as.numeric(test$Latitude)

distance <- function(longitude, latitude) {
  
  #Euclidian distances aren't accurate because we are on a sphere
  #dist1 <- sqrt((stations[1,]$Latitude-latitude)^2+(stations[1,]$Longitude-longitude)^2)
  #dist2 <- sqrt((stations[2,]$Latitude-latitude)^2+(stations[2,]$Longitude-longitude)^2)
  
  #Instead, let's use distHaversine from geosphere
  #Haversine distance : http://en.wikipedia.org/wiki/Haversine_formula
  
  dist1 <- distHaversine(c(stations[1,]$Longitude,stations[1,]$Latitude),c(longitude,latitude))
  dist2 <- distHaversine(c(stations[2,]$Longitude,stations[2,]$Latitude),c(longitude,latitude))
  
  
  if(dist1<dist2){
    return(1)
  }
  return(2)
}

stations<-data.frame(c(1,2),c(41.995,41.786),c(-87.933,-87.752))
names(stations)<-c("Station","Latitude","Longitude")

train_merged$Station<-mapply(distance,train_merged$Longitude,train_merged$Latitude)
test$Station<-mapply(distance,test$Longitude,test$Latitude)

# Add columns with weather info
Tavg <- (weather$Tmax + weather$Tmin)/2
weather$Tavg[weather$Tavg == "M"] <- Tavg[weather$Tavg == "M"]

weather$Tavg <- as.numeric(weather$Tavg)
#weather$Heat <- as.numeric(weather$Heat)
#weather$Cool <- as.numeric(weather$Cool)
weather$SnowFall <- as.numeric(weather$SnowFall)
weather$PrecipTotal <- as.numeric(weather$PrecipTotal)
weather$AvgSpeed <- as.numeric(weather$AvgSpeed)

SnowAvg <- mean(weather$SnowFall, na.rm=T)
weather$SnowFall[is.na(weather$SnowFall)] <- SnowAvg
SpeedAvg <- mean(weather$AvgSpeed, na.rm=T)
weather$AvgSpeed[is.na(weather$AvgSpeed)] <- SpeedAvg
PrecipAvg <- mean(weather$PrecipTotal, na.rm=T)
weather$PrecipTotal[is.na(weather$PrecipTotal)] <- PrecipAvg
WetAvg <- mean(weather$WetBulb, na.rm=T)
weather$WetBulb[is.na(weather$WetBulb)] <- WetAvg

train_merged$Date <- as.Date(train_merged$Date)
train_merged <- merge(train_merged,weather,by = c("Date","Station"))
test_merged <- merge(test,weather,by = c("Date","Station"))

# Train random forest
set.seed(1)

extractFeatures <- function(data) {
  features <- c("Year",
                "Month",
                "Day",
                "Latitude",
                "Longitude",
              #  "NumMosquitos",    unknown in test.csv!
                "Species",
                "Station",
                "Tmax",
                "Tmin",
              #  "Tavg",            has NAs
                "DewPoint",
                "WetBulb",
                "SnowFall",
                "PrecipTotal",
                "ResultSpeed",
                "ResultDir",
                "AvgSpeed")
  
  return(data[,features])
}

trainFea <- extractFeatures(train_merged)
testFea  <- extractFeatures(test_merged)

submission <- data.frame(Id = test$Id, WnvPresent=NA)

rf <- randomForest(trainFea, train_merged$WnvPresent, ntree=1000, importance=TRUE, nodesize = 1)
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

# ggsave("Mosquito_feature_importance.png", p)

# Predict Random forest
Wnv_pred_rf <- predict(rf, testFea)
Wnv_pred[Wnv_pred_rf<0.15] <- 0
Wnv_pred[Wnv_pred_rf>0.15] <- 1

# Wnv_pred[Wnv_pred > 0.5] <- 1
# Wnv_pred[Wnv_pred <= 0.5] <- 0

submission_complete <- data.frame(Id = test$Id, WnvPresent = Wnv_pred)

write.csv(submission_complete, file = "Mosquito_random_forest_in_R.csv", row.names=FALSE)