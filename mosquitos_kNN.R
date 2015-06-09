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

Num_by_all <- lapply(split(train$NumMosquitos, list(train$Date,train$Trap,train$Species,train$Block,train$Street,train$AddressNumberAndStreet,train$Latitude,train$Longitude), drop = T,sep="*"),sum)
train_Num <- cbind(read.table(text = names(Num_by_all), sep = "*", colClasses = "character"),unlist(Num_by_all))
WnvP_by_all <- lapply(split(train$WnvPresent, list(train$Date,train$Trap,train$Species), drop = T,sep="*"),max)
train_WnvP <- cbind(read.table(text = names(WnvP_by_all), sep = "*", colClasses = "character"),unlist(WnvP_by_all))

train_merged <- merge(train_Num,train_WnvP, by = c("V1","V2","V3"))
names(train_merged) <- c("Date","Trap","Species","Block","Street","AddressNumberAndStreet","Latitude","Longitude","NumMosquitos","WnvPresent")

# write_csv(train, file.path(data_dir, "train_check.csv"))

## If the number of musquitos is larger than 50, the record gets split over multiple rows --> OK!
#logic <- as.logical(rep(TRUE,times = nrow(train)))
#for (i in nrow(train):1) {
#  if (train[i,]$NumMosquitos == 50) {
#    train[i,]$NumMosquitos <- train[i,]$NumMosquitos + train[i+1,]$NumMosquitos
#    train[i,]$WnvPresent <- max(train[i,]$WnvPresent, train[i+1,]$WnvPresent)
#    logic[i+1] <- FALSE
#  }
#}
#train <- train[which(logic),]

## If a species is not detected, there is no record --> add a row for each level of Species (with NumMosquitos = 0) --> OK!
train_merged$Species <- as.factor(train_merged$Species)
levels(train_merged$Species) <- c("Num_Erraticus","Num_Pipiens","Num_Pipiens.Restuans","Num_Restuans","Num_Salinarius","Num_Tarsalis","Num_Territans")
train_cast_count <- dcast(train_merged,  Date + Street + Trap + AddressNumberAndStreet + Latitude +
                      Longitude ~ Species, value.var = "NumMosquitos", fill = 0, fun.aggregate = sum)
  
# Add a column with total Num of mosquitos  --> OK!
train_cast_count_split <- subset(train_cast_count,select = levels(train_merged$Species))
Num_Total <- apply(train_cast_count_split,1,sum)
train_cast_count$Num_Total <- Num_Total
#write_csv(train_cast_count, file.path(data_dir, "train_check.csv"))

  # Add columns indicating in which species the virus was found
train_cast_virus <- dcast(train_merged,  Date + Trap ~ Species, value.var = "WnvPresent", fill = 0, fun.aggregate = sum)
names(train_cast_virus) <- c("Date","Trap","WnvP_in_Erraticus","WnvP_in_Pipiens","WnvP_in_Pipiens.Restuans","WnvP_in_Restuans","WnvP_in_Salinarius","WnvP_in_Tarsalis","WnvP_in_Territans")

  # Add a column with WnvP from any Mosquito species --> to do!
train_cast_virus_split <- subset(train_cast_virus,select = c("WnvP_in_Erraticus","WnvP_in_Pipiens","WnvP_in_Pipiens.Restuans","WnvP_in_Restuans","WnvP_in_Salinarius","WnvP_in_Tarsalis","WnvP_in_Territans"))
WnvP_Total <- apply(train_cast_virus_split,1,max)
train_cast_virus$WnvP_Total <- WnvP_Total

  # Merge counts and virus-present data for train
train_cast_merged <- merge(train_cast_count,train_cast_virus,by = c("Date","Trap"))   
write_csv(train_cast_merged, file.path(data_dir, "train_check.csv"))

# Add a column indicating the trap location was sprayed that day

# Add a column with the nearest weather station

# Add columns with weather info


# Simple approach: k-nearest neighbors
# find the nearest trap and take the average WnvP over the four training years
# Set to 1 if P>0.5
train_2007 <- subset(train_cast_merged,subset = grepl("^2007",train_cast_merged$Date))
train_2009 <- subset(train_cast_merged,subset = grepl("^2007",train_cast_merged$Date))
train_2011 <- subset(train_cast_merged,subset = grepl("^2007",train_cast_merged$Date))
train_2013 <- subset(train_cast_merged,subset = grepl("^2007",train_cast_merged$Date))

Date <- test$Date[1]
Month <- month(Date)
Day <- day(Date)

Date_to_match <- paste(Month,Day,sep="-")

Traps_checked <- subset(train_2007,subset = grepl(Date_to_match,train_2007$Date)) , select = c("Trap","Longitude","Latitude"))
Trap_locs_train <- subset