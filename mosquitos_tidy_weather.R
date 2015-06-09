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
weather <- read_csv(file.path(data_dir, "weather.csv"))

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

# Don't fix missing Sunrise, but convert to numeric
weather$Sunrise <- as.numeric(weather$Sunrise)

# Don't fix missing Sunset, but convert to numeric
weather$Sunset <- as.numeric(weather$Sunset)

# Fix missing AvgSpeed, by copying value from other station
weather$AvgSpeed <- as.numeric(weather$AvgSpeed)
for (i in which(is.na(weather$AvgSpeed))) {
  Index_other <- which(weather$Station == 3-weather$Station[i] & weather$Date == weather$Date[i])   # same date, different station
  weather$AvgSpeed[i] <- weather$AvgSpeed[Index_other]
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

# Add a column with the Average Temp of the Week
weather$Year <- year(ymd(weather$Date))
weather$Week <- week(ymd(weather$Date))
weather$Tavg <- as.numeric(weather$Tavg)
Tavg_by_Week <- lapply(split(weather$Tavg,list(weather$Year,weather$Week)),mean,na.rm=T)
Tavg_by_Week <- cbind(read.table(text = names(Tavg_by_Week), sep = ".", colClasses = "character"),unlist(Tavg_by_Week))
names(Tavg_by_Week) <- c("Year","Week","Tavg_ThisWeek")
# Add rows as "previous weeks"
Tavg_prev_Weeks <- data.frame(rep(c("2007","2008","2009","2010","2011","2012","2013","2014"),times = 3),
                                rep(c(15,16,17), each = 8),
                                rep(Tavg_by_Week$Tavg_ThisWeek[1:8],times=3))
names(Tavg_prev_Weeks) <- names(Tavg_by_Week)
Tavg_by_Week <- rbind(Tavg_prev_Weeks,Tavg_by_Week)
weather <- merge(weather,Tavg_by_Week,by = c("Year","Week"))

# Add a column with the Average Temp of the Week before
Tavg_by_Prev1Week <- Tavg_by_Week
names(Tavg_by_Prev1Week) <- c("Year","Week","Tavg_Prev1Week")
weather$Prev1Week <- weather$Week - 1
weather <- merge(weather,Tavg_by_Prev1Week,by.x = c("Year","Prev1Week"),by.y = c("Year","Week"))

# Add a column with the Average Temp of two Weeks before
Tavg_by_Prev2Week <- Tavg_by_Week
names(Tavg_by_Prev2Week) <- c("Year","Week","Tavg_Prev2Week")
weather$Prev2Week <- weather$Week - 2
weather <- merge(weather,Tavg_by_Prev2Week,by.x = c("Year","Prev2Week"),by.y = c("Year","Week"))

# Add a column with the Average Temp of the Week before
Tavg_by_Prev3Week <- Tavg_by_Week
names(Tavg_by_Prev3Week) <- c("Year","Week","Tavg_Prev3Week")
weather$Prev3Week <- weather$Week - 3
weather <- merge(weather,Tavg_by_Prev3Week,by.x = c("Year","Prev3Week"),by.y = c("Year","Week"))

## Same for TotalPrecipitation
# Add a column with the Total Precipitation of the Week
weather$PrecipTotal <- as.numeric(weather$PrecipTotal)
Precip_by_Week <- lapply(split(weather$PrecipTotal,list(weather$Year,weather$Week)),sum,na.rm=T)
Precip_by_Week <- cbind(read.table(text = names(Precip_by_Week), sep = ".", colClasses = "character"),unlist(Precip_by_Week))
names(Precip_by_Week) <- c("Year","Week","Precip_ThisWeek")
# Add rows as "previous weeks"
Precip_prev_Weeks <- data.frame(rep(c("2007","2008","2009","2010","2011","2012","2013","2014"),times = 3),
                              rep(c(15,16,17), each = 8),
                              rep(Precip_by_Week$Precip_ThisWeek[1:8],times=3))
names(Precip_prev_Weeks) <- names(Precip_by_Week)
Precip_by_Week <- rbind(Precip_prev_Weeks,Precip_by_Week)
weather <- merge(weather,Precip_by_Week,by = c("Year","Week"))

# Add a column with the Total Precipitation of the Week before
Precip_by_Prev1Week <- Precip_by_Week
names(Precip_by_Prev1Week) <- c("Year","Week","Precip_Prev1Week")
weather$Prev1Week <- weather$Week - 1
weather <- merge(weather,Precip_by_Prev1Week,by.x = c("Year","Prev1Week"),by.y = c("Year","Week"))

# Add a column with the Total Precipitation of two Weeks before
Precip_by_Prev2Week <- Precip_by_Week
names(Precip_by_Prev2Week) <- c("Year","Week","Precip_Prev2Week")
weather$Prev2Week <- weather$Week - 2
weather <- merge(weather,Precip_by_Prev2Week,by.x = c("Year","Prev2Week"),by.y = c("Year","Week"))

# Add a column with the Total Precipitation of three Weeks before
Precip_by_Prev3Week <- Precip_by_Week
names(Precip_by_Prev3Week) <- c("Year","Week","Precip_Prev3Week")
weather$Prev3Week <- weather$Week - 3
weather <- merge(weather,Precip_by_Prev3Week,by.x = c("Year","Prev3Week"),by.y = c("Year","Week"))

# Add a column with a warming variable DW, as in paper
Tbase <- 71.6    # in Fahrenheit, = 22 deg Celsius
weather$DW <- rep(0,times=nrow(weather))
weather$DW[weather$Tavg_ThisWeek > Tbase] <- weather$Tavg_ThisWeek[weather$Tavg_ThisWeek > Tbase] - Tbase
# Add a column with a cooling variable DC, as in paper
weather$DC <- rep(0,times=nrow(weather))
weather$DC[weather$Tavg_ThisWeek < Tbase] <- Tbase - weather$Tavg_ThisWeek[weather$Tavg_ThisWeek < Tbase]

# Clean up Week columns
weather$Week <- NULL
weather$Prev1Week <- NULL
weather$Prev2Week <- NULL
weather$Prev3Week <- NULL

weather$Heat <- as.numeric(weather$Heat)
weather$Cool <- as.numeric(weather$Cool)

weather$Depth <- NULL


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

weather_merged$Sunrise.y <- NULL
weather_merged$Sunset.y <- NULL

write_csv(weather_merged,file.path(data_dir, "weather_tidy.csv"))