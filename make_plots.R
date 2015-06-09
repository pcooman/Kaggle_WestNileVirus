library(dplyr)
library(readr)
library(ggmap)
library(lubridate)
library(corrplot)

data_dir <- "input/"
train <- read_csv(file.path(data_dir, "train.csv"))
mapdata <- readRDS(file.path(data_dir, "mapdata_copyright_openstreetmap_contributors.rds"))
train$Date <- as.Date(train$Date)

# Which date has the most measurements?
counts_by_date <- train %>% group_by(Date) %>% summarise(NumMeasurements = n()) %>% arrange(desc(NumMeasurements)) %>% head
counts_by_date

date_to_show <- counts_by_date$Date[1]

single_date_grouped_by_location <- train %>% 
  filter(Date == "2007-08-01") %>%
  group_by(Longitude, Latitude) %>%
  summarize(NumMosquitos = sum(NumMosquitos))

qplot(single_date_grouped_by_location$NumMosquitos) + 
  scale_x_log10() + 
  xlab("Number of Mosquitos") +
  ylab("Number of test sites w/ this many mosquitos") +
  ggtitle("Distribution of Mosquito Counts (Log Scale")

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

windows()
g <- ggmap(mapdata) + 
  geom_point(aes(x=Longitude, y=Latitude, color=NumMosquitos), size=3, data=single_date_grouped_by_location) + 
  scale_color_continuous(trans="log") +
  ggtitle(sprintf("Spatial Arrangement of Mosquito Counts on %s", date_to_show))
print(g)

train_merged$Longitude <- as.numeric(train_merged$Longitude)
train_merged$Latitude <- as.numeric(train_merged$Latitude)
train_merged$Species <- as.factor(train_merged$Species)
train_merged$WnvPresent <- as.factor(train_merged$WnvPresent)
train_merged$Year <- year(ymd(train_merged$Date))
train_merged$Month <- month(ymd(train_merged$Date))
train_merged$Day <- strptime(train_merged$Date, "%Y-%m-%d")$yday+1

for (i in levels(train_merged$Species)) {
  train_plot <- train_merged[train_merged$Species == i & train_merged$WnvPresent == 1,]
  num_WnvP <- nrow(train_plot)
  
  if (num_WnvP > 0) {
    train_plot$Index <- 1:num_WnvP
    train_plot <- train_plot[rev(train_plot$Index),]
    
    windows()
    g <- ggmap(mapdata) + 
      geom_point(aes(x=Longitude, y=Latitude, size = Day), shape= 21, color = "red", data=train_plot) +
      geom_point(aes(x=Longitude, y=Latitude), shape= 4, size = 1, color = "black", data=train_merged) +
      facet_wrap(~ Year, nrow = 1, ncol = 4) +
      ggtitle(sprintf("West Nile Virus in %s by Year", i))
    print(g)  
  } else {
    print(paste("No West Nile Virus present in ",i))
  }
  
}

library(geosphere)


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


#Station 1: CHICAGO O'HARE INTERNATIONAL AIRPORT Lat: 41.995 Lon: -87.933 Elev: 662 ft. above sea level
#Station 2: CHICAGO MIDWAY INTL ARPT Lat: 41.786 Lon: -87.752 Elev: 612 ft. above sea level

stations<-data.frame(c(1,2),c(41.995,41.786),c(-87.933,-87.752))
names(stations)<-c("Station","Latitude","Longitude")

train_merged$Station<-mapply(distance,train_merged$Longitude,train_merged$Latitude)

#Let's make a plot to see if everything is OK
windows()
pl <- ggmap(mapdata) +
  geom_point(data=train_merged,aes(x=Longitude,y=Latitude,color=factor(Station))) +
  geom_point(data=stations,aes(x=Longitude, y=Latitude,colour=Station),colour=c("red","green"),size=4)
print(pl)

############ Spray data
spray$Year <- year(ymd(spray$Date))
spray_2011 <- spray[spray$Year == 2011,]
spray_2013 <- spray[spray$Year == 2013,]

windows()
pl <- ggmap(mapdata) +
  geom_point(data=spray_2013,aes(x=Longitude,y=Latitude, color = Date), shape = 19) 
#  facet_wrap(~ Year, nrow = 1, ncol = 2)
print(pl)

#### in 2011
zone_min_Lat <- min(spray_2011$Latitude,na.rm=T)
zone_max_Lat <- max(spray_2011$Latitude,na.rm=T)
zone_min_Lon <- min(spray_2011$Longitude,na.rm=T)
zone_max_Lon <- max(spray_2011$Longitude,na.rm=T)

train_2011 <- subset(train_merged, subset = (train_merged$Year == 2011 &
                                               zone_min_Lat < train_merged$Latitude &
                                               zone_max_Lat > train_merged$Latitude &
                                               zone_min_Lon < train_merged$Longitude &
                                               zone_max_Lon > train_merged$Longitude))

train_2011$Day <- strptime(train_2011$Date, format = "%Y-%m-%d")$yday + 1
spray_day <- strptime(spray_2011$Date, format = "%Y-%m-%d")$yday + 1
# sprayed on days 241 and 250

windows()
pl <- ggplot() +
  geom_line(data=train_2011,aes(x=Day,y=NumMosquitos, color = Species)) +
  facet_wrap(~ Trap, nrow = 1, ncol = 4)
print(pl)

####

windows()
hist(weather$WetBulb)

weather$Month <- month(weather$Date)
weather$WetBulb <- as.numeric(weather$WetBulb)
windows()
pl <- ggplot(data = weather,aes(x=Date ,y=WetBulb)) +
  geom_bar(stat = "Identity") +
  facet_wrap(~ Month, nrow = 1, ncol = 6)
print(pl)


#############
train <- read_csv(file.path(data_dir, "train_full.csv"))
windows()
corrplot(train, method = "circle")
