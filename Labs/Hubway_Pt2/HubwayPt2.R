# Hubway Lab Part 2
# Author: Leila Erbay

#opening files
setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/Lab6")
trips <- read.csv("hubway_trips.csv")
stations <- read.csv("hubway_stations.csv")

#merge strt_statn into trips
mergedData <-merge(trips, stations, by.y = "id", by.x= "strt_statn",all.x= T)
names(mergedData) <-gsub("station","strt_statn_name", names(mergedData))

#merge end_statn into mergedData
mergedData <- merge(mergedData,stations, by.y = "id", by.x = "end_statn", all.x = T)
names(mergedData) <- gsub("station", "end_statn_name", names(mergedData))

#renaming columns
names(mergedData)[names(mergedData) == 'status'] <- "end_statn_status"
names(mergedData)[names(mergedData) == 'status.y'] <- "start_statn_status"
names(mergedData)[names(mergedData) == 'status.x'] <- "status"

#remove stations that are not existing
mergedData <- mergedData[mergedData$start_statn_status == "Existing" & mergedData$end_statn_status == "Existing",]

library(plyr)

#Finding the station with the most freq start
mostFreqStart <-count(mergedData,"strt_statn")
mostFreqStart <- mostFreqStart[mostFreqStart$freq ==max(mostFreqStart$freq),]
mostFreqStart<- stations[stations$id == mostFreqStart$strt_statn,] 
#22


#finding the station with the most freq end
mostFreqEnd <- count(mergedData, "end_statn")
mostFreqEnd <- mostFreqEnd[mostFreqEnd$freq ==max(mostFreqEnd$freq),]
mostFreqEnd<- stations[stations$id == mostFreqEnd$end_statn,] 
#22


# finding averages by start station
aveStart <- aggregate(duration~strt_statn, data = mergedData,FUN = mean )

#find start station with the largest ave
index <- which.max(aveStart$duration)
maxAveStart<- stations[stations$id ==  (aveStart[index,]$strt_statn),]

#find start station with the shortest ave
index <- which.min(aveStart$duration)
minAveStart <-stations[stations$id ==  (aveStart[index,]$strt_statn),]

#finding averages by end station
aveEnd <- aggregate(duration~end_statn, data = mergedData, FUN = mean)

#find end station with the largest ave
index <- which.max(aveEnd$duration)
maxAveEnd <-stations[stations$id ==  (aveEnd[index,]$end_statn),]

#find end station with smallest ave
index <-which.min(aveEnd$duration)
minAveEnd <-stations[stations$id ==  (aveEnd[index,]$end_statn),]

#most freq zip code with end station == 63
sortByDor <- mergedData[mergedData$end_statn == '63',]
freqByZip <- count( sortByDor, "zip_code")
mostFreqZipCode <-  freqByZip[which.max(freqByZip$freq),]

#create a table of based on the gender
x <- table(mergedData$gender)
plotByGender <- barplot(x, main="Male vs Female Graph", ylim=c(0, 200000))

#percentage of male gender:
malepct <- (x[2])/(x[1]+x[2]) #0.7439792 

#histogram of all durations
durations <- aggregate(duration~bike_nr, data = mergedData, FUN = sum)
histDur<-hist(x=durations$duration, xlab="duration", breaks=10000, xlim = c(0,8500000), ylim = c(0,10),main="Histogram of Durations")

durations$duration <- durations$duration/3600
durations <- durations[durations$duration <= 1,]

#histogram of rides only 1 hour long
histHrDur <- hist(x=durations$duration, xlab = "durations under an hour", breaks = 20, xlim = c(0,1), ylim=c(0,2), main = "Histogram of Durations Under an Hour")

library(ggmap)
library(mapproj)

map <- get_map(location= c(-71.09320,42.35810 ), source="google",color ="color",maptype = "roadmap", zoom =13)
print(ggmap(map, extent="device") + geom_point(data = stations, aes(x = stations[,6], y = stations[,5]), size = 1, color = "darkred"))
                                                                            
ggsave("FinalMap.png", plot = last_plot(),  units = "in")

