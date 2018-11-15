#Lab 4 - Hubway Pt I
#Author: Leila Erbay

###### PART A #############
library(readxl)
stations <- read.csv("hubway_stations.csv")

colNamesStations <- colnames(stations)
# id, terminal, station, municipal, lat, lng, status

uniqueStns <- unique(stations$station)
uniqueStnsAmt <- length(uniqueStns)

#137 unique stations


existingStns <- subset(stations, stations$status == "Existing")
uniqueExistingStns <- unique(existingStns)
uniqueExistStnsAmt <-length(uniqueExistingStns$station)
#130 unique existing stations

############ PART B #######################
trips <- read.csv("hubway_trips.csv")

numCols <- ncol(trips)  #13 Col
numRows <- nrow(trips)  #350615 Row

uniqueZipCodes <- unique(trips$zip_code) #375 Unique Zip Codes
uniqueBikeIDs <- unique(trips$bike_nr) #882 unique bikes

library(plyr)
numRides <- count(trips,"bike_nr")

maxValue <- numRides[which.max(numRides[ ,2 ]),] #B00401
minValue <- numRides[which.min(numRides[,2]),] #T01093


#duration of each bike: need bike, duration
duration <- count(trips, "bike_nr", "duration")

##OTHER WAYS TO FIND DURATION
#duration <- aggregate(duration~bike_nr, data= trips, sum)
#duration <- summaryBy(duration~bike_nr,data = trips,FUN= sum )
#duration <- data.frame(tapply(trips$duration, trips$bike_nr, sum))

longestRidden <- duration[which.max(duration[, 'freq']),] #B00585
shortestRidden <- duration[which.min(duration[, 'freq']),] #T01380

#SPECIFYING BIKE B00585
longestRiddenDF <- trips[trips$bike_nr == "B00585",]

freqEnd <- count(longestRiddenDF, "end_statn")
mostFreqEnd <- freqEnd[freqEnd$freq == max(freqEnd$freq),]

#OTHER WAYS TO FIND MAX FREQUENT END STATION
# freqEnd2 <-  table(longestRiddenDF$end_statn)
# index <- which.max(freqEnd2)
# freqEnd2[index]
#freqEnd2 <- summaryBy(bike_nr~end_statn, data = longestRiddenDF,FUN= length)
#freqEnd2 <- aggregate(bike_nr~end_statn, data = longestRiddenDF, FUN =length)

#TAPPLY:
# freqEnd2 <- data.frame(tapply(longestRiddenDF$hubway_id,longestRiddenDF$end_statn, length))
# library(data.table)
# freqEnd2 <- setDT(freqEnd2, keep.rownames = T)[]
# mostFreqEnd2 <- freqEnd2[which.max(as.numeric(unlist(freqEnd2[,2]))),]

freqStart <- count(longestRiddenDF, "strt_statn")
mostFreqStart <- freqStart[freqStart$freq == max(freqStart$freq),]

#OTHER WAYS TO FIND MAX FREQUENT START STATION
# freqStart2 <-table(longestRiddenDF$strt_statn)
# mostFreqStart2 <- freqStart2[which.max(freqStart2)]
#freqStart2 <- summaryBy(bike_nr~strt_statn, data = longestRiddenDF,FUN= length)
#freqStart2 <- aggregate(bike_nr~strt_statn, data = longestRiddenDF, FUN =length)

#TAPPLY:
# freqStart2 <- data.frame(tapply(longestRiddenDF$hubway_id, longestRiddenDF$strt_statn, length))
# freqStart2 <- setDT(freqStart2, keep.rownames=T)[]
# mostFreqStart2 <- freqStart2[which.max(as.numeric(unlist(freqStart2[,2]))),]

#stationStart <- freqStart2[which.max(freqStart2$tapply.longestRiddenDF.hubway_id..longestRiddenDF.strt_statn..),]
#stationStart <- freqStart2[which.max(freqStart2$bike_nr),]


#for bike B00585
mostFreqEndSttn <- stations[stations$id == mostFreqEnd$end_statn,]
mostFreqEndSttn$station #South Station

mostFreqStartSttn <- stations[stations$id == mostFreqStart$strt_statn,]
mostFreqStartSttn$station #South Station


