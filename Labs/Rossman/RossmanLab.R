#Rossman Lab
#Author: Leila Erbay

setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/lab7")
rossmanInfo <- read.csv("Rossmann Lab - Data.csv")

format <- colnames(rossmanInfo)

uniqueVals <- sapply(rossmanInfo[,c("Store","Year","StoreType","Assortment")], function(x) length(unique(x)))
#[1115 = Stores, 3 = Year, 4 = Store Types, 3 = Assorments]

uniqueCombo <- unique(rossmanInfo[, c("Store","Year","StoreType","Assortment")])
#3345 unique combos

missingInfo <- rossmanInfo[!complete.cases(rossmanInfo),]
#there is no missing info

summaryOut <- summary(rossmanInfo[, c(1:4,6:9,11,14:16)])


plotSummary <- boxplot(rossmanInfo[, c(1:4,6:9,11,14:16)], main = "boxplot of all data", col = "blue")

library(outliers)
outs <- as.data.frame(outlier(rossmanInfo[, c(1:4,6:9,11,14:16)]))

closedStores <- rossmanInfo[rossmanInfo$Open == 0 & (rossmanInfo$Sales !=0 | rossmanInfo$Customers !=0),]
#No idiosyncrasies 

monthYear <- unique(rossmanInfo[,c('Year', 'Month')])
salesPromo <- rossmanInfo[rossmanInfo$Promo == 0,]

#SUBSETTING ON OPEN STORES ONLY
openStores <- rossmanInfo[rossmanInfo$Open == 1,]


#Sales and competition distance  BOX PLOT
salesSum <- boxplot(openStores$Sales, main= "plot of Sales", col = "red")
competitionDistSum <- boxplot(openStores$CompetitionDistance, main = "plot of Competition Distance", col = "dimgrey")

#sales and competition distance HIST
salesHist <- hist(openStores$Sales, main = "Histogram of open stores' sales")
compDistHist <- hist(openStores$CompetitionDistance, main = "histogram of open stores' competition distance")

#Sales and Comp Distance Info
salesCompInfo<-aggregate(Sales~Store, openStores, FUN = sum)
info2 <- aggregate(CompetitionDistance~Store, openStores, FUN = sum)
salesCompInfo <- cbind(salesCompInfo, info[,2])
names(salesCompInfo)[3] <- "Competition Distance"

#pdf of Sales and Comp Distance
### FREQUENCY DISTRIBUTION ..###

customersHist <- hist(openStores$Customers, main= "histogram of open pages")

storesWithHolidays <- rossmanInfo[rossmanInfo$StateHoliday != "0",]

uniqueStateHolidays <- unique(storesWithHolidays[c('StateHoliday', 'Store','Date')])
numStoreTypes <- unique(rossmanInfo$StoreType)#4
numStoreAssortments <- unique(rossmanInfo$Assortment)# 3

uniqueHolidays <- unique(storesWithHolidays[,c('')])
####################  PART 2 ###########################
#descriptive stats of competitive distance
library(pastecs)
compDistDesc <- as.data.frame(stat.desc(rossmanInfo$CompetitionDistance))
compDistHist <- hist(rossmanInfo$CompetitionDistance, main = "Histogram of Competition Distance", xlab="Competition Distance", ylab = "Frequency", breaks = 50,  freq = T)

#PERCENTAGE LINE
x <-rossmanInfo$CompetitionDistance
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(compDistHist$mids[1:2]*length(x))
lines(xfit, yfit, col = "blue", lwd = 2)


#ASSORTMENT TYPE
asstmtType <-(rossmanInfo$Assortment)
tableAT <-(asstmtType)
barplot(table(asstmtType), ylab = "frequencies", main = "Assortment Type Frequencies")
hist(table(asstmtType), breaks = 10,  freq = T, col="dimgrey", main= "Frequencies of Assortment Type", xlab = "num occurrences")


#STORE SALES
data2014 <- rossmanInfo[rossmanInfo$Year == 2014,]
salesPerDay <- aggregate(Sales~DayOfWeek, data2014, FUN = sum)
names(salesPerDay)[3] <- "Ave Sales"

for(i in salesPerDay$DayOfWeek[i]){
  salesPerDay$AveSales[i] <- salesPerDay$Sales[i] / length(data2014[data2014$DayOfWeek == i,])
}

# salesPerDayPlot <-plot(data2014$DayOfWeek, data2014$Sales, type = "l")


timeSeries <-ggplot(salesPerDay, aes(DayOfWeek, AveSales)) + geom_line() + xlab("") + ylab("Ave Sales")
print(timeSeries)


