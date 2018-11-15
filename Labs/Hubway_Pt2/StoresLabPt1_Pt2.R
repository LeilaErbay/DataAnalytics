#Lab 6: Stores Parts | and ||
#Author: Leila Erbay

#reading store data and dictionary
library(readxl)
storeData <- read_excel("Storedata.xlsx")
dictionary <- read_excel("Storedata.xlsx", sheet = 2)

#determine the number of distinct stores
library(plyr)
numStores <- count(storeData, "STORE_ID")
numStores <- length(numStores$STORE_ID) #113 Stores

#det. the number of regions
numRegions <- count(storeData,"Region") #3 Regions

#determine num of years
numYears <- count(storeData, "YEAR")  #only 1 year of data = 2007

#data for only store 19
store19 <- storeData[storeData$STORE_ID == "19",]
totalSTisp19 <- sum(store19$SALES_TISP)

#make a copy of store data
storeCopy <- storeData
View(storeCopy)

#determine the stores which don't have 12 months worth of data
storesShortOf12 <- as.data.frame.matrix(table(storeCopy$STORE_ID, storeCopy$MONTH))
storesShortOf12 <- cbind(storesShortOf12,"total")
storesShortOf12[,13] <-rowSums(storesShortOf12[,1:12])

resultShortOf12 <- subset(storesShortOf12, storesShortOf12[,13] < 12)
library(data.table)
resultShortOf12 <- setDT(resultShortOf12, keep.rownames=T)[]
resultShortOf12 <- resultShortOf12[,c(1,13)]

#adding columns and respective data
storeCopy$Avg_Price <- storeCopy$SALES_TISP/storeCopy$SALES_UNITS
storeCopy$Space_Yield <- storeCopy$SALES_TISP/storeCopy$SPACE
storeCopy$NDSA_Yield <- storeCopy$SALES_TISP/storeCopy$NDSA
storeCopy$LOG_TISP <- log10(storeCopy$SALES_TISP)
storeCopy$LOG_TESP <- log10(storeCopy$SALES_TESP)
storeCopy$LOG_SPACE <- log10(storeCopy$SPACE)
storeCopy$LOG_NDSA <- log10(storeCopy$NDSA)
storeCopy$LOG_price <- log10(storeCopy$Avg_Price)
storeCopy$LOG_Space_Yield <- log10(storeCopy$Space_Yield)
storeCopy$LOG_NDSA_Yield <- log10(storeCopy$LOG_NDSA)
storeCopy$Date <-as.Date(with(storeCopy, paste(YEAR, MONTH, 1, sep="-")),"%Y-%m-%d")

# typeof(storeCopy$Region[1])

#Adding a region text col with respective conditional values
storeCopy$RegionsText[storeCopy$Region ==1] <- "South"
storeCopy$RegionsText[storeCopy$Region ==2] <- "Pacific"
storeCopy$RegionsText[storeCopy$Region ==3] <- "MidAtlantic"

#Gathering data based on Format
convenienceData <- storeCopy[storeCopy$FORMAT =="Convenience", c('STORE_ID', 'SALES_TISP', 'NDSA')]
chemistData <- storeCopy[storeCopy$FORMAT == "Chemist",c('STORE_ID', 'SALES_TISP', 'NDSA')]
destinationData <- storeCopy[storeCopy$FORMAT == "Destination",c('STORE_ID', 'SALES_TISP', 'NDSA')]

######################### SALES_ TISP#####################################
#SALES_TISP by FORMAT
salesTispConv_Total <-sum(convenienceData$SALES_TISP)
salesTisp_ChemTotal <-sum(chemistData$SALES_TISP)
salesTisp_DestTotal <- sum(destinationData$SALES_TISP)

#Ave of SALES_TISP per Format
salesTisp_ConvAvg <- salesTispConv_Total/length(convenienceData$SALES_TISP)
salesTisp_ChemAvg <- salesTisp_ChemTotal/length(chemistData$SALES_TISP)
salesTisp_DestAvg <- salesTisp_DestTotal/length(destinationData$SALES_TISP)

#max of SALES_TISP per Format
salesTisp_ConvMax <- max(convenienceData$SALES_TISP)
salesTisp_ChemMax <- max(chemistData$SALES_TISP)
salesTisp_DestMax <- max(destinationData$SALES_TISP)

#min of SALES_TISP per Format
salesTisp_ConvMin <- min(convenienceData$SALES_TISP)
salesTisp_ChemMin <- min(chemistData$SALES_TISP)
salesTisp_DestMin <- min(destinationData$SALES_TISP)

#high bounds of SALES_TISP per Format
salesTisp_ConvPctH <- max(quantile(convenienceData$SALES_TISP, 0.99))
salesTisp_ChemPctH <- max(quantile(chemistData$SALES_TISP, 0.99))
salesTisp_DestPctH <- max(quantile(destinationData$SALES_TISP, 0.99))

#low bounds of SALES_TISP per Format
salesTisp_ConvPctL <- min(quantile(convenienceData$SALES_TISP, 0.01))
salesTisp_ChemPctL <- min(quantile(chemistData$SALES_TISP, 0.01))
salesTisp_DestPctL <- min(quantile(destinationData$SALES_TISP, 0.01))

#Higher Outliers per format
salesTisp_ConvOutH <- convenienceData[convenienceData$SALES_TISP >= salesTisp_ConvPctH,]
salesTisp_ChemOutH <- chemistData[chemistData$SALES_TISP >= salesTisp_ChemPctH,]
salesTisp_DestOutH <- destinationData[destinationData$SALES_TISP >= salesTisp_DestPctH,]

#lower outliers of SALES_TISP per Format
salesTisp_ConvOutL <- convenienceData[convenienceData$SALES_TISP <= salesTisp_ConvPctL,]
salesTisp_ChemOutL <- chemistData[chemistData$SALES_TISP <= salesTisp_ChemPctL,]
salesTisp_DestOutL <- destinationData[destinationData$SALES_TISP <= salesTisp_DestPctL,]

storeData_NoOutliers <- storeCopy[(storeCopy$SALES_TISP != salesTisp_ConvOutH$SALES_TISP & storeCopy$SALES_TISP != salesTisp_ChemOutH$SALES_TISP 
                                  & storeCopy$SALES_TISP != salesTisp_ConvOutL$SALES_TISP & storeCopy$SALES_TISP != salesTisp_ChemOutL$SALES_TISP) ,]

storeData_NoOutliers<- storeData_NoOutliers[!(storeData_NoOutliers$SALES_TISP %in% salesTisp_DestOutH$SALES_TISP) & !(storeData_NoOutliers$SALES_TISP %in% salesTisp_DestOutL$SALES_TISP),]


################################## NDSA #####################################################
#sum of ndsa per format
ndsa_ConvTotal <- sum(convenienceData$NDSA)
ndsa_ChemTotal <- sum(chemistData$NDSA)
ndsa_DestTotal <- sum(destinationData$NDSA)

#ave of ndsa per format
ndsa_ConvAvg <- ndsa_ConvTotal/length(convenienceData$NDSA)
ndsa_ChemAvg <- ndsa_ChemTotal/length(chemistData$NDSA)
ndsa_DestAvg <- ndsa_DestTotal/length(destinationData$NDSA)

#max of NDSA per format
ndsa_ConvMax <- max(convenienceData$NDSA)
ndsa_ChemMax <- max(chemistData$NDSA)
ndsa_DestMax <- max(destinationData$NDSA)

#max of NDSA per format
ndsa_ConvMin <- min(convenienceData$NDSA)
ndsa_ChemMin <- min(chemistData$NDSA)
ndsa_DestMin <- min(destinationData$NDSA)

#high bounds of NDSA per Format
ndsa_ConvPctH <- max(quantile(convenienceData$NDSA, 0.99))
ndsa_ChemPctH <- max(quantile(chemistData$NDSA, 0.99))
ndsa_DestPctH <- max(quantile(destinationData$NDSA, 0.99))

#low bounds of NDSA per Format
ndsa_ConvPctL <- min(quantile(convenienceData$NDSA, 0.01))
ndsa_ChemPctL <- min(quantile(chemistData$NDSA, 0.01))
ndsa_DestPctL <- min(quantile(destinationData$NDSA, 0.01))

#Higher Outliers of NDSA per format
ndsa_ConvOutH <- convenienceData[convenienceData$NDSA >= ndsa_ConvPctH,]
ndsa_ChemOutH <- chemistData[chemistData$NDSA >= ndsa_ChemPctH,]
ndsa_DestOutH <- destinationData[destinationData$NDSA >= ndsa_DestPctH,]

#lower outliers of NDSA per Format
ndsa_ConvOutL <- convenienceData[convenienceData$NDSA <= ndsa_ConvPctL,]
ndsa_ChemOutL <- chemistData[chemistData$NDSA <= ndsa_ChemPctL,]
ndsa_DestOutL <- destinationData[destinationData$NDSA <= ndsa_DestPctL,]

############# REMOVE SALES_TISP OUTLIERS #################
storeData_NoOutliers <- storeCopy[(storeCopy$SALES_TISP != salesTisp_ConvOutH$SALES_TISP & storeCopy$SALES_TISP != salesTisp_ChemOutH$SALES_TISP 
                                   & storeCopy$SALES_TISP != salesTisp_ConvOutL$SALES_TISP & storeCopy$SALES_TISP != salesTisp_ChemOutL$SALES_TISP) ,]

storeData_NoOutliers<- storeData_NoOutliers[!(storeData_NoOutliers$SALES_TISP %in% salesTisp_DestOutH$SALES_TISP) & !(storeData_NoOutliers$SALES_TISP %in% salesTisp_DestOutL$SALES_TISP),]


########## SALES PER MONTH BY FORMAT #############
storeData_NoOutliersTotals <- count(storeData_NoOutliers, c('MONTH','SALES_TISP', 'FORMAT') )

#TOTALS for CONVENIENCE PER MONTH
storeData_MonthlyConv <- storeData_NoOutliersTotals[storeData_NoOutliersTotals$FORMAT == "Convenience",]
convMonth <- aggregate(SALES_TISP~MONTH, storeData_MonthlyConv, FUN = sum)

#TOTALS for CHEMIST per MONTH
storeData_MonthlyChem <-storeData_NoOutliersTotals[storeData_NoOutliersTotals$FORMAT == "Chemist",]
chemMonth <- aggregate(SALES_TISP~MONTH, storeData_MonthlyChem, FUN = sum)

#TOTALS for CHEMIST per MONTH
storeData_MonthlyDest <-storeData_NoOutliersTotals[storeData_NoOutliersTotals$FORMAT == "Destination",]
destMonth <- aggregate(SALES_TISP~MONTH, storeData_MonthlyDest, FUN = sum)

final_monthlyTotals <- final_monthlyTotals[,c(1,2,3,4,6)]

final_monthlyTotals <- cbind(convMonth[,1:2], chemMonth[,2], destMonth[,2])
names(final_monthlyTotals) <- c("Month","Convenience", "Chemist", "Destination")
final_monthlyTotals$Total <- NA

final_monthlyTotals$Total <- rowSums(final_monthlyTotals[,c(2,3,4)])


library(writexl)
write_xlsx(final_monthlyTotals, "final_monthlyTotals.xlsx")


library(gridExtra)
png("totalsByMonth.png", height = 50*nrow(final_monthlyTotals), width = 200*ncol(final_monthlyTotals))
grid.table(final_monthlyTotals)
dev.off()