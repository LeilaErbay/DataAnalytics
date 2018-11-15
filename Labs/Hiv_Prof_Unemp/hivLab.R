#TITLE: HIV LAB
#Author: LEILA ERBAY


hivData <- read.csv("region_prevalence.csv")
hivData<- cbind(hivData, NA)
names(hivData)[3] <- "posTest"

#Values from General Test
posGivenHiv <- 0.990
posGivenNHiv <- 0.015

#calculate the positive test totals per region
hivData$posTest <- posGivenHiv* hivData$Adult_Prevalence + posGivenNHiv*(1-hivData$Adult_Prevalence)



hivData<- cbind(hivData, NA)
names(hivData)[4] <- "posterior"

#calculate the posterior values per region
hivData$posterior<- (posGivenHiv* hivData$Adult_Prevalence) / hivData$posTest

lebanonPatient <- hivData[8,]
hivData <- hivData[, 1:4]

hivData <- cbind(hivData, NA)
names(hivData)[5] <- "pos2Tests"

hivData$pos2Tests <- (posGivenHiv^2)*hivData$Adult_Prevalence + (posGivenNHiv^2)*(1-hivData$Adult_Prevalence)

hivData <- cbind(hivData, NA)
names(hivData)[6] <- "posterior2Test"

hivData$posterior2Test <- ((posGivenHiv^2)*hivData$Adult_Prevalence)/hivData$pos2Tests
