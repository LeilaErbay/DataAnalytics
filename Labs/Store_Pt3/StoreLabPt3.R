#LAB 10:STORES LAB 3
#AUTHOR: LEILA ERBAY

setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/Lab10")
library(readxl)
storesData <- read_excel("Store data.xlsx")

###### SECTION 1 ###############
storesData$SPACE_EFF <- storesData$SALES_UNITS/storesData$SPACE
storesData$LOG_TISP <- log10(storesData$SALES_TISP)
storesData$LOG_NDSA <- log10(storesData$NDSA)
storesData$LOG_SPACE <- log10(storesData$SPACE)
storesData$LOG_SPACE_EFF <- log10(storesData$SPACE_EFF)

fit <- data.frame(matrix(NA, nrow = 14, ncol = 2))
names(fit)<- c("corr_coefficient", "r_squared")

fitList <- list()
fitList[[1]] <- lm(storesData$SALES_TISP~storesData$SPACE)
fitList[[2]] <- lm(storesData$SALES_TISP~storesData$TXNS)
fitList[[3]] <- lm(storesData$SALES_TISP ~ storesData$NDSA)
fitList[[4]] <- lm(storesData$LOG_TISP~storesData$LOG_NDSA)
fitList[[5]] <- lm(storesData$LOG_TISP~ storesData$LOG_SPACE)
fitList[[6]] <- lm(storesData$LOG_TISP~storesData$MONTH)
fitList[[7]] <- lm(storesData$LOG_TISP~storesData$SPACE_EFF)
fitList[[8]] <- lm(storesData$LOG_TISP~storesData$LOG_SPACE_EFF)
fitList[[9]] <- lm(storesData$LOG_TISP~storesData$NDSA+ storesData$TXNS)
fitList[[10]] <- lm(storesData$LOG_TISP~storesData$MONTH+storesData$LOG_SPACE)
fitList[[11]] <- lm(storesData$LOG_TISP~storesData$MONTH + storesData$SPACE_EFF)
fitList[[12]]<- lm(storesData$LOG_TISP~storesData$MONTH + storesData$LOG_SPACE_EFF)
fitList[[13]] <- lm(storesData$LOG_TISP~storesData$MONTH+ storesData$LOG_NDSA)
fitList[[14]] <- lm(storesData$LOG_TISP~storesData$MONTH + storesData$LOG_NDSA + storesData$LOG_SPACE_EFF)

#DETERMINE r and R^2
for(i in 1:nrow(fit)){
  fit[i,1]<-sqrt(summary(fitList[[i]])$r.squared)
  fit[i,2] <- summary(fitList[[i]])$r.squared
  
}

#wait for enter
readkey <- function(){
  cat ("Press [enter] to continue")
  line <- readline()
}

#plotting graphs of each linear model's QQ and Residual
plotGraphs <- function(i){
  if (length(fitList[[i]]$coefficients) > 2) next()
  else{
    plot(fitList[[i]], which=c(1,2))
  }
}

#Print plots with AB LINE and QQ and RESIDUAL vs Predicted
for(i in 1:length(fitList)){
  if(length(fitList[[i]]$coefficients)>2) {
    print(c("list", i , "is a multiple regression"))
    next()

  }
  else{
    wordsX <- toString(fitList[[i]]$terms[[3]])
    wordsY <- toString(fitList[[i]]$terms[[2]])
    x <- unlist(strsplit(wordsX, ", "))[3]
    y <- unlist(strsplit(wordsY, ", "))[3]
    data <- cbind(storesData[,x], storesData[,y])
    plot(data[,x], data[,y], xlab = x, ylab = y, main= c(x,  " vs" , y) )
    abline(fitList[[i]], col = "red")
    readkey()
    
    plotGraphs(i)
    
    #CHALLENGE:
    hist(fitList[[i]]$residuals, xlab = c("residuals of ", x, "and", y), main=c("Histogram of Residuals of ", x, "and", y) )
    
    #RESIDUAL vs TIME
    residual <- fitList[[i]]$residuals
    plot(storesData$MONTH, residual, xlab = "MONTHS", ylab= "Residuals", main = "Residuals vs Time")
    
    readkey()
    #ALL GRAPHS FOR LINEAR MODELS
    layout(1,1)
    # plot(fitList[[i]])
    # readkey()
    # 
  }
}


