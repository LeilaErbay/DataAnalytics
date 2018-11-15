#TITLE: MORTALITY LAB
#AUTHOR: LEILA ERBAY

setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/Lab9")
mortalityData <- as.data.frame(read.csv("Mortality Lab - Data.csv"))

########### (a) ###############
histMortality <- hist(mortalityData$Mortality, xlab = "Number of Deaths", main = "Histogram of Mortality Rate")
histNOx <- hist(mortalityData$NOx, xlab = "Nitrous Oxide Concentration", main= "Histogram of NOx Concentrations", breaks = 30, xlim = c(0,max(mortalityData$NOx)))
mortalityData$LogNOx <- "LogNOx"

#Computing the Log(Nox)
mortalityData$LogNOx <- log(mortalityData$NOx)

#plotting y = mortality , x = log(NOx) -- SIMPLE LIN REGRESSION
fit <- lm(formula = mortalityData$Mortality~mortalityData$LogNOx)
plot(mortalityData$LogNOx, mortalityData$Mortality, xlab = "LogNOx levels", ylab = "Mortality Rate", main = "Mortality Rate against LogNOx Levels")
abline(fit, col = "red")

#coefficients of lin reg.
coeff <- coefficients(fit)  #<--- Y = b0 + b1X + E
## Y-int = 905.6, slope =15.1

######## MANUAL CALCULATIONS ############
meanMortality <- mean(mortalityData$Mortality)  #Y
meanLogNOx <- mean(mortalityData$LogNOx)        #X
sYY <- sum((mortalityData$Mortality- meanMortality)^2) #Syy
sXX <- sum((mortalityData$LogNOx- meanLogNOx)^2) #Sxx
sXY <- sum((mortalityData$Mortality- meanMortality)*(mortalityData$LogNOx- meanLogNOx))

slope <- sXY / sXX  #905.61
intercept <- meanMortality - slope*meanLogNOx  #15.098


###### PREDICTED MORTALITY ######
mortalityData$PredictedMortality <- "PredictedMortality"
mortalityData$Residuals <- "Residuals"

#y = mX + b --> predictedMortality = slope* logNOx + intercept
mortalityData$PredictedMortality <- slope * mortalityData$LogNOx + intercept 



##### NORMALITY TESTING #######
histLogNOx <- hist(mortalityData$LogNOx, xlab= "Log Nitrous Oxide Concentrations", main= "Histogram of Log NOx Concentrations")
mortalityData$Residuals <- resid(fit)
hist(mortalityData$ActualResiduals, xlab = "Observed Residuals", main = "Histogram of Observed Residuals")


###### HOMOSCEDASTICITY   ##########
#ACTUAL
plot(fit)


######## Coefficient of Determination ###
#Actual
rSqActual <- cor(mortalityData$LogNOx, mortalityData$Mortality)^2


