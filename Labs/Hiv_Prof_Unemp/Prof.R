#TITLE: PROFESSORS LAB
#AUTHOR: LEILA ERBAY


library(readxl)
profData <- read_excel("Professor Salaries - Data.xlsx")

#Q1 = 0.421875
probQ1 <- sum(profData$discipline == "A" & profData$salary >100000) / sum(profData$salary >100000)

#Q2 = 0.773049
probQ2 <- sum(profData$salary > 100000 & (profData$yrs.service >20 & profData$sex == "Male")) / sum(profData$yrs.service >20 & profData$sex == "Male")

#Q3a = 0.0251889
probQ3a <- sum(profData$sex == "Female" & profData$rank== "AssocProf") / nrow(profData)


#Q3b 
probQ3b <- sum(profData$sex == "Female" & profData$rank== "AssocProf" & profData$yrs.since.phd > 10) / sum(profData$sex == "Female" & profData$rank== "AssocProf")

#Q4 = 0.619047
probQ4 <- sum(profData$salary <100000 & ( profData$sex == "Male" & profData$yrs.service <10)) / sum(profData$sex == "Male" & profData$yrs.service <10)
