#TITLE: UNEMPLOYMENT LAB
#AUTHOR: LEILA ERBAY
# 
# P(B|A) = P(BA)/P(A) = P(B) 
# P(A|B) = P(BA)/P(B) = P(A)

library(readxl)
unemplmt <- read_excel("Unemployment Lab - Data.xlsx")


########### Q1 #########################
#A = job lost due to slacking     #B = tenure less than 3 years
probA <- sum(unemplmt$joblost == "slack_work") / nrow(unemplmt)
probB <- sum(unemplmt$tenure <10) / nrow(unemplmt)

#P(B | A) = P( B & A)/P(A) == 0.87209  != P(A)
probBgivenA <- sum(unemplmt$tenure < 10 & unemplmt$joblost == "slack_work") / sum(unemplmt$joblost == "slack_work")

# P(A|B) = 0,51201 != P(B)
probAgivenB <- sum(unemplmt$tenure < 10 & unemplmt$joblost == "slack_work") / sum(unemplmt$tenure < 10)


################### Q2 ######################
#A = joblost due to slacking
#B = person = white
#C = person = male

#P(BC|A) = P(B AND C AND A )/P(A) = .682601205
probBCgiveA <- sum(unemplmt$sex == "male" & unemplmt$nwhite == "no" & unemplmt$joblost == "slack_work") / sum(unemplmt$joblost == "slack_work")

################## Q3 ###########################
#A = female
#B = has young children
#P(A and B) = P(A) *P(B|A) = P(A)P(A and B)/P(A)= P(A and B) == 0.043674
jointProb <- sum(unemplmt$sex == "female" & unemplmt$dykids == "yes") / nrow(unemplmt)


################## Q4 ##########################
#A = slacked
#B = position is abolished

#P(A) + P(B) = 0.55854
addProb <- sum(unemplmt$joblost == "slack_work" | unemplmt$joblost == "position_abolished") /nrow(unemplmt)
# check <- sum(unemplmt$joblost == "slack_work") /nrow(unemplmt) + sum(unemplmt$joblost == "position_abolished") /nrow(unemplmt)


################### Q5 ###################################
#P(non white & female & has young kids & slacked) = 0.005741
qE <- sum(unemplmt$nwhite == "yes" & unemplmt$sex == "female" & unemplmt$dykids == "yes" & unemplmt$joblost =="slack_work") / nrow(unemplmt)

#P(heads and age>=20 & age < 30) = 0.175927
qF <- sum(unemplmt$head == "yes" & unemplmt$age >19 & unemplmt$age < 30) /nrow(unemplmt)

#P(exp < 5) = 0.6134919
qG <- sum(unemplmt$tenure < 5) /nrow(unemplmt)


#################### Q6 ############################
#P(Slack) vs P(Other)
probSlack <- sum(unemplmt$joblost == "slack_work") / nrow(unemplmt) # 0.47611
probOther <- sum(unemplmt$joblost == "other") / nrow(unemplmt)  #0.405167

#P(male) vs P(female)
probMale <- sum(unemplmt$sex == "male")/nrow(unemplmt)  #0.764199
probFemale <- sum(unemplmt$sex == "female") / nrow(unemplmt) #0.235800


