#LAB11: Chi-Squared Lab
#Author: Leila Erbay

setwd("/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/Lab11")

#Test for independence 
test1 <- chisq.test(rbind(c(42,203),c(7,114)),correct= FALSE)
pVal_test1 <- test1[["p.value"]]

test2 <-chisq.test(rbind(c(42,7),c(203,114)),correct= FALSE)
pVal_test2 <- test2[["p.value"]]

mx1_observed<- rbind(c(42,203),c(7,114))

mx1 <- as.table(rbind(c(42,203),c(7,114)))
mx1_wMargins<-addmargins(mx1)
mx1_wMargins

mx1_wMarginsProb <- matrix(nrow=3, ncol=3)
for(i in 1:nrow(mx1_wMargins)){
  for(j in 1:ncol(mx1_wMargins)){
    mx1_wMarginsProb[i,j] <-mx1_wMargins[i,j]/mx1_wMargins[nrow(mx1_wMargins),ncol(mx1_wMargins)]
    
    }
}

mx1_observedProb <- matrix(nrow=2, ncol=2)
#OBSERVED Probabilities
for(i in 1:nrow(mx1_wMargins)-1){
  for(j in 1:ncol(mx1_wMargins)-1){
    mx1_observedProb[i,j] <- mx1_wMarginsProb[i,ncol(mx1_wMarginsProb)]*mx1_wMarginsProb[nrow(mx1_wMarginsProb),j]
    
  }
}

  
#test stat: INDEPENDENCE
testStat <- chisq.test(mx1_observed, correct= FALSE)

expectedMatrix <- matrix(nrow=2, ncol=2)

for(i in 1:nrow(expectedMatrix)){
  for(j in 1:ncol(expectedMatrix)){
   expectedMatrix[i,j] <- mx1_wMargins[i,ncol(mx1_wMargins)]*mx1_wMargins[nrow(mx1_wMargins),j]/mx1_wMargins[nrow(mx1_wMargins), ncol(mx1_wMargins)]
  }
}

mx3_observed <- rbind(c(49, 196), c(0,121))
testStat3 <- chisq.test(mx3_observed, correct = F)

mx2_observed <- rbind(c(33,212), cbind(16,105))

testStat2_indpt <- chisq.test(mx2_observed, correct = F)


### part f - SIMULATION ###
chi2 <- rchisq(20000, df=1)
hist(chi2, 30)
abline(v=9.01)
length(chi2[chi2 > 5])
length(chi2)
471/20000

### FISHER TEST ON FIRST SET OF DATA:
fisher.test(rbind(c(42,203),c(7,114)))


#### F TEST
var2 <- 3.42^2
var1 <- 3.63^2
f_test <-var2/var1   #var2 < var1
#df = 49, 49
pval <- pf(var2/var1, 49, 49, lower.tail = T)
pval2 <- pf(var1/var2, 49, 49, lower.tail= F)
