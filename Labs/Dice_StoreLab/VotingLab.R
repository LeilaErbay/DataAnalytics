#p = 0.618
#n = 20
#size = 20

#
hist(rbinom(20,100, 0.618))

trials <- numeric(length = 100)
#sum(rbinom(20,20, 0.618))

for(i in seq_along(trials)){
  trials[i] <- sum(rbinom(20,1, 0.618))
}
mean(trials)

hist(trials)
# 