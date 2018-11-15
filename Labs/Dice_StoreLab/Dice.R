library(graphics)

# 5 random dice rolls  
sample(1:6, 5, replace = T)
# sum of 5 random dice rolls
sum(sample(1:6, 5, replace = T))
# average of 5 random dice rolls
mean(sample(1:6, 5, replace = T))

# What is the average of 10 experiments? 

experiments10 <- numeric(length = 10)
for(i in seq_along(experiments10)){
  experiments10[i] <- sum(sample(1:6, 5, replace=T))
}
mean(experiments10)
print(experiments10)
# What is the average of 100 experiments?
experiments100 <- numeric(length = 100)
for(i in seq_along(experiments100)) {
  experiments100[i] <- sum(sample(1:6, 5, replace=T))
}
mean(experiments100)
print(experiments100)

# What is the average of 1000 experiments? Plot below to see the number it converges 
experiments1000 <- numeric(length = 1000)
for(i in seq_along(experiments1000)) {
  experiments1000[i] <-sum(sample(1:6, 5, replace=T))
}
mean(experiments1000)
print(experiments1000)

means1000 <- numeric(length = 1000)
for(i in seq_along(experiments1000)) {
  means1000[i] <- mean(experiments1000[1:i])
}

# print(means1000[1])
# means1000[3] <- mean(means1000[1:2])
# means1000 <- experiments1000[1:1000]

# 
# means1000[222]


plot(1:1000, means1000, 'l')

