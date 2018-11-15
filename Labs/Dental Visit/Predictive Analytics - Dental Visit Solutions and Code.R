## 
## Predictive Analytics - Day 1
## 

# setwd('')

##  Outlier Detection on the Iris Dataset

## By clustering method 
# remove species from the data to cluster 
iris2 <- iris[, 1:4]
summary(iris2)
# Run clustering for three clusters
kmeans.result <- kmeans(iris2, centers = 3) 
# cluster centers 
kmeans.result$centers
# cluster IDs 
kmeans.result$cluster  
# Get the centers
centers <- kmeans.result$centers[kmeans.result$cluster, ] 
# Calculate distances between objects and cluster centers 
distances <- sqrt(rowSums((iris2 - centers)^2))

# Apply univariate outlier detection to distances
outlierValues <- boxplot.stats(distances)$out
outliers <- which(distances %in% outlierValues)

# plot clusters 
plot(iris2[, c("Sepal.Length", "Sepal.Width")],
     pch = "o",
     col = kmeans.result$cluster,
     cex = 0.8,
     main = "Iris clusters and outliers") 
# plot cluster centers 
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3,
       pch = 10,
       cex = 2.5) 
# plot outliers 
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")],
       pch = "+",
       col = 4,
       cex = 1.5)



# Explore the data
setwd('/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/Dental Visit')
train = read.csv("DentalVisit-Train.csv")
dim(train)
str(train)

##  Isolate numeric variables for exploration. We'll explore the factors later.
summary(train[,c("confident","bmi","children")])

##  bmi has an extreme outlier which is likely
##  erroneous and will certainly skew analysis.
##  How would our data look if we removed it?.

max.bmi <- max(train$bmi, na.rm = TRUE)

bmi.omit <- na.omit(train$bmi[train$bmi < max.bmi])

max(train$bmi, na.rm=TRUE)
max(bmi.omit, na.rm=TRUE)


par(mfrow=c(1,2))
# Without missing value filtered
hist(train$bmi,
     col="darkblue",
     main="History of bmi (full)",
     cex.axis = 1.5,
     cex.lab = 1.5)

# With missing value filtered
hist(bmi.omit,
     col="darkgreen",
     main="History of bmi (Outlier Removed)",
     cex.axis = 1.5,
     cex.lab = 1.5)

hist(log(bmi.omit),
     col="darkgreen",
     main="History of bmi (Outlier Removed)",
     cex.axis = 1.5,
     cex.lab = 1.5)


## Let's take a look at the categorical variables.
summary(train[,!(names(train) %in% c("confident","bmi","children"))])

table(train$agegrp)


par(mar=c(6,3,1,1)+1) 
barplot(table(train$agegrp),
        col="darkgreen",
        main = "Age group distribution",
        las = 2)

# Create racial percentage table
age.tbl <- table(train$agegrp)
age.perc <- round(100*age.tbl/sum(age.tbl),2)
barplot(age.perc,
        col = "darkgreen",
        main = "Age group distribution",
        ylab = "%", las = 2)


# "bmi" and "children" scatter plot
par(mfrow=c(1,2))
plot(train[which( train$children > 0 & train$bmi < 55), ]$children,
     train[which( train$children > 0 & train$bmi < 55), ]$bmi,
     ylab = "BMI",
     xlab = "Number of children",
     main = "BMI vs # Children",
     col="darkgreen",
     type = "p")
plot(jitter(train[which( train$children > 0 & train$bmi < 55), ]$children,
            amount = .45),
     train[which( train$children > 0 & train$bmi < 55), ]$bmi,
     ylab = "BMI",
     xlab = "Number of children (Jittered)",
     main = "BMI vs # Children",
     col="darkblue",
     type = "p")

#bmi vs. race boxplots
par(mfrow=c(1,1))
boxplot(bmi ~ race,
        data = train[train$bmi < 60,],
        xlab = "Race",
        ylab = "BMI",
        col = "darkred")

# Check distribution of age group over race
ageByRace = table(train$agegrp, train$race)
ageByRace
ageByRace = round(t(t(ageByRace) / colSums(ageByRace)),2)
ageByRace
# Sanity Check:
colSums(ageByRace)

# print number of lines with missing values
sum(rowSums(is.na(train)) > 0)
sum(rowSums(is.na(train)) > 0) / nrow(train)

# print columns with missing values
colSums(is.na(train))

# Imputing meds using logistic regression

# Number of missing values for "meds" (37) 
nbr.misg <- sum(is.na(train$meds)) 

# Find useful variables:
colSums(is.na(train[is.na(train$meds),]))

# library(MASS) 
MedsReg <- glm(meds ~ ., family = binomial, data = na.omit(train[,-1])) 
# Extract the temporary data for logistic regression modeling 
mod.train <- subset(train, select = c("meds", "sex", "insured")) 
# Model training 
MedsReg <- glm(meds ~ sex + insured,
               family = binomial,
               data = na.omit(mod.train)) 
# Modeling results summary 
summary(MedsReg) 


# Estimate meds value using the model 
z <- predict(MedsReg, train, type = 'response') 
# Fill in the missing values of 'meds' with estimated values from the model 
newMeds <- train$meds
newMeds[is.na(train$meds)] <- ifelse(z[is.na(train$meds)]>=0.5,"Yes","No")

# Number of missing values after imputation dropped to 0 
nbr.misg.new <- sum(is.na(newMeds))
nbr.misg.new


# LRT on interactions (on clean data set)

train <- read.csv("DentalVisit-Clean.csv")
# Setting indicator category for NAs
# (imputation option #3)
train$healthgroup <- addNA(train$healthgroup)
train$agegrp <- addNA(train$agegrp)
train$race <- addNA(train$race)
train$employ.ins <- addNA(train$employ.ins)
train$insured <- addNA(train$insured)
train$employ <- addNA(train$employ)
train$marital.stat <- addNA(train$marital.stat)
train$postponed.care <- addNA(train$postponed.care)
train$emergency <- addNA(train$emergency)
train$specialist <- addNA(train$specialist)
train$meds <- addNA(train$meds)
train$health <- addNA(train$health)
train$educ <- addNA(train$educ)

# 
# 
m1 <- glm(dental.visit ~ meds+emergency,data=train,family=binomial)
m2 <- glm(dental.visit~meds*emergency,data=train,family=binomial)
m2a <- glm(dental.visit~meds+emergency + meds:emergency,data=train,family=binomial)
anova(m1,m2,test='Chisq') ## LIKELIHOOD RATIO TEST

# Model selection

train$log.bmi <- log(train$bmi)

# Set dental.visit from "Yes"/"No" to 1/0 (This makes it easier to work with)
train$dental.visit <- ifelse(train$dental.visit == "Yes", 1, 0)

# Fit model with all interesting features and theorized interactions
base1 <- glm(dental.visit ~ phone + sex + agegrp + race + employ.ins + insured +
                            employ + marital.stat + postponed.care + emergency +
                            specialist + meds + log.bmi + children + confident +
                            educ + health + log.bmi + phone:confident +
                            employ.ins:specialist + sex:marital.stat +
                            race:employ,
             data=train,
             family = "binomial")

# install.packages("MASS")
library(MASS)
stepsDental <- stepAIC(base1, direction="both")
summary(stepsDental)

##  Evaluate Model

# Create predicted response from training set
y <- round(predict(stepsDental, train, type="response"))

#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)


confusionMatrix(table(data.frame(train$dental.visit, y)))


##############################################################
###########  Appendix Reference: Imputing Children ###########
##############################################################

# Make histogram of the number of children
par(mfrow=c(1,1))
hist(train$children,
     breaks = 15,
     col = "darkgreen",
     xlab="number of children",
     main="Histogram of children")

# Imputing the number of children using ZeroInflPoisson

# Number of missing values for "children" (201)
chld.nbr.mis <- sum(is.na(train$children))
# install.packages("pscl")
library(pscl)

# To estimate "children" values using zero-inflated count model using other variables
# Zero-inflated count models are two-component mixture models combining a point mass
# at zero with a proper count distribution.

ZeroInfChildren <- zeroinfl(formula = children ~ agegrp + marital.stat + employ + race + insured + employ.ins + sex + health, data = na.omit(train[c("children", "agegrp", "marital.stat", "employ","race", "health","insured", "employ.ins", "sex")]))

# To fill in the missing values for "children" using the model estimate
newchildren <- train$children
z <- predict(ZeroInfChildren, train)
for (ele in 1:length(train$children)){
  if (is.na(train$children[ele])) {
    newchildren[ele] <- z[ele]
  } 
}
train$newchildren <- as.integer(newchildren)

# Number of missing values for "children" after imputation dropped to 51
chld.nbr.mis.new <- sum(is.na(train$newchildren))
chld.nbr.mis.new

# Make histogram of the number of children 
par(mfrow=c(1,2)) 
hist(train$children,
     breaks = 5,
     xlab = "number of children",
     main = "Histogram of children (before imputation)",
     col = "dark green") 
hist(train$newchildren,
     breaks = 5,
     xlab = "number of children",
     main = "Histogram of children (after imputation)",
     col = "dark blue")

tbl.old = table(train$children) 
tbl.new = table(train$newchildren) 
diff.tbl = tbl.old - tbl.new
diff.tbl

##############################################################
########  Appendix Reference: Imputing Children (End) ########
##############################################################

