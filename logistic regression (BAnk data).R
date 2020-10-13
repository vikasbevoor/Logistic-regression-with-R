bank <- read.csv(file.choose())# Choose the claimants Data set
View(bank)
is.na(bank)

attach(bank)

install.packages("moments")
library(moments)

summary(bank)
str(bank)

#Graphical exploration
hist(job)
summary(age)
skewness(age)
kurtosis(age)
qqnorm(age)
qqline(age)

install.packages("caTools")
library(caTools)

#Splitting data into training and testing
split <- sample.split(bank, SplitRatio = .8)

training <- subset(bank, split == "TRUE")
testing <- subset(bank, split == "FALSE")

# Creating the logistic model
model <- glm(y~.,data=training,family = "binomial")
summary(model)

#Removing individual insignificant variables and checking
m2 <- glm(y~.-age,data=training,family = "binomial")
summary(m2)

m3 <- glm(y~.-default,data=training,family = "binomial")
summary(m3)

m4 <- glm(y~.-pdays,data=training,family = "binomial")
summary(m4)

m5 <- glm(y~.-previous,data=training,family = "binomial")
summary(m5)

# Final model
model.final <- glm(y~.-default-age-pdays,data=training,family = "binomial")
summary(model.final)

#Implementing the final model
res <- predict(model.final, testing, type = "response")

# Confusion matrix table 
conf <- table(Actualvalue = testing$y, PredictedValue = res>0.5)
conf

#Accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy

# To optimize the threshold values
rest <- predict(model.final, training, type = "response")

library(ROCR)
rocrpred<-prediction(rest,training$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,print.cutoffs.at = seq(0.1,by=0.1))

#with lower threshold value
# Confusion matrix table 
conf <- table(Actualvalue = testing$y, PredictedValue = res>0.3)
conf
#Accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy

#with higher threshold value
# Confusion matrix table 
conf <- table(Actualvalue = testing$y, PredictedValue = res>0.7)
conf
#Accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy



