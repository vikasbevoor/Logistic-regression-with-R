crd <- read.csv(file.choose())
View(crd)
is.na(crd)

crd1 <- crd[,-1]
View(crd1)
attach(crd1)
summary(crd1)
str(crd1)

install.packages("moments")
library(moments)

#Graphical exploration
hist(age)
summary(age)
skewness(age)
kurtosis(age)
qqnorm(age)
qqline(age)

hist(income)
summary(income)
skewness(income)
kurtosis(income)
qqnorm(income)
qqline(income)

install.packages("caTools")
library(caTools)

split <- sample.split(crd1, SplitRatio = .8)
split
training <- subset(crd1, split == "TRUE")
testing <- subset(crd1, split == "FALSE")

# Creating the logistic model
model <- glm(card~.,data=training ,family = "binomial")
summary(model)

m1 <-  glm(card~.-expenditure ,data=training ,family = "binomial")
summary(m1)

m2 <-  glm(card~.-owner ,data=training ,family = "binomial")
summary(m2)

m3 <-  glm(card~.-selfemp ,data=training ,family = "binomial")
summary(m3)

m4 <-  glm(card~.-months ,data=training ,family = "binomial")
summary(m4)

m5 <-  glm(card~.-selfemp-months ,data=training ,family = "binomial")
summary(m5)

# Final model
model.final <-  glm(card~.-selfemp-months ,data=training ,family = "binomial")
summary(model.final)

#Implementing the final model
res <- predict(model.final, testing, type = "response")

# Confusion matrix table 
conf <- table(Actualvalue = testing$card, PredictedValue = res>0.5)
conf

#Accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy

# To optimize the threshold values
rest <- predict(model.final, training, type = "response")

library(ROCR)
rocrpred<-prediction(rest,training$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,print.cutoffs.at = seq(0.1,by=0.1))

#with lower threshold value
# Confusion matrix table 
conf <- table(Actualvalue = testing$card, PredictedValue = res>0.3)
conf
#Accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy

#with higher threshold value
# Confusion matrix table 
conf <- table(Actualvalue = testing$card, PredictedValue = res>0.7)
conf
#Accuracy
Accuracy<-sum(diag(conf)/sum(conf))
Accuracy

