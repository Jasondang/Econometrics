# Training Exercise 4.4
rm(list = ls())
library(dplyr)
library(AER)
library(dynlm)
library(egcm)
library(ecm)

setwd("~/Documents/2017/Coursera/Econometrics/Week 6")
testData <- read.table("TrainExer64.csv", header=TRUE, sep = ",")


# Question 1
model1 <- dynlm(DX1 ~ lag(DX1,1) + lag(DX1,2) + lag(DX2,1) + lag(DX2,2), data = testData)
summary(model1, robust = T)

model2 <- dynlm(DX1 ~ lag(DX1,1) + lag(DX1,2), data = testData)
summary(model2, robust = T)

anova(model1, model2)

# As F value is less than 3.3 which is the critical value, we do not reject the null hypothesis

model3 <- dynlm(DX2 ~ lag(DX1,1) + lag(DX1,2) + lag(DX2,1) + lag(DX2,2), data = testData)
summary(model3, robust = T)

model4 <- dynlm(DX2 ~ lag(DX2,1) + lag(DX2,2), data = testData)
summary(model4, robust = T)

anova(model3, model4)

# As F value is greater than 3.3, we reject the null hypothesis
# Question 2i
modelADF1 <- dynlm(DX1 ~ lag(YEAR,1) + lag(DX1,1) + lag(X1,1), data = testData)
summary(modelADF1)

# As t value is greater than -3.5, X1 is not stationary

# Question 2ii
modelADF2 <- dynlm(DX2 ~ lag(YEAR,1) + lag(DX2,1) + lag(X2,1), data = testData)
summary(modelADF2)

# As t value is greather than -3.5, X2 is not stationary

# Question 3
modelEG1 <- lm(X2 ~ X1, data = testData)
summary(modelEG1)

testData$resi <- residuals(modelEG1)
testData$Dresi <- c(NA, diff(residuals(modelEG1)))

modelEG2 <- dynlm(Dresi ~ lag(resi,1) + lag(Dresi,1), data = testData)
summary(modelEG2)

# Since t value is less than the critical value which is -3.4, we reject that e is non stationary
# So X1 and X2 are cointegrated

# Question 4

xeq=testData[c('X1', 'X2')]
xtr=testData$DX1
xtr = testData['X1']

modelECM1 <- ecm(testData$DX1, xeq,xtr, includeIntercept = TRUE)

summary(modelECM1)


data(Wilshire)
#Use 2014-12-01 and earlier data to build models
trn <- Wilshire[Wilshire$date<='2014-12-01',]
#Assume all predictors are needed in the equilibrium and transient terms of ecm
xeq <- xtr <- trn[c('CorpProfits', 'FedFundsRate', 'UnempRate')]
model1 <- ecm(trn$Wilshire5000, xeq, xtr, includeIntercept=TRUE)
summary(model1)
#Assume CorpProfits and FedFundsRate are in the equilibrium term,
#UnempRate has only transient impacts
xeq <- trn[c('CorpProfits', 'FedFundsRate')]
xtr <- trn['UnempRate']
model2 <- ecm(trn$Wilshire5000, xeq, xtr, includeIntercept=TRUE)
summary(model2)
