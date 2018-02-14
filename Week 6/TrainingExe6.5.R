# Training Exercise 4.4
rm(list = ls())
library(dplyr)
library(AER)
library(dynlm)
library(egcm)
library(ecm)

setwd("~/Documents/2017/Coursera/Econometrics/Week 6")
testData <- read.table("TrainExer65.csv", header=TRUE, sep = ",")

# Question 1 
plot(testData$YEAR, testData$LOGIP, col="blue", type = 'l')
lines(testData$YEAR, testData$LOGCLI, col="red")

# May be cointegrated based on the graphs produced above. 

plot(testData$YEAR, testData$GIP, col="blue", type = 'l')
plot(testData$YEAR, testData$GCLI, col="red", type = 'l')

# Rather stationary

# Question 2a
modelADF1 <- dynlm(GIP ~ YEAR + lag(GIP,1) + lag(GIP,2) + lag(LOGIP,1), data = testData)
summary(modelADF1)

# Question 2b
modelADF2 <- dynlm(GCLI ~ lag(YEAR,1) + lag(GCLI,1) + lag(GCLI,2) + lag(LOGCLI,1), data = testData)
summary(modelADF2)


# Question 3a
modelEG1 <- dynlm(LOGIP ~ LOGCLI, data = testData)
summary(modelEG1)

testData$resi <- residuals(modelEG1)
testData$Dresi <- c(NA, diff(residuals(modelEG1)))

modelEG2 <- dynlm(Dresi ~ lag(resi,1) + lag(Dresi,1) + lag(Dresi,2), data = testData)
summary(modelEG2)


