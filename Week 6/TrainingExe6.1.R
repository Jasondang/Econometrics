# Training Exercise 4.4
rm(list = ls())
library(dplyr)
library(AER)
library(dynlm)

setwd("~/Documents/2017/Coursera/Econometrics/Week 6")
testData <- read.table("TrainExer61.csv", header=TRUE, sep = ",")

plot(testData$X)
plot(testData$Y)
plot(testData$X, testData$Y)

# Question 2
modelCor <- lm(EPSY ~ EPSX, data = testData)
summary(modelCor)

# Question 3
modelLag <- dynlm(EPSY ~ EPSX + lag(EPSY,3) + lag(EPSX,3) + lag(EPSY,2) + lag(EPSX,2) + lag(EPSY,1) + lag(EPSX,1), data = testData)
summary(modelLag)

modelFTest <- lm(EPSY ~ 0, data = testData)
summary(modelFTest)

# Question 4
model4 <- lm(Y ~ X, data=testData)
summary(model4)

# Question 5
resiData <- as.data.frame(residuals(model4))

model5 <- dynlm(resiData$`residuals(model4)` ~ lag(resiData$`residuals(model4)`,1))
summary(model5)
