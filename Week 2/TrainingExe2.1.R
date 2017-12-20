# Training Exercise 2.1

rm(list = ls())

setwd("~/Documents/2017/Coursera/Econometrics/Week 2")
wageData <- read.table("Dataset2.txt", header=TRUE)

# Question 1
model_1 <- lm(wageData$LogWage~wageData$Female)
summary(model_1)

# Question 2
wageData$Resi <- wageData$LogWage - (4.73-0.25*(wageData$Female))

model_2 <- lm(wageData$Resi~wageData$Edu) 
model_3 <- lm(wageData$Resi~wageData$Parttime)
summary(model_2)
summary(model_3)

# Question 3
#For every unit increase of education and part time, there is an expected wage increase of 22% and 10% respectively