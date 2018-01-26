# Training Exercise 4.5
rm(list = ls())
library(dplyr)
library(AER)

setwd("~/Documents/2017/Coursera/Econometrics/Week 4")
gpaData <- read.table("TrainExer45.txt", header=TRUE, sep = ",")

# Question 1a
model1 <- lm(GPA ~ PARTICIPATION + GENDER, data = gpaData)
summary(model1)

# Question 1b
modelStageOne <- lm(PARTICIPATION ~ EMAIL + GENDER, data = gpaData)
model2SLS <- ivreg(GPA ~ GENDER + PARTICIPATION|EMAIL + GENDER, data = gpaData)
summary(modelStageOne)
summary(model2SLS)


# Question 1d
a = 1/(1000-3)