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
