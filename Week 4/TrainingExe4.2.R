# Training Exercise 4.2
rm(list = ls())
library(dplyr)

setwd("~/Documents/2017/Coursera/Econometrics/Week 4")
indexData <- read.table("TrainExer42.txt", header=TRUE, sep = ",")

# Question 1a
model1 <- lm(SALES0_0 ~ PRICE0, data = indexData)
summary(model1)
