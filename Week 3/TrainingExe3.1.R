# Training Exercise 3.1
rm(list = ls())
library(dplyr)

setwd("~/Documents/2017/Coursera/Econometrics/Week 3")
indexData <- read.table("TrainExer 3-1-corrected.txt", header=TRUE)

# Question 1

indexData <- indexData %>%
  mutate(Diff = c(0, diff(log(Index))))
model_1 <- lm(Diff ~ BookMarket, data = indexData)
summary(model_1)
