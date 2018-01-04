# Training Exercise 3.1
rm(list = ls())
library(dplyr)

setwd("~/Documents/2017/Coursera/Econometrics/Week 3")
indexData <- read.table("TrainExer 3-3-corrected.txt", header=TRUE)

# Question 1b

indexData <- indexData %>%
  mutate(Log_Diff = c(0, diff(log(Index))))
indexData <- indexData %>%
  mutate(Index_Square = c(BookMarket*BookMarket))
model_1 <- lm(Log_Diff ~ BookMarket + Index_Square, data = indexData)
summary(model_1)


# Question 1c
indexData$DummyVar[indexData$Year >=1980] = 1
indexData$DummyVar[indexData$Year <1980] = 0
indexData <- indexData %>%
  mutate(Dummy = c(DummyVar*BookMarket))
model_2 <- lm(Log_Diff ~ BookMarket + BookMarket * DummyVar, data = indexData)
summary(model_2)


model_2 <- lm(Log_Diff ~ BookMarket + Dummy, data = indexData)
summary(model_2)


