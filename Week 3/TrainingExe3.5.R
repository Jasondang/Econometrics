# Training Exercise 3.1
rm(list = ls())
library(dplyr)

setwd("~/Documents/2017/Coursera/Econometrics/Week 3")
indexData <- read.table("TrainExer 3-5-corrected.txt", header=TRUE)

# Question 1a
model_1 <- lm(LogEqPrem ~ BookMarket + DivPrice + EarnPrice + Inflation + NTIS, data = indexData)
summary(model_1)

model_2 <- lm(LogEqPrem ~ BookMarket, data = indexData)
summary(model_2)

r1squared = summary(model_1)$r.squared
r2squared = summary(model_2)$r.squared
n = 87
k = 6

a = (r1squared - r2squared)/4
b = (1-r1squared)/(n-k)

f_test = a/b

pf(q=1, df1=4, df2=81, lower.tail=FALSE)

# Question 1b
model_2 <- lm(LogEqPrem ~ BookMarket, data = indexData)
summary(model_2)


# Question 1c
indexData$DummyVar[indexData$Year >=1980] = 1
indexData$DummyVar[indexData$Year <1980] = 0
indexData <- indexData %>%
  mutate(Dummy = c(DummyVar*BookMarket))
model_2 <- lm(Log_Diff ~ BookMarket + Dummy, data = indexData)
summary(model_2)


