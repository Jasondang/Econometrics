# Training Exercise 3.1
rm(list = ls())
library(dplyr)

setwd("~/Documents/2017/Coursera/Econometrics/Week 3")
indexData <- read.table("TrainExer 3-1-corrected.txt", header=TRUE)

# Question 1

indexData <- indexData %>%
  mutate(Log_Diff = c(0, diff(log(Index))))
model_1 <- lm(Log_Diff ~ BookMarket, data = indexData)
summary(model_1)


# Question 2

model_2 <- lm(Index ~ BookMarket, data = indexData)
summary(model_2)

# Question 3

res_1 <- resid(model_1)
plot(indexData$Year, res_1, xlab="Year", ylab="Residuals", main="Log Residual", type="l")
abline(0,0)

res_2 <- resid(model_2)
plot(indexData$Year, res_2, xlab="Year", ylab="Residuals", main="Residual", type="l")
abline(0,0)
