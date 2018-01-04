# Training Exercise 3.5
rm(list = ls())
library(dplyr)

setwd("~/Documents/2017/Coursera/Econometrics/Week 3")
indexData <- read.table("TrainExer 3-5-corrected.txt", header=TRUE)

# Question 1a
model1 <- lm(LogEqPrem ~ BookMarket + DivPrice + EarnPrice + Inflation + NTIS, data = indexData)
summary(model1)

model2 <- lm(LogEqPrem ~ BookMarket, data = indexData)
summary(model2)

r1squared = summary(model1)$r.squared
r2squared = summary(model2)$r.squared
n = 87
k = 6

a = (r1squared - r2squared)/4
b = (1-r1squared)/(n-k)

f_test = a/b

pf(q=1, df1=4, df2=81, lower.tail=FALSE)

# Question 1b
model3 <- lm(LogEqPrem ~ BookMarket, data = indexData)
summary(model3)

# Sum of residuals for first regression model
sumRes1 <- sum(model3$residuals^2)

indexData <- indexData %>%
  mutate(logEqReg = c((model3$coefficients[1] + model3$coefficients[2] * BookMarket)^2))

model4 <- lm(LogEqPrem ~ BookMarket + logEqReg, data = indexData)
summary(model4)
sumRes2 <- sum(model4$residuals^2)

# F-Test Calculation
g = 1
n = 87
k = 3
a2 = (sumRes1-sumRes2)/g
b2 = (sumRes2)/(n-k)

fTest2 = a2/b2

pf(q=fTest2, df1=g, df2=n-k, lower.tail=FALSE)


# Question 1c
model5 <- lm(LogEqPrem ~ BookMarket, data = indexData)
sumRes3 <- sum(model5$residuals^2)

# Sub Sample pre 1980
sampleDataPre <- indexData[indexData$Year < 1980,]
model6 <- lm(LogEqPrem ~ BookMarket, data = sampleDataPre)
sumRes4 <- sum(model6$residuals^2)

# Sub Sample post 1980
sampleDataPost <- indexData[indexData$Year >= 1980,]
model7 <- lm(LogEqPrem ~ BookMarket, data = sampleDataPost)
sumRes5 <- sum(model7$residuals^2)

# F-Test
s0 = sumRes3
s1 = sumRes4
s2 = sumRes5

k = 2
n = 87

a3 = (s0 - (s1 + s2))/k
b3 = (s1 + s2)/(n-(2*k))

fTest3 = a3/b3

pf(q=fTest3, df1=k, df2=(n-(2*k)), lower.tail=FALSE)

# Question 1d
# Chow forecast statistic
n2 = 34
n1 = 53
a4 = (s0 - s1)/n2
b4 = s1/(n1-k)

fTest4 = a4/b4
pf(q=fTest4, df1=n2, df2=n1-k, lower.tail=FALSE)
