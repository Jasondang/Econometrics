# Test Exercise 4
rm(list = ls())
library(dplyr)
library(AER)

setwd("~/Documents/2017/Coursera/Econometrics/Week 4")
wageData <- read.table("Test_4.txt", header=TRUE, sep = ",")

wageData$experSquared <- (wageData$exper)^2
wageData$ageSquared <- (wageData$age)^2

olsModel <- lm(logw ~ educ + exper + experSquared + smsa + south, data = wageData)
olsModel$coefficients
summary(olsModel)


model1Stage <- lm(educ ~ age + ageSquared + nearc + daded + momed, data = wageData)
model1Stage$coefficients
summary(model1Stage)

model2SLS <- ivreg(logw ~ educ + exper + experSquared + smsa + south | age + ageSquared + nearc + daded + momed + smsa + south, data = wageData )
summary(model2SLS, diagnostics = TRUE)

qchisq(0.05, df=5, lower.tail = FALSE)

