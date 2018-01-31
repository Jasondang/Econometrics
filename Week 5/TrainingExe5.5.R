# Training Exercise 5.5
rm(list = ls())
library(dplyr)
library(AER)

setwd("~/Documents/2017/Coursera/Econometrics/Week 5")
responseData <- read.csv("Data Lecture 5-5.csv", header=TRUE, sep =",")

responseData$age2 <- (responseData$age/10)^2

model <- glm(response ~ male + activity + age + age2, family = binomial(link="logit"), data = responseData)
summary(model,  diagnostics = TRUE)
logLik(model)

responseData$response <- -responseData$response + 1

model <- glm(response ~ male + activity + age + age2, family = binomial(link="logit"), data = responseData)
summary(model,  diagnostics = TRUE)
l1 <- logLik(model)[1]

model2 <- glm(response ~ age + age2, family = binomial(link="logit"), data = responseData)
summary(model2,  diagnostics = TRUE)
l0 <- logLik(model2)[1]


lr <- -2*(l0-l1)
qchisq(0.05, df=2, lower.tail = FALSE)
