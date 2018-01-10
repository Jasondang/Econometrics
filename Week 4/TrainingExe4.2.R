# Training Exercise 4.2
rm(list = ls())
library(dplyr)

setwd("~/Documents/2017/Coursera/Econometrics/Week 4")
indexData <- read.table("TrainExer42.txt", header=TRUE, sep = ",")

# Question 1a
model0_0 <- lm(SALES0_0 ~ PRICE0, data = indexData)
model0_1 <- lm(SALES0_1 ~ PRICE1, data = indexData)
model0_5 <- lm(SALES0_5 ~ PRICE5, data = indexData)
model0_10 <- lm(SALES0_10 ~ PRICE10, data = indexData)

coefficientTable <- matrix(c(model0_0$coefficients[2], 
                             model0_1$coefficients[2], 
                             model0_5$coefficients[2], 
                             model0_10$coefficients[2],
                             summary(model0_0)$r.squared,
                             summary(model0_1)$r.squared,
                             summary(model0_5)$r.squared,
                             summary(model0_10)$r.squared),ncol=4,byrow=TRUE)

colnames(coefficientTable) <- c("beta=0", "beta=1", "beta=5", "beta=10")
rownames(coefficientTable) <- c("alpha=0", "R-Squared")





