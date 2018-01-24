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

# Since values of B are close to 1, price is not an endogenous as events do not affect price
# For higher B, variation in sales increases and this is explained by the price

# Question 1b
model0_0 <- lm(SALES0_0 ~ PRICE0, data = indexData)
model1_0 <- lm(SALES1_0 ~ PRICE0, data = indexData)
model5_0 <- lm(SALES5_0 ~ PRICE0, data = indexData)
model10_0 <- lm(SALES10_0 ~ PRICE0, data = indexData)

coefficientTable2 <- matrix(c(model0_0$coefficients[2], 
                              model1_0$coefficients[2], 
                              model5_0$coefficients[2], 
                              model10_0$coefficients[2],
                              summary(model0_0)$r.squared,
                              summary(model1_0)$r.squared,
                              summary(model5_0)$r.squared,
                              summary(model10_0)$r.squared),ncol=4,byrow=TRUE)

colnames(coefficientTable2) <- c("alpha=0", "alpha=1", "alpha=5", "alpha=10")
rownames(coefficientTable2) <- c("beta=0", "R-Squared")


