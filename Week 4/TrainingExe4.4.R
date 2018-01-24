# Training Exercise 4.4
rm(list = ls())
library(dplyr)
library(AER)

setwd("~/Documents/2017/Coursera/Econometrics/Week 4")
gasData <- read.table("TrainExer44.txt", header=TRUE, sep = ",")

# Question 1a
# High demand of gasoline directly affects the price of gasoline

# Question 1b
model1 <- ivreg(GC ~ PG + RI| RI + RPT + RPN + RPU, data = gasData)

# Question 1c
qchisq(0.05, df=2, lower.tail = FALSE)
summary(mode1, diagnostics = TRUE)

#Since Sargon test statistic is less than 5% of chi square distribution, cannot reject null hypothesis